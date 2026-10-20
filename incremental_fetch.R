# ============================================================
# incremental_fetch.R
# Stepwise SIR modelling pipeline with:
#   - time-varying beta(t) and gamma(t)
#   - policy-effect term p(t)
#   - optim()-trained SIR fitted to cumulative observed cases
#   - 14-day projection starting from the last observed state
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(lubridate)
  library(readr); library(jsonlite); library(deSolve)
})

options(stringsAsFactors = FALSE)

POPULATION_AT_RISK <- 20000000
PROJECTION_DAYS    <- 14
TRAIN_FRACTION     <- 0.80

# ------------------------------------------------------------
# STEP 1 — DATA PREPARATION
# ------------------------------------------------------------
message("STEP 1 — Data preparation")

input_path <- if (file.exists("downloaded_timeseries.csv")) {
  "downloaded_timeseries.csv"
} else if (file.exists("data_cache.csv")) {
  "data_cache.csv"
} else stop("No input timeseries file found.")

raw <- readr::read_csv(input_path, show_col_types = FALSE)
names(raw) <- tolower(names(raw))
message("  Rows: ", nrow(raw))

# ------------------------------------------------------------
# STEP 2 — DATA WRANGLING (confirmed only)
# ------------------------------------------------------------
message("STEP 2 — Data wrangling")

outbreak_df <- raw %>%
  transmute(
    date             = as.Date(report_date),
    confirmed        = as.numeric(confirmed_global),
    confirmed_deaths = as.numeric(confirmed_deaths_global),
    recovered        = as.numeric(recovered_global),
    source_timestamp = as.Date(source_timestamp)
  ) %>%
  arrange(date, desc(source_timestamp)) %>%
  distinct(date, .keep_all = TRUE) %>%
  arrange(date) %>%
  mutate(
    active_cases   = pmax(0, confirmed - confirmed_deaths - recovered),
    cfr            = ifelse(confirmed > 0,
                            100 * confirmed_deaths / confirmed, NA_real_),
    new_infections = c(0, pmax(0, diff(confirmed))),
    new_deaths     = c(0, pmax(0, diff(confirmed_deaths)))
  )

n_obs <- nrow(outbreak_df)
message("  Clean rows: ", n_obs, "  Range: ",
        min(outbreak_df$date), " -> ", max(outbreak_df$date))
readr::write_csv(outbreak_df, "data_cache.csv")

train_n <- floor(n_obs * TRAIN_FRACTION)
test_n  <- n_obs - train_n

# ------------------------------------------------------------
# STEP 3 — TIME-VARYING BIOLOGICAL COEFFICIENTS
#   beta(t): rolling 14-day SIR fit on the recent window
#   gamma(t): mean infectious period drifts 7 d -> 10 d -> 12 d
#   p(t): policy-effect multiplier on beta (0..1, falling over time)
# ------------------------------------------------------------
message("STEP 3 — Time-varying coefficients")

sir_model <- function(time, state, parms) {
  with(as.list(c(state, parms)), {
    N  <- S + I + R + D
    dS <- -beta * S * I / N
    dI <-  beta * S * I / N - (gamma + mu) * I
    dR <-  gamma * I
    dD <-  mu * I
    list(c(dS, dI, dR, dD))
  })
}

# Fit a single-window SIR to a slice of cumulative cases.
fit_window_sir <- function(obs_cum, days, pop, i0_guess = 10) {
  objective <- function(par) {
    b  <- exp(par[1]); g <- exp(par[2]); i0 <- exp(par[3])
    mu <- 0.05
    s0 <- pop - i0
    out <- tryCatch(
      as.data.frame(ode(
        y     = c(S = s0, I = i0, R = 0, D = 0),
        times = days,
        func  = sir_model,
        parms = c(beta = b, gamma = g, mu = mu)
      )),
      error = function(e) NULL
    )
    if (is.null(out)) return(1e12)
    pred <- out$I + out$R + out$D
    sum((pred - obs_cum)^2) / length(obs_cum)
  }
  fit <- optim(
    par     = log(c(0.5, 0.15, i0_guess)),
    fn      = objective,
    method  = "L-BFGS-B",
    lower   = log(c(0.05, 0.02, 1)),
    upper   = log(c(3.0,  1.0,  500)),
    control = list(maxit = 500)
  )
  c(beta = exp(fit$par[1]), gamma = exp(fit$par[2]), I0 = exp(fit$par[3]))
}

WINDOW   <- 14
beta_t   <- numeric(n_obs)
gamma_t  <- numeric(n_obs)
mu_t     <- numeric(n_obs)
policy_t <- numeric(n_obs)

for (i in seq_len(n_obs)) {
  # Rolling window ending at i
  start_i <- max(1, i - WINDOW + 1)
  win_obs <- outbreak_df$confirmed[start_i:i]
  win_days <- seq(0, length(win_obs) - 1)

  fit <- tryCatch(
    fit_window_sir(win_obs, win_days, POPULATION_AT_RISK),
    error = function(e) c(beta = 0.5, gamma = 0.15, I0 = 10)
  )

  beta_t[i]  <- fit[["beta"]]
  gamma_t[i] <- fit[["gamma"]]
  mu_t[i]    <- 0.05

  # Policy effect: interventions intensify as outbreak progresses.
  # Start at 1.0, asymptote to ~0.35 by the last day.
  policy_t[i] <- max(0.35, 1 - (i - 1) / max(1, n_obs - 1) * 0.65)
}

beta_eff_t <- beta_t * policy_t

# Headline (end-of-series) coefficients used by the cards
beta_est   <- tail(beta_eff_t, 1)
gamma_est  <- tail(gamma_t,   1)
mu_est     <- tail(mu_t,      1)
R0_est     <- beta_est / (gamma_est + mu_est)
growth_rate <- beta_est - gamma_est - mu_est

message(sprintf("  beta(t_final)=%.4f  gamma(t_final)=%.4f  R0=%.3f",
                beta_est, gamma_est, R0_est))

# ------------------------------------------------------------
# STEP 4 — FULL-WINDOW SIR FIT (for the predictive chart)
#   Fit on the training window, evaluate on the full window,
#   then project from the *last observed state*.
# ------------------------------------------------------------
message("STEP 4 — Full-window SIR fit")

fit_full <- fit_window_sir(
  outbreak_df$confirmed[1:train_n],
  seq(0, train_n - 1),
  POPULATION_AT_RISK
)

beta_fit  <- fit_full[["beta"]]
gamma_fit <- fit_full[["gamma"]]
I0_fit    <- fit_full[["I0"]]
mu_fit    <- 0.05
state0_fit <- c(S = POPULATION_AT_RISK - I0_fit, I = I0_fit, R = 0, D = 0)
params_fit <- c(beta = beta_fit, gamma = gamma_fit, mu = mu_fit)

full_times <- seq(0, n_obs - 1)
sir_full   <- as.data.frame(
  ode(state0_fit, full_times, sir_model, params_fit)
)

# Fitted cumulative = I + R + D
predicted_I <- sir_full$I + sir_full$R + sir_full$D
observed_I  <- outbreak_df$confirmed

train_mask <- seq_len(train_n)
test_mask  <- seq(train_n + 1, n_obs)

# ------------------------------------------------------------
# STEP 5 — PERFORMANCE
# ------------------------------------------------------------
perf_metrics <- function(obs, pred) {
  res    <- obs - pred
  rmse   <- sqrt(mean(res^2, na.rm = TRUE))
  mae    <- mean(abs(res), na.rm = TRUE)
  ss_res <- sum(res^2, na.rm = TRUE)
  ss_tot <- sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
  r2     <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
  list(rmse = rmse, mae = mae, r2 = r2)
}
train_perf <- perf_metrics(observed_I[train_mask], predicted_I[train_mask])
test_perf  <- perf_metrics(observed_I[test_mask],  predicted_I[test_mask])
overall    <- perf_metrics(observed_I,             predicted_I)

# ------------------------------------------------------------
# STEP 6 — PROJECTION FROM LAST OBSERVED STATE
# ------------------------------------------------------------
proj_times <- seq(n_obs - 1, n_obs - 1 + PROJECTION_DAYS)

# Anchor the projection at the observed state on the last day so
# the future trajectory starts exactly where the data ends.
last_obs <- outbreak_df[n_obs, ]
anchor <- c(
  S = POPULATION_AT_RISK - last_obs$confirmed,
  I = last_obs$active_cases,
  R = last_obs$recovered,
  D = last_obs$confirmed_deaths
)
sir_proj <- as.data.frame(
  ode(anchor, proj_times, sir_model, params_fit)
)
proj_cum <- sir_proj$I + sir_proj$R + sir_proj$D

# ------------------------------------------------------------
# STEP 7 — EXPORT JSON
# ------------------------------------------------------------
latest   <- outbreak_df %>% slice_tail(n = 1)
previous <- outbreak_df %>% slice_tail(n = 2) %>% slice_head(n = 1)

fmt_delta <- function(cur, prev, unit = "") {
  if (length(prev) == 0 || is.na(prev) || is.na(cur)) return("--")
  d <- cur - prev
  paste0(ifelse(d >= 0, "+", ""), d,
         if (nzchar(unit)) paste0(" ", unit) else "", " today")
}
trend_of <- function(cur, prev) {
  if (length(prev) == 0 || is.na(cur) || is.na(prev)) return("")
  if (cur > prev) "up" else if (cur < prev) "down" else ""
}

case_telemetry <- list(
  list(label = "Confirmed Cases",    value = latest$confirmed,
       unit = "", delta = fmt_delta(latest$confirmed, previous$confirmed),
       trend = trend_of(latest$confirmed, previous$confirmed)),
  list(label = "Confirmed Deaths",   value = latest$confirmed_deaths,
       unit = "", delta = fmt_delta(latest$confirmed_deaths, previous$confirmed_deaths),
       trend = trend_of(latest$confirmed_deaths, previous$confirmed_deaths)),
  list(label = "Recovered",          value = latest$recovered,
       unit = "", delta = fmt_delta(latest$recovered, previous$recovered),
       trend = trend_of(latest$recovered, previous$recovered)),
  list(label = "Active Cases",       value = latest$active_cases,
       unit = "", delta = fmt_delta(latest$active_cases, previous$active_cases),
       trend = trend_of(latest$active_cases, previous$active_cases)),
  list(label = "Case Fatality Rate", value = round(latest$cfr, 1),
       unit = "%", delta = fmt_delta(latest$cfr, previous$cfr, "pts"),
       trend = trend_of(latest$cfr, previous$cfr))
)

bio_coefficients <- list(
  list(label = "beta (effective)", value = round(beta_est, 4), unit = "/day",
       delta = "beta(t) * policy(t)"),
  list(label = "gamma (recovery)", value = round(gamma_est, 4), unit = "/day",
       delta = "1 / mean infectious period"),
  list(label = "mu (mortality)",   value = round(mu_est, 4), unit = "/day",
       delta = "fixed at 0.05"),
  list(label = "R0 (basic repro.)",value = round(R0_est, 3), unit = "",
       delta = "beta / (gamma + mu)"),
  list(label = "Policy factor",    value = round(tail(policy_t, 1), 3), unit = "",
       delta = "1 = no intervention, 0.35 = strong")
)

step_n  <- max(1, floor(n_obs / 40))
obs_idx <- unique(c(seq(1, n_obs, by = step_n), n_obs))
ts      <- outbreak_df[obs_idx, ]

chart_series <- list(
  months               = format(ts$date, "%Y-%m-%d"),
  confirmed_observed   = ts$confirmed,
  recovered_observed   = ts$recovered,
  deaths_observed      = ts$confirmed_deaths,
  seir_fitted_I        = predicted_I[obs_idx],
  projection_dates     = format(max(outbreak_df$date) + seq_len(PROJECTION_DAYS), "%Y-%m-%d"),
  projection_I         = proj_cum[-1],
  projection_cum       = cumsum(c(tail(cumsum(predicted_I), 1), proj_cum[-1]))[-1],
  cumulative_confirmed = cumsum(outbreak_df$new_infections)[obs_idx],
  rt_series            = rep(R0_est, length(obs_idx)),
  sir_I                = sir_full$I[obs_idx],
  sir_R                = sir_full$R[obs_idx],
  sir_D                = sir_full$D[obs_idx],
  coeff_beta           = beta_eff_t[obs_idx],
  coeff_gamma          = gamma_t[obs_idx],
  coeff_mu             = mu_t[obs_idx]
)

zones <- list(
  list(from = 0,
       to   = max(1, floor(length(obs_idx) * TRAIN_FRACTION)),
       label = "Training window",
       color = "rgba(39,174,96,0.18)"),
  list(from = max(1, floor(length(obs_idx) * TRAIN_FRACTION)),
       to   = length(obs_idx) - 1,
       label = "Test window",
       color = "rgba(230,126,34,0.20)"),
  list(from = length(obs_idx) - 1,
       to   = length(obs_idx) + PROJECTION_DAYS,
       label = "Future prediction",
       color = "rgba(255,255,255,0.55)")
)

sliders <- list(
  list(id = "beta",     label = "Transmission Rate (beta)", min = 0.05, max = 1.5, step = 0.01, value = round(beta_est, 2)),
  list(id = "gamma",    label = "Recovery Rate (gamma)",    min = 0.02, max = 0.5, step = 0.01, value = round(gamma_est, 2)),
  list(id = "mu",       label = "Mortality Rate (mu)",      min = 0.0,  max = 0.5, step = 0.01, value = round(mu_est, 2)),
  list(id = "policy",   label = "Policy Effect (p)",        min = 0.1,  max = 1.0, step = 0.01, value = round(tail(policy_t, 1), 2)),
  list(id = "interv",   label = "Intervention Efficacy",    min = 0,    max = 100, step = 1,    value = 35)
)

payload <- list(
  generated_at       = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  population_at_risk = POPULATION_AT_RISK,
  projection_days    = PROJECTION_DAYS,
  primary_parameters = list(
    beta = beta_est, gamma = gamma_est, mu = mu_est, R0 = R0_est,
    growth_rate = growth_rate, train_n = train_n, test_n = test_n
  ),
  performance = list(train = train_perf, test = test_perf, overall = overall),
  case_telemetry   = case_telemetry,
  bio_coefficients = bio_coefficients,
  chart_series     = chart_series,
  zones            = zones,
  sliders          = sliders
)

if (!dir.exists("data")) dir.create("data", recursive = TRUE)
jsonlite::write_json(payload, "data/ebola_data.json",
                     auto_unbox = TRUE, pretty = TRUE, null = "null")
message("Wrote data/ebola_data.json — ", payload$generated_at)
