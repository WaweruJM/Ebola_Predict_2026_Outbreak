# ============================================================
# incremental_fetch.R — stepwise SIR modelling, confirmed only
# Refactored to use optim() for NLS fitting.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(lubridate)
  library(readr); library(jsonlite); library(deSolve)
})

options(stringsAsFactors = FALSE)

POPULATION_AT_RISK <- 20000000
PROJECTION_DAYS    <- 14
TRAIN_FRACTION     <- 0.80

# ---------- STEP 1 — DATA PREPARATION ----------
message("STEP 1 — Data preparation")
input_path <- if (file.exists("downloaded_timeseries.csv")) {
  "downloaded_timeseries.csv"
} else if (file.exists("data_cache.csv")) {
  "data_cache.csv"
} else stop("No input timeseries file found.")

raw <- readr::read_csv(input_path, show_col_types = FALSE)
names(raw) <- tolower(names(raw))

# ---------- STEP 2 — DATA WRANGLING (confirmed only) ----------
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
    cfr            = ifelse(confirmed > 0, 100 * confirmed_deaths / confirmed, NA_real_),
    new_infections = c(0, pmax(0, diff(confirmed))),
    new_deaths     = c(0, pmax(0, diff(confirmed_deaths)))
  )

n_obs <- nrow(outbreak_df)
readr::write_csv(outbreak_df, "data_cache.csv")

train_n     <- floor(n_obs * TRAIN_FRACTION)
test_n      <- n_obs - train_n
train_times <- seq(0, train_n - 1, by = 1)
full_times  <- seq(0, n_obs - 1, by = 1)

# ---------- STEP 3 & 4 — NLS MODEL TRAINING ----------
message("STEP 3 & 4 — Training SIR via Non-Linear Least Squares")

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

# Objective function for optim()
# We predict the cumulative infected (I + R + D) and compare to `confirmed`.
sir_objective <- function(par, times, data) {
  # Ensure parameters are positive
  beta  <- exp(par[1])
  gamma <- exp(par[2])
  i0    <- exp(par[3])
  mu    <- 0.05 # fixed mortality
  
  state0 <- c(S = POPULATION_AT_RISK - i0, I = i0, R = 0, D = 0)
  out <- tryCatch({
    as.data.frame(ode(state0, times, sir_model, c(beta=beta, gamma=gamma, mu=mu)))
  }, error = function(e) NULL)
  
  if (is.null(out)) return(1e12)
  
  predicted_cumulative <- out$I + out$R + out$D
  
  # Sum of squared errors, normalized to avoid huge numbers
  sum((predicted_cumulative - data)^2) / length(data)
}

# Initial guesses: beta=0.5, gamma=0.1, I0=10
init_par <- log(c(0.5, 0.1, 10))
train_data <- outbreak_df$confirmed[1:train_n]

fit <- optim(
  par = init_par, 
  fn = sir_objective, 
  times = train_times, 
  data = train_data,
  method = "L-BFGS-B",
  control = list(maxit = 1000)
)

# Extract optimized parameters
beta_est  <- exp(fit$par[1])
gamma_est <- exp(fit$par[2])
I0        <- exp(fit$par[3])
mu_est    <- 0.05
R0_est    <- beta_est / (gamma_est + mu_est)
growth_rate <- beta_est - gamma_est - mu_est

message(sprintf("  Optimized beta=%.4f, gamma=%.4f, I0=%.1f, R0=%.3f",
                beta_est, gamma_est, I0, R0_est))

# ---------- STEP 5 — FULL WINDOW & TEST ----------
message("STEP 5 — Evaluating on full window")

state0 <- c(S = POPULATION_AT_RISK - I0, I = I0, R = 0, D = 0)
params <- c(beta = beta_est, gamma = gamma_est, mu = mu_est)

sir_full <- as.data.frame(ode(state0, full_times, sir_model, params))

# Predicted cumulative cases = I + R + D
predicted_I <- sir_full$I + sir_full$R + sir_full$D
observed_I  <- outbreak_df$confirmed

train_mask <- seq_len(train_n)
test_mask  <- seq(train_n + 1, n_obs)

# ---------- STEP 6 — PERFORMANCE ----------
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

# ---------- STEP 7 — PROJECTION ----------
proj_times <- seq(n_obs - 1, n_obs - 1 + PROJECTION_DAYS, by = 1)
last_row   <- tail(sir_full, 1)
proj_state <- setNames(as.numeric(last_row[, c("S","I","R","D")]), c("S","I","R","D"))
sir_proj   <- as.data.frame(ode(proj_state, proj_times, sir_model, params))

# Predicted cumulative for projection
proj_cum <- sir_proj$I + sir_proj$R + sir_proj$D

# ---------- Cards (5 per row) ----------
latest   <- outbreak_df %>% slice_tail(n = 1)
previous <- outbreak_df %>% slice_tail(n = 2) %>% slice_head(n = 1)

fmt_delta <- function(cur, prev, unit = "") {
  if (length(prev) == 0 || is.na(prev) || is.na(cur)) return("--")
  d <- cur - prev
  paste0(ifelse(d >= 0, "+", ""), d, if (nzchar(unit)) paste0(" ", unit) else "", " today")
}
trend_of <- function(cur, prev) {
  if (length(prev) == 0 || is.na(cur) || is.na(prev)) return("")
  if (cur > prev) "up" else if (cur < prev) "down" else ""
}

case_telemetry <- list(
  list(label = "Confirmed Cases",     value = latest$confirmed, unit = "", delta = fmt_delta(latest$confirmed, previous$confirmed), trend = trend_of(latest$confirmed, previous$confirmed)),
  list(label = "Confirmed Deaths",    value = latest$confirmed_deaths, unit = "", delta = fmt_delta(latest$confirmed_deaths, previous$confirmed_deaths), trend = trend_of(latest$confirmed_deaths, previous$confirmed_deaths)),
  list(label = "Recovered",           value = latest$recovered, unit = "", delta = fmt_delta(latest$recovered, previous$recovered), trend = trend_of(latest$recovered, previous$recovered)),
  list(label = "Active Cases",        value = latest$active_cases, unit = "", delta = fmt_delta(latest$active_cases, previous$active_cases), trend = trend_of(latest$active_cases, previous$active_cases)),
  list(label = "Case Fatality Rate",  value = round(latest$cfr, 1), unit = "%", delta = fmt_delta(latest$cfr, previous$cfr, "pts"), trend = trend_of(latest$cfr, previous$cfr))
)

bio_coefficients <- list(
  list(label = "beta (Transmission Rate)", value = round(beta_est, 4), unit = "/day", delta = "NLS fitted"),
  list(label = "gamma (Recovery Rate)",    value = round(gamma_est, 4), unit = "/day", delta = "NLS fitted"),
  list(label = "mu (Mortality Rate)",      value = round(mu_est, 4), unit = "/day", delta = "Fixed"),
  list(label = "R0 (Basic Reproduction)",  value = round(R0_est, 3), unit = "", delta = "beta / (gamma + mu)"),
  list(label = "Growth Rate (r)",          value = round(growth_rate, 4), unit = "/day", delta = "beta - gamma - mu")
)

# ---------- Chart series ----------
step_n  <- max(1, floor(n_obs / 40))
obs_idx <- unique(c(seq(1, n_obs, by = step_n), n_obs))
ts      <- outbreak_df[obs_idx, ]

chart_series <- list(
  months               = format(ts$date, "%Y-%m-%d"),
  confirmed_observed   = ts$confirmed,
  deaths_observed      = ts$confirmed_deaths,
  recovered_observed   = ts$recovered,
  seir_fitted_I        = predicted_I[obs_idx],
  projection_dates     = format(max(outbreak_df$date) + seq_len(PROJECTION_DAYS), "%Y-%m-%d"),
  projection_I         = proj_cum[-1],
  projection_cum       = cumsum(c(tail(cumsum(predicted_I), 1), proj_cum[-1]))[-1],
  cumulative_confirmed = cumsum(outbreak_df$new_infections)[obs_idx],
  rt_series            = rep(R0_est, length(obs_idx)),
  # SIR Compartments for the chart
  sir_I = sir_full$I[obs_idx],
  sir_R = sir_full$R[obs_idx],
  sir_D = sir_full$D[obs_idx],
  # Coefficients
  coeff_beta  = rep(beta_est, length(obs_idx)),
  coeff_gamma = rep(gamma_est, length(obs_idx)),
  coeff_mu    = rep(mu_est, length(obs_idx)),
  train_perf  = train_perf,
  test_perf   = test_perf,
  overall     = overall
)

n_pts <- length(chart_series$months)
zones <- list(
  list(from = 0, to = max(1, floor(n_pts * TRAIN_FRACTION)), label = "Training window", color = "rgba(39,174,96,0.18)"),
  list(from = max(1, floor(n_pts * TRAIN_FRACTION)), to = n_pts - 1, label = "Test window", color = "rgba(230,126,34,0.20)"),
  list(from = n_pts - 1, to = n_pts + length(chart_series$projection_dates), label = "Future prediction", color = "rgba(255,255,255,0.55)")
)

sliders <- list(
  list(id = "beta", label = "Transmission Rate (beta)", min = 0.05, max = 1.5, step = 0.01, value = round(beta_est, 2)),
  list(id = "gamma", label = "Recovery Rate (gamma)", min = 0.02, max = 0.5, step = 0.01, value = round(gamma_est, 2)),
  list(id = "mu", label = "Mortality Rate (mu)", min = 0.0, max = 0.5, step = 0.01, value = round(mu_est, 2)),
  list(id = "zoonotic", label = "Zoonotic Spillover", min = 0, max = 50, step = 1, value = 12),
  list(id = "interv", label = "Intervention Efficacy", min = 0, max = 100, step = 1, value = 35)
)

payload <- list(
  generated_at       = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  population_at_risk = POPULATION_AT_RISK,
  projection_days    = PROJECTION_DAYS,
  primary_parameters = list(beta = beta_est, gamma = gamma_est, mu = mu_est, R0 = R0_est, growth_rate = growth_rate, train_n = train_n, test_n = test_n),
  performance        = list(train = train_perf, test = test_perf, overall = overall),
  case_telemetry     = case_telemetry,
  bio_coefficients   = bio_coefficients,
  chart_series       = chart_series,
  zones              = zones,
  sliders            = sliders
)

if (!dir.exists("data")) dir.create("data", recursive = TRUE)
jsonlite::write_json(payload, "data/ebola_data.json", auto_unbox = TRUE, pretty = TRUE, null = "null")
message("Wrote data/ebola_data.json")
