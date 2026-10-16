# ============================================================
# incremental_fetch.R — stepwise SIR modelling pipeline
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(lubridate)
  library(readr); library(jsonlite); library(deSolve)
})

options(stringsAsFactors = FALSE)

POPULATION_AT_RISK <- 20000000
PROJECTION_DAYS    <- 14
TRAIN_FRACTION     <- 0.80

# ============================================================
# STEP 1 — DATA PREPARATION
# ============================================================
message("STEP 1 — Data preparation")

input_path <- if (file.exists("downloaded_timeseries.csv")) {
  "downloaded_timeseries.csv"
} else if (file.exists("data_cache.csv")) {
  "data_cache.csv"
} else stop("No input timeseries file found.")

raw <- readr::read_csv(input_path, show_col_types = FALSE)
names(raw) <- tolower(names(raw))
message("  Rows: ", nrow(raw))

# ============================================================
# STEP 2 — DATA WRANGLING
# ============================================================
message("STEP 2 — Data wrangling")

outbreak_df <- raw %>%
  transmute(
    date             = as.Date(report_date),
    suspected        = as.numeric(suspected_global),
    confirmed        = as.numeric(confirmed_global),
    total            = as.numeric(total_global),
    suspected_deaths = as.numeric(suspected_deaths_global),
    confirmed_deaths = as.numeric(confirmed_deaths_global),
    deaths           = suspected_deaths + confirmed_deaths,
    recovered        = as.numeric(recovered_global),
    primary_source   = primary_source,
    source_timestamp = as.Date(source_timestamp)
  ) %>%
  arrange(date, desc(source_timestamp)) %>%
  distinct(date, .keep_all = TRUE) %>%
  arrange(date) %>%
  mutate(
    active_cases   = pmax(0, total - deaths - recovered),
    cfr            = ifelse(total > 0, 100 * deaths / total, NA_real_),
    new_infections = c(0, pmax(0, diff(total))),
    new_deaths     = c(0, pmax(0, diff(deaths)))
  )

n_obs <- nrow(outbreak_df)
message("  Clean rows: ", n_obs, "  Range: ",
        min(outbreak_df$date), " -> ", max(outbreak_df$date))

readr::write_csv(outbreak_df, "data_cache.csv")

# ============================================================
# STEP 3 — PRIMARY PARAMETERS FROM RAW DATA
# ============================================================
message("STEP 3 — Computing primary parameters")

N_total <- POPULATION_AT_RISK
S0      <- N_total - outbreak_df$total[1]
I0      <- max(1, outbreak_df$active_cases[1])
R0_comp <- outbreak_df$recovered[1]
D0      <- outbreak_df$deaths[1]

# Growth rate from daily incidence
early_n   <- max(5, floor(n_obs * 0.30))
early_idx <- seq_len(early_n)
early_inc <- outbreak_df$new_infections[early_idx]
pos       <- early_inc > 0

if (sum(pos) >= 4) {
  df_fit      <- data.frame(t = early_idx[pos], y = log(early_inc[pos]))
  growth_fit  <- lm(y ~ t, data = df_fit)
  growth_rate <- max(coef(growth_fit)[2], 0.005)
} else {
  growth_rate <- 0.05
}

# Recovery and mortality rates
infectious_period_days <- 10
gamma_est              <- 1 / infectious_period_days
cfr_latest             <- tail(outbreak_df$cfr[!is.na(outbreak_df$cfr)], 1)
mu_est                 <- (cfr_latest / 100) / infectious_period_days

# Transmission and reproduction
beta_est <- growth_rate + gamma_est + mu_est
R0_est   <- beta_est / (gamma_est + mu_est)

params <- c(beta = beta_est, gamma = gamma_est, mu = mu_est)

message(sprintf("  growth rate r = %.4f /day", growth_rate))
message(sprintf("  beta          = %.4f /day", beta_est))
message(sprintf("  gamma         = %.4f /day", gamma_est))
message(sprintf("  mu            = %.4f /day", mu_est))
message(sprintf("  R0            = %.3f", R0_est))

# ============================================================
# STEP 4 — SIR MODEL, TRAIN
# ============================================================
message("STEP 4 — Training SIR model")

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

state0 <- c(S = S0, I = I0, R = R0_comp, D = D0)

train_n     <- floor(n_obs * TRAIN_FRACTION)
train_times <- seq(0, train_n - 1, by = 1)
sir_train   <- as.data.frame(
  ode(y = state0, times = train_times, func = sir_model, parms = params)
)

# ============================================================
# STEP 5 — MODEL TESTING ON FULL WINDOW
# ============================================================
message("STEP 5 — Testing SIR model")

test_n     <- n_obs - train_n
full_days  <- n_obs - 1
full_times <- seq(0, full_days, by = 1)
sir_full   <- as.data.frame(
  ode(y = state0, times = full_times, func = sir_model, parms = params)
)

predicted_I <- sir_full$I[seq_len(n_obs)]
observed_I  <- outbreak_df$total
train_mask  <- seq_len(train_n)
test_mask   <- seq(train_n + 1, n_obs)

# ============================================================
# STEP 6 — PERFORMANCE
# ============================================================
message("STEP 6 — Performance metrics")

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

# ============================================================
# STEP 7 — PROJECTION + EXPORT
# ============================================================
message("STEP 7 — 14-day projection")

proj_times <- seq(full_days, full_days + PROJECTION_DAYS, by = 1)
last_row   <- tail(sir_full, 1)
proj_state <- setNames(
  as.numeric(last_row[, c("S", "I", "R", "D")]),
  c("S", "I", "R", "D")
)
sir_proj <- as.data.frame(
  ode(y = proj_state, times = proj_times, func = sir_model, parms = params)
)

# --- Cards (5 per row) ---
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
  list(label = "Suspected Cases",    value = latest$suspected,
       unit = "", delta = fmt_delta(latest$suspected, previous$suspected),
       trend = trend_of(latest$suspected, previous$suspected)),
  list(label = "Total Cases",        value = latest$total,
       unit = "", delta = fmt_delta(latest$total, previous$total),
       trend = trend_of(latest$total, previous$total)),
  list(label = "Total Deaths",       value = latest$deaths,
       unit = "", delta = fmt_delta(latest$deaths, previous$deaths),
       trend = trend_of(latest$deaths, previous$deaths)),
  list(label = "Case Fatality Rate", value = round(latest$cfr, 1),
       unit = "%", delta = fmt_delta(latest$cfr, previous$cfr, "pts"),
       trend = trend_of(latest$cfr, previous$cfr))
)

bio_coefficients <- list(
  list(label = "beta (Transmission Rate)", value = round(beta_est, 4), unit = "/day",
       delta = sprintf("beta = r + gamma + mu")),
  list(label = "gamma (Recovery Rate)",    value = round(gamma_est, 4), unit = "/day",
       delta = sprintf("gamma = 1 / %d days", infectious_period_days)),
  list(label = "mu (Mortality Rate)",      value = round(mu_est, 4), unit = "/day",
       delta = sprintf("mu = CFR / %d", infectious_period_days)),
  list(label = "R0 (Basic Reproduction)",  value = round(R0_est, 3), unit = "",
       delta = "R0 = beta / (gamma + mu)"),
  list(label = "Growth Rate (r)",          value = round(growth_rate, 4), unit = "/day",
       delta = "Log-linear fit on early incidence")
)

# --- Chart series ---
step_n  <- max(1, floor(n_obs / 40))
obs_idx <- unique(c(seq(1, n_obs, by = step_n), n_obs))
ts      <- outbreak_df[obs_idx, ]

chart_series <- list(
  months                = format(ts$date, "%Y-%m-%d"),
  suspected_observed    = ts$suspected,
  confirmed_observed    = ts$confirmed,
  total_observed        = ts$total,
  deaths_observed       = ts$deaths,
  seir_fitted_I         = predicted_I[obs_idx],
  seir_fitted_cum       = cumsum(predicted_I)[obs_idx],
  projection_dates      = format(max(outbreak_df$date) + seq_len(PROJECTION_DAYS), "%Y-%m-%d"),
  projection_I          = sir_proj$I[-1],
  projection_cum        = cumsum(c(tail(cumsum(predicted_I), 1), sir_proj$I[-1]))[-1],
  rt                    = rep(R0_est, length(obs_idx)),
  rt_series             = rep(R0_est, length(obs_idx)),
  cumulative_confirmed  = cumsum(outbreak_df$new_infections)[obs_idx],
  cumulative_projected  = NA_real_,
  zoonotic_events       = rep(NA_real_, length(obs_idx)),
  sir_S = sir_full$S[obs_idx],
  sir_I = sir_full$I[obs_idx],
  sir_R = sir_full$R[obs_idx],
  sir_D = sir_full$D[obs_idx],
  param_beta  = beta_est,
  param_gamma = gamma_est,
  param_mu    = mu_est
)

n_pts <- length(chart_series$months)
zones <- list(
  list(from = 0,
       to   = max(1, floor(n_pts * 0.15)),
       label = "Animal-to-Human (Retrospective)"),
  list(from = max(1, floor(n_pts * 0.15)),
       to   = n_pts - 1,
       label = "Observed Data"),
  list(from = n_pts - 1,
       to   = n_pts - 1,
       label = "Future Prediction")
)

sliders <- list(
  list(id = "beta",     label = "Transmission Rate (beta)", min = 0.05, max = 1.0, step = 0.01, value = round(beta_est, 2)),
  list(id = "gamma",    label = "Recovery Rate (gamma)",    min = 0.02, max = 0.5, step = 0.01, value = round(gamma_est, 2)),
  list(id = "mu",       label = "Mortality Rate (mu)",      min = 0.0,  max = 0.5, step = 0.01, value = round(mu_est, 2)),
  list(id = "zoonotic", label = "Zoonotic Spillover",       min = 0, max = 50, step = 1, value = 12),
  list(id = "interv",   label = "Intervention Efficacy",    min = 0, max = 100, step = 1, value = 35)
)

payload <- list(
  generated_at       = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  population_at_risk = POPULATION_AT_RISK,
  projection_days    = PROJECTION_DAYS,
  primary_parameters = list(
    beta = beta_est, gamma = gamma_est, mu = mu_est,
    R0 = R0_est, growth_rate = growth_rate,
    train_n = train_n, test_n = test_n
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
message("Wrote data/ebola_data.json")
