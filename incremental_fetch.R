# ============================================================
# incremental_fetch.R
# Stepwise SIR outbreak modelling pipeline
#   STEP 1  Data preparation
#   STEP 2  Data wrangling
#   STEP 3  Computation of primary parameters (from raw data)
#   STEP 4  Model generation — training set
#   STEP 5  Model testing — holdout set
#   STEP 6  Model performance analysis
#   STEP 7  Projection (14 days) + export JSON for the portal
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(lubridate)
  library(readr); library(jsonlite); library(deSolve)
})

options(stringsAsFactors = FALSE)

POPULATION_AT_RISK <- 20000000   # 20 million
PROJECTION_DAYS   <- 14          # next two weeks
TRAIN_FRACTION    <- 0.80        # 80 % train / 20 % test

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

message("  Rows: ", nrow(raw), "  Columns: ", paste(names(raw), collapse = ", "))

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
    active_cases = pmax(0, total - deaths - recovered),
    cfr          = ifelse(total > 0, 100 * deaths / total, NA_real_)
  )

# Daily incidence (new cases per day) — the series the SIR model fits
outbreak_df <- outbreak_df %>%
  mutate(
    new_infections = c(0, pmax(0, diff(total))),
    new_deaths     = c(0, pmax(0, diff(deaths)))
  )

n_obs <- nrow(outbreak_df)
message("  Clean rows: ", n_obs, "  Range: ", min(outbreak_df$date),
        " → ", max(outbreak_df$date))

readr::write_csv(outbreak_df, "data_cache.csv")

# ============================================================
# STEP 3 — COMPUTATION OF PRIMARY PARAMETERS FROM RAW DATA
# ============================================================
message("STEP 3 — Computing primary parameters from raw data")

# --- 3a. Cumulative counts
N_total      <- POPULATION_AT_RISK
S0           <- N_total - outbreak_df$total[1]
I0           <- outbreak_df$total[1]
R0_comp      <- outbreak_df$recovered[1]

# --- 3b. Effective contact rate (β) from early exponential growth
#     I(t) ≈ I0 * exp(r * t)  →  r from log-linear fit on the first
#     portion of the incidence curve.
early_n      <- max(5, floor(n_obs * 0.30))
early_idx    <- seq_len(early_n)
early_inc    <- pmax(outbreak_df$total[early_idx], 1)

growth_fit   <- tryCatch(
  lm(log(early_inc) ~ early_idx),
  error = function(e) NULL
)
growth_rate  <- if (!is.null(growth_fit)) coef(growth_fit)[2] else 0.10
# Ensure a sensible positive growth rate
growth_rate  <- max(growth_rate, 0.01)

# --- 3c. Recovery rate (γ) from mean infectious period
#     Literature (WHO Ebola): mean infectious period ≈ 10 days
infectious_period_days <- 10
gamma_est              <- 1 / infectious_period_days

# --- 3d. Mortality rate (μ) from observed CFR
cfr_latest   <- tail(outbreak_df$cfr[!is.na(outbreak_df$cfr)], 1)
mu_est       <- cfr_latest / 100   # convert % → fraction

# --- 3e. Transmission rate (β) — SIR identity at early phase
#     r = β - γ - μ   →   β = r + γ + μ
beta_est     <- growth_rate + gamma_est + mu_est

# --- 3f. Basic reproduction number R0 = β / (γ + μ)
R0_est       <- beta_est / (gamma_est + mu_est)

params <- c(beta = beta_est, gamma = gamma_est, mu = mu_est)

message(sprintf("  growth rate r    = %.4f /day", growth_rate))
message(sprintf("  beta (β)         = %.4f /day", beta_est))
message(sprintf("  gamma (γ)        = %.4f /day (1/%.0f d)", gamma_est, infectious_period_days))
message(sprintf("  mu (μ)           = %.4f /day (CFR = %.1f%%)", mu_est, cfr_latest))
message(sprintf("  R0 = β/(γ+μ)     = %.3f", R0_est))

# ============================================================
# STEP 4 — MODEL GENERATION: TRAIN / TEST SPLIT
# ============================================================
message("STEP 4 — Training the SIR model")

# SIR with mortality:
#   dS/dt = -β S I / N
#   dI/dt =  β S I / N - (γ + μ) I
#   dR/dt =  γ I
#   dD/dt =  μ I
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

state0 <- c(S = S0, I = I0, R = R0_comp, D = outbreak_df$deaths[1])

# Train on the first TRAIN_FRACTION of the observations
train_n     <- floor(n_obs * TRAIN_FRACTION)
train_days  <- train_n - 1
train_times <- seq(0, train_days, by = 1)

sir_train <- as.data.frame(
  ode(y = state0, times = train_times, func = sir_model, parms = params)
)

# ============================================================
# STEP 5 — MODEL TESTING ON HOLDOUT
# ============================================================
message("STEP 5 — Testing the SIR model on holdout")

# Continue the simulation over the full observed window; the last
# (n_obs − train_n) points form the test window.
test_n       <- n_obs - train_n
full_days    <- n_obs - 1
full_times   <- seq(0, full_days, by = 1)

sir_full <- as.data.frame(
  ode(y = state0, times = full_times, func = sir_model, parms = params)
)

# Align predictions with observations (observed window)
predicted_I  <- sir_full$I[seq_len(n_obs)]
observed_I   <- outbreak_df$total
train_mask   <- seq_len(train_n)
test_mask    <- seq(train_n + 1, n_obs)

# ============================================================
# STEP 6 — MODEL PERFORMANCE ANALYSIS
# ============================================================
message("STEP 6 — Performance metrics")

perf_metrics <- function(obs, pred) {
  res  <- obs - pred
  rmse <- sqrt(mean(res^2, na.rm = TRUE))
  mae  <- mean(abs(res), na.rm = TRUE)
  # R²
  ss_res <- sum(res^2, na.rm = TRUE)
  ss_tot <- sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
  r2     <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
  list(rmse = rmse, mae = mae, r2 = r2)
}

train_perf <- perf_metrics(observed_I[train_mask], predicted_I[train_mask])
test_perf  <- perf_metrics(observed_I[test_mask],  predicted_I[test_mask])
overall    <- perf_metrics(observed_I,             predicted_I)

message(sprintf("  TRAIN  RMSE=%.1f  MAE=%.1f  R²=%.3f",
                train_perf$rmse, train_perf$mae, train_perf$r2))
message(sprintf("  TEST   RMSE=%.1f  MAE=%.1f  R²=%.3f",
                test_perf$rmse,  test_perf$mae,  test_perf$r2))
message(sprintf("  OVERALL RMSE=%.1f  MAE=%.1f  R²=%.3f",
                overall$rmse, overall$mae, overall$r2))

# ============================================================
# STEP 7 — PROJECTION (14 days) + EXPORT JSON
# ============================================================
message("STEP 7 — 14-day projection + JSON export")

last_row   <- tail(sir_full, 1)
proj_state <- setNames(
  as.numeric(last_row[, c("S", "I", "R", "D")]),
  c("S", "I", "R", "D")
)

sir_proj <- as.data.frame(
  ode(y = proj_state, times = proj_times, func = sir_model, parms = params)
)

# ---------- Card telemetry — five per row ----------
latest   <- outbreak_df %>% slice_tail(n = 1)
previous <- outbreak_df %>% slice_tail(n = 2) %>% slice_head(n = 1)

fmt_delta <- function(cur, prev, unit = "") {
  if (length(prev) == 0 || is.na(prev) || is.na(cur)) return("—")
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
       unit = "%", delta = paste0(fmt_delta(latest$cfr, previous$cfr), " (pts)"),
       trend = trend_of(latest$cfr, previous$cfr))
)

bio_coefficients <- list(
  list(label = "β (Transmission Rate)",   value = round(beta_est, 4), unit = "/day",
       delta = sprintf("β = r + γ + μ = %.4f + %.4f + %.4f", growth_rate, gamma_est, mu_est)),
  list(label = "γ (Recovery Rate)",       value = round(gamma_est, 4), unit = "/day",
       delta = sprintf("γ = 1/%d days infectious period", infectious_period_days)),
  list(label = "μ (Mortality Rate)",      value = round(mu_est, 4), unit = "/day",
       delta = sprintf("μ = CFR = %.1f%%", cfr_latest)),
  list(label = "R₀ (Basic Reproduction)", value = round(R0_est, 3), unit = "",
       delta = sprintf("R₀ = β/(γ+μ) = %.4f / %.4f", beta_est, gamma_est + mu_est)),
  list(label = "Growth Rate (r)",         value = round(growth_rate, 4), unit = "/day",
       delta = "Log-linear fit on early incidence")
)

# ---------- Chart series ----------
step_n  <- max(1, floor(n_obs / 40))
obs_idx <- unique(c(seq(1, n_obs, by = step_n), n_obs))
ts      <- outbreak_df[obs_idx, ]

chart_series <- list(
  months                = format(ts$date, "%Y-%m-%d"),
  suspected_observed    = ts$suspected,
  confirmed_observed    = ts$confirmed,
  total_observed        = ts$total,
  deaths_observed       = ts$deaths,
  seir_fitted_I         = predicted_I[obs_idx],   # SIR I compartment
  seir_fitted_cum       = cumsum(predicted_I)[obs_idx],
  projection_dates      = format(max(outbreak_df$date) + seq_len(PROJECTION_DAYS), "%Y-%m-%d"),
  projection_I          = sir_proj$I[-1],
  projection_cum        = cumsum(c(tail(cumsum(predicted_I), 1), sir_proj$I[-1]))[-1],
  rt                    = rep(R0_est, length(obs_idx)),
  rt_series             = rep(R0_est, length(obs_idx)),
  cumulative_confirmed  = cumsum(outbreak_df$new_infections)[obs_idx],
  cumulative_projected  = NA_real_,
  zoonotic_events       = rep(NA_real_, length(obs_idx)),
  # SIR compartments for the dedicated SIR chart
  sir_S = sir_full$S[obs_idx],
  sir_I = sir_full$I[obs_idx],
  sir_R = sir_full$R[obs_idx],
  sir_D = sir_full$D[obs_idx],
  # Parameter lines (dotted) for the SIR chart
  param_beta  = beta_est,
  param_gamma = gamma_est,
  param_mu    = mu_est
)

# ---------- Zones (dark grey / light grey / white) ----------
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

# ---------- Sliders ----------
sliders <- list(
  list(id = "beta",     label = "Transmission Rate (β)", min = 0.05, max = 1.0, step = 0.01, value = round(beta_est, 2)),
  list(id = "gamma",    label = "Recovery Rate (γ)",     min = 0.02, max = 0.5, step = 0.01, value = round(gamma_est, 2)),
  list(id = "mu",       label = "Mortality Rate (μ)",    min = 0.0,  max = 0.5, step = 0.01, value = round(mu_est, 2)),
  list(id = "zoonotic", label = "Zoonotic Spillover",    min = 0, max = 50, step = 1, value = 12),
  list(id = "interv",   label = "Intervention Efficacy", min = 0, max = 100, step = 1, value = 35)
)

# ---------- Export ----------
payload <- list(
  generated_at      = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  population_at_risk = POPULATION_AT_RISK,
  projection_days    = PROJECTION_DAYS,
  primary_parameters = list(
    beta        = beta_est,
    gamma       = gamma_est,
    mu          = mu_est,
    R0          = R0_est,
    growth_rate = growth_rate,
    train_n     = train_n,
    test_n      = test_n
  ),
  performance = list(
    train   = train_perf,
    test    = test_perf,
    overall = overall
  ),
  case_telemetry   = case_telemetry,
  bio_coefficients = bio_coefficients,
  chart_series     = chart_series,
  zones            = zones,
  sliders          = sliders
)

if (!dir.exists("data")) dir.create("data", recursive = TRUE)
jsonlite::write_json(payload, "data/ebola_data.json",
                     auto_unbox = TRUE, pretty = TRUE, null = "null")
message("✔ Wrote data/ebola_data.json at ", payload$generated_at)
