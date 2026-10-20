# ============================================================
# incremental_fetch.R — Stepwise SIR modelling (Confirmed Only)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(lubridate)
  library(readr); library(jsonlite); library(deSolve)
})

options(stringsAsFactors = FALSE)

POPULATION_AT_RISK <- 20000000
PROJECTION_DAYS    <- 14
TRAIN_FRACTION     <- 0.80

# ---------- STEP 1 & 2: DATA PREP & WRANGLING ----------
message("STEP 1 & 2 — Data Preparation")
input_path <- if (file.exists("downloaded_timeseries.csv")) "downloaded_timeseries.csv" else "data_cache.csv"
if (!file.exists(input_path)) stop("No input file found.")

raw <- readr::read_csv(input_path, show_col_types = FALSE)
names(raw) <- tolower(names(raw))

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
    new_infections = c(0, pmax(0, diff(confirmed)))
  )

n_obs <- nrow(outbreak_df)
last_date <- max(outbreak_df$date)
message("  Latest date in data: ", last_date)
readr::write_csv(outbreak_df, "data_cache.csv")

# ---------- STEP 3: SIR MODEL DEFINITION ----------
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

# ---------- STEP 4: TRAINING (Fit to Cumulative Cases) ----------
message("STEP 4 — Training SIR Model via optim()")
train_n   <- floor(n_obs * TRAIN_FRACTION)
train_idx <- 1:train_n
train_obs <- outbreak_df$confirmed[train_idx]
train_t   <- 0:(train_n - 1)

# Objective: minimize SSE between observed cumulative cases and model (I+R+D)
sir_objective <- function(par) {
  beta  <- exp(par[1])
  gamma <- exp(par[2])
  i0    <- exp(par[3])
  mu    <- 0.05 # Fixed mortality
  
  state0 <- c(S = POPULATION_AT_RISK - i0, I = i0, R = 0, D = 0)
  out <- tryCatch(
    as.data.frame(ode(state0, train_t, sir_model, c(beta=beta, gamma=gamma, mu=mu))),
    error = function(e) NULL
  )
  if (is.null(out)) return(1e12)
  
  predicted_cum <- out$I + out$R + out$D
  sum((predicted_cum - train_obs)^2)
}

# Initial guess: beta=0.5, gamma=0.1, I0=10
fit <- optim(
  par = log(c(0.5, 0.1, 10)),
  fn = sir_objective,
  method = "L-BFGS-B",
  lower = log(c(0.01, 0.01, 1)),
  upper = log(c(5.0, 1.0, 1000)),
  control = list(maxit = 1000)
)

beta_est  <- exp(fit$par[1])
gamma_est <- exp(fit$par[2])
I0_fit    <- exp(fit$par[3])
mu_est    <- 0.05
R0_est    <- beta_est / (gamma_est + mu_est)

message(sprintf("  Fitted: beta=%.4f, gamma=%.4f, R0=%.3f", beta_est, gamma_est, R0_est))

# ---------- STEP 5: TESTING & FULL FIT ----------
message("STEP 5 — Evaluating on Full Window")
full_t <- 0:(n_obs - 1)
state0_full <- c(S = POPULATION_AT_RISK - I0_fit, I = I0_fit, R = 0, D = 0)
sir_full <- as.data.frame(ode(state0_full, full_t, sir_model, c(beta=beta_est, gamma=gamma_est, mu=mu_est)))

predicted_I <- sir_full$I + sir_full$R + sir_full$D
observed_I  <- outbreak_df$confirmed

# Performance
perf_metrics <- function(obs, pred) {
  res <- obs - pred
  list(
    rmse = sqrt(mean(res^2, na.rm = TRUE)),
    mae  = mean(abs(res), na.rm = TRUE),
    r2   = 1 - sum(res^2, na.rm = TRUE) / sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
  )
}
train_perf <- perf_metrics(observed_I[1:train_n], predicted_I[1:train_n])
test_perf  <- perf_metrics(observed_I[(train_n+1):n_obs], predicted_I[(train_n+1):n_obs])
overall    <- perf_metrics(observed_I, predicted_I)

# ---------- STEP 6: PROJECTION (Anchored to Last Observed State) ----------
message("STEP 6 — 14-Day Projection")
proj_t <- n_obs - 1 + 0:PROJECTION_DAYS
last_row <- tail(sir_full, 1)
# Anchor to observed state so projection starts where data ends
anchor_state <- c(
  S = POPULATION_AT_RISK - tail(outbreak_df$confirmed, 1),
  I = tail(outbreak_df$active_cases, 1),
  R = tail(outbreak_df$recovered, 1),
  D = tail(outbreak_df$confirmed_deaths, 1)
)
sir_proj <- as.data.frame(ode(anchor_state, proj_t, sir_model, c(beta=beta_est, gamma=gamma_est, mu=mu_est)))
proj_cum <- sir_proj$I + sir_proj$R + sir_proj$D

# ---------- STEP 7: EXPORT JSON ----------
latest   <- tail(outbreak_df, 1)
previous <- tail(outbreak_df, 2)[1,]

fmt_delta <- function(cur, prev, unit = "") {
  if (is.na(prev) || is.na(cur)) return("--")
  d <- cur - prev
  paste0(ifelse(d >= 0, "+", ""), d, if (nzchar(unit)) paste0(" ", unit) else "", " today")
}

case_telemetry <- list(
  list(label = "Confirmed Cases",    value = latest$confirmed, unit = "", delta = fmt_delta(latest$confirmed, previous$confirmed), trend = ifelse(latest$confirmed >= previous$confirmed, "up", "down")),
  list(label = "Confirmed Deaths",   value = latest$confirmed_deaths, unit = "", delta = fmt_delta(latest$confirmed_deaths, previous$confirmed_deaths), trend = ifelse(latest$confirmed_deaths >= previous$confirmed_deaths, "up", "down")),
  list(label = "Recovered",          value = latest$recovered, unit = "", delta = fmt_delta(latest$recovered, previous$recovered), trend = ifelse(latest$recovered >= previous$recovered, "up", "down")),
  list(label = "Active Cases",       value = latest$active_cases, unit = "", delta = fmt_delta(latest$active_cases, previous$active_cases), trend = ifelse(latest$active_cases >= previous$active_cases, "up", "down")),
  list(label = "Case Fatality Rate", value = round(latest$cfr, 1), unit = "%", delta = fmt_delta(latest$cfr, previous$cfr, "pts"), trend = ifelse(latest$cfr >= previous$cfr, "up", "down"))
)

bio_coefficients <- list(
  list(label = "beta (Transmission Rate)", value = round(beta_est, 4), unit = "/day", delta = "Fitted via NLS"),
  list(label = "gamma (Recovery Rate)",    value = round(gamma_est, 4), unit = "/day", delta = "Fitted via NLS"),
  list(label = "mu (Mortality Rate)",      value = round(mu_est, 4), unit = "/day", delta = "Fixed at 0.05"),
  list(label = "R0 (Basic Reproduction)",  value = round(R0_est, 3), unit = "", delta = "beta / (gamma + mu)"),
  list(label = "Growth Rate (r)",          value = round(beta_est - gamma_est - mu_est, 4), unit = "/day", delta = "beta - gamma - mu")
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
  projection_dates     = format(last_date + 1:PROJECTION_DAYS, "%Y-%m-%d"),
  projection_I         = proj_cum[-1],
  projection_cum       = cumsum(c(tail(cumsum(predicted_I), 1), proj_cum[-1]))[-1],
  sir_I                = sir_full$I[obs_idx],
  sir_R                = sir_full$R[obs_idx],
  sir_D                = sir_full$D[obs_idx],
  coeff_beta           = rep(beta_est, length(obs_idx)),
  coeff_gamma          = rep(gamma_est, length(obs_idx)),
  coeff_mu             = rep(mu_est, length(obs_idx))
)

n_pts <- length(chart_series$months)
zones <- list(
  list(from = 0, to = floor(n_pts * TRAIN_FRACTION), label = "Training", color = "rgba(39,174,96,0.15)"),
  list(from = floor(n_pts * TRAIN_FRACTION), to = n_pts - 1, label = "Test", color = "rgba(230,126,34,0.15)"),
  list(from = n_pts - 1, to = n_pts + PROJECTION_DAYS, label = "Projection", color = "rgba(255,255,255,0.4)")
)

sliders <- list(
  list(id = "beta", label = "Transmission Rate (beta)", min = 0.05, max = 1.5, step = 0.01, value = round(beta_est, 2)),
  list(id = "gamma", label = "Recovery Rate (gamma)", min = 0.02, max = 0.5, step = 0.01, value = round(gamma_est, 2)),
  list(id = "mu", label = "Mortality Rate (mu)", min = 0.0, max = 0.5, step = 0.01, value = round(mu_est, 2)),
  list(id = "zoonotic", label = "Zoonotic Spillover", min = 0, max = 50, step = 1, value = 12),
  list(id = "interv", label = "Intervention Efficacy", min = 0, max = 100, step = 1, value = 35)
)

payload <- list(
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  last_data_date = format(last_date, "%Y-%m-%d"),
  population_at_risk = POPULATION_AT_RISK,
  projection_days = PROJECTION_DAYS,
  primary_parameters = list(beta = beta_est, gamma = gamma_est, mu = mu_est, R0 = R0_est, growth_rate = beta_est - gamma_est - mu_est),
  performance = list(train = train_perf, test = test_perf, overall = overall),
  case_telemetry = case_telemetry,
  bio_coefficients = bio_coefficients,
  chart_series = chart_series,
  zones = zones,
  sliders = sliders
)

if (!dir.exists("data")) dir.create("data", recursive = TRUE)
jsonlite::write_json(payload, "data/ebola_data.json", auto_unbox = TRUE, pretty = TRUE, null = "null")
message("Wrote data/ebola_data.json — last date: ", payload$last_data_date)
