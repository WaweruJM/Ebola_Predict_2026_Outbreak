# ============================================================
# incremental_fetch.R — stepwise SIR modelling, confirmed only
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
message("STEP 2 — Data wrangling (confirmed only)")

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

# ---------- STEP 3 — PRIMARY PARAMETERS ----------
message("STEP 3 — Primary parameters")

N_total <- POPULATION_AT_RISK
S0      <- N_total - outbreak_df$confirmed[1]
I0      <- max(1, outbreak_df$active_cases[1])
R0_comp <- outbreak_df$recovered[1]
D0      <- outbreak_df$confirmed_deaths[1]

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

infectious_period_days <- 10
gamma_est              <- 1 / infectious_period_days
cfr_latest             <- tail(outbreak_df$cfr[!is.na(outbreak_df$cfr)], 1)
mu_est                 <- (cfr_latest / 100) / infectious_period_days

beta_est <- growth_rate + gamma_est + mu_est
R0_est   <- beta_est / (gamma_est + mu_est)
params   <- c(beta = beta_est, gamma = gamma_est, mu = mu_est)

message(sprintf("  r=%.4f  beta=%.4f  gamma=%.4f  mu=%.4f  R0=%.3f",
                growth_rate, beta_est, gamma_est, mu_est, R0_est))

# ---------- STEP 4 — SIR MODEL + TRAIN ----------
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
test_n      <- n_obs - train_n
train_times <- seq(0, train_n - 1, by = 1)
sir_train   <- as.data.frame(ode(state0, train_times, sir_model, params))

# ---------- STEP 5 — FULL WINDOW ----------
full_days  <- n_obs - 1
full_times <- seq(0, full_days, by = 1)
sir_full   <- as.data.frame(ode(state0, full_times, sir_model, params))

predicted_I <- sir_full$I[seq_len(n_obs)]
observed_I  <- outbreak_df$confirmed
train_mask  <- seq_len(train_n)
test_mask   <- seq(train_n + 1, n_obs)

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
proj_times <- seq(full_days, full_days + PROJECTION_DAYS, by = 1)
last_row   <- tail(sir_full, 1)
proj_state <- setNames(as.numeric(last_row[, c("S","I","R","D")]),
                       c("S","I","R","D"))
sir_proj <- as.data.frame(ode(proj_state, proj_times, sir_model, params))

# ---------- Cards (5 per row) ----------
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
  list(label = "Confirmed Cases",     value = latest$confirmed,
       unit = "", delta = fmt_delta(latest$confirmed, previous$confirmed),
       trend = trend_of(latest$confirmed, previous$confirmed)),
  list(label = "Confirmed Deaths",    value = latest$confirmed_deaths,
       unit = "", delta = fmt_delta(latest$confirmed_deaths, previous$confirmed_deaths),
       trend = trend_of(latest$confirmed_deaths, previous$confirmed_deaths)),
  list(label = "Recovered",           value = latest$recovered,
       unit = "", delta = fmt_delta(latest$recovered, previous$recovered),
       trend = trend_of(latest$recovered, previous$recovered)),
  list(label = "Active Cases",        value = latest$active_cases,
       unit = "", delta = fmt_delta(latest$active_cases, previous$active_cases),
       trend = trend_of(latest$active_cases, previous$active_cases)),
  list(label = "Case Fatality Rate",  value = round(latest$cfr, 1),
       unit = "%", delta = fmt_delta(latest$cfr
