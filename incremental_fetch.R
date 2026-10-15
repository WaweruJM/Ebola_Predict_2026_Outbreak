# ============================================================
# incremental_fetch.R
# Incremental Ebola outbreak data sync + metrics + JSON export
# ============================================================

suppressPackageStartupMessages({
  library(httr); library(jsonlite); library(dplyr)
  library(tidyr); library(lubridate); library(readr)
})

options(stringsAsFactors = FALSE)

# ---------- 1. INGEST ----------
input_path <- if (file.exists("downloaded_timeseries.csv")) {
  "downloaded_timeseries.csv"
} else if (file.exists("data_cache.csv")) {
  "data_cache.csv"
} else {
  stop("No input timeseries file found.")
}

raw <- readr::read_csv(input_path, show_col_types = FALSE)

# Minimal schema enforcement — adapt names to your real columns.
outbreak_df <- raw %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

# Persist cache
readr::write_csv(outbreak_df, "data_cache.csv")

# ---------- 2. SMOOTHED / DERIVED QUANTITIES ----------
# (Your existing modelling code that produces `bio_coeffs` and
#  `timeseries_df` should live here. Placeholder below.)
if (!exists("bio_coeffs")) {
  bio_coeffs <- tibble::tribble(
    ~label,                        ~value, ~unit, ~ci,
    "R0 (Basic Reproduction)",      1.85,  "",    "95% CI: 1.62-2.08",
    "Rt (Effective Repro.)",        1.24,  "",    "95% CI: 1.08-1.41",
    "Incubation Period",            9.4,   "d",   "Mean, IQR 7-12 d",
    "Infectious Period",            7.8,   "d",   "Mean, IQR 5-10 d",
    "Serial Interval",              12.1,  "d",   "Mean, SD 4.3 d",
    "CFR (Modelled)",               68.2,  "%",   "Range 62-74%",
    "Doubling Time",                14.6,  "d",   "Early phase",
    "Generation Time",              15.3,  "d",   "Mean estimate"
  )
}

# Fallback timeseries projection
if (!exists("timeseries_df")) {
  timeseries_df <- tibble(
    month = format(seq(as.Date("2025-01-01"),
                       by = "month", length.out = 24), "%b %Y"),
    reported_cases       = c(2,5,9,18,34,62,105,168,242,318,392,455,510,548,570, rep(NA, 9)),
    projected_cases      = c(rep(NA, 14), 570,612,648,672,685,690,688,680,668),
    deaths               = c(1,3,6,12,24,44,74,118,170,224,276,320,358,384,398, rep(NA, 9)),
    rt                   = c(2.4,2.3,2.2,2.1,1.95,1.85,1.78,1.72,1.66,1.58,1.5,1.42,
                             1.35,1.28,1.22,1.18,1.15,1.12,1.10,1.09,1.08,1.07,1.06,1.05),
    rt_lower             = NA_real_, rt_upper = NA_real_,
    zoonotic_events      = c(4,6,8,10,12,11,9,7,6,5,4,3,3,2,2, rep(NA, 9)),
    cumulative_confirmed = c(2,7,16,34,68,130,235,403,645,963,1355,1810,2320,2868,3438, rep(NA, 9)),
    cumulative_projected = c(rep(NA, 14), 3438,4050,4698,5370,6055,6745,7433,8101,8731)
  )
}

# ---------- 3. CARD TELEMETRY ----------
latest   <- outbreak_df %>% slice_tail(n = 1)
previous <- outbreak_df %>% slice_tail(n = 2) %>% slice_head(n = 1)

fmt_delta <- function(cur, prev, unit = "") {
  if (is.na(prev) || is.na(cur)) return("")
  d <- cur - prev
  paste0(ifelse(d >= 0, "+", ""), d, if (unit != "") paste0(" ", unit) else "")
}
trend_of <- function(cur, prev) ifelse(!is.na(cur) && !is.na(prev) && cur >= prev, "up", "down")

case_telemetry <- list(
  list(label = "Confirmed Cases",         value = latest$confirmed_cases,
       unit = "", delta = paste(fmt_delta(latest$confirmed_cases, previous$confirmed_cases), "today"),
       trend = trend_of(latest$confirmed_cases, previous$confirmed_cases)),
  list(label = "Suspected Cases",         value = latest$suspected_cases,
       unit = "", delta = paste(fmt_delta(latest$suspected_cases, previous$suspected_cases), "today"),
       trend = trend_of(latest$suspected_cases, previous$suspected_cases)),
  list(label = "Total Deaths",            value = latest$deaths,
       unit = "", delta = paste(fmt_delta(latest$deaths, previous$deaths), "today"),
       trend = trend_of(latest$deaths, previous$deaths)),
  list(label = "Recovered",               value = latest$recovered,
       unit = "", delta = paste(fmt_delta(latest$recovered, previous$recovered), "today"),
       trend = trend_of(latest$recovered, previous$recovered)),
  list(label = "Active Cases",            value = latest$active_cases,
       unit = "", delta = paste(fmt_delta(latest$active_cases, previous$active_cases), "today"),
       trend = trend_of(latest$active_cases, previous$active_cases)),
  list(label = "Case Fatality Rate",      value = round(latest$cfr, 1),
       unit = "%", delta = paste(fmt_delta(latest$cfr, previous$cfr, "pts")),
       trend = trend_of(latest$cfr, previous$cfr)),
  list(label = "Affected Districts",      value = latest$affected_districts,
       unit = "", delta = paste(fmt_delta(latest$affected_districts, previous$affected_districts), "today"),
       trend = trend_of(latest$affected_districts, previous$affected_districts)),
  list(label = "Health Workers Infected", value = latest$health_workers_infected,
       unit = "", delta = paste(fmt_delta(latest$health_workers_infected, previous$health_workers_infected), "today"),
       trend = trend_of(latest$health_workers_infected, previous$health_workers_infected))
)

bio_coefficients <- lapply(seq_len(nrow(bio_coeffs)), function(i) {
  list(label = bio_coeffs$label[i],
       value = round(bio_coeffs$value[i], 2),
       unit  = bio_coeffs$unit[i],
       delta = bio_coeffs$ci[i])
})

# ---------- 4. CHART SERIES + ZONES + SLIDERS ----------
chart_series <- list(
  months               = as.character(timeseries_df$month),
  reported_cases       = timeseries_df$reported_cases,
  projected_cases      = timeseries_df$projected_cases,
  deaths               = timeseries_df$deaths,
  rt                   = timeseries_df$rt,
  rt_lower             = timeseries_df$rt_lower,
  rt_upper             = timeseries_df$rt_upper,
  zoonotic_events      = timeseries_df$zoonotic_events,
  cumulative_confirmed = timeseries_df$cumulative_confirmed,
  cumulative_projected = timeseries_df$cumulative_projected
)

zones <- list(
  list(from = 0,  to = 4,  label = "Animal-to-Human (Retrospective)"),
  list(from = 4,  to = 12, label = "Observed Data"),
  list(from = 12, to = length(chart_series$months) - 1, label = "Future Prediction")
)

sliders <- list(
  list(id = "beta",     label = "Transmission Rate (β)", min = 0.1, max = 1.0, step = 0.01, value = 0.42),
  list(id = "gamma",    label = "Recovery Rate (γ)",     min = 0.05, max = 0.5, step = 0.01, value = 0.13),
  list(id = "mu",       label = "Mortality Rate (μ)",    min = 0.0, max = 1.0, step = 0.01, value = 0.68),
  list(id = "zoonotic", label = "Zoonotic Spillover",    min = 0, max = 50, step = 1, value = 12),
  list(id = "interv",   label = "Intervention Efficacy", min = 0, max = 100, step = 1, value = 35)
)

# ---------- 5. EXPORT JSON ----------
payload <- list(
  generated_at     = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
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
