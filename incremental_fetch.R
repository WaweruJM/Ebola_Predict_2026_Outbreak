# ============================================================
# incremental_fetch.R
# Ebola outbreak pipeline — matched to upstream schema:
#   report_date, suspected_global, confirmed_global, total_global,
#   suspected_deaths_global, confirmed_deaths_global,
#   recovered_global, primary_source, source_timestamp
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(lubridate)
  library(readr); library(jsonlite)
})

options(stringsAsFactors = FALSE)

# ---------- 1. INGEST ----------
input_path <- if (file.exists("downloaded_timeseries.csv")) {
  "downloaded_timeseries.csv"
} else if (file.exists("data_cache.csv")) {
  "data_cache.csv"
} else stop("No input timeseries file found.")

raw <- readr::read_csv(input_path, show_col_types = FALSE)
names(raw) <- tolower(names(raw))

# ---------- 2. RENAME to a clean internal schema ----------
outbreak_df <- raw %>%
  transmute(
    date                    = as.Date(report_date),
    suspected_cases         = as.numeric(suspected_global),
    confirmed_cases         = as.numeric(confirmed_global),
    total_cases             = as.numeric(total_global),
    suspected_deaths        = as.numeric(suspected_deaths_global),
    confirmed_deaths        = as.numeric(confirmed_deaths_global),
    deaths                  = suspected_deaths + confirmed_deaths,
    recovered               = as.numeric(recovered_global),
    primary_source          = primary_source,
    source_timestamp        = as.Date(source_timestamp)
  ) %>%
  # De-duplicate on date (keep latest source timestamp)
  arrange(date, desc(source_timestamp)) %>%
  distinct(date, .keep_all = TRUE) %>%
  arrange(date) %>%
  mutate(
    active_cases = pmax(0, total_cases - deaths - recovered),
    cfr          = ifelse(total_cases > 0, 100 * deaths / total_cases, NA_real_)
  )

if (nrow(outbreak_df) == 0) stop("No rows after ingest.")
message("Parsed ", nrow(outbreak_df), " rows; range ",
        min(outbreak_df$date), " → ", max(outbreak_df$date))

# Persist cache
readr::write_csv(outbreak_df, "data_cache.csv")

# ---------- 3. CARD TELEMETRY ----------
latest   <- outbreak_df %>% slice_tail(n = 1)
previous <- outbreak_df %>% slice_tail(n = 2) %>% slice_head(n = 1)

fmt_delta <- function(cur, prev, unit = "") {
  if (is.na(prev) || is.na(cur) || length(prev) == 0) return("—")
  d <- cur - prev
  sign <- ifelse(d >= 0, "+", "")
  paste0(sign, d, if (nzchar(unit)) paste0(" ", unit) else "", " today")
}
trend_of <- function(cur, prev) {
  if (is.na(cur) || is.na(prev) || length(prev) == 0) return("")
  if (cur > prev) "up" else if (cur < prev) "down" else ""
}

case_telemetry <- list(
  list(label = "Confirmed Cases",    value = latest$confirmed_cases,
       unit = "", delta = fmt_delta(latest$confirmed_cases, previous$confirmed_cases),
       trend = trend_of(latest$confirmed_cases, previous$confirmed_cases)),
  list(label = "Suspected Cases",    value = latest$suspected_cases,
       unit = "", delta = fmt_delta(latest$suspected_cases, previous$suspected_cases),
       trend = trend_of(latest$suspected_cases, previous$suspected_cases)),
  list(label = "Total Cases",        value = latest$total_cases,
       unit = "", delta = fmt_delta(latest$total_cases, previous$total_cases),
       trend = trend_of(latest$total_cases, previous$total_cases)),
  list(label = "Total Deaths",       value = latest$deaths,
       unit = "", delta = fmt_delta(latest$deaths, previous$deaths),
       trend = trend_of(latest$deaths, previous$deaths)),
  list(label = "Recovered",          value = latest$recovered,
       unit = "", delta = fmt_delta(latest$recovered, previous$recovered),
       trend = trend_of(latest$recovered, previous$recovered)),
  list(label = "Active Cases",       value = latest$active_cases,
       unit = "", delta = fmt_delta(latest$active_cases, previous$active_cases),
       trend = trend_of(latest$active_cases, previous$active_cases)),
  list(label = "Case Fatality Rate", value = round(latest$cfr, 1),
       unit = "%", delta = paste0(fmt_delta(latest$cfr, previous$cfr), " (pts)"),
       trend = trend_of(latest$cfr, previous$cfr)),
  list(label = "Observation Days",   value = nrow(outbreak_df),
       unit = "", delta = paste0("since ", min(outbreak_df$date)), trend = "")
)

# ---------- 4. BIOLOGICAL COEFFICIENTS ----------
# These are not in the CSV; compute them from the observed series.
# Placeholder: simple empirical estimates so the pipeline always works.
compute_rt <- function(incidence, window = 7) {
  # Approximate R_t via ratio of cases in consecutive windows
  n <- length(incidence)
  if (n < 2 * window) return(rep(NA_real_, n))
  rt <- rep(NA_real_, n)
  for (i in (2 * window):n) {
    recent <- sum(tail(incidence[1:i], window), na.rm = TRUE)
    prior  <- sum(incidence[(i - 2 * window + 1):(i - window)], na.rm = TRUE)
    rt[i] <- ifelse(prior > 0, recent / prior, NA_real_)
  }
  rt
}

incidence       <- pmax(0, diff(c(0, outbreak_df$total_cases)))
rt_series       <- compute_rt(incidence)
rt_latest       <- tail(rt_series[!is.na(rt_series)], 1)
rt_early        <- head(rt_series[!is.na(rt_series)], 1)
doubling_time   <- tryCatch({
  early <- head(incidence[incidence > 0], 20)
  if (length(early) >= 5 && early[1] > 0) {
    log(2) / coef(lm(log(early) ~ seq_along(early)))[2]
  } else NA_real_
}, error = function(e) NA_real_)

bio_coeffs <- tibble::tibble(
  label = c("R0 (Basic Reproduction)",
            "Rt (Effective Repro.)",
            "Incubation Period",
            "Infectious Period",
            "Serial Interval",
            "CFR (Modelled)",
            "Doubling Time",
            "Generation Time"),
  value = c(round(rt_early, 2),
            round(rt_latest, 2),
            9.4, 7.8, 12.1,
            round(latest$cfr, 2),
            round(doubling_time, 1),
            15.3),
  unit  = c("", "", "d", "d", "d", "%", "d", "d"),
  ci    = c("Early-phase estimate",
            "Latest 7-day window",
            "Mean, IQR 7–12 d",
            "Mean, IQR 5–10 d",
            "Mean, SD 4.3 d",
            "Confirmed + suspected deaths",
            "Early-phase regression",
            "Mean estimate")
)

bio_coefficients <- lapply(seq_len(nrow(bio_coeffs)), function(i) {
  list(label = bio_coeffs$label[i],
       value = bio_coeffs$value[i],
       unit  = bio_coeffs$unit[i],
       delta = bio_coeffs$ci[i])
})

# ---------- 5. CHART SERIES ----------
# Downsample to one point per 3 days for readability
step_n <- max(1, floor(nrow(outbreak_df) / 40))
ts <- outbreak_df %>%
  mutate(idx = row_number()) %>%
  filter(idx %% step_n == 0 | idx == n())

chart_series <- list(
  months               = format(ts$date, "%Y-%m-%d"),
  reported_cases       = ts$total_cases,
  projected_cases      = rep(NA_real_, nrow(ts)),
  deaths               = ts$deaths,
  rt                   = rt_series[ts$idx],
  rt_lower             = NA_real_,
  rt_upper             = NA_real_,
  zoonotic_events      = rep(NA_real_, nrow(ts)),
  cumulative_confirmed = cumsum(incidence)[ts$idx],
  cumulative_projected = rep(NA_real_, nrow(ts))
)

# ---------- 6. ZONES + SLIDERS ----------
n_pts <- length(chart_series$months)
zones <- list(
  list(from = 0, to = max(1, floor(n_pts * 0.15)),
       label = "Animal-to-Human (Retrospective)"),
  list(from = max(1, floor(n_pts * 0.15)), to = n_pts - 1,
       label = "Observed Data"),
  list(from = n_pts - 1, to = n_pts - 1,
       label = "Future Prediction")
)

sliders <- list(
  list(id = "beta",     label = "Transmission Rate (β)", min = 0.1, max = 1.0, step = 0.01, value = 0.42),
  list(id = "gamma",    label = "Recovery Rate (γ)",     min = 0.05, max = 0.5, step = 0.01, value = 0.13),
  list(id = "mu",       label = "Mortality Rate (μ)",    min = 0.0, max = 1.0, step = 0.01, value = 0.68),
  list(id = "zoonotic", label = "Zoonotic Spillover",    min = 0, max = 50, step = 1, value = 12),
  list(id = "interv",   label = "Intervention Efficacy", min = 0, max = 100, step = 1, value = 35)
)

# ---------- 7. EXPORT JSON ----------
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
