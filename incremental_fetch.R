# =========================================================================
# REGIONAL HEALTH DATA HUB: COMPILER & TEMPLATE INJECTION ENGINE
# =========================================================================
temp_download_path <- "downloaded_timeseries.csv"
cache_filename     <- "data_cache.csv"
N                  <- 12500000 

if (!file.exists(temp_download_path)) {
  stop("[CRITICAL ERROR] The data payload file was not found on local disk.")
}

remote_df  <- read.csv(temp_download_path, stringsAsFactors = FALSE)
col_names  <- colnames(remote_df)

# Dynamic Column Headers Extraction Alignment Matrix
date_idx   <- grep("date", col_names, ignore.case = TRUE)
cases_idx  <- grep("confirm", col_names, ignore.case = TRUE)
deaths_idx <- grep("death", col_names, ignore.case = TRUE)
recov_idx  <- grep("recov", col_names, ignore.case = TRUE)

if(is.na(date_idx))   date_idx   <- 1
if(is.na(cases_idx))  cases_idx  <- 2
if(is.na(deaths_idx)) deaths_idx <- 3
if(is.na(recov_idx))  recov_idx  <- 4

# Extract vectors
report_date_raw <- remote_df[, date_idx]
cases_vec       <- as.numeric(remote_df[, cases_idx])
deaths_vec      <- as.numeric(remote_df[, deaths_idx])
recov_vec       <- as.numeric(remote_df[, recov_idx])

# Handle gaps cleanly
cases_vec[is.na(cases_vec)]   <- 0
deaths_vec[is.na(deaths_vec)] <- 0
recov_vec[is.na(recov_vec)]   <- 0

# Calculations for cumulative metrics
cumulative_infected  <- max(cases_vec)
cumulative_dead      <- max(deaths_vec)
cumulative_recovered <- max(recov_vec)

processed_bridge <- data.frame(report_date = as.Date(report_date_raw))
processed_bridge$R_obs <- deaths_vec + recov_vec
processed_bridge$I_obs <- pmax(0, cases_vec - processed_bridge$R_obs)

remote_processed <- na.omit(processed_bridge)
remote_processed <- remote_processed[order(remote_processed$report_date), ]

# Parameter calculation function
compute_row_parameters <- function(df_matrix) {
  total_rows   <- nrow(df_matrix)
  beta_vector  <- rep(0.25, total_rows)
  gamma_vector <- rep(0.08, total_rows)
  r0_vector    <- rep(3.12, total_rows)
  perf_vector  <- rep(5.0, total_rows)
  
  if (total_rows < 7) return(data.frame(beta=beta_vector, gamma=gamma_vector, r0=r0_vector, perf_score=perf_vector))
  
  for (i in 7:total_rows) {
    window <- df_matrix[(i-6):i, ]
    g_val  <- mean(diff(window$R_obs) / pmax(1, window$I_obs[-7]))
    b_val  <- mean((diff(window$I_obs) + diff(window$R_obs)) * N / (pmax(1, window$I_obs[-7]) * (N - window$I_obs[-7] - window$R_obs[-7])))
    
    gamma_vector[i] <- pmax(0.01, pmin(0.4, g_val))
    beta_vector[i]  <- pmax(0.01, pmin(0.8, b_val))
    r0_vector[i]    <- beta_vector[i] / gamma_vector[i]
    
    if (i > 14) {
      past_observed  <- df_matrix$I_obs[(i-6):i]
      perf_vector[i] <- (mean(abs(past_observed - mean(past_observed))) / pmax(1, mean(past_observed))) * 100
    }
  }
  beta_vector[1:6]  <- beta_vector[7]; gamma_vector[1:6] <- gamma_vector[7]; r0_vector[1:6] <- r0_vector[7]
  perf_vector[1:14] <- perf_vector[15]
  return(data.frame(beta = beta_vector, gamma = gamma_vector, r0 = r0_vector, perf_score = perf_vector))
}

# Evaluate the cache
if (file.exists(cache_filename)) {
  local_cache <- read.csv(cache_filename)
  local_cache$report_date <- as.Date(local_cache$report_date)
  last_cached_date <- max(local_cache$report_date)
  new_entries <- subset(remote_processed, report_date > last_cached_date)
  
  if (nrow(new_entries) > 0) {
    starting_time_idx <- max(local_cache$time) + 1
    new_entries$time  <- starting_time_idx:(starting_time_idx + nrow(new_entries) - 1)
    combined_temp     <- rbind(local_cache[, c("report_date", "I_obs", "R_obs", "time")], new_entries[, c("report_date", "I_obs", "R_obs", "time")])
    combined_temp     <- combined_temp[order(combined_temp$report_date), ]
    calculated_metrics <- compute_row_parameters(combined_temp)
    clean_df          <- cbind(combined_temp[, c("report_date", "time", "I_obs", "R_obs")], calculated_metrics)
  } else {
    clean_df <- local_cache
  }
} else {
  remote_processed$time <- 1:nrow(remote_processed)
  calculated_metrics   <- compute_row_parameters(remote_processed)
  clean_df             <- cbind(remote_processed, calculated_metrics)
}

write.csv(clean_df, cache_filename, row.names = FALSE)

# =========================================================================
# 3. HTML PORTAL INJECTION BLOCK TEMPLATE
# =========================================================================
current_beta  <- tail(clean_df$beta, 1)
current_gamma <- tail(clean_df$gamma, 1)
current_r0    <- tail(clean_df$r0, 1)

# Format explicit strings arrays for browser client components pass
js_dates  <- paste0('"', format(clean_df$report_date, "%d %b"), '"', collapse=",")
js_I_obs  <- paste(clean_df$I_obs, collapse=",")
js_beta   <- paste(round(clean_df$beta, 4), collapse=",")
js_gamma  <- paste(round(clean_df$gamma, 4), collapse=",")
js_r0     <- paste(round(clean_df$r0, 2), collapse=",")

html_content <- paste0('<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Regional Health Data Hub | WHO AFRO Mirror</title>
    <link href="https://googleapis.com" rel="stylesheet">
    <script src="https://plot.ly"></script>
    <style>
        :root { --who-blue: #0078d4; --bg-dark: #121212; --card-bg: #ffffff; --text-primary: #333333; }
        body { font-family: "Roboto", sans-serif; margin: 0; padding: 0; background-color: #f4f6f9; color: var(--text-primary); }
        header { background-color: var(--who-blue); color: white; padding: 15px 30px; display: flex; align-items: center; justify-content: space-between; box-shadow: 0 2px 5px rgba(0,0,0,0.1); }
        header h1 { margin: 0; font-size: 22px; font-weight: 700; letter-spacing: -0.5px; }
        .branding { font-size: 12px; opacity: 0.85; text-transform: uppercase; font-weight: bold; }
        .grid-container { display: grid; grid-template-columns: 1.2fr 0.8fr; gap: 25px; padding: 30px; max-width: 1600px; margin: 0 auto; height: calc(100vh - 100px); }
        .left-graphs-panel { display: flex; flex-direction: column; gap: 20px; overflow-y: auto; padding-right: 5px; }
        .right-kpi-panel { display: flex; flex-direction: column; gap: 15px; }
        .card { background: var(--card-bg); border-radius: 6px; box-shadow: 0 4px 12px rgba(0,0,0,0.05); border: 1px solid #e1e4e8; padding: 20px; }
        .graph-card { min-height: 420px; }
        .kpi-title { font-size: 11px; text-transform: uppercase; color: #666; font-weight: bold; margin-bottom: 5px; letter-spacing: 0.5px; }
        .kpi-value { font-size: 32px; font-weight: 700; color: #111; line-height: 1; }
        .kpi-value.danger { color: #d9534f; }
        .kpi-value.success { color: #5cb85c; }
        .kpi-value.warning { color: #f0ad4e; }
        .kpi-value.info { color: #0275d8; }
        .sandbox-controls { background: #eef2f7; padding: 15px; border-radius: 4px; margin-top: 10px; }
        .slider-group { margin-bottom: 12px; }
        .slider-group label { display: block; font-size: 12px; font-weight: bold; margin-bottom: 4px; }
        input[type=range] { width: 100%; }
        .footer-banner { font-size: 11px; color: #777; text-align: center; padding: 15px; margin-top: 20px; border-top: 1px solid #ddd; }
    </style>
</head>
<body>

<header>
    <div>
        <h1>Ebola Disease Outbreak Analysis Portal</h1>
        <div class="branding">WHO-AFRO Mirror Platform Alignment Matrix</div>
    </div>
    <div style="text-align: right; font-size: 12px;">Data Ingestion: <b>Live Incremental Cache</b></div>
</header>

<div class="grid-container">
    <!-- LEFT PANEL: STAND-ALONE VERTICAL GRAPH STACK -->
    <div class="left-graphs-panel">
        <div class="card graph-card">
            <div id="outbreak_sandbox_plot" style="width:100%; height:380px;"></div>
        </div>
        <div class="card graph-card">
            <div id="parameter_trends_plot" style="width:100%; height:380px;"></div>
        </div>
    </div>

    <!-- RIGHT PANEL: VERTICAL KPI & CUMULATIVE VALUE BLOCKS -->
    <div class="right-kpi-panel">
        <!-- Cumulative Section Headers -->
        <div style="font-weight: bold; font-size: 14px; border-left: 4px solid var(--who-blue); padding-left: 8px; margin-bottom: 5px;">CUMULATIVE CASE TELEMETRY</div>
        
        <div class="card">
            <div class="kpi-title">Total Laboratory Confirmed Cases</div>
            <div class="kpi-value warning">', cumulative_infected, '</div>
        </div>
        <div class="card">
            <div class="kpi-title">Total Registered Recoveries</div>
            <div class="kpi-value success">', cumulative_recovered, '</div>
        </div>
        <div class="card">
            <div class="kpi-title">Total Registered Fatalities</div>
            <div class="kpi-value danger">', cumulative_dead, '</div>
        </div>

        <div style="font-weight: bold; font-size: 14px; border-left: 4px solid var(--who-blue); padding-left: 8px; margin-top: 15px; margin-bottom: 5px;">CURRENT BIOLOGICAL COEFFICIENTS</div>

        <div class="card">
            <div class="kpi-title">Effective Transmission Rate (β)</div>
            <div class="kpi-value info" id="kpi_beta">', round(current_beta, 4), '</div>
        </div>
        <div class="card">
            <div class="kpi-title">Clinical Recovery Rate (γ)</div>
            <div class="kpi-value success" id="kpi_gamma">', round(current_gamma, 4), '</div>
        </div>
        <div class="card">
            <div class="kpi-title">Effective Reproduction Number (R_t)</div>
            <div class="kpi-value danger" id="kpi_r0">', round(current_r0, 2), '</div>
        </div>

        <!-- Interactive Sandbox Panel -->
        <div class="card" style="border-top: 3px solid var(--who-blue);">

# Clean up temp file to keep repository workspace neat
if(file.exists(temp_download_path)) { file.remove(temp_download_path) }
cat("[SUCCESS] Data cache processed and written safely without dimensional alignment error exceptions.\n")
