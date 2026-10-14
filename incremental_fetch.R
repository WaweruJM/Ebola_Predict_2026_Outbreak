# =========================================================================
# INCREMENTAL SELECTION ARCHITECTURE - FAIL-SAFE METRIC MATCHING
# =========================================================================
temp_download_path <- "downloaded_timeseries.csv"
cache_filename <- "data_cache.csv"
N <- 12500000 # Population constraint constant

if (!file.exists(temp_download_path)) {
  stop("[CRITICAL ERROR] The data payload file was not found on local disk.")
}

# Read data safely off the local virtual workspace
remote_df <- read.csv(temp_download_path, stringsAsFactors = FALSE)
col_names  <- colnames(remote_df)

# --- DYNAMIC COLUMN HEADERS MATCHING ---
# Find columns by matching patterns case-insensitively to avoid explicit spelling failures
date_idx   <- grep("date", col_names, ignore.case = TRUE)[1]
cases_idx  <- grep("confirm", col_names, ignore.case = TRUE)[1]
deaths_idx <- grep("death", col_names, ignore.case = TRUE)[1]
recov_idx  <- grep("recov", col_names, ignore.case = TRUE)[1]

# If exact pattern matching fails, map to known index structures as fallbacks
if(is.na(date_idx))   date_idx   <- 1
if(is.na(cases_idx))  cases_idx  <- 2
if(is.na(deaths_idx)) deaths_idx <- 3
if(is.na(recov_idx))  recov_idx  <- 4

cat(sprintf("[PIPELINE DIAGNOSTIC] Mapping columns -> Date: %s | Cases: %s | Deaths: %s | Recovered: %s\n", 
            col_names[date_idx], col_names[cases_idx], col_names[deaths_idx], col_names[recov_idx]))

# Isolate standardized vectors safely without assignment mismatch risks
report_date <- as.Date(remote_df[, date_idx])
cases_vec   <- as.numeric(remote_df[, cases_idx])
deaths_vec  <- as.numeric(remote_df[, deaths_idx])
recov_vec   <- as.numeric(remote_df[, recov_idx])

# Handle any structural parsing conversion anomalies safely
cases_vec[is.na(cases_vec)]   <- 0
deaths_vec[is.na(deaths_vec)] <- 0
recov_vec[is.na(recov_vec)]   <- 0

# --- COMPUTE DYNAMIC COMPARTMENTS ---
# Safely assign processed arrays into a clean temporary data frame matrix
processed_bridge <- data.frame(report_date = report_date)
processed_bridge$R_obs <- deaths_vec + recov_vec
processed_bridge$I_obs <- pmax(0, cases_vec - processed_bridge$R_obs)

remote_processed <- na.omit(processed_bridge)
remote_processed <- remote_processed[order(remote_processed$report_date), ]

# HELPER FUNCTION TO CALCULATE EPIDEMIOLOGICAL PARAMETERS ON THE FLY
compute_row_parameters <- function(df_matrix) {
  total_rows <- nrow(df_matrix)
  beta_vector  <- rep(0.25, total_rows)
  gamma_vector <- rep(0.08, total_rows)
  r0_vector    <- rep(3.12, total_rows)
  perf_vector  <- rep(5.0, total_rows)
  
  if (total_rows < 7) return(data.frame(beta=beta_vector, gamma=gamma_vector, r0=r0_vector, perf_score=perf_vector))
  
  for (i in 7:total_rows) {
    window <- df_matrix[(i-6):i, ]
    g_val <- mean(diff(window$R_obs) / pmax(1, window$I_obs[-7]))
    b_val <- mean((diff(window$I_obs) + diff(window$R_obs)) * N / (pmax(1, window$I_obs[-7]) * (N - window$I_obs[-7] - window$R_obs[-7])))
    
    gamma_vector[i] <- pmax(0.01, pmin(0.4, g_val))
    beta_vector[i]  <- pmax(0.01, pmin(0.8, b_val))
    r0_vector[i]    <- beta_vector[i] / gamma_vector[i]
    
    if (i > 14) {
      past_observed <- df_matrix$I_obs[(i-6):i]
      perf_vector[i] <- (mean(abs(past_observed - mean(past_observed))) / pmax(1, mean(past_observed))) * 100
    }
  }
  beta_vector[1:6]  <- beta_vector; gamma_vector[1:6] <- gamma_vector; r0_vector[1:6] <- r0_vector
  perf_vector[1:14] <- perf_vector
  return(data.frame(beta = beta_vector, gamma = gamma_vector, r0 = r0_vector, perf_score = perf_vector))
}

# CONFIGURE PERSISTENT INTEGRATION LAYER SUB-SELECTION
if (file.exists(cache_filename)) {
  local_cache <- read.csv(cache_filename)
  local_cache$report_date <- as.Date(local_cache$report_date)
  
  last_cached_date <- max(local_cache$report_date)
  new_entries <- subset(remote_processed, report_date > last_cached_date)
  
  if (nrow(new_entries) > 0) {
    cat(sprintf("[PIPELINE] Found %d net new data rows.\n", nrow(new_entries)))
    starting_time_idx <- max(local_cache$time) + 1
    new_entries$time <- starting_time_idx:(starting_time_idx + nrow(new_entries) - 1)
    
    combined_temp <- rbind(local_cache[, c("report_date", "I_obs", "R_obs", "time")], new_entries[, c("report_date", "I_obs", "R_obs", "time")])
    combined_temp <- combined_temp[order(combined_temp$report_date), ]
    
    calculated_metrics <- compute_row_parameters(combined_temp)
    combined_final <- cbind(combined_temp[, c("report_date", "time", "I_obs", "R_obs")], calculated_metrics)
    write.csv(combined_final, cache_filename, row.names = FALSE)
  } else {
    cat("[PIPELINE] Cache database is already up to date.\n")
  }
} else {
  cat("[PIPELINE INITIALIZATION] Provisioning fresh data schema structure...\n")
  remote_processed$time <- 1:nrow(remote_processed)
  calculated_metrics <- compute_row_parameters(remote_processed)
  initial_payload <- cbind(remote_processed, calculated_metrics)
  write.csv(initial_payload, cache_filename, row.names = FALSE)
}

# Clean up temp file to keep repository workspace neat
if(file.exists(temp_download_path)) { file.remove(temp_download_path) }
cat("[SUCCESS] Data cache processed and written safely without dimensional alignment error exceptions.\n")
