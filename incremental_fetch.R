# =========================================================================
# INCREMENTAL SELECTION ARCHITECTURE - REINFORCED PROCESSING
# =========================================================================
temp_download_path <- "downloaded_timeseries.csv"
cache_filename <- "data_cache.csv"
N <- 12500000 # Population scale anchor constraint constant

if (!file.exists(temp_download_path)) {
  stop("[CRITICAL ERROR] The data payload file was not found on disk.")
}

# Read data safely
remote_df <- read.csv(temp_download_path, stringsAsFactors = FALSE)

# --- DYNAMIC DATE COLUMN DETECTION ---
# Find which column contains the date information, matching variations case-insensitively
date_col_idx <- grep("date", colnames(remote_df), ignore.case = TRUE)

if (length(date_col_idx) == 0) {
  stop("[CRITICAL ERROR] Could not locate a date column. Available columns are: ", 
       paste(colnames(remote_df), collapse = ", "))
}

# Standardize the date column name for the rest of the script
colnames(remote_df)[date_col_idx[1]] <- "report_date"

# Safely convert to R Date object without dropping rows
remote_df$report_date <- as.Date(remote_df$report_date)

# Verify conversion success
if (all(is.na(remote_df$report_date))) {
  stop("[CRITICAL ERROR] Date conversion failed. Please check the text format of your source dates.")
}

# --- DERIVE ACTIVE COMPARTMENTS ---
remote_df$R_obs <- remote_df$confirmed_deaths_global + remote_df$recovered_global
remote_df$I_obs <- remote_df$confirmed_global - remote_df$R_obs

# Subset and clean missing values
remote_processed <- na.omit(remote_df[, c("report_date", "I_obs", "R_obs")])
remote_processed <- remote_processed[order(remote_processed$report_date), ]

# HELPER FUNCTION TO CALCULATE EPIDEMIOLOGICAL PARAMETERS ON THE FLY
compute_row_parameters <- function(df_matrix) {
  total_rows <- nrow(df_matrix)
  beta_vector  <- rep(0.25, total_rows)
  gamma_vector <- rep(0.08, total_rows)
  r0_vector    <- rep(3.12, total_rows)
  perf_vector  <- rep(0, total_rows)
  
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
cat("[SUCCESS] Pipeline completed calculations data pass smoothly.\n")

  initial_payload <- cbind(remote_processed, calculated_metrics)
  
  write.csv(initial_payload, cache_filename, row.names = FALSE)
}
