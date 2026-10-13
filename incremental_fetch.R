# =========================================================================
# INCREMENTAL SELECTION ARCHITECTURE WITH PARAMETER TRACKING
# =========================================================================
target_raw_url <- "https://githubusercontent.com"
cache_filename <- "data_cache.csv"
N <- 12500000 # Population scale anchor constraint constant

# 1. FETCH & PROCESS NEW DATA FROM THE REMOTE SOURCE
remote_df <- read.csv(target_raw_url)
remote_df$report_date <- as.Date(remote_df$report_date)

# Derive missing active case dimensions
remote_df$R_obs <- remote_df$confirmed_deaths_global + remote_df$recovered_global
remote_df$I_obs <- remote_df$confirmed_global - remote_df$R_obs
remote_processed <- na.omit(remote_df[, c("report_date", "I_obs", "R_obs")])
remote_processed <- remote_processed[order(remote_processed$report_date), ]

# 2. HELPER FUNCTION TO CALCULATE EPIDEMIOLOGICAL PARAMETERS ON THE FLY
compute_row_parameters <- function(df_matrix) {
  total_rows <- nrow(df_matrix)
  
  beta_vector  <- rep(0.25, total_rows)
  gamma_vector <- rep(0.08, total_rows)
  r0_vector    <- rep(3.12, total_rows)
  perf_vector  <- rep(0, total_rows)
  
  if (total_rows < 7) return(data.frame(beta=beta_vector, gamma=gamma_vector, r0=r0_vector, perf_score=perf_vector))
  
  for (i in 7:total_rows) {
    window <- df_matrix[(i-6):i, ]
    
    # Discrete evaluations
    g_val <- mean(diff(window$R_obs) / pmax(1, window$I_obs[-7]))
    b_val <- mean((diff(window$I_obs) + diff(window$R_obs)) * N / (pmax(1, window$I_obs[-7]) * (N - window$I_obs[-7] - window$R_obs[-7])))
    
    gamma_vector[i] <- pmax(0.01, pmin(0.4, g_val))
    beta_vector[i]  <- pmax(0.01, pmin(0.8, b_val))
    r0_vector[i]    <- beta_vector[i] / gamma_vector[i]
    
    # Calculate performance tracking variance score (7-Day rolling Mean Absolute Percentage Error proxy)
    if (i > 14) {
      past_observed <- df_matrix$I_obs[(i-6):i]
      perf_vector[i] <- (mean(abs(past_observed - mean(past_observed))) / pmax(1, mean(past_observed))) * 100
    }
  }
  
  # Fill initial window buffer gaps with the first stable calculated row index
  beta_vector[1:6]  <- beta_vector[7]
  gamma_vector[1:6] <- gamma_vector[7]
  r0_vector[1:6]    <- r0_vector[7]
  perf_vector[1:14] <- perf_vector[15]
  
  return(data.frame(beta = beta_vector, gamma = gamma_vector, r0 = r0_vector, perf_score = perf_vector))
}

# 3. CONFIGURE PERSISTENT INTEGRATION LAYER SUB-SELECTION
if (file.exists(cache_filename)) {
  local_cache <- read.csv(cache_filename)
  local_cache$report_date <- as.Date(local_cache$report_date)
  
  last_cached_date <- max(local_cache$report_date)
  new_entries <- subset(remote_processed, report_date > last_cached_date)
  
  if (nrow(new_entries) > 0) {
    cat(sprintf("[PIPELINE] Found %d net new data rows. Appending to storage cache...\n", nrow(new_entries)))
    
    # Reconstruct timeline sequencing alignment 
    starting_time_idx <- max(local_cache$time) + 1
    new_entries$time <- starting_time_idx:(starting_time_idx + nrow(new_entries) - 1)
    
    # Temporary join structure to compute parameters over the expanded history timeline
    combined_temp <- rbind(local_cache[, c("report_date", "I_obs", "R_obs")], new_entries[, c("report_date", "I_obs", "R_obs")])
    combined_temp$time <- 1:nrow(combined_temp)
    
    # Re-evaluate trends across the entire sequential time matrix
    calculated_metrics <- compute_row_parameters(combined_temp)
    combined_final <- cb <- cbind(combined_temp, calculated_metrics)
    
    write.csv(combined_final, cache_filename, row.names = FALSE)
  } else {
    cat("[PIPELINE] Cache database is already up to date. Zero database transformations required.\n")
  }
} else {
  cat("[PIPELINE INITIALIZATION] No local database detected. Provisioning fresh data schema structure...\n")
  remote_processed$time <- 1:nrow(remote_processed)
  
  calculated_metrics <- compute_row_parameters(remote_processed)
  initial_payload <- cbind(remote_processed, calculated_metrics)
  
  write.csv(initial_payload, cache_filename, row.names = FALSE)
}