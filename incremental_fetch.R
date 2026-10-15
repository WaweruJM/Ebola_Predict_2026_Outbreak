# =========================================================================
# REGIONAL HEALTH DATA HUB: COMPILER & TEMPLATE INJECTION ENGINE
# =========================================================================
# Refactored build script.
# Produces: data_cache.csv (incremental) and index.html (dashboard).
#
# LEFT PANEL (3 stacked graphs):
#   1. outbreak_sandbox_plot  -> Observed + 14-day forecast (I compartment)
#   2. parameter_trends_plot  -> beta_t, gamma_t, R_t historical
#   3. sir_dynamics_plot      -> S, I, R compartment curves (live, slider-driven)
#
# RIGHT PANEL: KPIs + beta/gamma sandbox sliders.
# =========================================================================

# ---- Configuration ------------------------------------------------------
temp_download_path <- "downloaded_timeseries.csv"
cache_filename     <- "data_cache.csv"
output_html        <- "index.html"
N                  <- 12500000L
PLOTLY_CDN         <- "https://cdn.plot.ly/plotly-2.27.0.min.js"
GOOGLE_FONT        <- "https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap"

# ---- 1. Ingest & Validate Raw Data -------------------------------------
if (!file.exists(temp_download_path)) {
  stop("[CRITICAL ERROR] The data payload file was not found on local disk: ",
       temp_download_path)
}

remote_df <- read.csv(temp_download_path, stringsAsFactors = FALSE)
col_names <- colnames(remote_df)

pick_col <- function(name, fallback_pos) {
  idx <- which(col_names == name)
  if (length(idx) == 0) {
    if (fallback_pos > length(col_names)) {
      stop(sprintf("Column '%s' missing and fallback position %d out of range.",
                   name, fallback_pos))
    }
    warning(sprintf("Column '%s' not found; using positional fallback #%d.",
                    name, fallback_pos))
    return(fallback_pos)
  }
  idx[1]
}

date_idx   <- pick_col("report_date",              1)
cases_idx  <- pick_col("confirmed_global",         3)
deaths_idx <- pick_col("confirmed_deaths_global",  6)
recov_idx  <- pick_col("recovered_global",         7)

cat(sprintf(
  "[PIPELINE DIAGNOSTIC] Mapping columns -> Date: %s | Cases: %s | Deaths: %s | Recovered: %s\n",
  col_names[date_idx], col_names[cases_idx],
  col_names[deaths_idx], col_names[recov_idx]
))

# ---- 2. Extract & Clean Vectors ----------------------------------------
report_date_raw <- remote_df[[date_idx]]
cases_vec       <- suppressWarnings(as.numeric(remote_df[[cases_idx]]))
deaths_vec      <- suppressWarnings(as.numeric(remote_df[[deaths_idx]]))
recov_vec       <- suppressWarnings(as.numeric(remote_df[[recov_idx]]))

cases_vec[is.na(cases_vec)]   <- 0
deaths_vec[is.na(deaths_vec)] <- 0
recov_vec[is.na(recov_vec)]   <- 0

cumulative_infected  <- max(cases_vec,  na.rm = TRUE)
cumulative_dead      <- max(deaths_vec, na.rm = TRUE)
cumulative_recovered <- max(recov_vec,  na.rm = TRUE)

processed_bridge <- data.frame(
  report_date = as.Date(report_date_raw),
  stringsAsFactors = FALSE
)
processed_bridge$R_obs <- deaths_vec + recov_vec
processed_bridge$I_obs <- pmax(0, cases_vec - processed_bridge$R_obs)

remote_processed <- na.omit(processed_bridge)
remote_processed <- remote_processed[order(remote_processed$report_date), ]
rownames(remote_processed) <- NULL

# ---- 3. Parameter Estimation -------------------------------------------
compute_row_parameters <- function(df_matrix) {
  n <- nrow(df_matrix)
  beta_vector  <- rep(0.25, n)
  gamma_vector <- rep(0.08, n)
  r0_vector    <- rep(3.12, n)
  perf_vector  <- rep(5.0,  n)

  if (n < 7) {
    return(data.frame(beta = beta_vector, gamma = gamma_vector,
                      r0 = r0_vector, perf_score = perf_vector))
  }

  for (i in 7:n) {
    window <- df_matrix[(i - 6):i, ]
    denom_g <- pmax(1, window$I_obs[-7])
    denom_b <- pmax(1, window$I_obs[-7]) *
               pmax(1, N - window$I_obs[-7] - window$R_obs[-7])

    g_val <- mean(diff(window$R_obs) / denom_g, na.rm = TRUE)
    b_val <- mean((diff(window$I_obs) + diff(window$R_obs)) * N / denom_b,
                  na.rm = TRUE)

    gamma_vector[i] <- max(0.01, min(0.40, g_val))
    beta_vector[i]  <- max(0.01, min(0.80, b_val))
    r0_vector[i]    <- beta_vector[i] / gamma_vector[i]

    if (i > 14) {
      past_observed  <- df_matrix$I_obs[(i - 6):i]
      perf_vector[i] <- (mean(abs(past_observed - mean(past_observed))) /
                         max(1, mean(past_observed))) * 100
    }
  }

  data.frame(beta = beta_vector, gamma = gamma_vector,
             r0 = r0_vector, perf_score = perf_vector)
}

# ---- 4. Incremental Cache Logic ----------------------------------------
if (file.exists(cache_filename)) {
  local_cache <- read.csv(cache_filename, stringsAsFactors = FALSE)
  local_cache$report_date <- as.Date(local_cache$report_date)
  last_cached_date <- max(local_cache$report_date, na.rm = TRUE)

  new_entries <- subset(remote_processed, report_date > last_cached_date)

  if (nrow(new_entries) > 0) {
    starting_time_idx <- max(local_cache$time, na.rm = TRUE) + 1L
    new_entries$time  <- seq.int(starting_time_idx,
                                 length.out = nrow(new_entries))

    combined_temp <- rbind(
      local_cache[,  c("report_date", "I_obs", "R_obs", "time")],
      new_entries[,  c("report_date", "I_obs", "R_obs", "time")]
    )
    combined_temp <- combined_temp[order(combined_temp$report_date), ]
    rownames(combined_temp) <- NULL

    calculated_metrics <- compute_row_parameters(combined_temp)
    clean_df <- cbind(
      combined_temp[, c("report_date", "time", "I_obs", "R_obs")],
      calculated_metrics
    )
  } else {
    clean_df <- local_cache
  }
} else {
  remote_processed$time <- seq_len(nrow(remote_processed))
  calculated_metrics    <- compute_row_parameters(remote_processed)
  clean_df              <- cbind(remote_processed, calculated_metrics)
}

write.csv(clean_df, cache_filename, row.names = FALSE)

# ---- 5. Prepare JS Payloads --------------------------------------------
current_beta  <- tail(clean_df$beta,  1)
current_gamma <- tail(clean_df$gamma, 1)
current_r0    <- tail(clean_df$r0,    1)

js_str_vec <- function(x) paste0('"', gsub('"', '\\"', x), '"', collapse = ",")
js_num_vec <- function(x) paste(format(x, scientific = FALSE, trim = TRUE),
                                collapse = ",")

js_dates <- js_str_vec(format(clean_df$report_date, "%d %b"))
js_I_obs <- js_num_vec(clean_df$I_obs)
js_R_obs <- js_num_vec(clean_df$R_obs)
js_beta  <- js_num_vec(round(clean_df$beta,  4))
js_gamma <- js_num_vec(round(clean_df$gamma, 4))
js_r0    <- js_num_vec(round(clean_df$r0,    2))

# ---- 6. Build HTML Template --------------------------------------------
h_lines <- c(
'<!DOCTYPE html>',
'<html lang="en">',
'<head>',
'  <meta charset="UTF-8">',
'  <meta name="viewport" content="width=device-width, initial-scale=1.0">',
'  <title>Regional Health Data Hub | WHO AFRO Mirror</title>',
sprintf('  <link href="%s" rel="stylesheet">', GOOGLE_FONT),
sprintf('  <script src="%s"></script>', PLOTLY_CDN),
'  <style>',
'    :root { --who-blue: #0078d4; --card-bg: #ffffff; --text-primary: #333333; }',
'    body { font-family: "Roboto", sans-serif; margin: 0; padding: 0; background: #f4f6f9; color: var(--text-primary); }',
'    header { background: var(--who-blue); color: #fff; padding: 15px 30px; display: flex; align-items: center; justify-content: space-between; box-shadow: 0 2px 5px rgba(0,0,0,0.1); }',
'    header h1 { margin: 0; font-size: 22px; font-weight: 700; }',
'    .branding { font-size: 12px; opacity: 0.85; text-transform: uppercase; font-weight: bold; }',
'    .grid-container { display: grid; grid-template-columns: 1.2fr 0.8fr; gap: 25px; padding: 30px; max-width: 1600px; margin: 0 auto; }',
'    .left-graphs-panel, .right-kpi-panel { display: flex; flex-direction: column; gap: 20px; }',
'    .card { background: var(--card-bg); border-radius: 6px; box-shadow: 0 4px 12px rgba(0,0,0,0.05); border: 1px solid #e1e4e8; padding: 20px; }',
'    .graph-card { min-height: 420px; }',
'    .kpi-title { font-size: 11px; text-transform: uppercase; color: #666; font-weight: bold; margin-bottom: 5px; letter-spacing: 0.5px; }',
'    .kpi-value { font-size: 32px; font-weight: 700; color: #111; line-height: 1; }',
'    .kpi-value.danger { color: #d9534f; }',
'    .kpi-value.success { color: #5cb85c; }',
'    .kpi-value.warning { color: #f0ad4e; }',
'    .kpi-value.info { color: #0275d8; }',
'    .sandbox-controls { background: #eef2f7; padding: 15px; border-radius: 4px; margin-top: 10px; }',
'    .slider-group { margin-bottom: 12px; }',
'    .slider-group label { display: block; font-size: 12px; font-weight: bold; margin-bottom: 4px; }',
'    input[type=range] { width: 100%; }',
'    .footer-banner { font-size: 11px; color: #777; text-align: center; padding: 15px; margin-top: 20px; border-top: 1px solid #ddd; }',
'    @media (max-width: 1100px) { .grid-container { grid-template-columns: 1fr; } }',
'  </style>',
'</head>',
'<body>',
'<header>',
'  <div>',
'    <h1>Ebola Disease Outbreak Analysis Portal</h1>',
'    <div class="branding">WHO-AFRO Mirror Platform Alignment Matrix</div>',
'  </div>',
'  <div style="text-align: right; font-size: 12px;">Data Ingestion: <b>Live Incremental Cache</b></div>',
'</header>',
'<div class="grid-container">',
'  <div class="left-graphs-panel">',
'    <div class="card graph-card">',
'      <div id="outbreak_sandbox_plot" style="width:100%; height:380px;"></div>',
'    </div>',
'    <div class="card graph-card">',
'      <div id="sir_dynamics_plot" style="width:100%; height:380px;"></div>',
'    </div>',
'    <div class="card graph-card">',
'      <div id="parameter_trends_plot" style="width:100%; height:380px;"></div>',
'    </div>',
'  </div>',
'  <div class="right-kpi-panel">',
'    <div style="font-weight:bold; font-size:14px; border-left:4px solid #0078d4; padding-left:8px;">CUMULATIVE CASE TELEMETRY</div>',
'    <div class="card"><div class="kpi-title">Total Laboratory Confirmed Cases</div>',
sprintf('      <div class="kpi-value warning">%s</div></div>', format(cumulative_infected, big.mark=",")),
'    <div class="card"><div class="kpi-title">Total Registered Recoveries</div>',
sprintf('      <div class="kpi-value success">%s</div></div>', format(cumulative_recovered, big.mark=",")),
'    <div class="card"><div class="kpi-title">Total Registered Fatalities</div>',
sprintf('      <div class="kpi-value danger">%s</div></div>', format(cumulative_dead, big.mark=",")),
'    <div style="font-weight:bold; font-size:14px; border-left:4px solid #0078d4; padding-left:8px; margin-top:15px;">CURRENT BIOLOGICAL COEFFICIENTS</div>',
'    <div class="card"><div class="kpi-title">Effective Transmission Rate (beta)</div>',
sprintf('      <div class="kpi-value info" id="kpi_beta">%s</div></div>', round(current_beta, 4)),
'    <div class="card"><div class="kpi-title">Clinical Recovery Rate (gamma)</div>',
sprintf('      <div class="kpi-value success" id="kpi_gamma">%s</div></div>', round(current_gamma, 4)),
'    <div class="card"><div class="kpi-title">Effective Reproduction Number (R_t)</div>',
sprintf('      <div class="kpi-value danger" id="kpi_r0">%s</div></div>', round(current_r0, 2)),
'    <div class="card">',
'      <div class="kpi-title">Epidemiological Forecasting Sandbox</div>',
'      <div class="sandbox-controls">',
'        <div class="slider-group">',
'          <label>Transmission Rate (beta): <span id="vBeta">', round(current_beta, 4), '</span></label>',
'          <input type="range" id="inputBeta" min="0.01" max="0.8" step="0.01" value="', round(current_beta, 2), '">',
'        </div>',
'        <div class="slider-group">',
'          <label>Recovery Rate (gamma): <span id="vGamma">', round(current_gamma, 4), '</span></label>',
'          <input type="range" id="inputGamma" min="0.01" max="0.4" step="0.01" value="', round(current_gamma, 2), '">',
'        </div>',
'        <div class="slider-group" style="margin-top:8px; font-size:11px; color:#555;">',
'          <label>Forecast Horizon (days): <span id="vHorizon">60</span></label>',
'          <input type="range" id="inputHorizon" min="14" max="180" step="1" value="60">',
'        </div>',
'      </div>',
'    </div>',
'  </div>',
'</div>',
'<div class="footer-banner">',
'  Ref. data.afro.who.int Mirror Alignment Map. Continuous tracking calculated serverless daily via localized structural R environments.',
'</div>',
'<script>',
'document.addEventListener("DOMContentLoaded", function() {',
sprintf('  const dateLabels = [%s];', js_dates),
sprintf('  const obs_I = [%s];',      js_I_obs),
sprintf('  const obs_R = [%s];',      js_R_obs),
sprintf('  const beta_arr = [%s];',   js_beta),
sprintf('  const gamma_arr = [%s];',  js_gamma),
sprintf('  const r0_arr = [%s];',     js_r0),
sprintf('  const N = %d;',            as.integer(N)),
'',
'  // ---- Core SIR integrator (Euler, dt = 1 day) --------------------',
'  function simulateSIR(beta, gamma, horizon) {',
'    const last_I = obs_I[obs_I.length - 1];',
'    const last_R = obs_R[obs_R.length - 1];',
'    let S = Math.max(0, N - last_I - last_R);',
'    let I = last_I;',
'    let R = last_R;',
'    const S_arr = [S], I_arr = [I], R_arr = [R];',
'    for (let t = 0; t < horizon; t++) {',
'      const dS = -(beta * S * I) / N;',
'      const dI =  (beta * S * I) / N - gamma * I;',
'      const dR =  gamma * I;',
'      S = Math.max(0, S + dS);',
'      I = Math.max(0, I + dI);',
'      R = Math.max(0, R + dR);',
'      S_arr.push(S); I_arr.push(I); R_arr.push(R);',
'    }',
'    return { S: S_arr, I: I_arr, R: R_arr };',
'  }',
'',
'  // ---- Graph 1: Observed + short forecast -------------------------',
'  function renderOutbreak(beta, gamma, horizon) {',
'    const sim = simulateSIR(beta, gamma, Math.min(horizon, 30));',
'    const ext_dates = [...dateLabels];',
'    for (let i = 1; i < sim.I.length; i++) ext_dates.push("+d" + i);',
'',
'    const trace_obs = { x: dateLabels, y: obs_I, mode: "markers",',
'      name: "Observed Active Cases", marker: { color: "#333", size: 6 } };',
'    const trace_sim = { x: ext_dates, y: sim.I, mode: "lines",',
'      name: "Model Path (short horizon)",',
'      line: { color: "#cc0000", width: 3 } };',
'',
'    Plotly.newPlot("outbreak_sandbox_plot", [trace_obs, trace_sim], {',
'      title: "Outbreak Trajectory & Predictive Sandbox (I compartment)",',
'      margin: { t: 40, b: 50, l: 60, r: 20 },',
'      xaxis: { title: "Chronological Matrix (Day/Month)" },',
'      yaxis: { title: "Active Case Volume" },',
'      legend: { orientation: "h", y: -0.2 }',
'    }, { responsive: true });',
'  }',
'',
'  // ---- Graph 2: S, I, R compartment dynamics ----------------------',
'  function renderSIRPlot(beta, gamma, horizon) {',
'    const sim = simulateSIR(beta, gamma, horizon);',
'    const ext_dates = [...dateLabels];',
'    // Pad the historical axis with placeholders so all three curves align',
'    // on the same timeline. Only the future segment carries the SIR shape;',
'    // we prepend the last observed points as the "now" anchor.',
'    const future_dates = [];',
'    for (let i = 0; i <= horizon; i++) future_dates.push("+d" + i);',
'',
'    const trace_S = { x: future_dates, y: sim.S, mode: "lines",',
'      name: "S — Susceptible", line: { color: "#0078d4", width: 3 } };',
'    const trace_I = { x: future_dates, y: sim.I, mode: "lines",',
'      name: "I — Infected",     line: { color: "#d9534f", width: 3 } };',
'    const trace_R = { x: future_dates, y: sim.R, mode: "lines",',
'      name: "R — Recovered/Removed", line: { color: "#5cb85c", width: 3 } };',
'',
'    Plotly.newPlot("sir_dynamics_plot", [trace_S, trace_I, trace_R], {',
'      title: "SIR Compartment Dynamics — S / I / R Evolution",',
'      margin: { t: 40, b: 50, l: 60, r: 20 },',
'      xaxis: { title: "Days from Now (t = 0 is last observed)" },',
'      yaxis: { title: "Population (persons)", type: "linear" },',
'      legend: { orientation: "h", y: -0.2 },',
'      hovermode: "x unified"',
'    }, { responsive: true });',
'  }',
'',
'  // ---- Graph 3: Historical parameter trends -----------------------',
'  function renderParameterTrends() {',
'    const t_beta  = { x: dateLabels, y: beta_arr,  mode: "lines",',
'      name: "Transmission Rate (beta_t)",',
'      line: { color: "#f0ad4e", width: 2 } };',
'    const t_gamma = { x: dateLabels, y: gamma_arr, mode: "lines",',
'      name: "Recovery Rate (gamma_t)",',
'      line: { color: "#5bc0de", width: 2 } };',
'    const t_r0    = { x: dateLabels, y: r0_arr,    mode: "lines",',
'      name: "Reproduction Number (R_t)",',
'      line: { color: "#d9534f", width: 2.5, dash: "dash" } };',
'',
'    Plotly.newPlot("parameter_trends_plot", [t_beta, t_gamma, t_r0], {',
'      title: "Biological Parameter Historical Trajectory Trends",',
'      margin: { t: 40, b: 50, l: 60, r: 20 },',
'      xaxis: { title: "Observation Interval Timeline" },',
'      yaxis: { title: "Coefficient Scaling Matrix" },',
'      legend: { orientation: "h", y: -0.2 }',
'    }, { responsive: true });',
'  }',
'',
'  // ---- Master render -----------------------------------------------',
'  function renderAll() {',
'    const beta    = parseFloat(document.getElementById("inputBeta").value);',
'    const gamma   = parseFloat(document.getElementById("inputGamma").value);',
'    const horizon = parseInt(document.getElementById("inputHorizon").value, 10);',
'',
'    document.getElementById("vBeta").innerText    = beta.toFixed(4);',
'    document.getElementById("vGamma").innerText   = gamma.toFixed(4);',
'    document.getElementById("vHorizon").innerText = horizon;',
'',
'    // Live KPI refresh',
'    document.getElementById("kpi_beta").innerText  = beta.toFixed(4);',
'    document.getElementById("kpi_gamma").innerText = gamma.toFixed(4);',
'    document.getElementById("kpi_r0").innerText    = (beta / gamma).toFixed(2);',
'',
'    renderOutbreak(beta, gamma, horizon);',
'    renderSIRPlot(beta, gamma, horizon);',
'    renderParameterTrends();',
'  }',
'',
'  ["inputBeta", "inputGamma", "inputHorizon"].forEach(function(id) {',
'    document.getElementById(id).addEventListener("input", renderAll);',
'  });',
'',
'  renderAll();',
'});',
'</script>',
'</body>',
'</html>'
)

html_content <- paste(h_lines, collapse = "\n")
writeLines(html_content, output_html, useBytes = TRUE)

if (file.exists(temp_download_path)) file.remove(temp_download_path)

cat(sprintf("[SUCCESS] Compilation engine wrote '%s' and updated '%s'.\n",
            output_html, cache_filename))
