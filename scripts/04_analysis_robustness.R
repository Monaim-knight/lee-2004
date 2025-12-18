# ============================================================================
# 04_analysis_robustness.R
# ============================================================================
# Robustness checks: rdrobust, bandwidth sensitivity, donut RD, placebos
# ============================================================================

source("scripts/03_analysis_baseline.R")

# Load helper functions
source("R/rd_helpers.R")

message("\n", rep("=", 70))
message("ROBUSTNESS ANALYSIS")
message(rep("=", 70))

# Ensure data is loaded
if (!exists("data")) {
  data <- readRDS("data/processed/lmb_clean.rds")
}

OUTPUT_DIR <- "outputs/tables"
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# ============================================================================
# 1. Bias-corrected local-polynomial RD (rdrobust)
# ============================================================================

message("\n1. Running bias-corrected RD estimation (rdrobust)...")

rd_est <- rdrobust::rdrobust(
  y = data$score, 
  x = data$demvoteshare, 
  c = 0.5
)

message("  - rdrobust estimation complete")
print(summary(rd_est))

# Save results
capture.output(
  print(summary(rd_est)),
  file = file.path(OUTPUT_DIR, "rdrobust_main_results.txt")
)

rdrobust_summary <- data.frame(
  model = "rdrobust",
  coef = rd_est$coef[1],
  se = rd_est$se[1],
  p_value = rd_est$pv[1],
  n_obs = rd_est$N,
  notes = "Bias-corrected local polynomial RD (CCT bandwidth)",
  stringsAsFactors = FALSE
)

# ============================================================================
# 2. Bandwidth sensitivity analysis
# ============================================================================

message("\n2. Running bandwidth sensitivity analysis...")

bandwidths <- c(0.03, 0.05, 0.08, 0.10)
bw_results <- run_bandwidth_sensitivity(
  y = data$score,
  x = data$demvoteshare,
  c = 0.5,
  bandwidths = bandwidths
)

write.csv(bw_results, 
          file.path(OUTPUT_DIR, "bandwidth_sensitivity.csv"), 
          row.names = FALSE)

message("  - Bandwidth sensitivity results saved")
print(bw_results)

# ============================================================================
# 3. Donut RD (exclude observations very near cutoff)
# ============================================================================

message("\n3. Running donut RD (excluding |x - 0.5| < 0.01)...")

donut_h0 <- 0.01
data_donut <- data %>%
  dplyr::filter(abs(demvoteshare - 0.5) >= donut_h0)

rd_donut <- rdrobust::rdrobust(
  y = data_donut$score,
  x = data_donut$demvoteshare,
  c = 0.5
)

message("  - Donut RD complete")
print(summary(rd_donut))

donut_summary <- data.frame(
  model = "donut_rd",
  coef = rd_donut$coef[1],
  se = rd_donut$se[1],
  p_value = rd_donut$pv[1],
  n_obs = rd_donut$N,
  notes = paste0("Donut RD excluding |x-0.5|<", donut_h0),
  stringsAsFactors = FALSE
)

# ============================================================================
# 4. Placebo cutoff tests
# ============================================================================

message("\n4. Running placebo cutoff tests...")

placebo_cutoffs <- c(0.45, 0.55)
placebo_results <- run_placebo_tests(
  y = data$score,
  x = data$demvoteshare,
  true_cutoff = 0.5,
  placebo_cutoffs = placebo_cutoffs
)

message("  - Placebo tests complete")
print(placebo_results)

# Combine donut and placebo results
donut_placebo_df <- rbind(
  donut_summary[, c("model", "coef", "p_value", "n_obs", "notes")],
  data.frame(
    model = paste0("placebo_", placebo_results$cutoff),
    coef = placebo_results$coef,
    p_value = placebo_results$p_value,
    n_obs = placebo_results$n_obs,
    notes = paste0("Placebo test at cutoff ", placebo_results$cutoff),
    stringsAsFactors = FALSE
  )
)

write.csv(donut_placebo_df,
          file.path(OUTPUT_DIR, "donut_placebo_results.csv"),
          row.names = FALSE)

# ============================================================================
# 5. Local randomization test
# ============================================================================

message("\n5. Running local randomization test (window ±0.02)...")

lr_test <- rdlocrand::rdrandinf(
  Y = data$score,
  R = data$demvoteshare,
  cutoff = 0.5,
  wl = 0.48,
  wr = 0.52
)

message("  - Local randomization test complete")
print(lr_test)

capture.output(
  print(lr_test),
  file = file.path(OUTPUT_DIR, "local_randomization_results.txt")
)

# ============================================================================
# 6. Comprehensive model summary
# ============================================================================

message("\n6. Compiling comprehensive model summary...")

# Load baseline results if needed
if (!exists("baseline_results")) {
  baseline_results <- read.csv(file.path(OUTPUT_DIR, "baseline_rd_results.csv"))
}

# Combine all results
all_results <- rbind(
  baseline_results,
  rdrobust_summary,
  donut_summary,
  data.frame(
    model = paste0("placebo_", placebo_results$cutoff),
    coef = placebo_results$coef,
    se = placebo_results$se,
    p_value = placebo_results$p_value,
    n_obs = placebo_results$n_obs,
    notes = paste0("Placebo test at cutoff ", placebo_results$cutoff),
    stringsAsFactors = FALSE
  )
)

write.csv(all_results,
          file.path(OUTPUT_DIR, "all_rd_models_summary.csv"),
          row.names = FALSE)

message("  - Comprehensive summary saved to: ", 
        file.path(OUTPUT_DIR, "all_rd_models_summary.csv"))
print(all_results)

# ============================================================================
# 7. Store key results for figures
# ============================================================================

assign("rd_est", rd_est, envir = .GlobalEnv)
assign("bw_results", bw_results, envir = .GlobalEnv)
assign("placebo_results", placebo_results, envir = .GlobalEnv)
assign("all_results", all_results, envir = .GlobalEnv)

message("\n", rep("=", 70))
message("Robustness analysis complete!")
message(rep("=", 70))


