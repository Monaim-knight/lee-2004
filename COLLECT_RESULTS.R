# ============================================================================
# COLLECT_RESULTS.R
# ============================================================================
# Run this AFTER completing all analysis steps to collect and summarize results
# ============================================================================

cat("\n", rep("=", 70), "\n")
cat("COLLECTING ALL RESULTS FOR REPORT\n")
cat(rep("=", 70), "\n\n")

# Check if results exist
if (!file.exists("outputs/tables/all_rd_models_summary.csv")) {
  stop("Please run the analysis first! Start with: source('run_all.R')")
}

# Load all result tables
cat("Loading result tables...\n")
baseline <- read.csv("outputs/tables/baseline_rd_results.csv")
all_models <- read.csv("outputs/tables/all_rd_models_summary.csv")
bandwidth <- read.csv("outputs/tables/bandwidth_sensitivity.csv")
placebo_donut <- read.csv("outputs/tables/donut_placebo_results.csv")

# Load data for calculations
if (file.exists("data/processed/lmb_clean.rds")) {
  data <- readRDS("data/processed/lmb_clean.rds")
} else {
  warning("Cleaned data not found. Some calculations may be unavailable.")
  data <- NULL
}

cat("\n", rep("=", 70), "\n")
cat("1. MAIN RD ESTIMATE (rdrobust)\n")
cat(rep("=", 70), "\n")
main_estimate <- all_models[all_models$model == "rdrobust", ]
if (nrow(main_estimate) > 0) {
  cat("Coefficient: ", round(main_estimate$coef, 4), "\n")
  cat("Robust SE:   ", round(main_estimate$se, 4), "\n")
  cat("P-value:     ", round(main_estimate$p_value, 4), "\n")
  cat("N obs:       ", main_estimate$n_obs, "\n")
  cat("\nInterpretation: Winning as a Democrat increases liberalism score by",
      round(main_estimate$coef, 3), "points\n")
  
  # Calculate percentage change if data available
  if (!is.null(data)) {
    mean_score <- mean(data$score, na.rm = TRUE)
    percent_change <- (main_estimate$coef / mean_score) * 100
    cat("This represents a ", round(percent_change, 1), 
        "% change relative to the mean score (", round(mean_score, 2), ")\n")
  }
} else {
  cat("Main estimate not found!\n")
}

cat("\n", rep("=", 70), "\n")
cat("2. BASELINE SPECIFICATIONS\n")
cat(rep("=", 70), "\n")
print(baseline)

cat("\n", rep("=", 70), "\n")
cat("3. BANDWIDTH SENSITIVITY\n")
cat(rep("=", 70), "\n")
print(bandwidth)

cat("\n", rep("=", 70), "\n")
cat("4. PLACEBO TESTS\n")
cat(rep("=", 70), "\n")
placebo_results <- placebo_donut[grepl("placebo", placebo_donut$model), ]
if (nrow(placebo_results) > 0) {
  print(placebo_results)
  cat("\nInterpretation: Placebo effects should be small/insignificant.\n")
  cat("True cutoff (0.5) should show significant effect.\n")
} else {
  cat("Placebo results not found!\n")
}

cat("\n", rep("=", 70), "\n")
cat("5. DONUT RD\n")
cat(rep("=", 70), "\n")
donut_result <- placebo_donut[placebo_donut$model == "donut_rd", ]
if (nrow(donut_result) > 0) {
  print(donut_result)
  cat("\nInterpretation: Donut RD excludes observations very near cutoff.\n")
  cat("Similar estimates suggest no manipulation.\n")
} else {
  cat("Donut RD results not found!\n")
}

cat("\n", rep("=", 70), "\n")
cat("6. MCCRARY DENSITY TEST\n")
cat(rep("=", 70), "\n")
if (file.exists("outputs/tables/mccrary_test_results.txt")) {
  cat(readLines("outputs/tables/mccrary_test_results.txt"), sep = "\n")
  cat("\nInterpretation: P-value > 0.05 suggests no manipulation at cutoff.\n")
} else {
  cat("McCrary test results not found!\n")
  cat("Check if mccrary_test object exists in environment.\n")
  if (exists("mccrary_test")) {
    print(summary(mccrary_test))
  }
}

cat("\n", rep("=", 70), "\n")
cat("7. LOCAL RANDOMIZATION TEST\n")
cat(rep("=", 70), "\n")
if (file.exists("outputs/tables/local_randomization_results.txt")) {
  cat(readLines("outputs/tables/local_randomization_results.txt"), sep = "\n")
} else {
  cat("Local randomization results not found!\n")
}

cat("\n", rep("=", 70), "\n")
cat("8. DATA SUMMARY\n")
cat(rep("=", 70), "\n")
if (!is.null(data)) {
  cat("Total observations: ", nrow(data), "\n")
  cat("Democrat winners:   ", sum(data$democrat_winner == 1), "\n")
  cat("Democrat losers:    ", sum(data$democrat_winner == 0), "\n")
  cat("Mean score:         ", round(mean(data$score, na.rm = TRUE), 3), "\n")
  cat("SD score:           ", round(sd(data$score, na.rm = TRUE), 3), "\n")
  cat("Mean vote share:    ", round(mean(data$demvoteshare, na.rm = TRUE), 3), "\n")
  cat("Vote share range:   [", round(min(data$demvoteshare, na.rm = TRUE), 3), 
      ", ", round(max(data$demvoteshare, na.rm = TRUE), 3), "]\n")
} else {
  cat("Data not available for summary.\n")
}

cat("\n", rep("=", 70), "\n")
cat("9. VALUES FOR REPORT (COPY THESE)\n")
cat(rep("=", 70), "\n")
if (nrow(main_estimate) > 0) {
  cat("\nFor reports/paper.md and README.md, use these values:\n\n")
  cat("Main estimate coefficient: ", round(main_estimate$coef, 4), "\n")
  cat("Robust SE:                 ", round(main_estimate$se, 4), "\n")
  cat("P-value:                   ", round(main_estimate$p_value, 4), "\n")
  cat("N observations:            ", main_estimate$n_obs, "\n")
  
  if (!is.null(data)) {
    mean_score <- mean(data$score, na.rm = TRUE)
    percent_change <- (main_estimate$coef / mean_score) * 100
    cat("Percentage change:          ", round(percent_change, 1), "%\n")
    cat("Mean liberalism score:     ", round(mean_score, 2), "\n")
  }
  
  cat("\nBandwidth sensitivity (copy table):\n")
  print(bandwidth)
  
  cat("\nPlacebo tests (copy table):\n")
  if (nrow(placebo_results) > 0) {
    print(placebo_results)
  }
}

cat("\n", rep("=", 70), "\n")
cat("RESULTS COLLECTION COMPLETE!\n")
cat(rep("=", 70), "\n")
cat("\nNext steps:\n")
cat("1. Copy the values above into reports/paper.md\n")
cat("2. Replace [X], [Y], [Z] placeholders with actual values\n")
cat("3. Update README.md results section\n")
cat("4. Review and verify all figures in outputs/figures/\n")
cat("\n")


