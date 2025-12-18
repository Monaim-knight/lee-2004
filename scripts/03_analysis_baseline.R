# ============================================================================
# 03_analysis_baseline.R
# ============================================================================
# Baseline RD analysis: standard specifications and McCrary density test
# ============================================================================

source("scripts/02_clean_data.R")

# Load helper functions
source("R/rd_helpers.R")

message("\n", rep("=", 70))
message("BASELINE RD ANALYSIS")
message(rep("=", 70))

# Load cleaned data if not in environment
if (!exists("data")) {
  data <- readRDS("data/processed/lmb_clean.rds")
}

# ============================================================================
# 1. Baseline RD regressions
# ============================================================================

message("\n1. Running baseline RD regressions...")

# Global regression (naive benchmark)
model_global <- run_rd_regression(data, spec = "global")
message("  - Global regression complete")

# Centered running variable
model_centered <- run_rd_regression(data, spec = "centered")
message("  - Centered RD regression complete")

# Interaction (different slopes)
model_interaction <- run_rd_regression(data, spec = "interaction")
message("  - Interaction RD regression complete")

# Quadratic
model_quadratic <- run_rd_regression(data, spec = "quadratic")
message("  - Quadratic RD regression complete")

# Windowed (0.45-0.55)
model_windowed <- run_rd_regression(data, spec = "quadratic", window = c(0.45, 0.55))
message("  - Windowed RD regression complete")

# Very narrow window (±0.02)
discontinuity_sample <- data %>%
  dplyr::filter(demvoteshare >= 0.48 & demvoteshare <= 0.52) %>%
  dplyr::mutate(demvoteshare_centered = demvoteshare - 0.5)

model_local <- lm(score ~ democrat_winner, data = discontinuity_sample)
message("  - Local narrow window regression complete")

# ============================================================================
# 2. Extract and save baseline results
# ============================================================================

baseline_results <- rbind(
  extract_rd_summary(model_global, "global"),
  extract_rd_summary(model_centered, "centered"),
  extract_rd_summary(model_interaction, "interaction"),
  extract_rd_summary(model_quadratic, "quadratic"),
  extract_rd_summary(model_windowed, "windowed_quadratic"),
  extract_rd_summary(model_local, "local_narrow")
)

baseline_results$notes <- c(
  "Global regression ignoring running variable",
  "RD with centered running variable",
  "RD with different slopes on each side",
  "RD with quadratic terms on each side",
  "RD with quadratic terms, windowed 0.45-0.55",
  "Local comparison within ±0.02 of cutoff"
)

# Save results
OUTPUT_DIR <- "outputs/tables"
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

write.csv(baseline_results, 
          file.path(OUTPUT_DIR, "baseline_rd_results.csv"), 
          row.names = FALSE)

message("\n2. Baseline results saved to: ", file.path(OUTPUT_DIR, "baseline_rd_results.csv"))
print(baseline_results)

# ============================================================================
# 3. McCrary density test
# ============================================================================

message("\n3. Running McCrary density test...")

mccrary_test <- rddensity::rddensity(
  X = data$demvoteshare, 
  c = 0.5
)

message("  - McCrary test complete")
print(summary(mccrary_test))

# Save test results
capture.output(
  print(summary(mccrary_test)),
  file = file.path(OUTPUT_DIR, "mccrary_test_results.txt")
)

message("  - Results saved to: ", file.path(OUTPUT_DIR, "mccrary_test_results.txt"))

# ============================================================================
# 4. Store models for later use
# ============================================================================

assign("model_global", model_global, envir = .GlobalEnv)
assign("model_centered", model_centered, envir = .GlobalEnv)
assign("model_interaction", model_interaction, envir = .GlobalEnv)
assign("model_quadratic", model_quadratic, envir = .GlobalEnv)
assign("model_windowed", model_windowed, envir = .GlobalEnv)
assign("model_local", model_local, envir = .GlobalEnv)
assign("mccrary_test", mccrary_test, envir = .GlobalEnv)
assign("discontinuity_sample", discontinuity_sample, envir = .GlobalEnv)

message("\n", rep("=", 70))
message("Baseline analysis complete!")
message(rep("=", 70))


