# ============================================================================
# test_rd.R
# ============================================================================
# Sanity checks and tests for RD analysis
# ============================================================================

# Load testthat if available (optional)
if (require("testthat", quietly = TRUE)) {
  library(testthat)
  use_testthat <- TRUE
} else {
  use_testthat <- FALSE
  message("testthat not available; running basic checks")
}

# Run analysis if outputs don't exist
if (!file.exists("outputs/tables/all_rd_models_summary.csv")) {
  message("Running analysis to generate test data...")
  source("run_all.R")
}

# Load results
all_results <- read.csv("outputs/tables/all_rd_models_summary.csv")
data <- readRDS("data/processed/lmb_clean.rds")

message("\n", rep("=", 70))
message("RUNNING SANITY CHECKS")
message(rep("=", 70))

# ============================================================================
# Test 1: Sample size checks
# ============================================================================

message("\n1. Sample size checks...")

test_sample_size <- function() {
  # Check that we have sufficient observations
  n_total <- nrow(data)
  n_treated <- sum(data$democrat_winner == 1)
  n_control <- sum(data$democrat_winner == 0)
  
  checks <- list(
    total_obs = n_total > 1000,
    treated_obs = n_treated > 100,
    control_obs = n_control > 100,
    balance = abs(n_treated - n_control) / n_total < 0.2
  )
  
  message("  Total observations: ", n_total)
  message("  Treated (Dem win): ", n_treated)
  message("  Control (Dem loss): ", n_control)
  message("  Balance check: ", ifelse(checks$balance, "PASS", "WARN"))
  
  if (!all(unlist(checks))) {
    warning("Some sample size checks failed")
  }
  
  return(checks)
}

sample_checks <- test_sample_size()

# ============================================================================
# Test 2: Cutoff behavior
# ============================================================================

message("\n2. Cutoff behavior checks...")

test_cutoff <- function() {
  # Check that cutoff is at 0.5
  cutoff <- 0.5
  at_cutoff <- sum(data$demvoteshare == cutoff)
  
  # Check treatment assignment
  above_cutoff <- sum(data$demvoteshare > cutoff)
  below_cutoff <- sum(data$demvoteshare < cutoff)
  
  # Check that treatment is correctly assigned
  treat_correct <- all(data$democrat_winner == as.integer(data$demvoteshare > cutoff))
  
  checks <- list(
    cutoff_defined = cutoff == 0.5,
    observations_above = above_cutoff > 0,
    observations_below = below_cutoff > 0,
    treatment_correct = treat_correct
  )
  
  message("  Observations at cutoff: ", at_cutoff)
  message("  Observations above cutoff: ", above_cutoff)
  message("  Observations below cutoff: ", below_cutoff)
  message("  Treatment assignment correct: ", ifelse(checks$treatment_correct, "PASS", "FAIL"))
  
  if (!all(unlist(checks))) {
    stop("Cutoff behavior checks failed")
  }
  
  return(checks)
}

cutoff_checks <- test_cutoff()

# ============================================================================
# Test 3: Estimate direction and stability
# ============================================================================

message("\n3. Estimate direction and stability checks...")

test_estimates <- function() {
  # Main estimates should be consistent in sign
  main_models <- c("centered", "interaction", "quadratic", "rdrobust")
  main_results <- all_results[all_results$model %in% main_models, ]
  
  # Check that estimates have consistent sign
  coefs <- main_results$coef[!is.na(main_results$coef)]
  signs_consistent <- length(unique(sign(coefs))) <= 1
  
  # Check that estimates are reasonable (not extreme)
  coefs_reasonable <- all(abs(coefs) < 100, na.rm = TRUE)
  
  # Check that standard errors are positive
  ses_positive <- all(main_results$se > 0, na.rm = TRUE)
  
  checks <- list(
    signs_consistent = signs_consistent,
    coefs_reasonable = coefs_reasonable,
    ses_positive = ses_positive
  )
  
  message("  Main model estimates:")
  print(main_results[, c("model", "coef", "se", "p_value")])
  message("  Signs consistent: ", ifelse(checks$signs_consistent, "PASS", "WARN"))
  message("  Coefficients reasonable: ", ifelse(checks$coefs_reasonable, "PASS", "WARN"))
  message("  Standard errors positive: ", ifelse(checks$ses_positive, "PASS", "FAIL"))
  
  if (!all(unlist(checks))) {
    warning("Some estimate checks failed")
  }
  
  return(checks)
}

estimate_checks <- test_estimates()

# ============================================================================
# Test 4: Placebo tests (should show null effects)
# ============================================================================

message("\n4. Placebo test checks...")

test_placebos <- function() {
  placebo_models <- grep("placebo", all_results$model, value = TRUE)
  placebo_results <- all_results[all_results$model %in% placebo_models, ]
  
  if (nrow(placebo_results) > 0) {
    # Placebo effects should be small and/or insignificant
    placebo_p_values <- placebo_results$p_value[!is.na(placebo_results$p_value)]
    placebo_insignificant <- mean(placebo_p_values > 0.05, na.rm = TRUE) > 0.5
    
    # Placebo coefficients should be smaller in magnitude than main effect
    main_coef <- all_results$coef[all_results$model == "rdrobust"][1]
    if (!is.na(main_coef)) {
      placebo_coefs <- abs(placebo_results$coef[!is.na(placebo_results$coef)])
      placebos_smaller <- all(placebo_coefs < abs(main_coef) * 1.5, na.rm = TRUE)
    } else {
      placebos_smaller <- NA
    }
    
    checks <- list(
      placebo_insignificant = placebo_insignificant,
      placebos_smaller = placebos_smaller
    )
    
    message("  Placebo results:")
    print(placebo_results[, c("model", "coef", "p_value")])
    message("  Placebos mostly insignificant: ", ifelse(checks$placebo_insignificant, "PASS", "WARN"))
    
    return(checks)
  } else {
    message("  No placebo results found")
    return(list())
  }
}

placebo_checks <- test_placebos()

# ============================================================================
# Test 5: Data integrity
# ============================================================================

message("\n5. Data integrity checks...")

test_data_integrity <- function() {
  # Check for missing values in key variables
  no_missing_score <- !any(is.na(data$score))
  no_missing_voteshare <- !any(is.na(data$demvoteshare))
  no_missing_treatment <- !any(is.na(data$democrat_winner))
  
  # Check that vote share is in valid range
  voteshare_valid <- all(data$demvoteshare >= 0 & data$demvoteshare <= 1, na.rm = TRUE)
  
  # Check that treatment is binary
  treatment_binary <- all(data$democrat_winner %in% c(0, 1), na.rm = TRUE)
  
  checks <- list(
    no_missing_score = no_missing_score,
    no_missing_voteshare = no_missing_voteshare,
    no_missing_treatment = no_missing_treatment,
    voteshare_valid = voteshare_valid,
    treatment_binary = treatment_binary
  )
  
  message("  No missing values in key variables: ", ifelse(all(c(no_missing_score, no_missing_voteshare, no_missing_treatment)), "PASS", "FAIL"))
  message("  Vote share in valid range [0,1]: ", ifelse(voteshare_valid, "PASS", "FAIL"))
  message("  Treatment is binary: ", ifelse(treatment_binary, "PASS", "FAIL"))
  
  if (!all(unlist(checks))) {
    stop("Data integrity checks failed")
  }
  
  return(checks)
}

integrity_checks <- test_data_integrity()

# ============================================================================
# Summary
# ============================================================================

message("\n", rep("=", 70))
message("TEST SUMMARY")
message(rep("=", 70))

all_checks <- c(
  sample_checks,
  cutoff_checks,
  estimate_checks,
  placebo_checks,
  integrity_checks
)

passed <- sum(unlist(all_checks), na.rm = TRUE)
total <- sum(!is.na(unlist(all_checks)))

message("\nTests passed: ", passed, " / ", total)

if (passed == total) {
  message("All checks passed! ✓")
} else {
  warning("Some checks failed or produced warnings. Review output above.")
}

message(rep("=", 70))


