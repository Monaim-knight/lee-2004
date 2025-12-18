# ============================================================================
# 00_setup.R
# ============================================================================
# Project setup: package management, options, and environment configuration
# ============================================================================

# Set seed for reproducibility
set.seed(42)

# Set options
options(
  stringsAsFactors = FALSE,
  scipen = 999,
  digits = 4,
  warn = 1
)

# Check if renv is available, initialize if needed
if (!require("renv", quietly = TRUE)) {
  install.packages("renv")
  library(renv)
}

# Initialize renv if not already done
if (!file.exists("renv.lock")) {
  message("Initializing renv...")
  renv::init(bare = TRUE)
}

# Restore packages from lock file if it exists
if (file.exists("renv.lock")) {
  message("Restoring packages from renv.lock...")
  renv::restore()
}

# Helper function for conditional package installation
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    message(paste("Installing", pkg, "..."))
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Core packages
core_packages <- c(
  "haven",      # Read Stata files
  "dplyr",      # Data manipulation
  "ggplot2",    # Visualization
  "tidyr",      # Data tidying
  "digest"      # Checksum verification
)

# Analysis packages
analysis_packages <- c(
  "rddensity",  # McCrary density test
  "rdrobust",   # Bias-corrected RD
  "rdlocrand"   # Local randomization tests
)

# Note: 'rdd' package is deprecated/removed from CRAN
# We use 'rdrobust' for all RD estimation instead

# Install and load all packages
message("Installing and loading packages...")
all_packages <- c(core_packages, analysis_packages)
invisible(lapply(all_packages, install_if_missing))

message("Setup complete!")
message("Packages loaded: ", paste(all_packages, collapse = ", "))

