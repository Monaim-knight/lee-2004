# ============================================================================
# 02_clean_data.R
# ============================================================================
# Clean data, create treatment indicator, and prepare for analysis
# ============================================================================

source("scripts/01_download_data.R")

# Define data file path if not set
if (!exists("DATA_FILE")) {
  DATA_FILE <- "data/raw/lmb-data.dta"
}

# Read data
message("Reading data from: ", DATA_FILE)
data_raw <- haven::read_dta(DATA_FILE)

message("Original data dimensions: ", nrow(data_raw), " rows, ", ncol(data_raw), " columns")

# Select key variables
data <- data_raw %>%
  dplyr::select(
    score,              # Outcome: legislator liberalism score
    democrat,           # Party indicator
    demvoteshare        # Running variable: Democratic two-party vote share
  ) %>%
  # Remove missing values in key variables
  dplyr::filter(
    !is.na(score),
    !is.na(demvoteshare),
    !is.na(democrat)
  )

message("After removing missing values: ", nrow(data), " rows")

# Create treatment indicator (1 if Democrat wins, 0 otherwise)
# Cutoff is 0.5 (50% vote share)
data <- data %>%
  dplyr::mutate(
    democrat_winner = as.integer(demvoteshare > 0.5),
    demvoteshare_centered = demvoteshare - 0.5
  )

# Verify treatment assignment
message("\nTreatment assignment summary:")
message("Democrat winners (demvoteshare > 0.5): ", sum(data$democrat_winner == 1))
message("Democrat losers (demvoteshare <= 0.5): ", sum(data$democrat_winner == 0))
message("Observations exactly at cutoff: ", sum(data$demvoteshare == 0.5))

# Save cleaned data
PROCESSED_DIR <- "data/processed"
if (!dir.exists(PROCESSED_DIR)) {
  dir.create(PROCESSED_DIR, recursive = TRUE)
}

saveRDS(data, file.path(PROCESSED_DIR, "lmb_clean.rds"))
write.csv(data, file.path(PROCESSED_DIR, "lmb_clean.csv"), row.names = FALSE)

message("\nCleaned data saved to:")
message("  - ", file.path(PROCESSED_DIR, "lmb_clean.rds"))
message("  - ", file.path(PROCESSED_DIR, "lmb_clean.csv"))

# Summary statistics
message("\nSummary statistics:")
message("Score - Mean: ", round(mean(data$score, na.rm = TRUE), 3), 
        ", SD: ", round(sd(data$score, na.rm = TRUE), 3))
message("Demvoteshare - Mean: ", round(mean(data$demvoteshare, na.rm = TRUE), 3),
        ", SD: ", round(sd(data$demvoteshare, na.rm = TRUE), 3))
message("Range: [", round(min(data$demvoteshare, na.rm = TRUE), 3), 
        ", ", round(max(data$demvoteshare, na.rm = TRUE), 3), "]")

# Store in global environment for subsequent scripts
assign("data", data, envir = .GlobalEnv)
message("\nData ready for analysis!")

