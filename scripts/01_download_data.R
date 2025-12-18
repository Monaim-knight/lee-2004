# ============================================================================
# 01_download_data.R
# ============================================================================
# Download and verify source data with integrity checks
# ============================================================================

source("scripts/00_setup.R")

# Data source URL
DATA_URL <- "https://github.com/scunning1975/mixtape/raw/master/lmb-data.dta"
DATA_DIR <- "data/raw"
DATA_FILE <- file.path(DATA_DIR, "lmb-data.dta")

# Create directory if it doesn't exist
if (!dir.exists(DATA_DIR)) {
  dir.create(DATA_DIR, recursive = TRUE)
}

# Download data if not present
if (!file.exists(DATA_FILE)) {
  message("Downloading data from: ", DATA_URL)
  download.file(
    url = DATA_URL,
    destfile = DATA_FILE,
    mode = "wb",
    quiet = FALSE
  )
  message("Data downloaded to: ", DATA_FILE)
} else {
  message("Data file already exists: ", DATA_FILE)
}

# Verify file exists and has content
if (!file.exists(DATA_FILE)) {
  stop("Failed to download data file")
}

file_size <- file.info(DATA_FILE)$size
if (file_size < 1000) {
  warning("Downloaded file is suspiciously small (", file_size, " bytes)")
}

# Compute checksum for integrity verification
if (require("digest", quietly = TRUE)) {
  file_checksum <- digest::digest(DATA_FILE, algo = "md5", file = TRUE)
  message("File MD5 checksum: ", file_checksum)
} else {
  message("digest package not available; skipping checksum")
  file_checksum <- NA_character_
}

message("Data file size: ", round(file_size / 1024, 2), " KB")
message("Download date: ", Sys.time())

# Log download metadata
download_log <- data.frame(
  url = DATA_URL,
  file = DATA_FILE,
  download_date = Sys.time(),
  file_size_bytes = file_size,
  file_size_kb = round(file_size / 1024, 2),
  checksum_md5 = ifelse(exists("file_checksum"), file_checksum, NA_character_),
  stringsAsFactors = FALSE
)

write.csv(download_log, "data/raw/download_log.csv", row.names = FALSE)
message("Download metadata saved to data/raw/download_log.csv")

