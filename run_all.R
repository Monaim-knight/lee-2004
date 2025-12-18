# ============================================================================
# run_all.R
# ============================================================================
# One-command runner: executes entire analysis pipeline
# ============================================================================

message("=", rep("=", 68))
message("LEE-MORETTI-BUTLER (2004) RD REPLICATION")
message("Running complete analysis pipeline...")
message("=", rep("=", 68))

start_time <- Sys.time()

# Run scripts in sequence
tryCatch({
  source("scripts/00_setup.R")
  source("scripts/01_download_data.R")
  source("scripts/02_clean_data.R")
  source("scripts/03_analysis_baseline.R")
  source("scripts/04_analysis_robustness.R")
  source("scripts/05_figures.R")
  
  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  message("\n", rep("=", 70))
  message("ANALYSIS COMPLETE!")
  message("Total runtime: ", round(elapsed, 2), " seconds")
  message(rep("=", 70))
  message("\nOutputs saved to:")
  message("  - Tables: outputs/tables/")
  message("  - Figures: outputs/figures/")
  message("  - Data: data/processed/")
  message("\nTo view results, check:")
  message("  - reports/paper.md (narrative report)")
  message("  - outputs/tables/all_rd_models_summary.csv (all estimates)")
  
}, error = function(e) {
  message("\nERROR: Analysis failed with the following error:")
  message(conditionMessage(e))
  traceback()
  stop("Analysis pipeline failed. Please check error messages above.")
})


