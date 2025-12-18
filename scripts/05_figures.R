# ============================================================================
# 05_figures.R
# ============================================================================
# Generate all figures for the analysis
# ============================================================================

source("scripts/04_analysis_robustness.R")

# Load plotting functions
source("R/plotting.R")

message("\n", rep("=", 70))
message("GENERATING FIGURES")
message(rep("=", 70))

# Ensure data is loaded
if (!exists("data")) {
  data <- readRDS("data/processed/lmb_clean.rds")
}

OUTPUT_DIR <- "outputs/figures"
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# ============================================================================
# 1. Main RD plot (binned scatter)
# ============================================================================

message("\n1. Creating main RD plot...")

p_rd <- create_rd_plot(
  data = data,
  cutoff = 0.5,
  n_bins = 100,
  title = "RD Design: Binned Averages of Vote Share vs Liberal Score"
)

save_plot(
  plot = p_rd,
  filename = file.path(OUTPUT_DIR, "rd_main_plot.pdf"),
  width = 7,
  height = 5
)

# ============================================================================
# 2. Density plot for McCrary test
# ============================================================================

message("\n2. Creating density plot...")

p_density <- create_density_plot(
  data = data,
  cutoff = 0.5,
  title = "Density of Democratic Vote Share with Cutoff"
)

save_plot(
  plot = p_density,
  filename = file.path(OUTPUT_DIR, "density_plot.pdf"),
  width = 7,
  height = 5
)

# ============================================================================
# 3. rdrobust RD plot
# ============================================================================

message("\n3. Creating rdrobust RD plot...")

pdf(file.path(OUTPUT_DIR, "rdplot_rdrobust.pdf"), width = 7, height = 5)
rdrobust::rdplot(
  y = data$score,
  x = data$demvoteshare,
  c = 0.5
)
dev.off()

message("  - rdrobust plot saved to: ", file.path(OUTPUT_DIR, "rdplot_rdrobust.pdf"))

# ============================================================================
# 4. Bandwidth sensitivity plot (if results exist)
# ============================================================================

if (exists("bw_results") && nrow(bw_results) > 0) {
  message("\n4. Creating bandwidth sensitivity plot...")
  
  p_bw <- ggplot2::ggplot(bw_results, ggplot2::aes(x = bandwidth, y = coef)) +
    ggplot2::geom_point(size = 3, color = "steelblue") +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = coef - 1.96 * se, ymax = coef + 1.96 * se),
      width = 0.005,
      color = "steelblue"
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::labs(
      title = "Bandwidth Sensitivity Analysis",
      x = "Bandwidth (h)",
      y = "RD Estimate",
      caption = "Error bars show 95% confidence intervals"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12)
    )
  
  save_plot(
    plot = p_bw,
    filename = file.path(OUTPUT_DIR, "bandwidth_sensitivity.pdf"),
    width = 7,
    height = 5
  )
}

# ============================================================================
# 5. Placebo test plot (if results exist)
# ============================================================================

if (exists("placebo_results") && nrow(placebo_results) > 0) {
  message("\n5. Creating placebo test plot...")
  
  # Add true cutoff result
  if (exists("rd_est")) {
    placebo_plot_data <- rbind(
      data.frame(
        cutoff = 0.5,
        coef = rd_est$coef[1],
        se = rd_est$se[1],
        type = "True cutoff"
      ),
      data.frame(
        cutoff = placebo_results$cutoff,
        coef = placebo_results$coef,
        se = placebo_results$se,
        type = "Placebo"
      )
    )
    
    p_placebo <- ggplot2::ggplot(placebo_plot_data, ggplot2::aes(x = cutoff, y = coef, color = type)) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = coef - 1.96 * se, ymax = coef + 1.96 * se),
        width = 0.01
      ) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      ggplot2::labs(
        title = "Placebo Cutoff Tests",
        x = "Cutoff Value",
        y = "RD Estimate",
        color = "Type",
        caption = "True cutoff at 0.5; placebos at 0.45 and 0.55"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        axis.title = ggplot2::element_text(size = 12),
        legend.position = "bottom"
      ) +
      ggplot2::scale_color_manual(values = c("True cutoff" = "steelblue", "Placebo" = "red"))
    
    save_plot(
      plot = p_placebo,
      filename = file.path(OUTPUT_DIR, "placebo_tests.pdf"),
      width = 7,
      height = 5
    )
  }
}

message("\n", rep("=", 70))
message("All figures generated!")
message("Output directory: ", OUTPUT_DIR)
message(rep("=", 70))

