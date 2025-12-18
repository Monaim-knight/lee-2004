# ============================================================================
# plotting.R
# ============================================================================
# Visualization functions for RD analysis
# ============================================================================

#' Create binned scatter plot for RD visualization
#'
#' @param data Data frame with score and demvoteshare
#' @param cutoff Cutoff value (default 0.5)
#' @param n_bins Number of bins (default 100)
#' @param title Plot title
#' @return ggplot object
create_rd_plot <- function(data, cutoff = 0.5, n_bins = 100, 
                            title = "RD Design: Binned Averages of Vote Share vs Liberal Score") {
  
  # Create bins
  df_binned <- data %>%
    dplyr::mutate(bin = dplyr::ntile(demvoteshare, n_bins)) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(
      avg_score = mean(score, na.rm = TRUE),
      avg_vote_share = mean(demvoteshare, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Create plot
  p <- ggplot2::ggplot(df_binned, ggplot2::aes(x = avg_vote_share, y = avg_score)) +
    ggplot2::geom_point(color = "steelblue", alpha = 0.7, size = 1.5) +
    ggplot2::geom_vline(xintercept = cutoff, linetype = "dashed", 
                        color = "red", linewidth = 1) +
    ggplot2::labs(
      title = title,
      x = "Democratic Vote Share (Running Variable)",
      y = "Liberalism Score (Outcome Variable)",
      caption = paste("Cutoff at", cutoff, "marked with red dashed line")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      plot.caption = ggplot2::element_text(size = 10, color = "gray50")
    )
  
  return(p)
}

#' Create density plot for McCrary test visualization
#'
#' @param data Data frame with demvoteshare
#' @param cutoff Cutoff value (default 0.5)
#' @param title Plot title
#' @return ggplot object
create_density_plot <- function(data, cutoff = 0.5, 
                                title = "Density of Democratic Vote Share with Cutoff") {
  
  p <- ggplot2::ggplot(data, ggplot2::aes(x = demvoteshare)) +
    ggplot2::geom_density(fill = "lightblue", alpha = 0.6, color = "steelblue") +
    ggplot2::geom_vline(xintercept = cutoff, linetype = "dashed", 
                        color = "red", linewidth = 1) +
    ggplot2::labs(
      title = title,
      x = "Democratic Vote Share",
      y = "Density",
      caption = paste("Cutoff at", cutoff, "marked with red dashed line")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::coord_cartesian(xlim = c(0.2, 0.8)) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      plot.caption = ggplot2::element_text(size = 10, color = "gray50")
    )
  
  return(p)
}

#' Save plot with consistent styling
#'
#' @param plot ggplot object
#' @param filename Output filename
#' @param width Plot width in inches (default 7)
#' @param height Plot height in inches (default 5)
save_plot <- function(plot, filename, width = 7, height = 5) {
  output_dir <- dirname(filename)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    device = "pdf"
  )
  
  message("Plot saved to: ", filename)
}

