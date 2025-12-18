# ============================================================================
# rd_helpers.R
# ============================================================================
# Helper functions for RD analysis
# ============================================================================

#' Run RD regression with specified specification
#'
#' @param data Data frame with score, democrat_winner, demvoteshare_centered
#' @param spec Specification type: "global", "centered", "interaction", "quadratic"
#' @param window Optional: restrict to window around cutoff (e.g., c(0.45, 0.55))
#' @return Fitted model object
run_rd_regression <- function(data, spec = "centered", window = NULL) {
  
  if (!is.null(window)) {
    data <- data %>%
      dplyr::filter(
        demvoteshare >= window[1] & demvoteshare <= window[2]
      ) %>%
      dplyr::mutate(
        demvoteshare_centered = demvoteshare - 0.5
      )
  }
  
  if (spec == "global") {
    model <- lm(score ~ democrat_winner, data = data)
  } else if (spec == "centered") {
    model <- lm(score ~ democrat_winner + demvoteshare_centered, data = data)
  } else if (spec == "interaction") {
    model <- lm(score ~ democrat_winner + demvoteshare_centered + 
                democrat_winner:demvoteshare_centered, data = data)
  } else if (spec == "quadratic") {
    model <- lm(score ~ democrat_winner + demvoteshare_centered + 
                I(demvoteshare_centered^2) +
                democrat_winner:demvoteshare_centered + 
                democrat_winner:I(demvoteshare_centered^2), data = data)
  } else {
    stop("Unknown specification: ", spec)
  }
  
  return(model)
}

#' Extract RD estimate summary
#'
#' @param model Fitted model object
#' @param model_name Character name for the model
#' @return Data frame with coefficient, SE, p-value, and sample size
extract_rd_summary <- function(model, model_name = "model") {
  coef_summary <- summary(model)$coefficients
  
  # Find treatment coefficient (usually second row, but check for democrat_winner)
  treat_idx <- which(rownames(coef_summary) == "democrat_winner")
  if (length(treat_idx) == 0) {
    # Try first non-intercept coefficient
    treat_idx <- 2
  }
  
  result <- data.frame(
    model = model_name,
    coef = coef_summary[treat_idx, "Estimate"],
    se = coef_summary[treat_idx, "Std. Error"],
    p_value = coef_summary[treat_idx, "Pr(>|t|)"],
    n_obs = length(model$residuals),
    stringsAsFactors = FALSE
  )
  
  return(result)
}

#' Run bandwidth sensitivity analysis
#'
#' @param y Outcome variable
#' @param x Running variable
#' @param c Cutoff value
#' @param bandwidths Vector of bandwidth values to test
#' @return Data frame with results for each bandwidth
run_bandwidth_sensitivity <- function(y, x, c = 0.5, bandwidths = c(0.03, 0.05, 0.08, 0.10)) {
  results <- lapply(bandwidths, function(h) {
    tryCatch({
      est <- rdrobust::rdrobust(y = y, x = x, c = c, h = h)
      data.frame(
        bandwidth = h,
        coef = est$coef[1],
        se = est$se[1],
        p_value = est$pv[1],
        n_obs = est$N,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      data.frame(
        bandwidth = h,
        coef = NA_real_,
        se = NA_real_,
        p_value = NA_real_,
        n_obs = NA_integer_,
        stringsAsFactors = FALSE
      )
    })
  })
  
  return(do.call(rbind, results))
}

#' Run placebo cutoff tests
#'
#' @param y Outcome variable
#' @param x Running variable
#' @param true_cutoff True cutoff value (default 0.5)
#' @param placebo_cutoffs Vector of placebo cutoff values
#' @return Data frame with results for each placebo cutoff
run_placebo_tests <- function(y, x, true_cutoff = 0.5, placebo_cutoffs = c(0.45, 0.55)) {
  results <- lapply(placebo_cutoffs, function(cc) {
    tryCatch({
      est <- rdrobust::rdrobust(y = y, x = x, c = cc)
      data.frame(
        cutoff = cc,
        coef = est$coef[1],
        se = est$se[1],
        p_value = est$pv[1],
        n_obs = est$N,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      data.frame(
        cutoff = cc,
        coef = NA_real_,
        se = NA_real_,
        p_value = NA_real_,
        n_obs = NA_integer_,
        stringsAsFactors = FALSE
      )
    })
  })
  
  return(do.call(rbind, results))
}


