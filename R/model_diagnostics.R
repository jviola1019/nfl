# =============================================================================
# R/model_diagnostics.R
# Model Calibration and Performance Diagnostics
# =============================================================================
# Functions for analyzing model calibration, computing performance metrics,
# and generating diagnostic reports.
#
# Usage:
#   source("R/model_diagnostics.R")
#   diagnostics <- compute_calibration_diagnostics(predictions, actuals)
#
# =============================================================================

#' Compute Calibration Diagnostics
#'
#' Analyzes model calibration by computing key metrics across probability bins.
#'
#' @param predicted Vector of predicted probabilities
#' @param actual Vector of actual outcomes (0/1)
#' @param n_bins Number of probability bins (default 10)
#' @return List with calibration metrics
#' @export
compute_calibration_diagnostics <- function(predicted, actual, n_bins = 10) {

  if (length(predicted) != length(actual)) {
    stop("predicted and actual must have same length")
  }

  # Remove NA values
  valid <- !is.na(predicted) & !is.na(actual)
  predicted <- predicted[valid]
  actual <- actual[valid]

  if (length(predicted) == 0) {
    return(list(
      n_samples = 0,
      brier_score = NA,
      log_loss = NA,
      calibration_by_bin = NULL,
      calibration_error = NA
    ))
  }

  # Overall metrics
  brier_score <- mean((predicted - actual)^2)
  log_loss <- -mean(actual * log(pmax(predicted, 1e-10)) +
                    (1 - actual) * log(pmax(1 - predicted, 1e-10)))

  # Bin-level calibration
  bins <- cut(predicted, breaks = seq(0, 1, length.out = n_bins + 1),
              include.lowest = TRUE)

  calibration_by_bin <- data.frame(
    bin = levels(bins),
    n = as.numeric(table(bins)),
    mean_predicted = tapply(predicted, bins, mean),
    mean_actual = tapply(actual, bins, mean)
  )
  calibration_by_bin$calibration_gap <- with(calibration_by_bin,
                                              mean_actual - mean_predicted)

  # Expected Calibration Error (ECE)
  weights <- calibration_by_bin$n / sum(calibration_by_bin$n, na.rm = TRUE)
  calibration_error <- sum(weights * abs(calibration_by_bin$calibration_gap),
                           na.rm = TRUE)

  list(
    n_samples = length(predicted),
    brier_score = brier_score,
    log_loss = log_loss,
    calibration_by_bin = calibration_by_bin,
    calibration_error = calibration_error
  )
}


#' Print Calibration Report
#'
#' Prints a formatted calibration report to console.
#'
#' @param diagnostics Output from compute_calibration_diagnostics()
#' @param method Calibration method name (for display)
#' @export
print_calibration_report <- function(diagnostics, method = "Unknown") {

  cat("\n")
  cat("=============================================================================\n")
  cat(sprintf("  CALIBRATION REPORT: %s\n", toupper(method)))
  cat("=============================================================================\n\n")

  if (is.null(diagnostics) || diagnostics$n_samples == 0) {
    cat("  No samples available for calibration analysis.\n\n")
    return(invisible(NULL))
  }

  cat(sprintf("  Samples: %d\n", diagnostics$n_samples))
  cat(sprintf("  Brier Score: %.4f (lower is better, 0.25 = random)\n",
              diagnostics$brier_score))
  cat(sprintf("  Log Loss: %.4f (lower is better)\n", diagnostics$log_loss))
  cat(sprintf("  Expected Calibration Error: %.4f (lower is better)\n\n",
              diagnostics$calibration_error))

  if (!is.null(diagnostics$calibration_by_bin)) {
    cat("  Calibration by Probability Bin:\n")
    cat("  --------------------------------\n")
    cat(sprintf("  %-12s %6s %8s %8s %8s\n",
                "Bin", "N", "Pred", "Actual", "Gap"))

    for (i in seq_len(nrow(diagnostics$calibration_by_bin))) {
      row <- diagnostics$calibration_by_bin[i, ]
      if (!is.na(row$mean_predicted)) {
        cat(sprintf("  %-12s %6d %8.3f %8.3f %+8.3f\n",
                    row$bin, row$n, row$mean_predicted,
                    row$mean_actual, row$calibration_gap))
      }
    }
  }

  cat("\n=============================================================================\n\n")
  invisible(diagnostics)
}


#' Compare Calibration Methods
#'
#' Compares multiple calibration methods and returns summary.
#'
#' @param predictions_list Named list of prediction vectors
#' @param actual Vector of actual outcomes
#' @return Data frame with comparison metrics
#' @export
compare_calibration_methods <- function(predictions_list, actual) {

  results <- lapply(names(predictions_list), function(method) {
    diag <- compute_calibration_diagnostics(predictions_list[[method]], actual)
    data.frame(
      method = method,
      n_samples = diag$n_samples,
      brier_score = diag$brier_score,
      log_loss = diag$log_loss,
      calibration_error = diag$calibration_error,
      stringsAsFactors = FALSE
    )
  })

  result_df <- do.call(rbind, results)
  result_df <- result_df[order(result_df$brier_score), ]
  rownames(result_df) <- NULL

  result_df
}


#' Compute Brier Skill Score
#'
#' Compares model Brier score against baseline (climatology or market).
#'
#' @param predicted Model predicted probabilities
#' @param actual Actual outcomes (0/1)
#' @param baseline Baseline probabilities (default 0.5 for random)
#' @return Brier Skill Score (-Inf to 1, positive = better than baseline)
#' @export
brier_skill_score <- function(predicted, actual, baseline = 0.5) {

  valid <- !is.na(predicted) & !is.na(actual)
  predicted <- predicted[valid]
  actual <- actual[valid]

  if (length(baseline) == 1) {
    baseline <- rep(baseline, length(predicted))
  } else {
    baseline <- baseline[valid]
  }

  brier_model <- mean((predicted - actual)^2)
  brier_baseline <- mean((baseline - actual)^2)

  if (brier_baseline == 0) return(NA)

  1 - brier_model / brier_baseline
}


#' Reliability Diagram Data
#'
#' Prepares data for plotting a reliability diagram.
#'
#' @param predicted Predicted probabilities
#' @param actual Actual outcomes (0/1)
#' @param n_bins Number of bins (default 10)
#' @return Data frame suitable for plotting
#' @export
reliability_diagram_data <- function(predicted, actual, n_bins = 10) {

  valid <- !is.na(predicted) & !is.na(actual)
  predicted <- predicted[valid]
  actual <- actual[valid]

  if (length(predicted) == 0) {
    return(data.frame(
      bin_center = numeric(),
      mean_predicted = numeric(),
      mean_actual = numeric(),
      n = numeric()
    ))
  }

  breaks <- seq(0, 1, length.out = n_bins + 1)
  bins <- cut(predicted, breaks = breaks, include.lowest = TRUE)

  data.frame(
    bin_center = (breaks[-length(breaks)] + breaks[-1]) / 2,
    mean_predicted = tapply(predicted, bins, mean),
    mean_actual = tapply(actual, bins, mean),
    n = as.numeric(table(bins))
  )
}
