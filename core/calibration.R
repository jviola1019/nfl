# =============================================================================
# FILE: core/calibration.R
# PURPOSE: Sport-agnostic probability calibration methods
#
# VERSION: 2.7.0
# LAST UPDATED: 2026-02-02
#
# DESCRIPTION:
#   Generic calibration framework for adjusting raw model probabilities
#   to match empirical outcomes. Supports multiple calibration methods.
#
# EXPORTS:
#   - calibrate_isotonic(): Isotonic regression calibration
#   - calibrate_platt(): Platt scaling (logistic)
#   - calibrate_spline(): GAM spline calibration
#   - reliability_diagram(): Generate calibration plot data
#   - expected_calibration_error(): ECE calculation
# =============================================================================

#' Clamp probability to valid range
#'
#' @param p Probability value(s)
#' @param eps Minimum distance from 0 and 1
#' @return Clamped probability
#' @export
clamp_probability <- function(p, eps = 1e-9) {
  p <- suppressWarnings(as.numeric(p))
  pmin(pmax(p, eps), 1 - eps)
}

#' Isotonic regression calibration
#'
#' Fits monotonic calibration function using isotonic regression.
#' Ensures calibrated probabilities are monotonically increasing.
#'
#' @param raw_probs Raw model probabilities (training)
#' @param outcomes Binary outcomes (0/1) for training
#' @param new_probs Probabilities to calibrate (optional)
#' @return If new_probs provided, calibrated probabilities; else, fitted model
#'
#' @details
#' Isotonic regression fits a step function that is monotonically
#' non-decreasing. This is appropriate when you expect higher raw
#' probabilities to correspond to higher true probabilities.
#'
#' @export
calibrate_isotonic <- function(raw_probs, outcomes, new_probs = NULL) {
  # Fit isotonic regression
  iso_fit <- isoreg(raw_probs, outcomes)

  if (is.null(new_probs)) {
    return(iso_fit)
  }

  # Apply calibration to new probabilities
  # Ensure monotonic x ordering for stepfun
  ord <- order(iso_fit$x)
  x_ord <- iso_fit$x[ord]
  y_ord <- iso_fit$yf[ord]
  sf <- stepfun(x_ord, c(y_ord[1], y_ord))
  calibrated <- sf(new_probs)

  clamp_probability(calibrated)
}

#' Platt scaling (logistic calibration)
#'
#' Fits logistic regression to calibrate probabilities.
#' cal_prob = 1 / (1 + exp(-(a * logit(raw_prob) + b)))
#'
#' @param raw_probs Raw model probabilities (training)
#' @param outcomes Binary outcomes (0/1) for training
#' @param new_probs Probabilities to calibrate (optional)
#' @return If new_probs provided, calibrated probabilities; else, fitted model
#'
#' @export
calibrate_platt <- function(raw_probs, outcomes, new_probs = NULL) {
  # Convert to log-odds for linear model
  log_odds <- log(clamp_probability(raw_probs) / (1 - clamp_probability(raw_probs)))

  # Fit logistic regression
  df_train <- data.frame(log_odds = log_odds, outcome = outcomes)
  fit <- glm(outcome ~ log_odds, data = df_train, family = binomial())

  if (is.null(new_probs)) {
    return(fit)
  }

  # Apply to new probabilities
  new_log_odds <- log(clamp_probability(new_probs) / (1 - clamp_probability(new_probs)))
  df_new <- data.frame(log_odds = new_log_odds)
  calibrated <- predict(fit, newdata = df_new, type = "response")

  clamp_probability(as.numeric(calibrated))
}

#' GAM spline calibration
#'
#' Fits GAM with smoothing spline for flexible calibration.
#' Requires mgcv package.
#'
#' @param raw_probs Raw model probabilities (training)
#' @param outcomes Binary outcomes (0/1) for training
#' @param new_probs Probabilities to calibrate (optional)
#' @param k Spline basis dimension (default: 10)
#' @return If new_probs provided, calibrated probabilities; else, fitted model
#'
#' @details
#' GAM spline calibration provides more flexibility than isotonic or
#' Platt scaling. The smoothing penalty prevents overfitting.
#' Validated to provide -6.9% Brier improvement for NFL predictions.
#'
#' @export
calibrate_spline <- function(raw_probs, outcomes, new_probs = NULL, k = 10) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    warning("mgcv package required for spline calibration; falling back to isotonic")
    return(calibrate_isotonic(raw_probs, outcomes, new_probs))
  }

  df_train <- data.frame(raw_prob = raw_probs, outcome = outcomes)
  fit <- mgcv::gam(outcome ~ s(raw_prob, k = k), data = df_train, family = binomial())

  if (is.null(new_probs)) {
    return(fit)
  }

  df_new <- data.frame(raw_prob = new_probs)
  calibrated <- predict(fit, newdata = df_new, type = "response")

  clamp_probability(as.numeric(calibrated))
}

#' Generate reliability diagram data
#'
#' Bins predictions and computes mean predicted vs actual probabilities.
#'
#' @param probs Predicted probabilities
#' @param outcomes Binary outcomes (0/1)
#' @param n_bins Number of bins (default: 10)
#' @return Data frame with bin centers, mean predicted, mean actual, counts
#'
#' @export
reliability_diagram <- function(probs, outcomes, n_bins = 10) {
  # Create equal-width bins
  breaks <- seq(0, 1, length.out = n_bins + 1)
  bins <- cut(probs, breaks = breaks, include.lowest = TRUE)

  # Compute statistics per bin
  result <- data.frame(
    bin_center = (breaks[-length(breaks)] + breaks[-1]) / 2,
    mean_predicted = tapply(probs, bins, mean, na.rm = TRUE),
    mean_actual = tapply(outcomes, bins, mean, na.rm = TRUE),
    count = as.numeric(table(bins))
  )

  result[complete.cases(result), ]
}

#' Expected Calibration Error (ECE)
#'
#' Measures calibration quality as weighted average of bin-level errors.
#'
#' @param probs Predicted probabilities
#' @param outcomes Binary outcomes (0/1)
#' @param n_bins Number of bins (default: 10)
#' @return ECE value (lower is better; 0 = perfectly calibrated)
#'
#' @details
#' ECE = sum(n_i / N * |mean_pred_i - mean_actual_i|)
#' where n_i is count in bin i, N is total count.
#'
#' @export
expected_calibration_error <- function(probs, outcomes, n_bins = 10) {
  rd <- reliability_diagram(probs, outcomes, n_bins)

  if (nrow(rd) == 0) return(NA_real_)

  n_total <- sum(rd$count, na.rm = TRUE)
  if (n_total == 0) return(NA_real_)

  # Weighted average of absolute errors
  sum(rd$count * abs(rd$mean_predicted - rd$mean_actual), na.rm = TRUE) / n_total
}

#' Brier score decomposition
#'
#' Decomposes Brier score into uncertainty, reliability, and resolution.
#'
#' @param probs Predicted probabilities
#' @param outcomes Binary outcomes (0/1)
#' @param n_bins Number of bins (default: 10)
#' @return List with uncertainty, reliability, resolution, and brier
#'
#' @details
#' Brier = Uncertainty - Resolution + Reliability
#' - Uncertainty: baseline difficulty (determined by outcome base rate)
#' - Resolution: model's ability to distinguish outcomes
#' - Reliability: calibration quality (lower is better)
#'
#' @export
brier_decomposition <- function(probs, outcomes, n_bins = 10) {
  # Overall base rate
  base_rate <- mean(outcomes, na.rm = TRUE)
  n_total <- length(outcomes)

  # Uncertainty (baseline)
  uncertainty <- base_rate * (1 - base_rate)

  # Bin probabilities
  breaks <- seq(0, 1, length.out = n_bins + 1)
  bins <- cut(probs, breaks = breaks, include.lowest = TRUE)

  # Per-bin statistics
  bin_means <- tapply(probs, bins, mean, na.rm = TRUE)
  bin_actuals <- tapply(outcomes, bins, mean, na.rm = TRUE)
  bin_counts <- as.numeric(table(bins))

  # Resolution
  resolution <- sum(bin_counts * (bin_actuals - base_rate)^2, na.rm = TRUE) / n_total

  # Reliability
  reliability <- sum(bin_counts * (bin_means - bin_actuals)^2, na.rm = TRUE) / n_total

  # Brier score
  brier <- mean((probs - outcomes)^2, na.rm = TRUE)

  list(
    uncertainty = uncertainty,
    resolution = resolution,
    reliability = reliability,
    brier = brier,
    skill_score = (resolution - reliability) / uncertainty
  )
}

#' Shrink probability toward reference
#'
#' Applies Bayesian shrinkage to model probabilities toward a reference
#' (e.g., market consensus or historical base rate).
#'
#' @param model_prob Model's raw probability
#' @param reference_prob Reference probability (e.g., market)
#' @param shrinkage Weight on reference (0 = model only, 1 = reference only)
#' @return Shrunk probability
#'
#' @export
shrink_probability <- function(model_prob, reference_prob, shrinkage = 0.5) {
  model_prob <- clamp_probability(model_prob)
  reference_prob <- clamp_probability(reference_prob)
  shrinkage <- pmax(0, pmin(1, shrinkage))

  shrunk <- (1 - shrinkage) * model_prob + shrinkage * reference_prob
  clamp_probability(shrunk)
}

#' Compute calibration metrics for binary outcomes
#'
#' @param probs Predicted probabilities
#' @param outcomes Binary outcomes (0/1)
#' @param n_bins Number of bins for ECE
#' @return Named list with brier, log_loss, ece, and n
#' @export
calibration_metrics <- function(probs, outcomes, n_bins = 10) {
  probs <- clamp_probability(as.numeric(probs))
  outcomes <- as.numeric(outcomes)
  valid <- !is.na(probs) & !is.na(outcomes)

  if (!any(valid)) {
    return(list(brier = NA_real_, log_loss = NA_real_, ece = NA_real_, n = 0L))
  }

  probs <- probs[valid]
  outcomes <- outcomes[valid]

  list(
    brier = mean((probs - outcomes)^2),
    log_loss = -mean(outcomes * log(probs) + (1 - outcomes) * log(1 - probs)),
    ece = expected_calibration_error(probs, outcomes, n_bins = n_bins),
    n = length(probs)
  )
}

#' Create expanding-window, time-aware cross-validation folds
#'
#' @param data Data frame sorted by time column (or sortable)
#' @param time_col Column name for event time
#' @param n_folds Number of folds
#' @param min_train_size Minimum train rows per fold
#' @param min_test_size Minimum test rows per fold
#' @return List of folds with train_idx, test_idx, and cutoff metadata
#' @export
create_time_aware_folds <- function(data,
                                    time_col,
                                    n_folds = 5,
                                    min_train_size = 200,
                                    min_test_size = 50) {
  if (!time_col %in% names(data)) stop("time_col missing from data")

  ord <- order(data[[time_col]], na.last = NA)
  if (length(ord) < (min_train_size + min_test_size)) {
    stop("Not enough rows for requested temporal folds")
  }

  n <- length(ord)
  test_block <- floor((n - min_train_size) / n_folds)
  test_block <- max(test_block, min_test_size)

  folds <- list()
  fold_id <- 1L
  start_test <- min_train_size + 1L

  while (start_test <= (n - min_test_size + 1L) && fold_id <= n_folds) {
    end_test <- min(start_test + test_block - 1L, n)
    train_pos <- seq_len(start_test - 1L)
    test_pos <- seq.int(start_test, end_test)

    train_idx <- ord[train_pos]
    test_idx <- ord[test_pos]

    folds[[fold_id]] <- list(
      fold_id = fold_id,
      train_idx = train_idx,
      test_idx = test_idx,
      train_max_time = max(data[[time_col]][train_idx], na.rm = TRUE),
      test_min_time = min(data[[time_col]][test_idx], na.rm = TRUE)
    )

    fold_id <- fold_id + 1L
    start_test <- end_test + 1L
  }

  if (!length(folds)) stop("Unable to create valid temporal folds")
  folds
}

#' Assert temporal leakage guards for one fold
#'
#' @param train_df Training partition
#' @param test_df Testing partition
#' @param time_col Event time column
#' @param info_time_cols Optional columns that must not exceed train cutoff
#' @return TRUE (invisible) if assertions pass
#' @export
assert_temporal_leakage_guards <- function(train_df,
                                           test_df,
                                           time_col,
                                           info_time_cols = character()) {
  train_max <- max(train_df[[time_col]], na.rm = TRUE)
  test_min <- min(test_df[[time_col]], na.rm = TRUE)

  if (!(train_max < test_min)) {
    stop("Leakage guard failed: training window overlaps testing window")
  }

  for (col in info_time_cols) {
    if (!col %in% names(train_df)) next
    if (any(train_df[[col]] > train_max, na.rm = TRUE)) {
      stop(sprintf("Leakage guard failed: '%s' contains future info in train split", col))
    }
  }

  invisible(TRUE)
}

#' Ensure default calibration method is not materially worse than alternatives
#'
#' @param summary_df Summary metrics by method
#' @param default_method Method expected to be production default
#' @param tolerance Named numeric vector for tolerated metric degradation
#' @return TRUE (invisible) if default method passes all tolerances
#' @export
assert_default_calibration_performance <- function(
    summary_df,
    default_method,
    tolerance = c(brier = 0.002, log_loss = 0.005, ece = 0.01)) {
  if (!default_method %in% summary_df$method) {
    stop(sprintf("Default method '%s' not present in summary", default_method))
  }

  metric_names <- intersect(names(tolerance), names(summary_df))
  default_row <- summary_df[summary_df$method == default_method, , drop = FALSE]

  for (metric in metric_names) {
    best <- min(summary_df[[metric]], na.rm = TRUE)
    allowed <- best + tolerance[[metric]]
    if (default_row[[metric]][1] > allowed) {
      stop(sprintf(
        "Default method '%s' underperforms on %s (%.6f > %.6f allowed)",
        default_method,
        metric,
        default_row[[metric]][1],
        allowed
      ))
    }
  }

  invisible(TRUE)
}
