# =============================================================================
# FILE: validation/calibration_harness.R
# PURPOSE: Time-aware calibration benchmarking harness with leakage guards
# =============================================================================



#' Run calibration benchmark on identical temporal folds
#'
#' @param data Data frame containing raw probabilities, outcomes, and time
#' @param prob_col Raw probability column name
#' @param outcome_col Binary outcome column name
#' @param time_col Time column name used for fold construction
#' @param info_time_cols Optional time columns for leakage checks
#' @param n_folds Number of folds
#' @param artifact_dir Output directory for reproducibility artifacts
#' @param default_method Production default method to validate
#' @return List with fold_metrics, summary, artifacts, folds
run_calibration_harness <- function(data,
                                    prob_col = "raw_prob",
                                    outcome_col = "outcome",
                                    time_col = "event_time",
                                    info_time_cols = c("label_available_time", "odds_available_time"),
                                    n_folds = 5,
                                    artifact_dir = file.path("validation", "artifacts"),
                                    default_method = "spline") {
  required <- c(prob_col, outcome_col, time_col)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols)) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }

  folds <- create_time_aware_folds(data, time_col = time_col, n_folds = n_folds)
  methods <- c("isotonic", "platt", "spline")

  fold_rows <- vector("list", length(folds) * length(methods))
  row_i <- 1L

  for (fold in folds) {
    train_df <- data[fold$train_idx, , drop = FALSE]
    test_df <- data[fold$test_idx, , drop = FALSE]

    assert_temporal_leakage_guards(
      train_df = train_df,
      test_df = test_df,
      time_col = time_col,
      info_time_cols = info_time_cols
    )

    train_prob <- as.numeric(train_df[[prob_col]])
    train_outcome <- as.numeric(train_df[[outcome_col]])
    test_prob <- as.numeric(test_df[[prob_col]])
    test_outcome <- as.numeric(test_df[[outcome_col]])

    calibrated_preds <- list(
      isotonic = calibrate_isotonic(train_prob, train_outcome, test_prob),
      platt = calibrate_platt(train_prob, train_outcome, test_prob),
      spline = calibrate_spline(train_prob, train_outcome, test_prob)
    )

    for (method in methods) {
      metrics <- calibration_metrics(calibrated_preds[[method]], test_outcome)
      fold_rows[[row_i]] <- data.frame(
        fold_id = fold$fold_id,
        method = method,
        brier = metrics$brier,
        log_loss = metrics$log_loss,
        ece = metrics$ece,
        n = metrics$n,
        train_max_time = fold$train_max_time,
        test_min_time = fold$test_min_time,
        stringsAsFactors = FALSE
      )
      row_i <- row_i + 1L
    }
  }

  fold_metrics <- do.call(rbind, fold_rows)
  summary <- aggregate(
    cbind(brier, log_loss, ece, n) ~ method,
    data = fold_metrics,
    FUN = mean
  )

  assert_default_calibration_performance(summary, default_method = default_method)

  dir.create(artifact_dir, recursive = TRUE, showWarnings = FALSE)
  fold_path <- file.path(artifact_dir, "calibration_fold_metrics.csv")
  summary_path <- file.path(artifact_dir, "calibration_summary.csv")
  rds_path <- file.path(artifact_dir, "calibration_harness_result.rds")

  utils::write.csv(fold_metrics, fold_path, row.names = FALSE)
  utils::write.csv(summary, summary_path, row.names = FALSE)

  output <- list(
    fold_metrics = fold_metrics,
    summary = summary,
    folds = folds,
    artifacts = list(fold_csv = fold_path, summary_csv = summary_path, rds = rds_path)
  )

  saveRDS(output, rds_path)
  output
}
