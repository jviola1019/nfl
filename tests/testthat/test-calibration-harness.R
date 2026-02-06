# =============================================================================
# Tests for time-aware calibration harness and leakage guards
# =============================================================================

source(file.path(.test_project_root, "core", "calibration.R"), local = FALSE)
source(file.path(.test_project_root, "validation", "calibration_harness.R"), local = FALSE)

make_calibration_fixture <- function(n = 480) {
  set.seed(2026)
  event_time <- as.POSIXct("2020-01-01", tz = "UTC") + seq_len(n) * 86400
  raw_prob <- plogis(seq(-2, 2, length.out = n) + rnorm(n, sd = 0.3))
  true_prob <- plogis(-0.3 + 0.8 * qlogis(pmin(pmax(raw_prob, 1e-6), 1 - 1e-6)))
  outcome <- rbinom(n, 1, true_prob)

  data.frame(
    event_time = event_time,
    raw_prob = raw_prob,
    outcome = outcome,
    label_available_time = event_time,
    odds_available_time = event_time,
    stringsAsFactors = FALSE
  )
}

test_that("time-aware folds enforce strict temporal split", {
  df <- make_calibration_fixture()
  folds <- create_time_aware_folds(df, time_col = "event_time", n_folds = 4, min_train_size = 180, min_test_size = 40)

  expect_true(length(folds) >= 3)
  expect_true(all(vapply(folds, function(f) f$train_max_time < f$test_min_time, logical(1))))
})

test_that("leakage guard catches future information in training", {
  df <- make_calibration_fixture()
  folds <- create_time_aware_folds(df, time_col = "event_time", n_folds = 3, min_train_size = 180, min_test_size = 40)
  fold <- folds[[1]]

  train_df <- df[fold$train_idx, , drop = FALSE]
  test_df <- df[fold$test_idx, , drop = FALSE]
  train_df$label_available_time[nrow(train_df)] <- test_df$event_time[1]

  expect_error(
    assert_temporal_leakage_guards(
      train_df,
      test_df,
      time_col = "event_time",
      info_time_cols = c("label_available_time", "odds_available_time")
    ),
    "future info"
  )
})

test_that("calibration harness returns fold outputs and summary artifacts", {
  df <- make_calibration_fixture()
  artifact_dir <- file.path(tempdir(), "calibration-harness-test")

  result <- run_calibration_harness(
    data = df,
    prob_col = "raw_prob",
    outcome_col = "outcome",
    time_col = "event_time",
    info_time_cols = c("label_available_time", "odds_available_time"),
    n_folds = 3,
    artifact_dir = artifact_dir,
    default_method = "spline"
  )

  expect_true(all(c("fold_metrics", "summary", "folds", "artifacts") %in% names(result)))
  expect_true(all(c("isotonic", "platt", "spline") %in% result$summary$method))

  expect_true(file.exists(result$artifacts$fold_csv))
  expect_true(file.exists(result$artifacts$summary_csv))
  expect_true(file.exists(result$artifacts$rds))
})

test_that("default calibration assertion fails on material underperformance", {
  summary_df <- data.frame(
    method = c("isotonic", "platt", "spline"),
    brier = c(0.20, 0.19, 0.205),
    log_loss = c(0.55, 0.53, 0.57),
    ece = c(0.04, 0.03, 0.055),
    stringsAsFactors = FALSE
  )

  expect_error(
    assert_default_calibration_performance(
      summary_df,
      default_method = "spline",
      tolerance = c(brier = 0.001, log_loss = 0.005, ece = 0.01)
    ),
    "underperforms"
  )
})
