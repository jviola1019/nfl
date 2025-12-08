#!/usr/bin/env Rscript
#
# Rolling Window Validation for NFL Prediction Model
#
# Purpose: Test model stability across different time periods
# Method: Multiple train/valid splits with rolling windows
#
# Author: Automated Model Optimization
# Date: 2025-12-08
# R Version: 4.5.1+
#

# =============================================================================
# SETUP
# =============================================================================

# Ensure R 4.5.1+ compatibility
if (getRversion() >= "4.5.0") {
  suppressWarnings(RNGversion("4.5.0"))
}

set.seed(471)

# Load required packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(glmmTMB)
})

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  Rolling Window Validation - NFL Prediction Model             ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# =============================================================================
# CONFIGURATION
# =============================================================================

# Define rolling windows (train → validate)
windows <- list(
  list(train_start = 2011, train_end = 2015, valid_year = 2016),
  list(train_start = 2012, train_end = 2016, valid_year = 2017),
  list(train_start = 2013, train_end = 2017, valid_year = 2018),
  list(train_start = 2014, train_end = 2018, valid_year = 2019),
  list(train_start = 2015, train_end = 2019, valid_year = 2020),
  list(train_start = 2016, train_end = 2020, valid_year = 2021),
  list(train_start = 2017, train_end = 2021, valid_year = 2022),
  list(train_start = 2018, train_end = 2022, valid_year = 2023)
)

n_windows <- length(windows)

cat(sprintf("Testing %d rolling windows:\n\n", n_windows))
for (i in seq_along(windows)) {
  w <- windows[[i]]
  cat(sprintf("  Window %d: Train %d-%d → Validate %d\n",
              i, w$train_start, w$train_end, w$valid_year))
}
cat("\n")

# =============================================================================
# LOAD DATA
# =============================================================================

cat("Loading NFL schedule data (2011-2024)...\n")
sched <- nflreadr::load_schedules(seasons = 2011:2024) %>%
  filter(game_type == "REG", !is.na(result))

cat(sprintf("✓ Loaded %d regular season games\n\n", nrow(sched)))

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Calculate Brier score
brier_score <- function(pred_prob, actual_outcome) {
  mean((pred_prob - actual_outcome)^2, na.rm = TRUE)
}

# Calculate log-loss
log_loss <- function(pred_prob, actual_outcome) {
  pred_prob <- pmin(pmax(pred_prob, 1e-10), 1 - 1e-10)  # Clip
  -mean(actual_outcome * log(pred_prob) + (1 - actual_outcome) * log(1 - pred_prob), na.rm = TRUE)
}

# Calculate accuracy
accuracy <- function(pred_prob, actual_outcome) {
  mean((pred_prob > 0.5) == actual_outcome, na.rm = TRUE)
}

# Simple model: GLMM only (no features)
fit_simple_glmm <- function(train_data) {
  # Prepare data for GLMM
  stacked <- bind_rows(
    train_data %>%
      transmute(
        points = home_score,
        is_home = TRUE,
        team = home_team,
        opp = away_team,
        season, week
      ),
    train_data %>%
      transmute(
        points = away_score,
        is_home = FALSE,
        team = away_team,
        opp = home_team,
        season, week
      )
  ) %>%
    filter(!is.na(points))

  # Fit negative binomial GLMM
  fit <- try(
    glmmTMB(
      points ~ is_home + (1|team) + (1|opp),
      family = nbinom2,
      data = stacked
    ),
    silent = TRUE
  )

  if (inherits(fit, "try-error")) {
    return(NULL)
  }

  fit
}

# Predict using GLMM
predict_glmm <- function(model, test_data) {
  if (is.null(model)) {
    return(rep(0.5, nrow(test_data)))
  }

  # Predict home and away scores
  home_pred <- predict(model, newdata = data.frame(
    is_home = TRUE,
    team = test_data$home_team,
    opp = test_data$away_team
  ), type = "response", allow.new.levels = TRUE)

  away_pred <- predict(model, newdata = data.frame(
    is_home = FALSE,
    team = test_data$away_team,
    opp = test_data$home_team
  ), type = "response", allow.new.levels = TRUE)

  # Simple win probability (higher score wins)
  # Use logistic function of point differential
  point_diff <- home_pred - away_pred
  plogis(point_diff / 10)  # Scale by 10 for reasonable probabilities
}

# =============================================================================
# ROLLING WINDOW VALIDATION
# =============================================================================

cat("Running rolling window validation...\n\n")

results_list <- list()

for (i in seq_along(windows)) {
  w <- windows[[i]]

  cat(sprintf("Window %d/%d: Train %d-%d → Validate %d\n",
              i, n_windows, w$train_start, w$train_end, w$valid_year))

  # Split data
  train_sched <- sched %>%
    filter(season >= w$train_start, season <= w$train_end)

  valid_sched <- sched %>%
    filter(season == w$valid_year)

  cat(sprintf("  Train: %d games, Valid: %d games\n",
              nrow(train_sched), nrow(valid_sched)))

  # Fit model
  cat("  Fitting GLMM...\n")
  model <- fit_simple_glmm(train_sched)

  if (is.null(model)) {
    cat("  ⚠ Model failed to converge\n\n")
    next
  }

  # Predict on validation set
  cat("  Predicting validation set...\n")
  valid_pred <- predict_glmm(model, valid_sched)

  # Calculate metrics
  valid_outcome <- as.integer(valid_sched$result > 0)

  brier <- brier_score(valid_pred, valid_outcome)
  logloss <- log_loss(valid_pred, valid_outcome)
  acc <- accuracy(valid_pred, valid_outcome)

  cat(sprintf("  Brier: %.4f | Log-Loss: %.4f | Accuracy: %.2f%%\n\n",
              brier, logloss, 100 * acc))

  # Store results
  results_list[[i]] <- data.frame(
    window = i,
    train_start = w$train_start,
    train_end = w$train_end,
    valid_year = w$valid_year,
    n_train = nrow(train_sched),
    n_valid = nrow(valid_sched),
    brier = brier,
    logloss = logloss,
    accuracy = acc,
    stringsAsFactors = FALSE
  )
}

# Combine results
results_df <- bind_rows(results_list)

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  ROLLING WINDOW VALIDATION RESULTS                             ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

cat("PERFORMANCE BY WINDOW:\n")
cat("─────────────────────────────────────────────────────────────────────────\n")
cat(sprintf("%-8s  %-12s  %-10s  %-10s  %-10s  %-10s\n",
            "Window", "Train→Valid", "N_Valid", "Brier", "Log-Loss", "Accuracy"))
cat("─────────────────────────────────────────────────────────────────────────\n")

for (i in 1:nrow(results_df)) {
  row <- results_df[i,]
  cat(sprintf("%-8d  %d-%d→%d  %-10d  %-10.4f  %-10.4f  %6.2f%%\n",
              row$window,
              row$train_start, row$train_end, row$valid_year,
              row$n_valid,
              row$brier,
              row$logloss,
              100 * row$accuracy))
}

cat("─────────────────────────────────────────────────────────────────────────\n")
cat("\n")

# Calculate summary statistics
cat("SUMMARY STATISTICS:\n")
cat("─────────────────────────────────────────────────────────────────────────\n")
cat(sprintf("%-20s  %-10s  %-10s  %-10s\n", "Metric", "Mean", "Std Dev", "Range"))
cat("─────────────────────────────────────────────────────────────────────────\n")

brier_mean <- mean(results_df$brier)
brier_sd <- sd(results_df$brier)
brier_range <- sprintf("[%.4f, %.4f]", min(results_df$brier), max(results_df$brier))

logloss_mean <- mean(results_df$logloss)
logloss_sd <- sd(results_df$logloss)
logloss_range <- sprintf("[%.4f, %.4f]", min(results_df$logloss), max(results_df$logloss))

acc_mean <- mean(results_df$accuracy)
acc_sd <- sd(results_df$accuracy)
acc_range <- sprintf("[%.2f%%, %.2f%%]", 100*min(results_df$accuracy), 100*max(results_df$accuracy))

cat(sprintf("%-20s  %-10.4f  %-10.4f  %-10s\n", "Brier Score", brier_mean, brier_sd, brier_range))
cat(sprintf("%-20s  %-10.4f  %-10.4f  %-10s\n", "Log-Loss", logloss_mean, logloss_sd, logloss_range))
cat(sprintf("%-20s  %-10.2f%%  %-10.2f%%  %-10s\n", "Accuracy", 100*acc_mean, 100*acc_sd, acc_range))
cat("─────────────────────────────────────────────────────────────────────────\n")
cat("\n")

# =============================================================================
# STABILITY ANALYSIS
# =============================================================================

cat("STABILITY ANALYSIS:\n")
cat("─────────────────────────────────────────────────────────────────────────\n")

# Coefficient of variation (lower is more stable)
brier_cv <- brier_sd / brier_mean
logloss_cv <- logloss_sd / logloss_mean
acc_cv <- acc_sd / acc_mean

cat(sprintf("Brier Score CV:   %.2f%%  ", 100 * brier_cv))
if (brier_cv < 0.05) {
  cat("✓ Very stable\n")
} else if (brier_cv < 0.10) {
  cat("✓ Stable\n")
} else {
  cat("⚠ Unstable - varies significantly across periods\n")
}

cat(sprintf("Log-Loss CV:      %.2f%%  ", 100 * logloss_cv))
if (logloss_cv < 0.05) {
  cat("✓ Very stable\n")
} else if (logloss_cv < 0.10) {
  cat("✓ Stable\n")
} else {
  cat("⚠ Unstable - varies significantly across periods\n")
}

cat(sprintf("Accuracy CV:      %.2f%%  ", 100 * acc_cv))
if (acc_cv < 0.05) {
  cat("✓ Very stable\n")
} else if (acc_cv < 0.10) {
  cat("✓ Stable\n")
} else {
  cat("⚠ Unstable - varies significantly across periods\n")
}

cat("\n")

# Trend analysis
if (nrow(results_df) >= 3) {
  # Linear trend in Brier score
  brier_trend <- coef(lm(brier ~ window, data = results_df))[2]

  cat("TREND ANALYSIS:\n")
  cat("─────────────────────────────────────────────────────────────────────────\n")
  cat(sprintf("Brier Score trend: %+.5f per window\n", brier_trend))

  if (abs(brier_trend) < 0.001) {
    cat("  → Stable over time ✓\n")
  } else if (brier_trend > 0) {
    cat("  → Degrading over time ⚠ (model may be aging)\n")
  } else {
    cat("  → Improving over time ✓ (model adapting well)\n")
  }
  cat("\n")
}

# =============================================================================
# RECOMMENDATIONS
# =============================================================================

cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  RECOMMENDATIONS                                               ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

if (brier_cv < 0.05 && logloss_cv < 0.05) {
  cat("✓ Model performance is VERY STABLE across time periods\n")
  cat("✓ Parameters are robust and generalize well\n")
  cat("✓ Safe to use for future predictions\n")
} else if (brier_cv < 0.10 && logloss_cv < 0.10) {
  cat("✓ Model performance is STABLE across time periods\n")
  cat("✓ Acceptable variation for NFL prediction\n")
  cat("✓ Safe to use for future predictions\n")
} else {
  cat("⚠ Model performance varies significantly across time periods\n")
  cat("⚠ Consider:\n")
  cat("  1. Adding recency weighting to training data\n")
  cat("  2. Reducing model complexity (fewer features)\n")
  cat("  3. Using ensemble of models from different windows\n")
}

cat("\n")

# Compare to static split
cat("COMPARISON TO STATIC SPLIT:\n")
cat("─────────────────────────────────────────────────────────────────────────\n")
cat(sprintf("Rolling window avg Brier:  %.4f ± %.4f\n", brier_mean, brier_sd))
cat(sprintf("Static split 2019-2022:     0.2118 (from RESULTS.md)\n"))
cat(sprintf("Difference:                 %+.4f\n", brier_mean - 0.2118))
cat("\n")

if (brier_mean < 0.2118) {
  cat("✓ Rolling validation shows BETTER average performance\n")
  cat("  → Model generalizes well across different time periods\n")
} else {
  cat("⚠ Rolling validation shows WORSE average performance\n")
  cat("  → Static split may have been optimistic\n")
  cat("  → True out-of-sample performance likely closer to %.4f\n", brier_mean)
}

# =============================================================================
# SAVE RESULTS
# =============================================================================

output_file <- "rolling_window_validation_results.rds"
results_obj <- list(
  windows = windows,
  results = results_df,
  summary_stats = data.frame(
    metric = c("Brier", "Log-Loss", "Accuracy"),
    mean = c(brier_mean, logloss_mean, acc_mean),
    sd = c(brier_sd, logloss_sd, acc_sd),
    cv = c(brier_cv, logloss_cv, acc_cv),
    min = c(min(results_df$brier), min(results_df$logloss), min(results_df$accuracy)),
    max = c(max(results_df$brier), max(results_df$logloss), max(results_df$accuracy))
  )
)

saveRDS(results_obj, output_file)
cat(sprintf("\n✓ Results saved to: %s\n", output_file))

cat("\n")
cat("════════════════════════════════════════════════════════════════\n")
cat("Rolling window validation complete!\n")
cat("════════════════════════════════════════════════════════════════\n")
cat("\n")
