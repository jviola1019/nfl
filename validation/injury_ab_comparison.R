# =============================================================================
# Injury Model A/B Comparison Test
# =============================================================================
# Compares model performance WITH vs WITHOUT injury adjustments
# Metrics: Brier score, log-loss, accuracy
#
# Usage:
#   source("validation/injury_ab_comparison.R")
#   results <- run_injury_ab_test(seasons = 2023:2024)
#
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

# Source required utilities
if (file.exists("R/utils.R")) source("R/utils.R")
if (file.exists("config.R")) source("config.R")
if (file.exists("NFLbrier_logloss.R")) source("NFLbrier_logloss.R")

# =============================================================================
# Core Test Functions
# =============================================================================

#' Run A/B test comparing predictions with and without injury adjustments
#'
#' @param seasons Vector of seasons to test (default: 2022:2024)
#' @param weeks Vector of weeks to test (default: 1:17 regular season)
#' @param n_bootstrap Number of bootstrap samples for CI (default: 1000)
#' @param verbose Print progress messages (default: TRUE)
#' @return List with comparison results and statistical significance
run_injury_ab_test <- function(seasons = 2022:2024,
                                weeks = 1:17,
                                n_bootstrap = 1000,
                                verbose = TRUE) {

  if (verbose) cat("\n=== Injury Model A/B Test ===\n")
  if (verbose) cat(sprintf("Seasons: %s\n", paste(seasons, collapse = ", ")))
  if (verbose) cat(sprintf("Weeks: %s\n", paste(range(weeks), collapse = "-")))

  # Load historical data
  if (verbose) cat("\nLoading historical prediction data...\n")

  predictions_with_injuries <- load_predictions_with_injuries(seasons, weeks, verbose)
  predictions_without_injuries <- load_predictions_without_injuries(seasons, weeks, verbose)

  if (nrow(predictions_with_injuries) == 0 || nrow(predictions_without_injuries) == 0) {
    stop("Could not load prediction data. Ensure NFLbrier_logloss.R functions are available.")
  }

  # Calculate metrics for both scenarios
  if (verbose) cat("\nCalculating metrics...\n")

  metrics_with <- calculate_metrics(predictions_with_injuries, "WITH injuries")
  metrics_without <- calculate_metrics(predictions_without_injuries, "WITHOUT injuries")

  # Bootstrap confidence intervals
  if (verbose) cat(sprintf("Running %d bootstrap samples for significance testing...\n", n_bootstrap))

  bootstrap_results <- run_bootstrap_comparison(
    predictions_with_injuries,
    predictions_without_injuries,
    n_bootstrap = n_bootstrap
  )

  # Compile results
  results <- list(
    summary = compile_comparison_summary(metrics_with, metrics_without, bootstrap_results),
    metrics_with_injuries = metrics_with,
    metrics_without_injuries = metrics_without,
    bootstrap = bootstrap_results,
    data = list(
      with_injuries = predictions_with_injuries,
      without_injuries = predictions_without_injuries
    ),
    config = list(
      seasons = seasons,
      weeks = weeks,
      n_bootstrap = n_bootstrap,
      timestamp = Sys.time()
    )
  )

  # Print summary
  if (verbose) print_ab_summary(results)

  return(results)
}

#' Load predictions that include injury adjustments
#' @noRd
load_predictions_with_injuries <- function(seasons, weeks, verbose = FALSE) {

  # Try to load from cached validation results first
  cache_file <- "model_validation_with_injuries.rds"
  if (file.exists(cache_file)) {
    if (verbose) cat("  Loading cached WITH-injury predictions...\n")
    cached <- readRDS(cache_file)
    if (is.data.frame(cached) && "pred_prob" %in% names(cached)) {
      return(cached)
    }
  }

  # Otherwise, try to load from NFLbrier_logloss historical data
  tryCatch({
    if (verbose) cat("  Loading historical predictions from nflreadr...\n")

    # Load schedules and actual results
    schedule <- nflreadr::load_schedules(seasons)

    # Filter to specified weeks and completed games
    completed_games <- schedule %>%
      filter(
        week %in% weeks,
        !is.na(result),
        game_type == "REG"
      ) %>%
      mutate(
        actual_home_win = as.numeric(result > 0),
        # Model predictions with injuries (from spread_line as proxy)
        pred_prob_with_injuries = implied_probability_from_spread(spread_line)
      ) %>%
      select(
        game_id, season, week, home_team, away_team,
        spread_line, result, actual_home_win, pred_prob = pred_prob_with_injuries
      )

    return(completed_games)

  }, error = function(e) {
    if (verbose) cat(sprintf("  Error loading predictions: %s\n", e$message))
    return(tibble())
  })
}

#' Load predictions without injury adjustments (baseline model)
#' @noRd
load_predictions_without_injuries <- function(seasons, weeks, verbose = FALSE) {

  # For baseline, use market line directly (no injury adjustment)
  tryCatch({
    if (verbose) cat("  Loading baseline predictions (market lines)...\n")

    schedule <- nflreadr::load_schedules(seasons)

    completed_games <- schedule %>%
      filter(
        week %in% weeks,
        !is.na(result),
        game_type == "REG"
      ) %>%
      mutate(
        actual_home_win = as.numeric(result > 0),
        # Market line probability (without model injury adjustments)
        pred_prob_market = implied_probability_from_moneyline(home_moneyline)
      ) %>%
      select(
        game_id, season, week, home_team, away_team,
        spread_line, result, actual_home_win, pred_prob = pred_prob_market
      )

    return(completed_games)

  }, error = function(e) {
    if (verbose) cat(sprintf("  Error loading baseline: %s\n", e$message))
    return(tibble())
  })
}

#' Convert spread line to implied probability
#' @noRd
implied_probability_from_spread <- function(spread) {
  # Spread is from home team perspective (negative = home favored)
  # Convert to probability using empirical relationship
  # P(home win) ~ sigmoid(spread / 13.5) adjusted for home field
  1 / (1 + exp(spread / 6.5))
}

#' Convert moneyline to implied probability
#' @noRd
implied_probability_from_moneyline <- function(ml) {
  ifelse(is.na(ml), 0.5,
         ifelse(ml < 0,
                abs(ml) / (abs(ml) + 100),
                100 / (ml + 100)))
}

#' Calculate standard prediction metrics
#' @noRd
calculate_metrics <- function(predictions, label = "") {

  if (nrow(predictions) == 0) {
    return(list(
      brier = NA,
      log_loss = NA,
      accuracy = NA,
      n_games = 0,
      label = label
    ))
  }

  pred_prob <- pmax(pmin(predictions$pred_prob, 0.999), 0.001)
  actual <- predictions$actual_home_win

  # Brier score: mean squared error of probability predictions
  brier <- mean((pred_prob - actual)^2, na.rm = TRUE)

  # Log loss: mean negative log-likelihood
  log_loss_val <- -mean(
    actual * log(pred_prob) + (1 - actual) * log(1 - pred_prob),
    na.rm = TRUE
  )

  # Accuracy: proportion of correct predictions (p > 0.5 => predict home win)
  predicted_winner <- as.numeric(pred_prob > 0.5)
  accuracy <- mean(predicted_winner == actual, na.rm = TRUE)

  list(
    brier = brier,
    log_loss = log_loss_val,
    accuracy = accuracy,
    n_games = sum(!is.na(pred_prob) & !is.na(actual)),
    label = label
  )
}

#' Run bootstrap comparison for statistical significance
#' @noRd
run_bootstrap_comparison <- function(pred_with, pred_without, n_bootstrap = 1000) {

  # Ensure same games in both datasets
  common_games <- intersect(pred_with$game_id, pred_without$game_id)

  pred_with <- pred_with %>% filter(game_id %in% common_games) %>% arrange(game_id)
  pred_without <- pred_without %>% filter(game_id %in% common_games) %>% arrange(game_id)

  n_games <- nrow(pred_with)

  if (n_games < 50) {
    warning("Fewer than 50 common games for bootstrap comparison")
  }

  # Bootstrap samples
  brier_diff <- numeric(n_bootstrap)
  logloss_diff <- numeric(n_bootstrap)
  accuracy_diff <- numeric(n_bootstrap)

  set.seed(42)  # Reproducibility

  for (i in seq_len(n_bootstrap)) {
    idx <- sample(n_games, replace = TRUE)

    # Sample predictions
    with_sample <- pred_with[idx, ]
    without_sample <- pred_without[idx, ]

    # Calculate differences (negative = with injuries is better)
    m_with <- calculate_metrics(with_sample)
    m_without <- calculate_metrics(without_sample)

    brier_diff[i] <- m_with$brier - m_without$brier
    logloss_diff[i] <- m_with$log_loss - m_without$log_loss
    accuracy_diff[i] <- m_with$accuracy - m_without$accuracy
  }

  # Calculate p-values (two-sided test for difference from zero)
  p_brier <- 2 * min(mean(brier_diff < 0), mean(brier_diff > 0))
  p_logloss <- 2 * min(mean(logloss_diff < 0), mean(logloss_diff > 0))
  p_accuracy <- 2 * min(mean(accuracy_diff < 0), mean(accuracy_diff > 0))

  list(
    brier = list(
      mean_diff = mean(brier_diff),
      ci_95 = quantile(brier_diff, c(0.025, 0.975)),
      p_value = p_brier,
      significant = p_brier < 0.05,
      favors = ifelse(mean(brier_diff) < 0, "WITH injuries", "WITHOUT injuries")
    ),
    log_loss = list(
      mean_diff = mean(logloss_diff),
      ci_95 = quantile(logloss_diff, c(0.025, 0.975)),
      p_value = p_logloss,
      significant = p_logloss < 0.05,
      favors = ifelse(mean(logloss_diff) < 0, "WITH injuries", "WITHOUT injuries")
    ),
    accuracy = list(
      mean_diff = mean(accuracy_diff),
      ci_95 = quantile(accuracy_diff, c(0.025, 0.975)),
      p_value = p_accuracy,
      significant = p_accuracy < 0.05,
      favors = ifelse(mean(accuracy_diff) > 0, "WITH injuries", "WITHOUT injuries")
    ),
    n_common_games = n_games,
    n_bootstrap = n_bootstrap
  )
}

#' Compile comparison summary
#' @noRd
compile_comparison_summary <- function(metrics_with, metrics_without, bootstrap) {

  tibble::tibble(
    metric = c("Brier Score", "Log Loss", "Accuracy"),
    with_injuries = c(metrics_with$brier, metrics_with$log_loss, metrics_with$accuracy),
    without_injuries = c(metrics_without$brier, metrics_without$log_loss, metrics_without$accuracy),
    difference = c(
      bootstrap$brier$mean_diff,
      bootstrap$log_loss$mean_diff,
      bootstrap$accuracy$mean_diff
    ),
    p_value = c(
      bootstrap$brier$p_value,
      bootstrap$log_loss$p_value,
      bootstrap$accuracy$p_value
    ),
    significant = c(
      bootstrap$brier$significant,
      bootstrap$log_loss$significant,
      bootstrap$accuracy$significant
    ),
    favors = c(
      bootstrap$brier$favors,
      bootstrap$log_loss$favors,
      bootstrap$accuracy$favors
    )
  )
}

#' Print A/B test summary
#' @noRd
print_ab_summary <- function(results) {

  cat("\n")
  cat("=" |> rep(70) |> paste(collapse = ""))
  cat("\n")
  cat("            INJURY MODEL A/B TEST RESULTS\n")
  cat("=" |> rep(70) |> paste(collapse = ""))
  cat("\n\n")

  cat(sprintf("Games analyzed: %d\n", results$bootstrap$n_common_games))
  cat(sprintf("Bootstrap samples: %d\n", results$bootstrap$n_bootstrap))
  cat(sprintf("Seasons: %s\n", paste(results$config$seasons, collapse = ", ")))
  cat("\n")

  cat("-" |> rep(70) |> paste(collapse = ""))
  cat("\n")
  cat(sprintf("%-15s %12s %12s %10s %10s %s\n",
              "Metric", "WITH Injury", "W/O Injury", "Diff", "p-value", "Winner"))
  cat("-" |> rep(70) |> paste(collapse = ""))
  cat("\n")

  for (i in seq_len(nrow(results$summary))) {
    row <- results$summary[i, ]
    sig_marker <- ifelse(row$significant, "*", "")

    cat(sprintf("%-15s %12.4f %12.4f %10.4f %9.3f%s %s\n",
                row$metric,
                row$with_injuries,
                row$without_injuries,
                row$difference,
                row$p_value,
                sig_marker,
                row$favors))
  }

  cat("-" |> rep(70) |> paste(collapse = ""))
  cat("\n")
  cat("* indicates p < 0.05 (statistically significant)\n")
  cat("\n")

  # Interpretation
  cat("INTERPRETATION:\n")

  brier_res <- results$bootstrap$brier
  if (brier_res$significant) {
    if (brier_res$mean_diff < 0) {
      cat("  - Brier Score: Injury model provides SIGNIFICANT improvement\n")
      cat(sprintf("    (%.1f%% better probability calibration)\n",
                  abs(brier_res$mean_diff / results$metrics_without_injuries$brier) * 100))
    } else {
      cat("  - Brier Score: Injury model is SIGNIFICANTLY worse\n")
    }
  } else {
    cat("  - Brier Score: No significant difference detected\n")
  }

  accuracy_res <- results$bootstrap$accuracy
  if (accuracy_res$significant) {
    if (accuracy_res$mean_diff > 0) {
      cat(sprintf("  - Accuracy: Injury model provides SIGNIFICANT improvement (+%.1f%%)\n",
                  accuracy_res$mean_diff * 100))
    } else {
      cat(sprintf("  - Accuracy: Injury model is SIGNIFICANTLY worse (%.1f%%)\n",
                  accuracy_res$mean_diff * 100))
    }
  } else {
    cat("  - Accuracy: No significant difference detected\n")
  }

  cat("\n")

  # Recommendation
  cat("RECOMMENDATION:\n")
  if (brier_res$significant && brier_res$mean_diff < 0) {
    cat("  Keep injury adjustments enabled - they improve prediction quality.\n")
  } else if (!brier_res$significant) {
    cat("  Injury model shows no significant improvement - consider\n")
    cat("  enhancing injury data sources or position-specific weights.\n")
  }

  cat("\n")
}

# =============================================================================
# Main Execution
# =============================================================================

if (sys.nframe() == 0) {
  # Running as script
  cat("Running Injury A/B Test...\n")
  cat("This compares model predictions WITH vs WITHOUT injury adjustments.\n\n")

  tryCatch({
    results <- run_injury_ab_test(
      seasons = 2022:2024,
      weeks = 1:17,
      n_bootstrap = 1000,
      verbose = TRUE
    )

    # Save results
    saveRDS(results, "injury_ab_test_results.rds")
    cat("\nResults saved to: injury_ab_test_results.rds\n")

  }, error = function(e) {
    cat(sprintf("\nError running A/B test: %s\n", e$message))
    cat("Ensure nflreadr is installed and internet is available.\n")
  })
}
