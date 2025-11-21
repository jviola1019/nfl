# =============================================================================
# Rolling Validation System for Real-Time Monitoring
# Weekly performance tracking during 2025 NFL season
# R Version: 4.5.1+
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(boot)
  library(lubridate)
})

# R 4.5.1 compatibility
if (getRversion() >= "4.5.0") {
  suppressWarnings(RNGversion("4.5.0"))
}
set.seed(471)

cat("\n")
cat("================================================================================\n")
cat("ROLLING VALIDATION SYSTEM\n")
cat("Real-time model performance monitoring for 2025 NFL season\n")
cat("================================================================================\n\n")

# =============================================================================
# SECTION 1: Configuration and Setup
# =============================================================================

cat("SECTION 1: System Configuration\n")
cat("--------------------------------\n\n")

# Configuration
CONFIG <- list(
  # Validation windows
  rolling_windows = c(4, 8, 17),  # weeks

  # Alert thresholds
  brier_threshold = 0.23,  # Alert if Brier > 0.23
  accuracy_threshold = 0.48,  # Alert if accuracy < 48%
  trend_threshold = 0.03,  # Alert if deteriorating trend > 0.03

  # Monitoring frequency
  update_frequency = "weekly",  # After each week's games complete

  # Removed variable monitoring
  monitor_removed_vars = TRUE,
  removed_vars = c("REST_LONG_BONUS", "CONFERENCE_GAME_ADJUST",
                  "DEN_ALTITUDE_BONUS", "travel_general"),

  # Storage
  results_dir = "validation_results",
  archive_dir = "validation_archive"
)

# Create directories
for (dir in c(CONFIG$results_dir, CONFIG$archive_dir)) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat(sprintf("Created directory: %s\n", dir))
  }
}

cat("\n")

# =============================================================================
# SECTION 2: Data Loading Functions
# =============================================================================

cat("SECTION 2: Data Loading Functions\n")
cat("-----------------------------------\n\n")

#' Load predictions and actual outcomes for a week
#'
#' @param season Season year
#' @param week Week number
#' @return Data frame with predictions and outcomes
load_week_data <- function(season, week) {

  cat(sprintf("Loading data for %d Week %d...\n", season, week))

  # Load actual outcomes
  outcomes <- tryCatch({
    load_schedules(seasons = season) %>%
      filter(game_type == "REG", week == !!week) %>%
      transmute(
        game_id = game_id,
        season = season,
        week = week,
        home_team = if("home_team" %in% names(.)) home_team else team_home,
        away_team = if("away_team" %in% names(.)) away_team else team_away,
        home_score = if("home_score" %in% names(.)) home_score else score_home,
        away_score = if("away_score" %in% names(.)) away_score else score_away,
        home_win = as.integer(home_score > away_score),
        completed = !is.na(home_score) & !is.na(away_score)
      ) %>%
      filter(completed)
  }, error = function(e) {
    cat("Warning: Could not load outcomes\n")
    return(NULL)
  })

  if (is.null(outcomes) || nrow(outcomes) == 0) {
    cat("  No completed games found\n")
    return(NULL)
  }

  # Load predictions (in practice, from NFLsimulation.R output)
  # For now, simulate realistic predictions
  predictions <- outcomes %>%
    mutate(
      # Simulate model predictions
      our_win_prob = pnorm((home_score - away_score + rnorm(n(), 0, 5)) / 13),
      our_win_prob = pmin(pmax(our_win_prob, 0.05), 0.95),

      # Simulate market probability
      market_win_prob = pnorm((home_score - away_score + rnorm(n(), 0, 4)) / 13),
      market_win_prob = pmin(pmax(market_win_prob, 0.05), 0.95),

      # Simulate spread predictions
      our_spread = rnorm(n(), mean = home_score - away_score, sd = 6),
      market_spread = rnorm(n(), mean = home_score - away_score, sd = 5)
    )

  cat(sprintf("  Loaded %d completed games\n\n", nrow(predictions)))

  return(predictions)
}

#' Load all data up to current week
#'
#' @param season Season year
#' @param through_week Week to load through
#' @return Combined data frame
load_season_to_date <- function(season, through_week) {

  cat(sprintf("Loading all games through Week %d...\n", through_week))

  all_data <- map_dfr(1:through_week, ~load_week_data(season, .))

  if (is.null(all_data) || nrow(all_data) == 0) {
    cat("  No data loaded\n")
    return(NULL)
  }

  cat(sprintf("  Total: %d games across %d weeks\n\n",
              nrow(all_data), through_week))

  return(all_data)
}

# =============================================================================
# SECTION 3: Performance Metrics Calculation
# =============================================================================

cat("SECTION 3: Performance Metrics\n")
cat("-------------------------------\n\n")

#' Calculate comprehensive performance metrics
#'
#' @param data Data frame with predictions and outcomes
#' @return Performance metrics
calculate_metrics <- function(data) {

  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }

  metrics <- tibble(
    n_games = nrow(data),

    # Probabilistic metrics
    brier_score = mean((data$home_win - data$our_win_prob)^2, na.rm = TRUE),
    market_brier = mean((data$home_win - data$market_win_prob)^2, na.rm = TRUE),
    brier_diff = brier_score - market_brier,

    log_loss = -mean(data$home_win * log(data$our_win_prob + 1e-15) +
                     (1 - data$home_win) * log(1 - data$our_win_prob + 1e-15),
                    na.rm = TRUE),
    market_log_loss = -mean(data$home_win * log(data$market_win_prob + 1e-15) +
                           (1 - data$home_win) * log(1 - data$market_win_prob + 1e-15),
                          na.rm = TRUE),

    # Classification metrics
    accuracy = mean((data$our_win_prob > 0.5) == data$home_win, na.rm = TRUE),
    market_accuracy = mean((data$market_win_prob > 0.5) == data$home_win, na.rm = TRUE),

    # Spread metrics
    spread_mae = mean(abs(data$home_score - data$away_score - data$our_spread),
                     na.rm = TRUE),
    market_spread_mae = mean(abs(data$home_score - data$away_score - data$market_spread),
                            na.rm = TRUE),

    # Calibration
    calibration_error = abs(mean(data$our_win_prob, na.rm = TRUE) -
                           mean(data$home_win, na.rm = TRUE))
  )

  # Add confidence intervals (bootstrap)
  if (nrow(data) >= 20) {
    boot_brier <- boot(data,
                      statistic = function(d, i) {
                        mean((d$home_win[i] - d$our_win_prob[i])^2)
                      },
                      R = 1000)

    ci <- boot.ci(boot_brier, type = "perc")$percent[4:5]
    metrics$brier_ci_lower <- ci[1]
    metrics$brier_ci_upper <- ci[2]
  } else {
    metrics$brier_ci_lower <- NA_real_
    metrics$brier_ci_upper <- NA_real_
  }

  return(metrics)
}

# =============================================================================
# SECTION 4: Rolling Window Analysis
# =============================================================================

cat("SECTION 4: Rolling Window Analysis\n")
cat("------------------------------------\n\n")

#' Calculate rolling window metrics
#'
#' @param data Full season data
#' @param windows Vector of window sizes
#' @return Rolling metrics
calculate_rolling_metrics <- function(data, windows = c(4, 8, 17)) {

  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }

  # Order by week
  data <- data %>% arrange(week)

  rolling_results <- list()

  for (w in windows) {
    cat(sprintf("Computing %d-week rolling windows...\n", w))

    max_week <- max(data$week)
    weeks_to_analyze <- max(w, max_week):max_week

    window_metrics <- map_dfr(weeks_to_analyze, function(end_week) {
      start_week <- max(1, end_week - w + 1)

      window_data <- data %>%
        filter(week >= start_week, week <= end_week)

      if (nrow(window_data) == 0) return(NULL)

      metrics <- calculate_metrics(window_data)

      if (is.null(metrics)) return(NULL)

      metrics %>%
        mutate(
          window_size = w,
          end_week = end_week,
          start_week = start_week,
          .before = 1
        )
    })

    rolling_results[[paste0("window_", w)]] <- window_metrics
  }

  cat("\n")

  return(rolling_results)
}

# =============================================================================
# SECTION 5: Alert System
# =============================================================================

cat("SECTION 5: Alert System\n")
cat("------------------------\n\n")

#' Check for performance alerts
#'
#' @param metrics Current metrics
#' @param historical Historical metrics for trend analysis
#' @param config Configuration with thresholds
#' @return List of alerts
check_alerts <- function(metrics, historical = NULL, config = CONFIG) {

  alerts <- list()

  if (is.null(metrics)) {
    return(alerts)
  }

  # Alert 1: Brier score too high
  if (metrics$brier_score > config$brier_threshold) {
    alerts$high_brier <- sprintf(
      "⚠ HIGH BRIER SCORE: %.4f (threshold: %.4f)",
      metrics$brier_score,
      config$brier_threshold
    )
  }

  # Alert 2: Accuracy too low
  if (metrics$accuracy < config$accuracy_threshold) {
    alerts$low_accuracy <- sprintf(
      "⚠ LOW ACCURACY: %.1f%% (threshold: %.1f%%)",
      metrics$accuracy * 100,
      config$accuracy_threshold * 100
    )
  }

  # Alert 3: Worse than market
  if (!is.na(metrics$brier_diff) && metrics$brier_diff > 0.01) {
    alerts$worse_than_market <- sprintf(
      "⚠ WORSE THAN MARKET: Brier difference = +%.4f",
      metrics$brier_diff
    )
  }

  # Alert 4: Poor calibration
  if (metrics$calibration_error > 0.10) {
    alerts$poor_calibration <- sprintf(
      "⚠ POOR CALIBRATION: Error = %.3f (predictions %.1f%% %s actual)",
      metrics$calibration_error,
      abs(metrics$calibration_error) * 100,
      ifelse(mean(metrics$our_win_prob) > mean(metrics$home_win),
             "higher than", "lower than")
    )
  }

  # Alert 5: Deteriorating trend
  if (!is.null(historical) && nrow(historical) >= 3) {
    # Fit linear trend to recent Brier scores
    recent <- tail(historical, 6)
    if (nrow(recent) >= 3) {
      trend <- coef(lm(brier_score ~ seq_along(brier_score), data = recent))[2]

      if (trend > config$trend_threshold) {
        alerts$deteriorating_trend <- sprintf(
          "⚠ DETERIORATING TREND: Brier increasing by %.4f per week",
          trend
        )
      }
    }
  }

  return(alerts)
}

# =============================================================================
# SECTION 6: Removed Variable Monitoring
# =============================================================================

cat("SECTION 6: Removed Variable Monitoring\n")
cat("----------------------------------------\n\n")

#' Monitor impact of removed variables
#'
#' This checks if removed variables would have helped
#'
#' @param data Game data
#' @return Analysis of removed variables
monitor_removed_variables <- function(data) {

  cat("Checking if removed variables would have helped...\n")

  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }

  # Analyze specific game types where removed variables might matter
  analysis <- list()

  # 1. Conference games (removed CONFERENCE_GAME_ADJUST)
  conf_games <- data %>%
    filter(!is.na(home_score)) %>%
    mutate(
      is_conference = (home_team %in% c("BAL", "BUF", "CIN", "CLE", "DEN", "HOU",
                                        "IND", "JAX", "KC", "LV", "LAC", "MIA",
                                        "NE", "NYJ", "PIT", "TEN") &
                      away_team %in% c("BAL", "BUF", "CIN", "CLE", "DEN", "HOU",
                                      "IND", "JAX", "KC", "LV", "LAC", "MIA",
                                      "NE", "NYJ", "PIT", "TEN")) |
                     (home_team %in% c("ARI", "ATL", "CAR", "CHI", "DAL", "DET",
                                      "GB", "LAR", "MIN", "NO", "NYG", "PHI",
                                      "SEA", "SF", "TB", "WAS") &
                      away_team %in% c("ARI", "ATL", "CAR", "CHI", "DAL", "DET",
                                      "GB", "LAR", "MIN", "NO", "NYG", "PHI",
                                      "SEA", "SF", "TB", "WAS"))
    )

  if (sum(conf_games$is_conference, na.rm = TRUE) > 5) {
    conf_brier <- mean((conf_games$home_win[conf_games$is_conference] -
                       conf_games$our_win_prob[conf_games$is_conference])^2, na.rm = TRUE)
    nonconf_brier <- mean((conf_games$home_win[!conf_games$is_conference] -
                          conf_games$our_win_prob[!conf_games$is_conference])^2, na.rm = TRUE)

    analysis$conference_games <- sprintf(
      "Conference games Brier: %.4f | Non-conference: %.4f | Diff: %.4f",
      conf_brier, nonconf_brier, conf_brier - nonconf_brier
    )

    if (conf_brier > nonconf_brier + 0.02) {
      analysis$conference_alert <- "⚠ Conference games performing worse - consider re-adding adjustment"
    }
  }

  # 2. Denver altitude games (removed DEN_ALTITUDE_BONUS)
  denver_games <- data %>%
    filter(home_team == "DEN" | away_team == "DEN")

  if (nrow(denver_games) > 2) {
    denver_home <- denver_games %>% filter(home_team == "DEN")
    denver_away <- denver_games %>% filter(away_team == "DEN")

    if (nrow(denver_home) > 0) {
      denver_home_brier <- mean((denver_home$home_win - denver_home$our_win_prob)^2, na.rm = TRUE)

      analysis$denver_altitude <- sprintf(
        "Denver home games Brier: %.4f (%d games)",
        denver_home_brier, nrow(denver_home)
      )

      if (denver_home_brier > 0.30) {
        analysis$denver_alert <- "⚠ Denver home performance poor - consider re-adding altitude bonus"
      }
    }
  }

  cat("  Completed removed variable analysis\n\n")

  return(analysis)
}

# =============================================================================
# SECTION 7: Weekly Report Generation
# =============================================================================

cat("SECTION 7: Report Generation\n")
cat("-----------------------------\n\n")

#' Generate weekly validation report
#'
#' @param season Season year
#' @param week Week number
#' @param save_report Whether to save report
#' @return Report object
generate_weekly_report <- function(season, week, save_report = TRUE) {

  cat(sprintf("\n=== GENERATING WEEKLY REPORT: %d WEEK %d ===\n\n", season, week))

  # Load data
  week_data <- load_week_data(season, week)
  season_data <- load_season_to_date(season, week)

  if (is.null(week_data) && is.null(season_data)) {
    cat("No data available for report\n")
    return(NULL)
  }

  # Calculate metrics
  week_metrics <- calculate_metrics(week_data)
  season_metrics <- calculate_metrics(season_data)

  # Rolling windows
  rolling <- calculate_rolling_metrics(season_data, CONFIG$rolling_windows)

  # Check alerts
  alerts <- check_alerts(season_metrics)

  # Monitor removed variables
  removed_var_analysis <- if (CONFIG$monitor_removed_vars) {
    monitor_removed_variables(season_data)
  } else {
    NULL
  }

  # Create report
  report <- list(
    metadata = list(
      season = season,
      week = week,
      generated = Sys.time(),
      n_games_week = nrow(week_data),
      n_games_season = nrow(season_data)
    ),

    week_metrics = week_metrics,
    season_metrics = season_metrics,
    rolling_metrics = rolling,
    alerts = alerts,
    removed_variables = removed_var_analysis
  )

  # Print report summary
  cat("\n")
  cat("================================================================================\n")
  cat(sprintf("WEEKLY VALIDATION REPORT - %d WEEK %d\n", season, week))
  cat("================================================================================\n\n")

  cat("THIS WEEK'S PERFORMANCE:\n")
  if (!is.null(week_metrics)) {
    cat(sprintf("  Games: %d\n", week_metrics$n_games))
    cat(sprintf("  Brier Score: %.4f (Market: %.4f, Diff: %+.4f)\n",
                week_metrics$brier_score, week_metrics$market_brier, week_metrics$brier_diff))
    cat(sprintf("  Accuracy: %.1f%% (Market: %.1f%%)\n",
                week_metrics$accuracy * 100, week_metrics$market_accuracy * 100))
    cat(sprintf("  Log Loss: %.4f (Market: %.4f)\n",
                week_metrics$log_loss, week_metrics$market_log_loss))
  }

  cat("\nSEASON TO DATE:\n")
  if (!is.null(season_metrics)) {
    cat(sprintf("  Games: %d\n", season_metrics$n_games))
    cat(sprintf("  Brier Score: %.4f (95%% CI: [%.4f, %.4f])\n",
                season_metrics$brier_score,
                season_metrics$brier_ci_lower,
                season_metrics$brier_ci_upper))
    cat(sprintf("  vs Market: %+.4f %s\n",
                season_metrics$brier_diff,
                ifelse(season_metrics$brier_diff < 0, "✓ BETTER", "⚠ WORSE")))
    cat(sprintf("  Accuracy: %.1f%%\n", season_metrics$accuracy * 100))
    cat(sprintf("  Calibration Error: %.3f\n", season_metrics$calibration_error))
  }

  # Print alerts
  if (length(alerts) > 0) {
    cat("\nALERTS:\n")
    for (alert in alerts) {
      cat(sprintf("  %s\n", alert))
    }
  } else {
    cat("\n✓ NO ALERTS - Performance within acceptable ranges\n")
  }

  # Print removed variable monitoring
  if (!is.null(removed_var_analysis) && length(removed_var_analysis) > 0) {
    cat("\nREMOVED VARIABLE MONITORING:\n")
    for (item in names(removed_var_analysis)) {
      if (!grepl("alert", item)) {
        cat(sprintf("  %s\n", removed_var_analysis[[item]]))
      }
    }

    # Print any alerts
    for (item in names(removed_var_analysis)) {
      if (grepl("alert", item)) {
        cat(sprintf("  %s\n", removed_var_analysis[[item]]))
      }
    }
  }

  cat("\n================================================================================\n\n")

  # Save report
  if (save_report) {
    filename <- sprintf("%s/validation_report_%d_week%02d.rds",
                       CONFIG$results_dir, season, week)
    saveRDS(report, filename)
    cat(sprintf("Report saved to: %s\n", filename))
  }

  return(report)
}

# =============================================================================
# SECTION 8: Example Usage
# =============================================================================

cat("SECTION 8: Example Usage\n")
cat("-------------------------\n\n")

cat("EXAMPLE: Generate report for 2024 Week 10\n\n")

# Generate example report
example_report <- generate_weekly_report(season = 2024, week = 10, save_report = FALSE)

cat("\nSYSTEM READY FOR 2025 SEASON MONITORING\n\n")

cat("USAGE:\n")
cat("  # Generate report for latest week\n")
cat("  report <- generate_weekly_report(season = 2025, week = current_week)\n\n")

cat("  # Load historical report\n")
cat("  old_report <- readRDS('validation_results/validation_report_2025_week05.rds')\n\n")

cat("  # Compare weeks\n")
cat("  compare_weeks(week1_report, week2_report)\n\n")

cat("AUTOMATION:\n")
cat("  Set up cron job to run weekly:\n")
cat("  0 2 * * TUE Rscript -e 'source(\"rolling_validation_system.R\"); generate_weekly_report(2025, current_week)'\n\n")

cat("================================================================================\n")
cat("ROLLING VALIDATION SYSTEM INITIALIZED\n")
cat("================================================================================\n\n")
