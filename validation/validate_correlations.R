# =============================================================================
# FILE: validation/validate_correlations.R
# PURPOSE: Validate correlation coefficients against historical nflreadr data
#
# VERSION: 2.9.0
# LAST UPDATED: 2026-02-04
#
# USAGE:
#   source("validation/validate_correlations.R")
#   validate_props_correlations()  # Run validation
#
# DESCRIPTION:
#   This script empirically validates the correlation coefficients used in the
#   player props model against historical NFL data from nflreadr (2019-2024).
#   Provides statistical evidence that the hyperparameters are data-driven.
#
# EXPECTED OUTPUT:
#   - Correlation comparison table
#   - 95% confidence intervals
#   - Pass/fail validation status
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

#' Validate props correlation coefficients against historical data
#'
#' @param seasons Vector of seasons to analyze (default 2019:2024)
#' @param verbose Print detailed output (default TRUE)
#' @return List with validation results
#' @export
validate_props_correlations <- function(seasons = 2019:2024, verbose = TRUE) {

  # Load config if not already loaded
  if (!exists("PROP_GAME_CORR_PASSING")) {
    config_path <- file.path(getwd(), "config.R")
    if (file.exists(config_path)) {
      source(config_path, local = FALSE)
    }
  }

  if (verbose) {
    cat("\n")
    cat("=================================================================\n")
    cat("  CORRELATION COEFFICIENT VALIDATION\n")
    cat("=================================================================\n")
    cat(sprintf("  Seasons: %d-%d\n", min(seasons), max(seasons)))
    cat("  Source: nflreadr player stats + schedules\n")
    cat("=================================================================\n\n")
  }

  # Check if nflreadr is available
  if (!requireNamespace("nflreadr", quietly = TRUE)) {
    stop("nflreadr package required. Install with: install.packages('nflreadr')")
  }

  results <- list()

  # -------------------------------------------------------------------------
  # Load data
  # -------------------------------------------------------------------------

  if (verbose) cat("Loading data...\n")

  tryCatch({
    # Load player stats
    player_stats <- nflreadr::load_player_stats(seasons, stat_type = "offense")

    # Load schedules for game totals
    schedules <- nflreadr::load_schedules(seasons)

    if (verbose) {
      cat(sprintf("  Loaded %s player stat records\n", format(nrow(player_stats), big.mark = ",")))
      cat(sprintf("  Loaded %s game records\n", format(nrow(schedules), big.mark = ",")))
    }

  }, error = function(e) {
    stop(sprintf("Failed to load data: %s", e$message))
  })

  # Calculate game totals
  games_with_totals <- schedules %>%
    filter(!is.na(home_score), !is.na(away_score)) %>%
    mutate(game_total = home_score + away_score) %>%
    select(game_id, game_total, home_team, away_team)

  # -------------------------------------------------------------------------
  # QB Passing ↔ Game Total Correlation
  # -------------------------------------------------------------------------

  if (verbose) cat("\nCalculating QB Passing ↔ Game Total correlation...\n")

  qb_stats <- player_stats %>%
    filter(position == "QB", attempts >= 10) %>%
    left_join(games_with_totals, by = "game_id") %>%
    filter(!is.na(passing_yards), !is.na(game_total))

  qb_corr <- cor(qb_stats$passing_yards, qb_stats$game_total, use = "complete.obs")

  # Bootstrap 95% CI
  set.seed(42)
  n_boot <- 1000
  qb_boot <- replicate(n_boot, {
    idx <- sample(nrow(qb_stats), replace = TRUE)
    cor(qb_stats$passing_yards[idx], qb_stats$game_total[idx], use = "complete.obs")
  })
  qb_ci <- quantile(qb_boot, c(0.025, 0.975))

  results$qb_passing <- list(
    empirical = qb_corr,
    config = if(exists("PROP_GAME_CORR_PASSING")) PROP_GAME_CORR_PASSING else NA,
    ci_lower = qb_ci[1],
    ci_upper = qb_ci[2],
    n_obs = nrow(qb_stats),
    valid = if(exists("PROP_GAME_CORR_PASSING"))
      PROP_GAME_CORR_PASSING >= qb_ci[1] && PROP_GAME_CORR_PASSING <= qb_ci[2] else NA
  )

  # -------------------------------------------------------------------------
  # RB Rushing ↔ Game Total Correlation
  # -------------------------------------------------------------------------

  if (verbose) cat("Calculating RB Rushing ↔ Game Total correlation...\n")

  rb_stats <- player_stats %>%
    filter(position == "RB", carries >= 5) %>%
    left_join(games_with_totals, by = "game_id") %>%
    filter(!is.na(rushing_yards), !is.na(game_total))

  rb_corr <- cor(rb_stats$rushing_yards, rb_stats$game_total, use = "complete.obs")

  # Bootstrap 95% CI
  rb_boot <- replicate(n_boot, {
    idx <- sample(nrow(rb_stats), replace = TRUE)
    cor(rb_stats$rushing_yards[idx], rb_stats$game_total[idx], use = "complete.obs")
  })
  rb_ci <- quantile(rb_boot, c(0.025, 0.975))

  results$rb_rushing <- list(
    empirical = rb_corr,
    config = if(exists("PROP_GAME_CORR_RUSHING")) PROP_GAME_CORR_RUSHING else NA,
    ci_lower = rb_ci[1],
    ci_upper = rb_ci[2],
    n_obs = nrow(rb_stats),
    valid = if(exists("PROP_GAME_CORR_RUSHING"))
      PROP_GAME_CORR_RUSHING >= rb_ci[1] && PROP_GAME_CORR_RUSHING <= rb_ci[2] else NA
  )

  # -------------------------------------------------------------------------
  # WR Receiving ↔ Team Passing Correlation
  # -------------------------------------------------------------------------

  if (verbose) cat("Calculating WR Receiving ↔ Team Passing correlation...\n")

  wr_stats <- player_stats %>%
    filter(position == "WR", targets >= 3) %>%
    left_join(games_with_totals, by = "game_id") %>%
    filter(!is.na(receiving_yards), !is.na(game_total))

  wr_corr <- cor(wr_stats$receiving_yards, wr_stats$game_total, use = "complete.obs")

  # Bootstrap 95% CI
  wr_boot <- replicate(n_boot, {
    idx <- sample(nrow(wr_stats), replace = TRUE)
    cor(wr_stats$receiving_yards[idx], wr_stats$game_total[idx], use = "complete.obs")
  })
  wr_ci <- quantile(wr_boot, c(0.025, 0.975))

  results$wr_receiving <- list(
    empirical = wr_corr,
    config = if(exists("PROP_GAME_CORR_RECEIVING")) PROP_GAME_CORR_RECEIVING else NA,
    ci_lower = wr_ci[1],
    ci_upper = wr_ci[2],
    n_obs = nrow(wr_stats),
    valid = if(exists("PROP_GAME_CORR_RECEIVING"))
      PROP_GAME_CORR_RECEIVING >= wr_ci[1] && PROP_GAME_CORR_RECEIVING <= wr_ci[2] else NA
  )

  # -------------------------------------------------------------------------
  # Summary Output
  # -------------------------------------------------------------------------

  if (verbose) {
    cat("\n")
    cat("=================================================================\n")
    cat("  VALIDATION RESULTS\n")
    cat("=================================================================\n\n")

    cat("| Parameter                    | Config | Empirical | 95% CI           | N     | Valid |\n")
    cat("|------------------------------|--------|-----------|------------------|-------|-------|\n")

    cat(sprintf("| QB Passing ↔ Game Total      | %.2f   | %.3f     | [%.3f, %.3f]   | %5d | %s   |\n",
                results$qb_passing$config, results$qb_passing$empirical,
                results$qb_passing$ci_lower, results$qb_passing$ci_upper,
                results$qb_passing$n_obs,
                if(isTRUE(results$qb_passing$valid)) "YES" else "NO"))

    cat(sprintf("| RB Rushing ↔ Game Total      | %.2f   | %.3f     | [%.3f, %.3f]   | %5d | %s   |\n",
                results$rb_rushing$config, results$rb_rushing$empirical,
                results$rb_rushing$ci_lower, results$rb_rushing$ci_upper,
                results$rb_rushing$n_obs,
                if(isTRUE(results$rb_rushing$valid)) "YES" else "NO"))

    cat(sprintf("| WR Receiving ↔ Game Total    | %.2f   | %.3f     | [%.3f, %.3f]   | %5d | %s   |\n",
                results$wr_receiving$config, results$wr_receiving$empirical,
                results$wr_receiving$ci_lower, results$wr_receiving$ci_upper,
                results$wr_receiving$n_obs,
                if(isTRUE(results$wr_receiving$valid)) "YES" else "NO"))

    cat("\n")

    # Overall validation status
    all_valid <- all(c(
      isTRUE(results$qb_passing$valid),
      isTRUE(results$rb_rushing$valid),
      isTRUE(results$wr_receiving$valid)
    ))

    if (all_valid) {
      cat("[VALIDATION PASSED] All correlation coefficients within empirical 95% CI\n")
    } else {
      cat("[VALIDATION WARNING] Some coefficients outside empirical bounds\n")
    }

    cat("\n")
  }

  invisible(results)
}

# Run validation if executed directly
if (sys.nframe() == 0) {
  validate_props_correlations()
}
