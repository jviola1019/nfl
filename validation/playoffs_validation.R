#!/usr/bin/env Rscript
# =============================================================================
# NFL Prediction Model - Playoffs Validation Script
# =============================================================================
# Validates playoff round detection, game counts, and feature adjustments.
# Run with: Rscript validation/playoffs_validation.R [season]
# =============================================================================

cat("\n")
cat("=================================================================\n")
cat("  NFL PLAYOFFS VALIDATION\n")
cat("=================================================================\n\n")

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
validation_season <- if (length(args) >= 1) as.integer(args[1]) else 2023

cat(sprintf("Validating playoffs for: %d season\n\n", validation_season))

# Load required packages
required_pkgs <- c("dplyr", "tibble")
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  stop(sprintf("Missing required packages: %s", paste(missing_pkgs, collapse = ", ")))
}

library(dplyr, warn.conflicts = FALSE)

# Source playoff module
source("R/playoffs.R")

# Track results
results <- list(
  passed = 0,
  failed = 0,
  warnings = character(0)
)

pass <- function(test_name) {
  cat(sprintf("  [PASS] %s\n", test_name))
  results$passed <<- results$passed + 1
}

fail <- function(test_name, reason = "") {
  msg <- if (nzchar(reason)) sprintf("%s: %s", test_name, reason) else test_name
  cat(sprintf("  [FAIL] %s\n", msg))
  results$failed <<- results$failed + 1
}

warn <- function(message) {
  cat(sprintf("  [WARN] %s\n", message))
  results$warnings <<- c(results$warnings, message)
}

# =============================================================================
# LOAD SCHEDULE DATA
# =============================================================================

cat("--- Loading Schedule Data ---\n")

schedule <- tryCatch({
  if (!requireNamespace("nflreadr", quietly = TRUE)) {
    stop("nflreadr package required")
  }
  nflreadr::load_schedules(seasons = validation_season)
}, error = function(e) {
  fail("Load schedule", e$message)
  NULL
})

if (is.null(schedule) || nrow(schedule) == 0) {
  cat("\nCannot proceed without schedule data.\n")
  quit(status = 1)
}

pass(sprintf("Loaded %d games for %d season", nrow(schedule), validation_season))

# Filter to playoffs
playoffs <- schedule %>%
  filter(.data$game_type %in% c("WC", "DIV", "CON", "SB"))

if (nrow(playoffs) == 0) {
  warn(sprintf("No playoff games found for %d season (may not have been played yet)", validation_season))
}

# =============================================================================
# ROUND GAME COUNT VALIDATION
# =============================================================================

cat("\n--- Round Game Count Validation ---\n")

expected_counts <- list(
  wild_card = 6,
  divisional = 4,
  conference = 2,
  super_bowl = 1
)

for (round_name in names(PLAYOFF_ROUNDS)) {
  round_config <- PLAYOFF_ROUNDS[[round_name]]
  week <- round_config$week

  round_games <- playoffs %>%
    filter(.data$week == week)

  actual_count <- nrow(round_games)
  expected_count <- expected_counts[[round_name]]

  if (actual_count == 0 && nrow(playoffs) == 0) {
    warn(sprintf("%s: No games (season not yet played)", round_config$name))
  } else if (actual_count == expected_count) {
    pass(sprintf("%s: %d games as expected", round_config$name, actual_count))
  } else {
    fail(sprintf("%s: expected %d, found %d", round_config$name, expected_count, actual_count))
  }
}

# =============================================================================
# ROUND DETECTION VALIDATION
# =============================================================================

cat("\n--- Round Detection Validation ---\n")

if (nrow(playoffs) > 0) {
  # Test each round
  for (round_name in names(PLAYOFF_ROUNDS)) {
    round_config <- PLAYOFF_ROUNDS[[round_name]]
    week <- round_config$week

    round_games <- playoffs %>%
      filter(.data$week == week)

    if (nrow(round_games) > 0) {
      # Test derive_playoff_round
      first_game <- round_games[1, ]
      detected_round <- derive_playoff_round(first_game)

      if (detected_round == round_name) {
        pass(sprintf("derive_playoff_round correctly identifies %s", round_config$name))
      } else {
        fail(sprintf("derive_playoff_round for %s", round_config$name),
             sprintf("got '%s'", detected_round))
      }

      # Test derive_playoff_round_from_week
      detected_from_week <- derive_playoff_round_from_week(week)
      if (detected_from_week == round_name) {
        pass(sprintf("derive_playoff_round_from_week(%d) = %s", week, round_name))
      } else {
        fail(sprintf("derive_playoff_round_from_week(%d)", week),
             sprintf("got '%s'", detected_from_week))
      }
    }
  }
} else {
  warn("Skipping round detection tests (no playoff games)")
}

# =============================================================================
# HOME FIELD ADVANTAGE VALIDATION
# =============================================================================

cat("\n--- Home Field Advantage Validation ---\n")

if (nrow(playoffs) > 0) {
  # Check that non-Super Bowl games have meaningful home field
  non_sb <- playoffs %>%
    filter(.data$week < 22)

  if (nrow(non_sb) > 0) {
    home_wins <- non_sb %>%
      filter(.data$home_score > .data$away_score) %>%
      nrow()

    home_win_pct <- home_wins / nrow(non_sb)

    cat(sprintf("  Historical home win rate (non-SB): %.1f%% (%d/%d games)\n",
                home_win_pct * 100, home_wins, nrow(non_sb)))

    # Home field should provide meaningful advantage in playoffs
    if (home_win_pct >= 0.45) {
      pass("Home field provides meaningful advantage in playoffs")
    } else {
      warn(sprintf("Home win rate unusually low: %.1f%%", home_win_pct * 100))
    }
  }

  # Super Bowl should be neutral site
  sb_config <- PLAYOFF_ROUNDS$super_bowl
  if (!sb_config$home_field) {
    pass("Super Bowl configured as neutral site")
  } else {
    fail("Super Bowl should be neutral site")
  }
}

# =============================================================================
# PLAYOFF FEATURES VALIDATION
# =============================================================================

cat("\n--- Playoff Features Validation ---\n")

# Shrinkage values should increase through playoffs (more market trust)
shrinkage_order <- c(
  PLAYOFF_FEATURES$shrinkage$wild_card,
  PLAYOFF_FEATURES$shrinkage$divisional,
  PLAYOFF_FEATURES$shrinkage$conference,
  PLAYOFF_FEATURES$shrinkage$super_bowl
)

if (all(diff(shrinkage_order) >= 0)) {
  pass("Shrinkage increases through playoff rounds")
} else {
  fail("Shrinkage should increase through playoffs",
       sprintf("values: %s", paste(shrinkage_order, collapse = " -> ")))
}

# HFA multipliers should make sense
if (PLAYOFF_FEATURES$hfa_multiplier$super_bowl == 1.0) {
  pass("Super Bowl HFA multiplier is 1.0 (neutral)")
} else {
  fail("Super Bowl should have neutral HFA (1.0)")
}

if (all(unlist(PLAYOFF_FEATURES$hfa_multiplier[c("wild_card", "divisional", "conference")]) > 1.0)) {
  pass("Non-SB playoff rounds have HFA boost > 1.0")
} else {
  fail("Non-SB rounds should have HFA multiplier > 1.0")
}

# =============================================================================
# PLAYOFF ADJUSTMENT CALCULATION VALIDATION
# =============================================================================

cat("\n--- Playoff Adjustment Calculation Validation ---\n")

# Test adjustment calculations
adj_wc <- calculate_playoff_adjustments("wild_card", "KC", "MIA")
adj_div <- calculate_playoff_adjustments("divisional", "KC", "HOU", home_had_bye = TRUE)
adj_sb <- calculate_playoff_adjustments("super_bowl", "KC", "SF")

if (adj_wc$shrinkage == PLAYOFF_FEATURES$shrinkage$wild_card) {
  pass("Wild Card shrinkage adjustment correct")
} else {
  fail("Wild Card shrinkage adjustment")
}

if (adj_div$home_rest_bonus > 0) {
  pass("Divisional bye team gets rest bonus")
} else {
  fail("Divisional bye team should get rest bonus")
}

if (adj_sb$hfa_multiplier == 1.0) {
  pass("Super Bowl has neutral HFA (1.0)")
} else {
  fail("Super Bowl should have neutral HFA")
}

# =============================================================================
# HISTORICAL PERFORMANCE SUMMARY
# =============================================================================

cat("\n--- Historical Performance Summary ---\n")

if (nrow(playoffs) > 0 && "home_score" %in% names(playoffs) && "away_score" %in% names(playoffs)) {
  # Only analyze completed games
  completed <- playoffs %>%
    filter(!is.na(.data$home_score) & !is.na(.data$away_score))

  if (nrow(completed) > 0) {
    summary <- completed %>%
      mutate(
        round = derive_playoff_round_from_week(.data$week),
        home_won = .data$home_score > .data$away_score,
        total_points = .data$home_score + .data$away_score,
        margin = abs(.data$home_score - .data$away_score)
      ) %>%
      group_by(.data$round) %>%
      summarise(
        games = n(),
        home_win_pct = mean(.data$home_won),
        avg_total = mean(.data$total_points),
        avg_margin = mean(.data$margin),
        .groups = "drop"
      )

    cat("\n  Round Performance Summary:\n")
    cat("  -------------------------\n")

    for (i in seq_len(nrow(summary))) {
      row <- summary[i, ]
      cat(sprintf("  %s: %d games, %.0f%% home wins, %.1f avg total, %.1f avg margin\n",
                  row$round, row$games,
                  row$home_win_pct * 100,
                  row$avg_total, row$avg_margin))
    }

    pass("Historical performance summary generated")
  } else {
    warn("No completed playoff games to analyze")
  }
} else {
  warn("Cannot generate historical summary (missing score data)")
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n")
cat("=================================================================\n")
cat("  VALIDATION SUMMARY\n")
cat("=================================================================\n")
cat(sprintf("  Season:   %d\n", validation_season))
cat(sprintf("  PASSED:   %d\n", results$passed))
cat(sprintf("  FAILED:   %d\n", results$failed))
cat(sprintf("  WARNINGS: %d\n", length(results$warnings)))
cat("=================================================================\n")

if (length(results$warnings) > 0) {
  cat("\nWarnings:\n")
  for (w in results$warnings) {
    cat(sprintf("  - %s\n", w))
  }
}

# Exit with appropriate code
if (results$failed > 0) {
  cat("\n[VALIDATION FAILED]\n")
  quit(status = 1)
} else {
  cat("\n[VALIDATION PASSED]\n")
  quit(status = 0)
}
