# =============================================================================
# Primetime Significance Test
# =============================================================================
# Tests whether Thursday/Sunday/Monday night game adjustments are statistically
# significant using bootstrap resampling.
#
# IMPORTANT: Per CLAUDE.md, new variables require p < 0.05 before enabling.
#
# Usage:
#   Rscript validation/primetime_significance_test.R
#
# Output:
#   - Console: p-values for each primetime category
#   - File: validation/primetime_significance_results.rds
#
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(nflreadr)
})

# Source config
if (file.exists("config.R")) source("config.R")

cat("\n")
cat("=============================================================================\n")
cat("  PRIMETIME SIGNIFICANCE TEST\n")
cat("=============================================================================\n")
cat("  Testing whether primetime adjustments are statistically significant\n")
cat("  Requirement: p < 0.05 for any adjustment to be enabled\n")
cat("=============================================================================\n\n")

# Load schedule data
sched <- tryCatch({
  nflreadr::load_schedules(seasons = 2018:2024)
}, error = function(e) {
  stop("Failed to load schedule data: ", conditionMessage(e))
})

# Classify games by time slot
classify_game_slot <- function(gametime, day_of_week) {
  # Convert gametime to numeric hour
  hour <- as.numeric(substr(gametime, 1, 2))
  hour <- ifelse(is.na(hour), 13, hour)  # Default to afternoon

  case_when(
    day_of_week == "Thursday" ~ "thursday_night",
    day_of_week == "Monday" ~ "monday_night",
    day_of_week == "Sunday" & hour >= 19 ~ "sunday_night",
    day_of_week == "Sunday" & hour >= 16 ~ "sunday_afternoon_late",
    day_of_week == "Sunday" ~ "sunday_afternoon_early",
    day_of_week == "Saturday" ~ "saturday",
    TRUE ~ "other"
  )
}

# Prepare data
games <- sched %>%
  filter(
    game_type == "REG",
    !is.na(result),
    !is.na(home_score),
    !is.na(away_score)
  ) %>%
  mutate(
    day_of_week = weekdays(as.Date(gameday)),
    game_slot = classify_game_slot(gametime, day_of_week),
    # Calculate model accuracy metrics
    home_win = as.integer(home_score > away_score),
    spread = abs(home_score - away_score)
  )

cat(sprintf("Loaded %d regular season games (2018-2024)\n\n", nrow(games)))

# Summarize by time slot
slot_summary <- games %>%
  group_by(game_slot) %>%
  summarise(
    n_games = n(),
    home_win_rate = mean(home_win, na.rm = TRUE),
    avg_spread = mean(spread, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_games))

cat("Game slot distribution:\n")
print(slot_summary)
cat("\n")

# Bootstrap test for primetime vs baseline
bootstrap_primetime_test <- function(games, primetime_slots, n_bootstrap = 5000) {

  # Split into primetime and baseline
  primetime_games <- games %>% filter(game_slot %in% primetime_slots)
  baseline_games <- games %>% filter(game_slot == "sunday_afternoon_early")

  if (nrow(primetime_games) < 50 || nrow(baseline_games) < 100) {
    return(list(
      p_value = NA,
      observed_diff = NA,
      message = "Insufficient sample size"
    ))
  }

  # Observed difference in home win rate
  primetime_rate <- mean(primetime_games$home_win, na.rm = TRUE)
  baseline_rate <- mean(baseline_games$home_win, na.rm = TRUE)
  observed_diff <- primetime_rate - baseline_rate

  # Bootstrap under null hypothesis (no difference)
  all_games <- bind_rows(primetime_games, baseline_games)
  n_primetime <- nrow(primetime_games)

  set.seed(42)
  boot_diffs <- replicate(n_bootstrap, {
    # Shuffle labels
    shuffled <- sample(nrow(all_games))
    perm_primetime <- all_games$home_win[shuffled[1:n_primetime]]
    perm_baseline <- all_games$home_win[shuffled[(n_primetime + 1):nrow(all_games)]]
    mean(perm_primetime, na.rm = TRUE) - mean(perm_baseline, na.rm = TRUE)
  })

  # Two-tailed p-value
  p_value <- mean(abs(boot_diffs) >= abs(observed_diff))

  list(
    p_value = p_value,
    observed_diff = observed_diff,
    primetime_rate = primetime_rate,
    baseline_rate = baseline_rate,
    n_primetime = n_primetime,
    n_baseline = nrow(baseline_games)
  )
}

# Test each primetime slot
cat("Running bootstrap significance tests (5000 iterations)...\n\n")

results <- list()

# Thursday Night
results$thursday <- bootstrap_primetime_test(games, "thursday_night")
cat(sprintf("Thursday Night Football:\n"))
cat(sprintf("  N games: %d primetime vs %d baseline\n",
            results$thursday$n_primetime, results$thursday$n_baseline))
cat(sprintf("  Home win rate: %.1f%% vs %.1f%% (diff: %+.1f%%)\n",
            results$thursday$primetime_rate * 100,
            results$thursday$baseline_rate * 100,
            results$thursday$observed_diff * 100))
cat(sprintf("  p-value: %.4f %s\n\n",
            results$thursday$p_value,
            if (results$thursday$p_value < 0.05) "*** SIGNIFICANT" else "(not significant)"))

# Sunday Night
results$sunday <- bootstrap_primetime_test(games, "sunday_night")
cat(sprintf("Sunday Night Football:\n"))
cat(sprintf("  N games: %d primetime vs %d baseline\n",
            results$sunday$n_primetime, results$sunday$n_baseline))
cat(sprintf("  Home win rate: %.1f%% vs %.1f%% (diff: %+.1f%%)\n",
            results$sunday$primetime_rate * 100,
            results$sunday$baseline_rate * 100,
            results$sunday$observed_diff * 100))
cat(sprintf("  p-value: %.4f %s\n\n",
            results$sunday$p_value,
            if (results$sunday$p_value < 0.05) "*** SIGNIFICANT" else "(not significant)"))

# Monday Night
results$monday <- bootstrap_primetime_test(games, "monday_night")
cat(sprintf("Monday Night Football:\n"))
cat(sprintf("  N games: %d primetime vs %d baseline\n",
            results$monday$n_primetime, results$monday$n_baseline))
cat(sprintf("  Home win rate: %.1f%% vs %.1f%% (diff: %+.1f%%)\n",
            results$monday$primetime_rate * 100,
            results$monday$baseline_rate * 100,
            results$monday$observed_diff * 100))
cat(sprintf("  p-value: %.4f %s\n\n",
            results$monday$p_value,
            if (results$monday$p_value < 0.05) "*** SIGNIFICANT" else "(not significant)"))

# Summary
cat("=============================================================================\n")
cat("  SUMMARY\n")
cat("=============================================================================\n")

any_significant <- any(
  results$thursday$p_value < 0.05,
  results$sunday$p_value < 0.05,
  results$monday$p_value < 0.05
)

if (any_significant) {
  cat("\n  SIGNIFICANT EFFECTS FOUND!\n\n")
  cat("  Recommended config.R changes:\n")
  cat("  -----------------------------\n")
  cat("  USE_PRIMETIME_ADJUSTMENTS <- TRUE\n")

  if (results$thursday$p_value < 0.05) {
    adj <- round(results$thursday$observed_diff * 7, 2)  # Convert to points
    cat(sprintf("  THURSDAY_NIGHT_ADJ <- %.2f  # p = %.4f\n", adj, results$thursday$p_value))
  }
  if (results$sunday$p_value < 0.05) {
    adj <- round(results$sunday$observed_diff * 7, 2)
    cat(sprintf("  SUNDAY_NIGHT_ADJ <- %.2f  # p = %.4f\n", adj, results$sunday$p_value))
  }
  if (results$monday$p_value < 0.05) {
    adj <- round(results$monday$observed_diff * 7, 2)
    cat(sprintf("  MONDAY_NIGHT_ADJ <- %.2f  # p = %.4f\n", adj, results$monday$p_value))
  }
} else {
  cat("\n  NO SIGNIFICANT EFFECTS FOUND\n\n")
  cat("  Per CLAUDE.md: Variables must have p < 0.05 before enabling.\n")
  cat("  Primetime adjustments should remain DISABLED.\n")
  cat("\n")
  cat("  Current config.R settings are correct:\n")
  cat("  USE_PRIMETIME_ADJUSTMENTS <- FALSE\n")
}

cat("\n=============================================================================\n\n")

# Save results
if (!dir.exists("validation")) dir.create("validation", recursive = TRUE)
saveRDS(results, "validation/primetime_significance_results.rds")
cat("Results saved to: validation/primetime_significance_results.rds\n")
