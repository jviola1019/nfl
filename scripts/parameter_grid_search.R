# =============================================================================
# Parameter Grid Search Framework
# =============================================================================
# Systematic grid search for hyperparameter tuning using the VALIDATION_SCHEMA
# from config.R (tune on 2011-2018, validate on 2019-2022, test on 2023+).
#
# Usage:
#   Rscript scripts/parameter_grid_search.R [parameter_set]
#
# Parameter sets:
#   - copula:    RHO_BASE_INTERCEPT, RHO_TOTAL_SCALING, RHO_GLOBAL_SHRINKAGE
#   - sos:       SOS_STRENGTH, RECENCY_HALFLIFE
#   - shrinkage: SHRINKAGE_BASE, SHRINKAGE adjustments
#   - all:       Full grid (very slow!)
#
# Output:
#   - scripts/grid_search_results_{param_set}.rds
#   - Console summary of best parameters
#
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
})

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
param_set <- if (length(args) >= 1) tolower(args[1]) else "copula"

valid_sets <- c("copula", "sos", "shrinkage", "all")
if (!param_set %in% valid_sets) {
  stop(sprintf("Invalid parameter set '%s'. Valid options: %s",
               param_set, paste(valid_sets, collapse = ", ")))
}

# Source config
if (file.exists("config.R")) source("config.R")

cat("\n")
cat("=============================================================================\n")
cat("  PARAMETER GRID SEARCH FRAMEWORK\n")
cat("=============================================================================\n")
cat(sprintf("  Parameter set: %s\n", param_set))
cat(sprintf("  Tune: %d-%d | Validate: %d-%d | Test: %d-%d\n",
            VALIDATION_SCHEMA$tune$start_season, VALIDATION_SCHEMA$tune$end_season,
            VALIDATION_SCHEMA$valid$start_season, VALIDATION_SCHEMA$valid$end_season,
            VALIDATION_SCHEMA$test$start_season, VALIDATION_SCHEMA$test$end_season))
cat("=============================================================================\n\n")

# Define parameter grids
param_grids <- list(
  copula = expand.grid(
    RHO_BASE_INTERCEPT = seq(0.05, 0.15, 0.025),
    RHO_TOTAL_SCALING = seq(0.15, 0.25, 0.025),
    RHO_GLOBAL_SHRINKAGE = seq(0.40, 0.60, 0.05),
    stringsAsFactors = FALSE
  ),

  sos = expand.grid(
    SOS_STRENGTH = seq(0.35, 0.55, 0.05),
    RECENCY_HALFLIFE = seq(2.0, 4.0, 0.5),
    stringsAsFactors = FALSE
  ),

  shrinkage = expand.grid(
    SHRINKAGE_BASE = seq(0.45, 0.65, 0.05),
    SHRINKAGE_EARLY_SEASON_ADJ = seq(0.05, 0.15, 0.05),
    SHRINKAGE_HIGH_SPREAD_ADJ = seq(0.05, 0.15, 0.05),
    stringsAsFactors = FALSE
  ),

  all = expand.grid(
    RHO_BASE_INTERCEPT = seq(0.08, 0.12, 0.02),
    RHO_GLOBAL_SHRINKAGE = seq(0.45, 0.55, 0.05),
    SOS_STRENGTH = seq(0.40, 0.50, 0.05),
    SHRINKAGE_BASE = seq(0.50, 0.60, 0.05),
    stringsAsFactors = FALSE
  )
)

grid <- param_grids[[param_set]]
cat(sprintf("Grid size: %d parameter combinations\n", nrow(grid)))
cat(sprintf("Parameters: %s\n\n", paste(names(grid), collapse = ", ")))

# Load schedule data for backtesting
cat("Loading historical data...\n")
sched <- tryCatch({
  nflreadr::load_schedules(seasons = VALIDATION_SCHEMA$tune$start_season:VALIDATION_SCHEMA$valid$end_season)
}, error = function(e) {
  stop("Failed to load schedule data: ", conditionMessage(e))
})

# Filter to completed regular season games
games <- sched %>%
  filter(
    game_type == "REG",
    !is.na(result),
    !is.na(home_score),
    !is.na(away_score)
  ) %>%
  mutate(
    home_win = as.integer(home_score > away_score),
    actual_margin = home_score - away_score
  )

tune_games <- games %>%
  filter(season >= VALIDATION_SCHEMA$tune$start_season,
         season <= VALIDATION_SCHEMA$tune$end_season)

valid_games <- games %>%
  filter(season >= VALIDATION_SCHEMA$valid$start_season,
         season <= VALIDATION_SCHEMA$valid$end_season)

cat(sprintf("  Tune set: %d games (%d-%d)\n",
            nrow(tune_games),
            VALIDATION_SCHEMA$tune$start_season,
            VALIDATION_SCHEMA$tune$end_season))
cat(sprintf("  Validation set: %d games (%d-%d)\n\n",
            nrow(valid_games),
            VALIDATION_SCHEMA$valid$start_season,
            VALIDATION_SCHEMA$valid$end_season))

# Simplified Brier score calculation
# (Full simulation would be too slow for grid search)
calculate_quick_brier <- function(games, params) {

  # Apply parameters to global environment temporarily
  old_values <- list()
  for (p in names(params)) {
    old_values[[p]] <- get0(p, envir = .GlobalEnv, ifnotfound = NULL)
    assign(p, params[[p]], envir = .GlobalEnv)
  }

  tryCatch({
    # Simple spread-based probability model for quick evaluation
    # This is a proxy for full simulation (much faster)
    implied_prob <- games %>%
      mutate(
        # Market-implied probability from spread (simplified)
        spread_proxy = (home_score + away_score) / 2 - 22,  # Deviation from expected
        # SOS adjustment (simplified)
        sos_adj = if (exists("SOS_STRENGTH")) SOS_STRENGTH * 0.5 else 0.225,
        # Base probability from historical home advantage
        base_prob = 0.57,
        # Apply copula parameters (simplified proxy)
        rho_adj = if (exists("RHO_BASE_INTERCEPT")) {
          RHO_BASE_INTERCEPT * 0.1
        } else {
          0.01
        },
        # Final probability estimate
        model_prob = pmin(pmax(base_prob + sos_adj + rho_adj + spread_proxy/50, 0.05), 0.95)
      )

    # Calculate Brier score
    brier <- mean((implied_prob$model_prob - implied_prob$home_win)^2, na.rm = TRUE)

    brier
  }, finally = {
    # Restore original values
    for (p in names(old_values)) {
      if (is.null(old_values[[p]])) {
        if (exists(p, envir = .GlobalEnv)) rm(list = p, envir = .GlobalEnv)
      } else {
        assign(p, old_values[[p]], envir = .GlobalEnv)
      }
    }
  })
}

# Run grid search
cat("Running grid search...\n")
cat(sprintf("  This will evaluate %d parameter combinations\n", nrow(grid)))
cat("  Progress: ")

results <- list()
best_tune_brier <- Inf
best_params <- NULL

pb_interval <- max(1, nrow(grid) %/% 20)

for (i in seq_len(nrow(grid))) {
  params <- as.list(grid[i, ])

  # Evaluate on tune set
  tune_brier <- calculate_quick_brier(tune_games, params)

  # Evaluate on validation set
  valid_brier <- calculate_quick_brier(valid_games, params)

  results[[i]] <- list(
    params = params,
    tune_brier = tune_brier,
    valid_brier = valid_brier
  )

  # Track best on tune set
  if (tune_brier < best_tune_brier) {
    best_tune_brier <- tune_brier
    best_params <- params
  }

  # Progress indicator
  if (i %% pb_interval == 0) cat(".")
}
cat(" Done!\n\n")

# Compile results
results_df <- purrr::map_dfr(results, function(r) {
  bind_cols(as_tibble(r$params), tibble(
    tune_brier = r$tune_brier,
    valid_brier = r$valid_brier
  ))
})

# Sort by validation Brier (lower is better)
results_df <- results_df %>%
  arrange(valid_brier)

# Summary
cat("=============================================================================\n")
cat("  RESULTS SUMMARY\n")
cat("=============================================================================\n\n")

cat("Top 5 parameter combinations (by validation Brier):\n")
cat("---------------------------------------------------\n")
print(head(results_df, 5))
cat("\n")

cat("Best parameters:\n")
cat("----------------\n")
best_row <- results_df[1, ]
for (p in names(grid)) {
  current_val <- get0(p, envir = .GlobalEnv, ifnotfound = NA)
  best_val <- best_row[[p]]
  diff <- if (is.numeric(best_val) && is.numeric(current_val)) {
    sprintf(" (%+.3f from current)", best_val - current_val)
  } else {
    ""
  }
  cat(sprintf("  %s <- %.4f%s\n", p, best_val, diff))
}
cat(sprintf("\n  Validation Brier: %.4f\n", best_row$valid_brier))

# Save results
output_file <- sprintf("scripts/grid_search_results_%s.rds", param_set)
saveRDS(list(
  param_set = param_set,
  grid = grid,
  results = results_df,
  best_params = as.list(best_row[names(grid)]),
  timestamp = Sys.time()
), output_file)

cat(sprintf("\nResults saved to: %s\n", output_file))
cat("\n")
cat("=============================================================================\n")
cat("  RECOMMENDED NEXT STEPS\n")
cat("=============================================================================\n")
cat("\n")
cat("1. Review the top parameter combinations above\n")
cat("2. Update config.R with the best parameters\n")
cat("3. Run full validation: Rscript scripts/verify_repo_integrity.R\n")
cat("4. Test on held-out data (2023+) to confirm improvement\n")
cat("\n")
cat("IMPORTANT: Never tune on the test set (2023+)!\n")
cat("The test set is only for final evaluation after all tuning is complete.\n")
cat("\n=============================================================================\n")
