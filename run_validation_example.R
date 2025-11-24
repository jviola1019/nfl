# =============================================================================
# Example: Run Full Validation Pipeline
# =============================================================================
#
# This script demonstrates how to use the train/validation/test pipeline
# for proper out-of-sample hyperparameter tuning and model evaluation.
#
# IMPORTANT: This uses historical data and takes time to run.
# Expect 10-30 minutes depending on grid size and system performance.
#
# =============================================================================

# Load configuration
if (!exists("VALIDATION_SCHEMA")) {
  source("config.R")
}

# Load validation pipeline functions
source("validation_pipeline.R")

# Load required data sources
library(nflreadr)
library(dplyr)

# =============================================================================
# OPTION 1: Quick Validation (Small Grid)
# =============================================================================

run_quick_validation <- function() {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  QUICK VALIDATION (Small Grid)\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")

  # Load schedule for market comparisons
  cat("Loading NFL schedule data...\n")
  sched <- nflreadr::load_schedules()

  # Define small grid for quick testing
  quick_grid <- expand.grid(
    GLMM_BLEND_W     = c(0.38, 0.5),      # Only 2 values
    RECENCY_HALFLIFE = c(3.0),             # Fixed at current default
    N_RECENT         = c(6),               # Fixed at current default
    SOS_STRENGTH     = c(0.45, 0.6),      # Only 2 values
    stringsAsFactors = FALSE
  )

  cat(sprintf("Grid size: %d combinations\n\n", nrow(quick_grid)))

  # Run validation
  results <- run_full_validation(
    schema       = VALIDATION_SCHEMA,
    tune_grid    = quick_grid,
    trials       = BACKTEST_TRIALS,
    save_results = TRUE,
    sched        = sched
  )

  # Print summary
  print_validation_summary(results, detailed = TRUE)

  invisible(results)
}

# =============================================================================
# OPTION 2: Full Validation (Default Grid)
# =============================================================================

run_full_validation_default <- function() {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  FULL VALIDATION (Default Grid)\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")

  # Load schedule
  cat("Loading NFL schedule data...\n")
  sched <- nflreadr::load_schedules()

  # Run with default grid (3×3×3×3 = 81 combinations)
  results <- run_full_validation(
    schema       = VALIDATION_SCHEMA,
    tune_grid    = NULL,  # Use default grid
    trials       = BACKTEST_TRIALS,
    save_results = TRUE,
    sched        = sched
  )

  # Print summary
  print_validation_summary(results, detailed = TRUE)

  invisible(results)
}

# =============================================================================
# OPTION 3: Custom Grid
# =============================================================================

run_custom_validation <- function() {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  CUSTOM VALIDATION\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")

  # Load schedule
  cat("Loading NFL schedule data...\n")
  sched <- nflreadr::load_schedules()

  # Define custom grid (focus on key parameters)
  custom_grid <- expand.grid(
    GLMM_BLEND_W     = seq(0.3, 0.5, by = 0.05),  # Finer resolution
    RECENCY_HALFLIFE = c(2.5, 3.0, 3.5),
    N_RECENT         = c(6),                       # Fixed
    SOS_STRENGTH     = c(0.45),                    # Fixed
    stringsAsFactors = FALSE
  )

  cat(sprintf("Grid size: %d combinations\n\n", nrow(custom_grid)))

  results <- run_full_validation(
    schema       = VALIDATION_SCHEMA,
    tune_grid    = custom_grid,
    trials       = BACKTEST_TRIALS,
    save_results = TRUE,
    sched        = sched
  )

  print_validation_summary(results, detailed = TRUE)

  invisible(results)
}

# =============================================================================
# OPTION 4: Just Tune Hyperparameters (No Full Validation)
# =============================================================================

run_tuning_only <- function() {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  HYPERPARAMETER TUNING ONLY\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")

  # Small grid for quick tuning
  tuning_grid <- expand.grid(
    GLMM_BLEND_W     = seq(0.3, 0.5, by = 0.05),
    RECENCY_HALFLIFE = c(2.0, 3.0, 4.0),
    N_RECENT         = c(4, 6, 8),
    SOS_STRENGTH     = c(0.3, 0.45, 0.6),
    stringsAsFactors = FALSE
  )

  # Tune
  tuning_results <- tune_hyperparams(
    schema  = VALIDATION_SCHEMA$tune,
    grid    = tuning_grid,
    trials  = BACKTEST_TRIALS,
    verbose = TRUE
  )

  # Show top 10 configurations
  cat("\nTop 10 Configurations by Brier Score:\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  print(head(tuning_results$tuning_results, 10))

  # Apply best
  apply_best_hyperparams(tuning_results$best_params, verbose = TRUE)

  invisible(tuning_results)
}

# =============================================================================
# OPTION 5: Load Previous Results and Analyze
# =============================================================================

analyze_previous_results <- function(results_path = NULL) {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  ANALYZE PREVIOUS VALIDATION RESULTS\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")

  # Find most recent results if path not provided
  if (is.null(results_path)) {
    results_files <- list.files("run_logs", pattern = "^validation_results_.*\\.rds$",
                                 full.names = TRUE)
    if (length(results_files) == 0) {
      stop("No validation results found in run_logs/")
    }
    results_path <- results_files[which.max(file.info(results_files)$mtime)]
    cat(sprintf("Loading most recent results: %s\n\n", basename(results_path)))
  }

  # Load results
  results <- readRDS(results_path)

  # Print summary
  print_validation_summary(results, detailed = TRUE)

  # Additional analysis
  cat("\nTuning Grid Results (top 5 by each metric):\n")
  cat("───────────────────────────────────────────────────────────────────\n")

  cat("\nBy Brier Score:\n")
  print(head(results$tuning$tuning_results %>%
               dplyr::arrange(brier2_mean) %>%
               dplyr::select(GLMM_BLEND_W, RECENCY_HALFLIFE, N_RECENT,
                             SOS_STRENGTH, brier2_mean), 5))

  cat("\nBy Log-Loss:\n")
  print(head(results$tuning$tuning_results %>%
               dplyr::arrange(logloss2_mean) %>%
               dplyr::select(GLMM_BLEND_W, RECENCY_HALFLIFE, N_RECENT,
                             SOS_STRENGTH, logloss2_mean), 5))

  # Check for overfitting
  train_brier <- results$tuning$tuning_results$brier2_mean[1]
  valid_brier <- results$validation$scores$overall$Brier2_cal
  test_brier  <- results$test$scores$overall$Brier2_cal

  cat("\nOverfitting Check:\n")
  cat("───────────────────────────────────────────────────────────────────\n")
  cat(sprintf("  Train Brier:      %.4f\n", train_brier))
  cat(sprintf("  Validation Brier: %.4f (diff: %+.4f)\n",
              valid_brier, valid_brier - train_brier))
  cat(sprintf("  Test Brier:       %.4f (diff: %+.4f)\n",
              test_brier, test_brier - train_brier))

  if (valid_brier > train_brier + 0.01) {
    cat("\n⚠️  Potential overfitting detected (validation >> train)\n")
  } else {
    cat("\n✓ No major overfitting detected\n")
  }

  invisible(results)
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

if (interactive()) {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  NFL Model Validation Examples\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("\nAvailable functions:\n")
  cat("  1. run_quick_validation()          - Fast, small grid (4 configs)\n")
  cat("  2. run_full_validation_default()   - Full grid (81 configs, ~30 min)\n")
  cat("  3. run_custom_validation()         - Custom grid example\n")
  cat("  4. run_tuning_only()               - Just hyperparameter tuning\n")
  cat("  5. analyze_previous_results()      - Load and analyze saved results\n")
  cat("\nExample usage:\n")
  cat("  results <- run_quick_validation()\n")
  cat("  print_validation_summary(results, detailed = TRUE)\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")

} else {
  # Non-interactive: run quick validation by default
  cat("\nRunning quick validation (non-interactive mode)...\n")
  results <- run_quick_validation()
}
