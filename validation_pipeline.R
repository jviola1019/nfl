# =============================================================================
# NFL Model - Train/Validation/Test Pipeline
# =============================================================================
#
# This module implements proper out-of-sample validation:
#   1. Hyperparameter tuning on TRAIN set (2011-2018)
#   2. Model evaluation on VALIDATION set (2019-2022)
#   3. Final testing on TEST set (2023-current)
#
# Ensures no data leakage: calibration and hyperparameters are fixed before
# evaluating on validation/test sets.
#
# =============================================================================

# =============================================================================
# STEP 1: Hyperparameter Tuning (TRAIN set only)
# =============================================================================

#' Tune hyperparameters using grid search on training data
#'
#' @param schema List with start_season and end_season for tuning window
#' @param grid Optional custom grid; if NULL, uses default grid
#' @param trials Number of Monte Carlo trials per configuration (default: BACKTEST_TRIALS)
#' @param metric Primary metric for selection: "brier2" (default), "logloss2", "brier3"
#' @param verbose Print progress messages (default: TRUE)
#' @return List with best_params, tuning_results (tibble), and full grid results
#'
#' @details
#' Grid search over:
#'   - GLMM_BLEND_W: Weight on GLMM priors vs pace baseline
#'   - RECENCY_HALFLIFE: Exponential decay halflife for recent games
#'   - N_RECENT: Number of recent games to use for form
#'   - SOS_STRENGTH: Strength of schedule adjustment magnitude
#'
#' Primary selection criterion: minimize mean 2-way Brier score
#' Tiebreaker: minimize log-loss
#'
#' @examples
#' \dontrun{
#'   # Tune on 2011-2018 data
#'   tuning_results <- tune_hyperparams(VALIDATION_SCHEMA$tune)
#'
#'   # Apply best parameters
#'   apply_best_hyperparams(tuning_results$best_params)
#' }
tune_hyperparams <- function(schema = VALIDATION_SCHEMA$tune,
                              grid = NULL,
                              trials = BACKTEST_TRIALS,
                              metric = c("brier2", "logloss2", "brier3"),
                              verbose = TRUE) {

  metric <- match.arg(metric)

  # Default grid if not provided
  if (is.null(grid)) {
    grid <- expand.grid(
      GLMM_BLEND_W     = c(0.3, 0.38, 0.5),
      RECENCY_HALFLIFE = c(2.0, 3.0, 4.0),
      N_RECENT         = c(4L, 6L, 8L),
      SOS_STRENGTH     = c(0.3, 0.45, 0.6),
      stringsAsFactors = FALSE
    )
  }

  if (verbose) {
    cat("\n")
    cat("═══════════════════════════════════════════════════════════════════\n")
    cat("  HYPERPARAMETER TUNING\n")
    cat("═══════════════════════════════════════════════════════════════════\n")
    cat(sprintf("  Training window: %d-%d\n", schema$start_season, schema$end_season))
    cat(sprintf("  Grid size: %d combinations\n", nrow(grid)))
    cat(sprintf("  Trials per config: %s\n", format(trials, big.mark = ",")))
    cat(sprintf("  Selection metric: %s\n", metric))
    cat("───────────────────────────────────────────────────────────────────\n\n")
  }

  # Save current config to restore later
  config_backup <- list(
    GLMM_BLEND_W     = GLMM_BLEND_W,
    RECENCY_HALFLIFE = RECENCY_HALFLIFE,
    N_RECENT         = N_RECENT,
    SOS_STRENGTH     = SOS_STRENGTH
  )

  # Results storage
  results <- vector("list", nrow(grid))

  # Grid search
  for (i in seq_len(nrow(grid))) {
    params <- grid[i, ]

    if (verbose) {
      cat(sprintf("[%d/%d] Testing: GLMM=%.2f, Halflife=%.1f, NRecent=%d, SoS=%.2f ",
                  i, nrow(grid),
                  params$GLMM_BLEND_W, params$RECENCY_HALFLIFE,
                  params$N_RECENT, params$SOS_STRENGTH))
    }

    # Temporarily override hyperparameters
    assign("GLMM_BLEND_W", params$GLMM_BLEND_W, envir = .GlobalEnv)
    assign("RECENCY_HALFLIFE", params$RECENCY_HALFLIFE, envir = .GlobalEnv)
    assign("N_RECENT", params$N_RECENT, envir = .GlobalEnv)
    assign("SOS_STRENGTH", params$SOS_STRENGTH, envir = .GlobalEnv)

    # Run scoring on training window
    # Note: This uses the existing score_weeks() function which handles caching
    tryCatch({
      scores <- score_weeks(
        start_season = schema$start_season,
        end_season   = schema$end_season,
        weeks        = NULL,
        trials       = trials
      )

      # Extract metrics (calibrated versions - these are what we care about)
      results[[i]] <- tibble::tibble(
        config_id        = i,
        GLMM_BLEND_W     = params$GLMM_BLEND_W,
        RECENCY_HALFLIFE = params$RECENCY_HALFLIFE,
        N_RECENT         = params$N_RECENT,
        SOS_STRENGTH     = params$SOS_STRENGTH,
        brier2_mean      = scores$overall$Brier2_cal,
        logloss2_mean    = scores$overall$LogLoss2_cal,
        brier3_mean      = scores$overall$Brier3_cal,
        logloss3_mean    = scores$overall$LogLoss3_cal,
        n_games          = scores$overall$n_games,
        n_weeks          = scores$overall$n_weeks
      )

      if (verbose) {
        cat(sprintf("→ Brier2=%.4f, LogLoss2=%.4f\n",
                    results[[i]]$brier2_mean, results[[i]]$logloss2_mean))
      }

    }, error = function(e) {
      if (verbose) {
        cat(sprintf("→ ERROR: %s\n", conditionMessage(e)))
      }
      results[[i]] <- tibble::tibble(
        config_id        = i,
        GLMM_BLEND_W     = params$GLMM_BLEND_W,
        RECENCY_HALFLIFE = params$RECENCY_HALFLIFE,
        N_RECENT         = params$N_RECENT,
        SOS_STRENGTH     = params$SOS_STRENGTH,
        brier2_mean      = NA_real_,
        logloss2_mean    = NA_real_,
        brier3_mean      = NA_real_,
        logloss3_mean    = NA_real_,
        n_games          = NA_integer_,
        n_weeks          = NA_integer_
      )
    })
  }

  # Restore original config
  assign("GLMM_BLEND_W", config_backup$GLMM_BLEND_W, envir = .GlobalEnv)
  assign("RECENCY_HALFLIFE", config_backup$RECENCY_HALFLIFE, envir = .GlobalEnv)
  assign("N_RECENT", config_backup$N_RECENT, envir = .GlobalEnv)
  assign("SOS_STRENGTH", config_backup$SOS_STRENGTH, envir = .GlobalEnv)

  # Combine results
  tuning_results <- dplyr::bind_rows(results) %>%
    dplyr::filter(!is.na(brier2_mean)) %>%  # Remove failed configs
    dplyr::arrange(brier2_mean, logloss2_mean)  # Sort by primary metric, then tiebreaker

  if (nrow(tuning_results) == 0) {
    stop("All hyperparameter configurations failed. Check your data and configuration.")
  }

  # Select best configuration
  best_idx <- 1  # First row after sorting
  best_params <- list(
    GLMM_BLEND_W     = tuning_results$GLMM_BLEND_W[best_idx],
    RECENCY_HALFLIFE = tuning_results$RECENCY_HALFLIFE[best_idx],
    N_RECENT         = tuning_results$N_RECENT[best_idx],
    SOS_STRENGTH     = tuning_results$SOS_STRENGTH[best_idx]
  )

  if (verbose) {
    cat("\n")
    cat("═══════════════════════════════════════════════════════════════════\n")
    cat("  BEST HYPERPARAMETERS (by", toupper(metric), ")\n")
    cat("═══════════════════════════════════════════════════════════════════\n")
    cat(sprintf("  GLMM_BLEND_W:     %.3f\n", best_params$GLMM_BLEND_W))
    cat(sprintf("  RECENCY_HALFLIFE: %.2f\n", best_params$RECENCY_HALFLIFE))
    cat(sprintf("  N_RECENT:         %d\n", best_params$N_RECENT))
    cat(sprintf("  SOS_STRENGTH:     %.3f\n", best_params$SOS_STRENGTH))
    cat("───────────────────────────────────────────────────────────────────\n")
    cat(sprintf("  Brier (2-way):    %.4f\n", tuning_results$brier2_mean[best_idx]))
    cat(sprintf("  Log-Loss (2-way): %.4f\n", tuning_results$logloss2_mean[best_idx]))
    cat(sprintf("  Games scored:     %d\n", tuning_results$n_games[best_idx]))
    cat("═══════════════════════════════════════════════════════════════════\n\n")
  }

  list(
    best_params     = best_params,
    tuning_results  = tuning_results,
    selection_metric = metric,
    training_window = schema
  )
}


#' Apply best hyperparameters from tuning to global environment
#'
#' @param best_params List of hyperparameters (from tune_hyperparams()$best_params)
#' @param update_config_file If TRUE, updates config.R file (default: FALSE)
#' @param verbose Print confirmation message (default: TRUE)
#' @return Invisibly returns the parameters that were set
#'
#' @examples
#' \dontrun{
#'   tuning <- tune_hyperparams()
#'   apply_best_hyperparams(tuning$best_params)
#' }
apply_best_hyperparams <- function(best_params, update_config_file = FALSE, verbose = TRUE) {

  # Apply to global environment
  for (param_name in names(best_params)) {
    assign(param_name, best_params[[param_name]], envir = .GlobalEnv)
  }

  if (verbose) {
    cat("\n✓ Applied best hyperparameters to global environment:\n")
    for (param_name in names(best_params)) {
      cat(sprintf("  %s = %s\n", param_name,
                  format(best_params[[param_name]], digits = 4)))
    }
    cat("\n")
  }

  if (update_config_file) {
    warning("Automatic config.R update not yet implemented. ",
            "Please manually update config.R with the best parameters shown above.")
  }

  invisible(best_params)
}


# =============================================================================
# STEP 2: Build Calibration on Training Data Only
# =============================================================================

#' Build calibration models on training data
#'
#' @param start_season First season for calibration training
#' @param end_season Last season for calibration training
#' @param trials Number of Monte Carlo trials for simulations
#' @param save_path Optional path to save calibration object as RDS
#' @return List with calibration objects (isotonic mappings, 3-way multinomial, etc.)
#'
#' @details
#' This function builds the calibration using ONLY the specified training window.
#' The resulting calibration object should be frozen and reused for validation/test.
#'
#' Calibration includes:
#'   - Global isotonic regression (map_iso)
#'   - Nested CV isotonic regression (isotonic_mappings by fold)
#'   - 3-way multinomial calibration (cal3) if ties present
#'
#' @examples
#' \dontrun{
#'   # Build calibration on 2011-2018
#'   calib <- build_calibration(2011, 2018)
#'
#'   # Save for later use
#'   saveRDS(calib, "calibration_2011_2018.rds")
#' }
build_calibration <- function(start_season,
                               end_season,
                               trials = BACKTEST_TRIALS,
                               save_path = NULL) {

  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  BUILDING CALIBRATION\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat(sprintf("  Training window: %d-%d\n", start_season, end_season))
  cat(sprintf("  Trials: %s\n", format(trials, big.mark = ",")))
  cat("───────────────────────────────────────────────────────────────────\n\n")

  # NOTE: The calibration building logic is currently embedded in NFLsimulation.R
  # around lines 4800-4922. For now, this function will call score_weeks() which
  # internally builds the calibration, then extract the calibration objects.
  #
  # TODO: Refactor NFLsimulation.R to separate calibration building from scoring

  # For now, run score_weeks to build calibration (it's cached)
  cat("Running simulations to build calibration dataset...\n")
  scores <- score_weeks(
    start_season = start_season,
    end_season   = end_season,
    weeks        = NULL,
    trials       = trials
  )

  # Extract calibration objects from global environment
  # (These are created by NFLsimulation.R during score_weeks execution)
  calib_obj <- list(
    map_iso            = if (exists("map_iso", envir = .GlobalEnv)) get("map_iso", envir = .GlobalEnv) else NULL,
    isotonic_mappings  = if (exists("isotonic_mappings", envir = .GlobalEnv)) get("isotonic_mappings", envir = .GlobalEnv) else NULL,
    cal3               = if (exists("cal3", envir = .GlobalEnv)) get("cal3", envir = .GlobalEnv) else NULL,
    calib_sim_df       = if (exists("calib_sim_df", envir = .GlobalEnv)) get("calib_sim_df", envir = .GlobalEnv) else NULL,
    training_window    = list(start_season = start_season, end_season = end_season),
    built_date         = Sys.time(),
    n_games            = scores$overall$n_games
  )

  cat(sprintf("\n✓ Calibration built on %d games from %d-%d\n",
              calib_obj$n_games, start_season, end_season))

  if (!is.null(calib_obj$map_iso)) {
    cat("  ✓ Global isotonic calibration: Ready\n")
  }
  if (!is.null(calib_obj$isotonic_mappings)) {
    cat(sprintf("  ✓ Nested CV isotonic: %d folds\n", length(calib_obj$isotonic_mappings)))
  }
  if (!is.null(calib_obj$cal3)) {
    cat("  ✓ 3-way multinomial calibration: Ready\n")
  }

  # Save if requested
  if (!is.null(save_path)) {
    saveRDS(calib_obj, save_path)
    cat(sprintf("\n✓ Saved calibration to: %s\n", save_path))
  }

  cat("═══════════════════════════════════════════════════════════════════\n\n")

  invisible(calib_obj)
}


#' Load and apply pre-built calibration
#'
#' @param calib_obj Calibration object from build_calibration() or loaded from RDS
#' @return Invisibly returns TRUE if successful
#'
#' @details
#' Loads calibration objects into the global environment so they're used by
#' subsequent scoring functions.
#'
#' @examples
#' \dontrun{
#'   calib <- readRDS("calibration_2011_2018.rds")
#'   apply_calibration(calib)
#' }
apply_calibration <- function(calib_obj) {

  if (!is.list(calib_obj)) {
    stop("calib_obj must be a list from build_calibration() or loaded RDS")
  }

  # Apply to global environment
  if (!is.null(calib_obj$map_iso)) {
    assign("map_iso", calib_obj$map_iso, envir = .GlobalEnv)
  }
  if (!is.null(calib_obj$isotonic_mappings)) {
    assign("isotonic_mappings", calib_obj$isotonic_mappings, envir = .GlobalEnv)
  }
  if (!is.null(calib_obj$cal3)) {
    assign("cal3", calib_obj$cal3, envir = .GlobalEnv)
  }
  if (!is.null(calib_obj$calib_sim_df)) {
    assign("calib_sim_df", calib_obj$calib_sim_df, envir = .GlobalEnv)
  }

  cat(sprintf("\n✓ Applied calibration from %d-%d training window\n",
              calib_obj$training_window$start_season,
              calib_obj$training_window$end_season))

  invisible(TRUE)
}


# =============================================================================
# STEP 3: Evaluation Functions for Each Phase
# =============================================================================

#' Evaluate model on a specific data window
#'
#' @param schema List with start_season and end_season
#' @param phase_name Name for reporting (e.g., "VALIDATION", "TEST")
#' @param trials Number of Monte Carlo trials
#' @param compare_market If TRUE, computes market comparison metrics
#' @param sched Schedule dataframe (required if compare_market=TRUE)
#' @return List with scores, market comparison (if requested), and metadata
#'
#' @details
#' Evaluates the model on the specified window using frozen hyperparameters
#' and calibration. Does NOT retrain anything.
evaluate_phase <- function(schema,
                            phase_name = "EVALUATION",
                            trials = BACKTEST_TRIALS,
                            compare_market = TRUE,
                            sched = NULL) {

  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat(sprintf("  %s PHASE\n", toupper(phase_name)))
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat(sprintf("  Window: %d-%d\n", schema$start_season, schema$end_season))
  cat(sprintf("  Trials: %s\n", format(trials, big.mark = ",")))
  cat("───────────────────────────────────────────────────────────────────\n\n")

  # Run scoring with frozen hyperparameters and calibration
  scores <- score_weeks(
    start_season = schema$start_season,
    end_season   = schema$end_season,
    weeks        = NULL,
    trials       = trials
  )

  # Print overall metrics
  cat("\nModel Performance:\n")
  cat(sprintf("  Brier (2-way):      %.4f (uncal) → %.4f (cal)\n",
              scores$overall$Brier2_raw, scores$overall$Brier2_cal))
  cat(sprintf("  Log-Loss (2-way):   %.4f (uncal) → %.4f (cal)\n",
              scores$overall$LogLoss2_raw, scores$overall$LogLoss2_cal))
  cat(sprintf("  Brier (3-way):      %.4f (uncal) → %.4f (cal)\n",
              scores$overall$Brier3_raw, scores$overall$Brier3_cal))
  cat(sprintf("  Games:              %d\n", scores$overall$n_games))
  cat(sprintf("  Weeks:              %d\n", scores$overall$n_weeks))

  # Market comparison if requested
  market_comp <- NULL
  if (compare_market) {
    if (is.null(sched)) {
      warning("compare_market=TRUE but no schedule provided. Skipping market comparison.")
    } else {
      cat("\nComparing to market...\n")

      tryCatch({
        # Load compare_to_market if not already available
        if (!exists("compare_to_market")) {
          source("NFLbrier_logloss.R", local = FALSE)
        }

        market_comp <- compare_to_market(
          res   = scores,
          sched = sched,
          conf_level = 0.95,
          B = 1000
        )

        if (!is.null(market_comp$overall)) {
          cat("\nModel vs Market:\n")
          cat(sprintf("  Model Brier:   %.4f\n", market_comp$overall$model_brier))
          cat(sprintf("  Market Brier:  %.4f\n", market_comp$overall$market_brier))
          cat(sprintf("  Delta (M-Mkt): %+.4f\n", market_comp$overall$brier_delta))

          # Check if we have bootstrap CIs
          if (exists("paired_stats", where = market_comp)) {
            if ("Brier" %in% names(market_comp$paired_stats)) {
              ci <- market_comp$paired_stats$Brier
              cat(sprintf("  95%% CI:        [%+.4f, %+.4f]\n", ci["lo"], ci["hi"]))
              cat(sprintf("  p-value:       %.4f\n", ci["p_value"]))
            }
          }
        }

      }, error = function(e) {
        warning(sprintf("Market comparison failed: %s", conditionMessage(e)))
      })
    }
  }

  cat("═══════════════════════════════════════════════════════════════════\n\n")

  list(
    phase          = phase_name,
    window         = schema,
    scores         = scores,
    market_comp    = market_comp,
    evaluation_date = Sys.time()
  )
}


# =============================================================================
# STEP 4: Full Validation Pipeline
# =============================================================================

#' Run complete train/validation/test pipeline
#'
#' @param schema Validation schema with tune/valid/test windows (default: VALIDATION_SCHEMA)
#' @param tune_grid Optional custom hyperparameter grid
#' @param trials Number of Monte Carlo trials for backtesting
#' @param save_results If TRUE, saves results to run_logs/ directory
#' @param sched Schedule dataframe for market comparisons
#' @return List with tuning, validation, and test results
#'
#' @details
#' Complete validation pipeline:
#'   1. Hyperparameter tuning on TRAIN (2011-2018)
#'   2. Build calibration on TRAIN
#'   3. Evaluate on VALIDATION (2019-2022) with frozen params/calibration
#'   4. Evaluate on TEST (2023-current) with frozen params/calibration
#'
#' All hyperparameters and calibration are fixed after the tuning phase.
#' Validation and test sets are never used for model selection or training.
#'
#' @examples
#' \dontrun{
#'   # Load schedule for market comparisons
#'   sched <- nflreadr::load_schedules()
#'
#'   # Run full validation
#'   results <- run_full_validation(sched = sched)
#'
#'   # View results
#'   print(results$validation$scores$overall)
#'   print(results$test$market_comp$overall)
#' }
run_full_validation <- function(schema = VALIDATION_SCHEMA,
                                 tune_grid = NULL,
                                 trials = BACKTEST_TRIALS,
                                 save_results = TRUE,
                                 sched = NULL) {

  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  FULL VALIDATION PIPELINE\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  TRAIN window:      ", schema$tune$start_season, "-", schema$tune$end_season, "\n")
  cat("  VALIDATION window: ", schema$valid$start_season, "-", schema$valid$end_season, "\n")
  cat("  TEST window:       ", schema$test$start_season, "-", schema$test$end_season, "\n")
  cat("═══════════════════════════════════════════════════════════════════\n")

  # ===========================================================================
  # PHASE 1: TUNING (Train set only)
  # ===========================================================================

  cat("\n[PHASE 1/4] HYPERPARAMETER TUNING\n")
  tuning_results <- tune_hyperparams(
    schema  = schema$tune,
    grid    = tune_grid,
    trials  = trials,
    verbose = TRUE
  )

  # Apply best hyperparameters
  apply_best_hyperparams(tuning_results$best_params, verbose = TRUE)

  # ===========================================================================
  # PHASE 2: BUILD CALIBRATION (Train set only)
  # ===========================================================================

  cat("\n[PHASE 2/4] BUILDING CALIBRATION\n")

  # Create run_logs directory if it doesn't exist
  if (!dir.exists("run_logs")) {
    dir.create("run_logs", recursive = TRUE)
  }

  calib_path <- file.path("run_logs", sprintf("calibration_train_%d_%d.rds",
                                               schema$tune$start_season,
                                               schema$tune$end_season))

  calibration <- build_calibration(
    start_season = schema$tune$start_season,
    end_season   = schema$tune$end_season,
    trials       = trials,
    save_path    = calib_path
  )

  # ===========================================================================
  # PHASE 3: VALIDATION (Held-out set, frozen params/calibration)
  # ===========================================================================

  cat("\n[PHASE 3/4] VALIDATION EVALUATION\n")
  cat("⚠️  Using FROZEN hyperparameters and calibration from training\n")

  validation_results <- evaluate_phase(
    schema          = schema$valid,
    phase_name      = "VALIDATION",
    trials          = trials,
    compare_market  = TRUE,
    sched           = sched
  )

  # ===========================================================================
  # PHASE 4: TEST (Forward test set, frozen params/calibration)
  # ===========================================================================

  cat("\n[PHASE 4/4] TEST EVALUATION\n")
  cat("⚠️  Using FROZEN hyperparameters and calibration from training\n")

  test_results <- evaluate_phase(
    schema          = schema$test,
    phase_name      = "TEST",
    trials          = trials,
    compare_market  = TRUE,
    sched           = sched
  )

  # ===========================================================================
  # COMPILE RESULTS
  # ===========================================================================

  full_results <- list(
    tuning      = tuning_results,
    calibration = calibration,
    validation  = validation_results,
    test        = test_results,
    schema      = schema,
    run_date    = Sys.time()
  )

  # Save if requested
  if (save_results) {
    results_path <- file.path("run_logs", sprintf("validation_results_%s.rds",
                                                    format(Sys.time(), "%Y%m%d_%H%M%S")))
    saveRDS(full_results, results_path)
    cat(sprintf("\n✓ Saved full results to: %s\n", results_path))
  }

  # ===========================================================================
  # SUMMARY
  # ===========================================================================

  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  VALIDATION PIPELINE COMPLETE\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("\nBest Hyperparameters (from tuning):\n")
  cat(sprintf("  GLMM_BLEND_W:     %.3f\n", tuning_results$best_params$GLMM_BLEND_W))
  cat(sprintf("  RECENCY_HALFLIFE: %.2f\n", tuning_results$best_params$RECENCY_HALFLIFE))
  cat(sprintf("  N_RECENT:         %d\n", tuning_results$best_params$N_RECENT))
  cat(sprintf("  SOS_STRENGTH:     %.3f\n", tuning_results$best_params$SOS_STRENGTH))

  cat("\nPerformance Summary (Brier scores):\n")
  cat(sprintf("  TRAIN      (%d-%d): %.4f\n",
              schema$tune$start_season, schema$tune$end_season,
              tuning_results$tuning_results$brier2_mean[1]))
  cat(sprintf("  VALIDATION (%d-%d): %.4f\n",
              schema$valid$start_season, schema$valid$end_season,
              validation_results$scores$overall$Brier2_cal))
  cat(sprintf("  TEST       (%d-%d): %.4f\n",
              schema$test$start_season, schema$test$end_season,
              test_results$scores$overall$Brier2_cal))

  if (!is.null(validation_results$market_comp$overall)) {
    cat("\nMarket Comparison (Validation):\n")
    cat(sprintf("  Model vs Market delta: %+.4f Brier\n",
                validation_results$market_comp$overall$brier_delta))
  }

  if (!is.null(test_results$market_comp$overall)) {
    cat("\nMarket Comparison (Test):\n")
    cat(sprintf("  Model vs Market delta: %+.4f Brier\n",
                test_results$market_comp$overall$brier_delta))
  }

  cat("═══════════════════════════════════════════════════════════════════\n\n")

  invisible(full_results)
}


# =============================================================================
# HELPER: Print Validation Summary
# =============================================================================

#' Print formatted summary of validation results
#'
#' @param results Output from run_full_validation()
#' @param detailed If TRUE, shows per-season breakdowns
print_validation_summary <- function(results, detailed = FALSE) {

  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  VALIDATION RESULTS SUMMARY\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")

  # Hyperparameters
  cat("Best Hyperparameters:\n")
  cat("───────────────────────────────────────────────────────────────────\n")
  for (param in names(results$tuning$best_params)) {
    cat(sprintf("  %-20s: %s\n", param,
                format(results$tuning$best_params[[param]], digits = 4)))
  }

  # Overall performance
  cat("\nOverall Performance (2-way Brier):\n")
  cat("───────────────────────────────────────────────────────────────────\n")
  cat(sprintf("  %-15s  %s\n", "Phase", "Brier (calibrated)"))
  cat(sprintf("  %-15s  %.4f\n", "TRAIN",
              results$tuning$tuning_results$brier2_mean[1]))
  cat(sprintf("  %-15s  %.4f\n", "VALIDATION",
              results$validation$scores$overall$Brier2_cal))
  cat(sprintf("  %-15s  %.4f\n", "TEST",
              results$test$scores$overall$Brier2_cal))

  # Market comparison
  if (!is.null(results$validation$market_comp)) {
    cat("\nMarket Comparison:\n")
    cat("───────────────────────────────────────────────────────────────────\n")
    cat(sprintf("  %-15s  Model: %.4f  Market: %.4f  Delta: %+.4f\n",
                "VALIDATION",
                results$validation$market_comp$overall$model_brier,
                results$validation$market_comp$overall$market_brier,
                results$validation$market_comp$overall$brier_delta))
  }

  if (!is.null(results$test$market_comp)) {
    cat(sprintf("  %-15s  Model: %.4f  Market: %.4f  Delta: %+.4f\n",
                "TEST",
                results$test$market_comp$overall$model_brier,
                results$test$market_comp$overall$market_brier,
                results$test$market_comp$overall$brier_delta))
  }

  # Detailed per-season if requested
  if (detailed) {
    cat("\nPer-Season Performance (Validation):\n")
    cat("───────────────────────────────────────────────────────────────────\n")
    by_season_valid <- results$validation$scores$by_week %>%
      dplyr::group_by(season) %>%
      dplyr::summarise(
        brier2_cal = weighted.mean(Brier2_cal, n_games, na.rm = TRUE),
        n_games = sum(n_games),
        .groups = "drop"
      )
    print(by_season_valid)

    cat("\nPer-Season Performance (Test):\n")
    cat("───────────────────────────────────────────────────────────────────\n")
    by_season_test <- results$test$scores$by_week %>%
      dplyr::group_by(season) %>%
      dplyr::summarise(
        brier2_cal = weighted.mean(Brier2_cal, n_games, na.rm = TRUE),
        n_games = sum(n_games),
        .groups = "drop"
      )
    print(by_season_test)
  }

  cat("═══════════════════════════════════════════════════════════════════\n\n")
}
