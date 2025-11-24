# =============================================================================
# Validation Reporting - Phase-Labeled Market Comparison
# =============================================================================
#
# This module generates reports that clearly label TRAIN/VALIDATION/TEST phases
# to avoid confusion when comparing model vs market performance.
#
# =============================================================================

#' Generate comprehensive validation report with phase labels
#'
#' @param results Output from run_full_validation()
#' @param output_file Optional path for HTML output
#' @return List of formatted tables
#'
#' @examples
#' \dontrun{
#'   results <- run_full_validation(sched = sched)
#'   report <- generate_validation_report(results)
#'   print(report$summary_table)
#' }
generate_validation_report <- function(results, output_file = NULL) {

  # =============================================================================
  # TABLE 1: Hyperparameter Selection Summary
  # =============================================================================

  hyperparam_table <- tibble::tibble(
    Parameter        = names(results$tuning$best_params),
    `Selected Value` = unlist(results$tuning$best_params),
    `Selection Window` = sprintf("%d-%d (TRAIN)",
                                   results$schema$tune$start_season,
                                   results$schema$tune$end_season),
    `Selection Metric` = results$tuning$selection_metric
  )

  # =============================================================================
  # TABLE 2: Performance by Phase (Out-of-Sample)
  # =============================================================================

  performance_table <- tibble::tibble(
    Phase = c("TRAIN (tuning)", "VALIDATION (held-out)", "TEST (forward)"),
    Window = c(
      sprintf("%d-%d", results$schema$tune$start_season,
              results$schema$tune$end_season),
      sprintf("%d-%d", results$schema$valid$start_season,
              results$schema$valid$end_season),
      sprintf("%d-%d", results$schema$test$start_season,
              results$schema$test$end_season)
    ),
    `N Games` = c(
      results$tuning$tuning_results$n_games[1],
      results$validation$scores$overall$n_games,
      results$test$scores$overall$n_games
    ),
    `Brier (uncal)` = c(
      NA_real_,  # Not stored for tuning
      results$validation$scores$overall$Brier2_raw,
      results$test$scores$overall$Brier2_raw
    ),
    `Brier (cal)` = c(
      results$tuning$tuning_results$brier2_mean[1],
      results$validation$scores$overall$Brier2_cal,
      results$test$scores$overall$Brier2_cal
    ),
    `LogLoss (cal)` = c(
      results$tuning$tuning_results$logloss2_mean[1],
      results$validation$scores$overall$LogLoss2_cal,
      results$test$scores$overall$LogLoss2_cal
    ),
    `Brier Improvement` = c(
      NA_real_,
      100 * (results$validation$scores$overall$Brier2_raw -
             results$validation$scores$overall$Brier2_cal) /
        results$validation$scores$overall$Brier2_raw,
      100 * (results$test$scores$overall$Brier2_raw -
             results$test$scores$overall$Brier2_cal) /
        results$test$scores$overall$Brier2_raw
    )
  )

  # =============================================================================
  # TABLE 3: Model vs Market Comparison (by Phase)
  # =============================================================================

  market_comp_table <- NULL

  if (!is.null(results$validation$market_comp) &&
      !is.null(results$test$market_comp)) {

    # Extract validation market comparison
    valid_mc <- results$validation$market_comp$overall
    test_mc  <- results$test$market_comp$overall

    market_comp_table <- tibble::tibble(
      Phase = c("VALIDATION", "TEST"),
      Window = c(
        sprintf("%d-%d", results$schema$valid$start_season,
                results$schema$valid$end_season),
        sprintf("%d-%d", results$schema$test$start_season,
                results$schema$test$end_season)
      ),
      `Model Brier` = c(valid_mc$model_brier, test_mc$model_brier),
      `Market Brier` = c(valid_mc$market_brier, test_mc$market_brier),
      `Delta (Model - Market)` = c(valid_mc$brier_delta, test_mc$brier_delta),
      `Model LogLoss` = c(valid_mc$model_logloss, test_mc$model_logloss),
      `Market LogLoss` = c(valid_mc$market_logloss, test_mc$market_logloss),
      `N Games` = c(valid_mc$n_games, test_mc$n_games)
    )

    # Add bootstrap CIs if available
    if ("paired_stats" %in% names(results$validation$market_comp)) {
      if ("Brier" %in% names(results$validation$market_comp$paired_stats)) {
        valid_ci <- results$validation$market_comp$paired_stats$Brier
        market_comp_table$`Brier CI (95%)` <- c(
          sprintf("[%+.4f, %+.4f]", valid_ci["lo"], valid_ci["hi"]),
          NA_character_  # Will add test CI if available
        )
        market_comp_table$`p-value` <- c(
          sprintf("%.4f", valid_ci["p_value"]),
          NA_character_
        )
      }
    }

    if ("paired_stats" %in% names(results$test$market_comp)) {
      if ("Brier" %in% names(results$test$market_comp$paired_stats)) {
        test_ci <- results$test$market_comp$paired_stats$Brier
        if ("Brier CI (95%)" %in% names(market_comp_table)) {
          market_comp_table$`Brier CI (95%)`[2] <-
            sprintf("[%+.4f, %+.4f]", test_ci["lo"], test_ci["hi"])
          market_comp_table$`p-value`[2] <- sprintf("%.4f", test_ci["p_value"])
        }
      }
    }
  }

  # =============================================================================
  # TABLE 4: Per-Season Performance (Phase-Labeled)
  # =============================================================================

  # Validation by season
  valid_by_season <- results$validation$scores$by_week %>%
    dplyr::group_by(season) %>%
    dplyr::summarise(
      phase = "VALIDATION",
      brier2_cal = weighted.mean(Brier2_cal, n_games, na.rm = TRUE),
      logloss2_cal = weighted.mean(LogLoss2_cal, n_games, na.rm = TRUE),
      n_games = sum(n_games),
      .groups = "drop"
    )

  # Test by season
  test_by_season <- results$test$scores$by_week %>%
    dplyr::group_by(season) %>%
    dplyr::summarise(
      phase = "TEST",
      brier2_cal = weighted.mean(Brier2_cal, n_games, na.rm = TRUE),
      logloss2_cal = weighted.mean(LogLoss2_cal, n_games, na.rm = TRUE),
      n_games = sum(n_games),
      .groups = "drop"
    )

  by_season_table <- dplyr::bind_rows(valid_by_season, test_by_season) %>%
    dplyr::arrange(season) %>%
    dplyr::select(phase, season, brier2_cal, logloss2_cal, n_games)

  # =============================================================================
  # Compile Report
  # =============================================================================

  report <- list(
    hyperparameters   = hyperparam_table,
    performance       = performance_table,
    market_comparison = market_comp_table,
    by_season         = by_season_table,
    metadata = list(
      run_date         = results$run_date,
      best_params      = results$tuning$best_params,
      train_window     = results$schema$tune,
      validation_window = results$schema$valid,
      test_window      = results$schema$test
    )
  )

  # Print formatted report to console
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat("  VALIDATION REPORT\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat(sprintf("  Generated: %s\n", format(results$run_date)))
  cat("═══════════════════════════════════════════════════════════════════\n\n")

  cat("TABLE 1: Selected Hyperparameters\n")
  cat("───────────────────────────────────────────────────────────────────\n")
  print(hyperparam_table, n = Inf)

  cat("\n\nTABLE 2: Performance by Phase (Out-of-Sample)\n")
  cat("───────────────────────────────────────────────────────────────────\n")
  cat("⚠️  IMPORTANT: VALIDATION and TEST use frozen hyperparameters\n")
  cat("             and calibration from TRAIN phase\n")
  cat("───────────────────────────────────────────────────────────────────\n")
  print(performance_table, n = Inf)

  if (!is.null(market_comp_table)) {
    cat("\n\nTABLE 3: Model vs Market (by Phase)\n")
    cat("───────────────────────────────────────────────────────────────────\n")
    cat("⚠️  Positive delta = model worse than market\n")
    cat("   Negative delta = model better than market\n")
    cat("───────────────────────────────────────────────────────────────────\n")
    print(market_comp_table, n = Inf)
  }

  cat("\n\nTABLE 4: Per-Season Performance (Phase-Labeled)\n")
  cat("───────────────────────────────────────────────────────────────────\n")
  print(by_season_table, n = Inf)

  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")

  # Save as HTML if requested
  if (!is.null(output_file)) {
    save_validation_html(report, output_file)
    cat(sprintf("✓ HTML report saved to: %s\n\n", output_file))
  }

  invisible(report)
}


#' Save validation report as HTML
#'
#' @param report Output from generate_validation_report()
#' @param output_file Path for HTML output
save_validation_html <- function(report, output_file) {

  # Simple HTML generation
  html <- sprintf("
<!DOCTYPE html>
<html>
<head>
  <title>NFL Model Validation Report</title>
  <style>
    body {
      font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      max-width: 1200px;
      margin: 40px auto;
      padding: 20px;
      background-color: #f5f5f5;
    }
    h1 {
      color: #2c3e50;
      border-bottom: 3px solid #3498db;
      padding-bottom: 10px;
    }
    h2 {
      color: #34495e;
      margin-top: 30px;
      border-left: 4px solid #3498db;
      padding-left: 10px;
    }
    table {
      width: 100%%;
      border-collapse: collapse;
      margin: 20px 0;
      background-color: white;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    th {
      background-color: #3498db;
      color: white;
      padding: 12px;
      text-align: left;
      font-weight: 600;
    }
    td {
      padding: 10px 12px;
      border-bottom: 1px solid #ecf0f1;
    }
    tr:hover {
      background-color: #f8f9fa;
    }
    .phase {
      font-weight: 600;
      color: #3498db;
    }
    .warning {
      background-color: #fff3cd;
      border: 1px solid #ffc107;
      padding: 15px;
      margin: 20px 0;
      border-radius: 4px;
    }
    .metadata {
      background-color: #e8f4f8;
      padding: 15px;
      margin: 20px 0;
      border-radius: 4px;
      font-size: 0.9em;
    }
  </style>
</head>
<body>
  <h1>NFL Model Validation Report</h1>
  <div class='metadata'>
    <strong>Generated:</strong> %s<br>
    <strong>Training Window:</strong> %d-%d<br>
    <strong>Validation Window:</strong> %d-%d<br>
    <strong>Test Window:</strong> %d-%d
  </div>

  <h2>Selected Hyperparameters</h2>
  %s

  <h2>Performance by Phase</h2>
  <div class='warning'>
    ⚠️ <strong>Important:</strong> VALIDATION and TEST phases use frozen hyperparameters
    and calibration from the TRAIN phase to ensure proper out-of-sample evaluation.
  </div>
  %s

  <h2>Model vs Market Comparison</h2>
  <div class='warning'>
    ⚠️ <strong>Delta interpretation:</strong> Positive = model worse than market,
    Negative = model better than market
  </div>
  %s

  <h2>Per-Season Performance</h2>
  %s

  <hr>
  <p style='text-align: center; color: #7f8c8d; font-size: 0.9em;'>
    Generated by NFL Model Validation Pipeline
  </p>
</body>
</html>
",
  format(report$metadata$run_date),
  report$metadata$train_window$start_season,
  report$metadata$train_window$end_season,
  report$metadata$validation_window$start_season,
  report$metadata$validation_window$end_season,
  report$metadata$test_window$start_season,
  report$metadata$test_window$end_season,
  knitr::kable(report$hyperparameters, format = "html"),
  knitr::kable(report$performance, format = "html", digits = 4),
  if (!is.null(report$market_comparison)) {
    knitr::kable(report$market_comparison, format = "html", digits = 4)
  } else {
    "<p><em>Market comparison not available</em></p>"
  },
  knitr::kable(report$by_season, format = "html", digits = 4)
  )

  writeLines(html, output_file)
}


#' Quick comparison of tuning grid results
#'
#' @param tuning_results Output from tune_hyperparams()$tuning_results
#' @param top_n Number of top configurations to show (default: 10)
#' @return Invisibly returns formatted table
plot_tuning_grid <- function(tuning_results, top_n = 10) {

  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n")
  cat(sprintf("  TOP %d HYPERPARAMETER CONFIGURATIONS\n", top_n))
  cat("═══════════════════════════════════════════════════════════════════\n\n")

  # Top configs by Brier
  cat("Ranked by 2-way Brier Score (calibrated):\n")
  cat("───────────────────────────────────────────────────────────────────\n")

  top_configs <- tuning_results %>%
    dplyr::arrange(brier2_mean) %>%
    dplyr::select(config_id, GLMM_BLEND_W, RECENCY_HALFLIFE, N_RECENT,
                  SOS_STRENGTH, brier2_mean, logloss2_mean) %>%
    head(top_n)

  print(top_configs, n = Inf)

  cat("\n")
  cat("═══════════════════════════════════════════════════════════════════\n\n")

  invisible(top_configs)
}
