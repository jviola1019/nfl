#!/usr/bin/env Rscript
# =============================================================================
# NFL Week Analysis Runner
# =============================================================================
#
# Usage:
#   Rscript run_week.R [week] [season]
#
# Examples:
#   Rscript run_week.R          # Uses defaults from config.R
#   Rscript run_week.R 15       # Week 15, current season
#   Rscript run_week.R 15 2024  # Week 15, 2024 season
#
# =============================================================================

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Source configuration first
source("config.R")

# Override with command line args if provided
if (length(args) >= 1) {
  WEEK_TO_SIM <- as.integer(args[1])
  if (is.na(WEEK_TO_SIM) || WEEK_TO_SIM < 1 || WEEK_TO_SIM > 22) {
    stop("Invalid week number. Must be 1-22.")
  }
  assign("WEEK_TO_SIM", WEEK_TO_SIM, envir = .GlobalEnv)
  message(sprintf("Week override: %d", WEEK_TO_SIM))
}

if (length(args) >= 2) {
  SEASON <- as.integer(args[2])
  if (is.na(SEASON) || SEASON < 2011 || SEASON > 2030) {
    stop("Invalid season. Must be 2011-2030.")
  }
  assign("SEASON", SEASON, envir = .GlobalEnv)
  message(sprintf("Season override: %d", SEASON))
}

# Print run info
cat("\n")
cat("=================================================================\n")
cat("  NFL WEEK ANALYSIS\n")
cat("=================================================================\n")
cat(sprintf("  Season: %d | Week: %d\n", SEASON, WEEK_TO_SIM))
cat(sprintf("  Trials: %s | Seed: %d\n", format(N_TRIALS, big.mark = ","), SEED))
cat("=================================================================\n\n")

# Check for required packages
required_pkgs <- c("tidyverse", "nflreadr", "gt")
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  cat("Installing missing packages:", paste(missing_pkgs, collapse = ", "), "\n")

  # Check if renv is active
  if (file.exists("renv.lock") && requireNamespace("renv", quietly = TRUE)) {
    message("Using renv for package management...")
    renv::restore()
  } else {
    install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
  }
}

# Load main packages
suppressPackageStartupMessages({
  library(tidyverse)
})

# Source the analysis code
message("Loading NFLmarket.R...")
source("NFLmarket.R")

# Run the analysis
message("\n--- Running Market Comparison Analysis ---\n")

# Generate output filename
output_file <- sprintf("NFLvsmarket_week%02d_%d.html", WEEK_TO_SIM, SEASON)

tryCatch({
  # Build comparison table
  comparison <- build_moneyline_comparison(
    season = SEASON,
    week = WEEK_TO_SIM,
    verbose = VERBOSE
  )

  if (nrow(comparison) == 0) {
    warning(sprintf("No games found for Week %d of %d season.", WEEK_TO_SIM, SEASON))
  } else {
    message(sprintf("Found %d games for analysis.", nrow(comparison)))

    # Export HTML report
    export_moneyline_comparison_html(
      comparison_tbl = comparison,
      file = output_file,
      title = sprintf("NFL Week %d (%d) - Blend vs Market Analysis", WEEK_TO_SIM, SEASON),
      verbose = VERBOSE,
      auto_open = interactive(),
      season = SEASON,
      week = WEEK_TO_SIM
    )

    cat("\n")
    cat("=================================================================\n")
    cat("  ANALYSIS COMPLETE\n")
    cat("=================================================================\n")
    cat(sprintf("  Report saved to: %s\n", output_file))
    cat("=================================================================\n")
  }

}, error = function(e) {
  cat("\n")
  cat("=================================================================\n")
  cat("  ERROR DURING ANALYSIS\n")
  cat("=================================================================\n")
  cat(sprintf("  %s\n", conditionMessage(e)))
  cat("=================================================================\n")

  # Print stack trace if available
  if (!is.null(e$call)) {
    cat("\nCall stack:\n")
    print(sys.calls())
  }

  quit(status = 1)
})

# Print summary statistics if available
if (exists("comparison") && nrow(comparison) > 0) {
  cat("\n--- Quick Summary ---\n")

  # Count recommendations
  rec_counts <- table(comparison$blend_recommendation, useNA = "ifany")
  cat("\nRecommendations:\n")
  print(rec_counts)

  # EV summary for bet recommendations
  bet_rows <- comparison$blend_ev_units > 0
  if (any(bet_rows, na.rm = TRUE)) {
    cat("\nPositive EV Bets:\n")
    cat(sprintf("  Count: %d\n", sum(bet_rows, na.rm = TRUE)))
    cat(sprintf("  Mean EV: %.2f%%\n", mean(comparison$blend_ev_units[bet_rows], na.rm = TRUE) * 100))
    cat(sprintf("  Total potential EV: %.4f units\n",
                sum(comparison$blend_confidence[bet_rows] * comparison$blend_ev_units[bet_rows], na.rm = TRUE)))
  }
}

cat("\nDone.\n")
