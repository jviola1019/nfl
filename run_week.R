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
# Output:
#   HTML report: NFLvsmarket_week{week}_{season}.html
#
# Debug mode:
#   options(nfl.ev_debug = TRUE)  # Shows EV calculation trace
#
# =============================================================================

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Override config before sourcing if args provided
if (length(args) >= 1) {
  week_arg <- as.integer(args[1])
  if (is.na(week_arg) || week_arg < 1 || week_arg > 22) {
    stop("Invalid week number. Must be 1-22.")
  }
}

if (length(args) >= 2) {
  season_arg <- as.integer(args[2])
  if (is.na(season_arg) || season_arg < 2011 || season_arg > 2030) {
    stop("Invalid season. Must be 2011-2030.")
  }
}

# Source configuration
source("config.R")

# Apply command line overrides AFTER sourcing config
if (length(args) >= 1) {
  WEEK_TO_SIM <- week_arg
  assign("WEEK_TO_SIM", WEEK_TO_SIM, envir = .GlobalEnv)
  message(sprintf("Week override: %d", WEEK_TO_SIM))
}

if (length(args) >= 2) {
  SEASON <- season_arg
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
required_pkgs <- c("tidyverse", "nflreadr")
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

# Run the full simulation which generates the HTML report
message("Running NFLsimulation.R...")
message("This will run Monte Carlo simulations and generate an HTML report.\n")

tryCatch({
  source("NFLsimulation.R")

  cat("\n")
  cat("=================================================================\n")
  cat("  ANALYSIS COMPLETE\n")
  cat("=================================================================\n")
  cat("  Check the run_logs/ folder for output files.\n")
  cat("  HTML report should open automatically if running interactively.\n")
  cat("=================================================================\n")

}, error = function(e) {
  cat("\n")
  cat("=================================================================\n")
  cat("  ERROR DURING ANALYSIS\n")
  cat("=================================================================\n")
  cat(sprintf("  %s\n", conditionMessage(e)))
  cat("=================================================================\n")
  quit(status = 1)
})

cat("\nDone.\n")
