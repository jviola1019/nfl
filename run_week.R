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

  # =============================================================================
  # CORRELATED PLAYER PROPS (if enabled in config.R)
  # =============================================================================
  if (exists("RUN_PLAYER_PROPS") && isTRUE(RUN_PLAYER_PROPS)) {
    cat("\n")
    cat("=================================================================\n")
    cat("  RUNNING CORRELATED PLAYER PROPS\n")
    cat("=================================================================\n")

    props_path <- file.path(getwd(), "R", "correlated_props.R")
    if (file.exists(props_path)) {
      source(props_path)

      # Run correlated props using exported simulation results
      if (exists("run_correlated_props", mode = "function") &&
          exists("last_simulation_results") &&
          length(last_simulation_results) > 0) {

        props_results <- tryCatch({
          run_correlated_props(
            game_sim_results = last_simulation_results,
            schedule_data = if (exists("schedule_for_props")) schedule_for_props else NULL,
            prop_types = if (exists("PROP_TYPES")) PROP_TYPES else c("passing", "rushing", "receiving", "td"),
            season = SEASON
          )
        }, error = function(e) {
          message(sprintf("Player props generation failed: %s", e$message))
          NULL
        })

        if (!is.null(props_results) && nrow(props_results) > 0) {
          # Export props results for HTML report
          assign("props_results", props_results, envir = .GlobalEnv)

          # Show quick summary
          positive_ev <- props_results %>%
            dplyr::filter(recommendation != "PASS") %>%
            nrow()

          cat(sprintf("  Generated %d player props (%d with positive EV)\n",
                      nrow(props_results), positive_ev))

          # Regenerate HTML report to include player props
          if (exists("export_moneyline_comparison_html", mode = "function") &&
              exists("moneyline_report_inputs") && !is.null(moneyline_report_inputs)) {
            cat("  Regenerating HTML report with player props...\n")
            tryCatch({
              # Rebuild comparison table if needed
              if (exists("build_moneyline_comparison_table", mode = "function") &&
                  !is.null(moneyline_report_inputs$comparison)) {
                report_tbl <- build_moneyline_comparison_table(
                  market_comparison_result = moneyline_report_inputs$comparison,
                  enriched_schedule = if (exists("sched")) sched else NULL,
                  join_keys = if (!is.null(moneyline_report_inputs$join_keys))
                                moneyline_report_inputs$join_keys else c("game_id", "season", "week"),
                  vig = 0.10,
                  verbose = FALSE
                )
                if (nrow(report_tbl) > 0) {
                  export_moneyline_comparison_html(
                    comparison_tbl = report_tbl,
                    file = file.path(getwd(), "NFLvsmarket_report.html"),
                    title = sprintf("NFL Week %d Analysis (Season %d) - With Player Props", WEEK_TO_SIM, SEASON),
                    verbose = FALSE,
                    auto_open = FALSE
                  )
                  cat("  HTML report updated with player props.\n")
                }
              }
            }, error = function(e) {
              message(sprintf("  Could not regenerate HTML: %s", e$message))
            })
          }
        } else {
          message("No player props were generated (possibly missing player data)")
        }
      } else {
        message("Simulation results not available for props correlation")
      }
    } else {
      message("correlated_props.R not found; skipping player props")
    }
  }

  cat("\n")
  cat("=================================================================\n")
  cat("  ANALYSIS COMPLETE\n")
  cat("=================================================================\n")
  cat("  Check the run_logs/ folder for output files.\n")
  cat("  HTML report should open automatically if running interactively.\n")
  if (exists("props_results") && !is.null(props_results)) {
    cat("  Player props included in analysis.\n")
  }
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

