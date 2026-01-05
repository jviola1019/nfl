# =============================================================================
# NFL Prediction Model - Data Validation Module
# =============================================================================
# Centralized data validation with graceful fallbacks and clear error messages.
# =============================================================================

#' Required columns for schedule data
SCHEDULE_REQUIRED_COLS <- c("game_id", "season", "week", "home_team", "away_team")

#' Required columns for simulation results
SIMULATION_REQUIRED_COLS <- c("game_id", "season", "week", "home_win_prob")

#' Validate data frame has required columns
#' @param df Data frame to validate
#' @param required Vector of required column names
#' @param label Label for error messages
#' @param strict If TRUE, stop on missing columns; if FALSE, return FALSE
#' @return TRUE if valid, FALSE or stops if invalid
validate_required_columns <- function(df, required, label = "data", strict = TRUE) {
  if (is.null(df)) {
    if (strict) stop(sprintf("%s is NULL", label))
    return(FALSE)
  }

  if (!inherits(df, "data.frame")) {
    if (strict) stop(sprintf("%s is not a data frame (got %s)", label, class(df)[1]))
    return(FALSE)
  }

  if (nrow(df) == 0) {
    if (strict) stop(sprintf("%s has zero rows", label))
    return(FALSE)
  }

  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    msg <- sprintf("%s is missing required columns: %s\n  Available: %s",
                   label,
                   paste(missing, collapse = ", "),
                   paste(head(names(df), 20), collapse = ", "))
    if (strict) stop(msg)
    warning(msg)
    return(FALSE)
  }

  TRUE
}

#' Validate numeric column
#' @param x Vector to validate
#' @param name Column name for messages
#' @param min_val Minimum allowed value (NULL = no minimum)
#' @param max_val Maximum allowed value (NULL = no maximum)
#' @param allow_na Allow NA values
#' @return TRUE if valid
validate_numeric_column <- function(x, name, min_val = NULL, max_val = NULL, allow_na = TRUE) {
  if (!is.numeric(x)) {
    stop(sprintf("Column '%s' must be numeric (got %s)", name, typeof(x)))
  }

  non_na <- x[!is.na(x)]

  if (!allow_na && any(is.na(x))) {
    stop(sprintf("Column '%s' contains %d NA values", name, sum(is.na(x))))
  }

  if (!is.null(min_val) && any(non_na < min_val)) {
    n_below <- sum(non_na < min_val)
    stop(sprintf("Column '%s' has %d values below minimum %s", name, n_below, min_val))
  }

  if (!is.null(max_val) && any(non_na > max_val)) {
    n_above <- sum(non_na > max_val)
    stop(sprintf("Column '%s' has %d values above maximum %s", name, n_above, max_val))
  }

  TRUE
}

#' Validate probability column
#' @param x Vector to validate
#' @param name Column name for messages
#' @param allow_na Allow NA values
#' @return TRUE if valid
validate_probability <- function(x, name, allow_na = TRUE) {
  validate_numeric_column(x, name, min_val = 0, max_val = 1, allow_na = allow_na)
}

#' Validate American odds
#' @param x Vector to validate
#' @param name Column name for messages
#' @return TRUE if valid (excludes zero)
validate_american_odds <- function(x, name) {
  x <- suppressWarnings(as.numeric(x))
  non_na <- x[!is.na(x)]

  if (any(non_na == 0)) {
    stop(sprintf("Column '%s' contains invalid zero odds", name))
  }

  TRUE
}

#' Load data with graceful fallback
#' @param load_fn Function to load data
#' @param fallback_fn Fallback function if load_fn fails
#' @param label Data label for messages
#' @return Loaded data or fallback
load_with_fallback <- function(load_fn, fallback_fn = NULL, label = "data") {
  result <- tryCatch({
    load_fn()
  }, error = function(e) {
    warning(sprintf("Failed to load %s: %s", label, conditionMessage(e)))
    if (!is.null(fallback_fn)) {
      message(sprintf("Using fallback for %s", label))
      return(fallback_fn())
    }
    return(NULL)
  })

  if (is.null(result)) {
    warning(sprintf("%s is unavailable and no fallback provided", label))
  }

  result
}

#' Validate injury data availability
#' @param season Season year
#' @param week Week number
#' @return List with available (TRUE/FALSE), data, and message
validate_injury_data <- function(season, week) {
  result <- list(
    available = FALSE,
    data = NULL,
    message = NULL,
    source = NULL
  )

  # Try nflreadr first
  tryCatch({
    if (requireNamespace("nflreadr", quietly = TRUE)) {
      injuries <- nflreadr::load_injuries(season)
      if (!is.null(injuries) && nrow(injuries) > 0) {
        result$available <- TRUE
        result$data <- injuries
        result$source <- "nflreadr"
        result$message <- sprintf("Loaded %d injury records for %d from nflreadr",
                                  nrow(injuries), season)
        return(result)
      }
    }
  }, error = function(e) {
    result$message <- sprintf("nflreadr injury load failed: %s", conditionMessage(e))
  })

  # Check for cached data
  cache_file <- file.path("~/.cache/nfl_sim_injuries", sprintf("injuries_%d.rds", season))
  if (file.exists(cache_file)) {
    tryCatch({
      cached <- readRDS(cache_file)
      if (!is.null(cached) && nrow(cached) > 0) {
        result$available <- TRUE
        result$data <- cached
        result$source <- "cache"
        result$message <- sprintf("Loaded %d cached injury records for %d", nrow(cached), season)
        return(result)
      }
    }, error = function(e) {
      # Cache read failed, continue
    })
  }

  # No data available
  result$message <- sprintf(
    "Injury data unavailable for %d week %d. Model will proceed without injury adjustments.\n  To fix: Ensure nflreadr is updated and network is available.",
    season, week
  )

  result
}

#' Validate weather data with fallback
#' @param game_ids Vector of game IDs
#' @param default_conditions Default weather conditions
#' @return Data frame with weather for each game
validate_weather_data <- function(game_ids, default_conditions = NULL) {
  if (is.null(default_conditions)) {
    default_conditions <- list(
      temp_f = 55,
      wind_mph = 8,
      dome = FALSE,
      precip_prob = 0.1
    )
  }

  # Create default weather for all games
  weather <- tibble::tibble(
    game_id = game_ids,
    temp_f = default_conditions$temp_f,
    wind_mph = default_conditions$wind_mph,
    dome = default_conditions$dome,
    precip_prob = default_conditions$precip_prob,
    weather_source = "default"
  )

  # Try to get actual weather (would implement API call here)
  # For now, just return defaults with warning
  n_games <- length(game_ids)
  if (n_games > 0) {
    warning(sprintf(
      "Using default weather conditions for %d games. Real weather data not implemented.",
      n_games
    ))
  }

  weather
}

#' Create validation report
#' @param schedule Schedule data frame
#' @param predictions Predictions data frame (optional)
#' @param injuries Injury data frame (optional)
#' @return List with validation results
create_validation_report <- function(schedule, predictions = NULL, injuries = NULL) {
  report <- list(
    timestamp = Sys.time(),
    schedule = list(),
    predictions = list(),
    injuries = list(),
    issues = character(),
    warnings = character()
  )

  # Validate schedule
  if (!is.null(schedule)) {
    report$schedule$rows <- nrow(schedule)
    report$schedule$columns <- names(schedule)
    report$schedule$seasons <- if ("season" %in% names(schedule)) unique(schedule$season) else NULL
    report$schedule$weeks <- if ("week" %in% names(schedule)) unique(schedule$week) else NULL

    missing_req <- setdiff(SCHEDULE_REQUIRED_COLS, names(schedule))
    if (length(missing_req) > 0) {
      report$issues <- c(report$issues,
                         sprintf("Schedule missing: %s", paste(missing_req, collapse = ", ")))
    }
  } else {
    report$issues <- c(report$issues, "Schedule data is NULL")
  }

  # Validate predictions
  if (!is.null(predictions)) {
    report$predictions$rows <- nrow(predictions)
    report$predictions$columns <- names(predictions)

    # Check probability range
    prob_cols <- grep("prob", names(predictions), value = TRUE, ignore.case = TRUE)
    for (col in prob_cols) {
      vals <- predictions[[col]]
      if (any(vals < 0 | vals > 1, na.rm = TRUE)) {
        report$issues <- c(report$issues,
                           sprintf("Prediction column '%s' has values outside [0,1]", col))
      }
    }
  }

  # Validate injuries
  if (!is.null(injuries)) {
    report$injuries$rows <- nrow(injuries)
    report$injuries$available <- TRUE
  } else {
    report$injuries$available <- FALSE
    report$warnings <- c(report$warnings, "Injury data unavailable")
  }

  report
}

#' Print validation report
#' @param report Validation report from create_validation_report
print_validation_report <- function(report) {
  cat("\n=== Data Validation Report ===\n")
  cat(sprintf("Timestamp: %s\n\n", report$timestamp))

  cat("Schedule:\n")
  cat(sprintf("  Rows: %d\n", report$schedule$rows %||% 0))
  cat(sprintf("  Seasons: %s\n", paste(report$schedule$seasons, collapse = ", ")))
  cat(sprintf("  Weeks: %s\n", paste(report$schedule$weeks, collapse = ", ")))

  if (length(report$issues) > 0) {
    cat("\nISSUES:\n")
    for (issue in report$issues) {
      cat(sprintf("  [ERROR] %s\n", issue))
    }
  }

  if (length(report$warnings) > 0) {
    cat("\nWARNINGS:\n")
    for (warn in report$warnings) {
      cat(sprintf("  [WARN] %s\n", warn))
    }
  }

  if (length(report$issues) == 0 && length(report$warnings) == 0) {
    cat("\nAll validations passed.\n")
  }

  cat("\n")
  invisible(report)
}

# =============================================================================
# DATA QUALITY GATE - Global Quality Tracking
# =============================================================================

#' Global data quality state
#' @description Tracks data quality across all components for transparency
DATA_QUALITY <- new.env(parent = emptyenv())
DATA_QUALITY$injury_status <- "unknown"
DATA_QUALITY$injury_seasons_missing <- character(0)
DATA_QUALITY$weather_status <- "unknown"
DATA_QUALITY$weather_games_fallback <- character(0)
DATA_QUALITY$market_status <- "unknown"
DATA_QUALITY$market_games_missing <- character(0)
DATA_QUALITY$schedule_status <- "unknown"
DATA_QUALITY$calibration_method <- "unknown"
DATA_QUALITY$calibration_leakage_free <- FALSE
DATA_QUALITY$timestamp <- Sys.time()

#' Reset data quality tracking
reset_data_quality <- function() {
  DATA_QUALITY$injury_status <- "unknown"
  DATA_QUALITY$injury_seasons_missing <- character(0)
  DATA_QUALITY$weather_status <- "unknown"
  DATA_QUALITY$weather_games_fallback <- character(0)
  DATA_QUALITY$market_status <- "unknown"
  DATA_QUALITY$market_games_missing <- character(0)
  DATA_QUALITY$schedule_status <- "unknown"
  DATA_QUALITY$calibration_method <- "unknown"
  DATA_QUALITY$calibration_leakage_free <- FALSE
  DATA_QUALITY$timestamp <- Sys.time()
  invisible(NULL)
}

#' Update injury quality status
#' @param status One of: "full", "partial", "unavailable"
#' @param missing_seasons Vector of seasons with missing data
update_injury_quality <- function(status, missing_seasons = character(0)) {
  valid_statuses <- c("full", "partial", "unavailable")
  if (!status %in% valid_statuses) {
    warning(sprintf("Invalid injury status '%s'; using 'unknown'", status))
    status <- "unknown"
  }
  DATA_QUALITY$injury_status <- status
  DATA_QUALITY$injury_seasons_missing <- as.character(missing_seasons)
  DATA_QUALITY$timestamp <- Sys.time()
  invisible(NULL)
}

#' Update weather quality status
#' @param status One of: "full", "partial_fallback", "all_fallback"
#' @param fallback_games Vector of game_ids using fallback weather
update_weather_quality <- function(status, fallback_games = character(0)) {
  valid_statuses <- c("full", "partial_fallback", "all_fallback")
  if (!status %in% valid_statuses) {
    warning(sprintf("Invalid weather status '%s'; using 'unknown'", status))
    status <- "unknown"
  }
  DATA_QUALITY$weather_status <- status
  DATA_QUALITY$weather_games_fallback <- as.character(fallback_games)
  DATA_QUALITY$timestamp <- Sys.time()
  invisible(NULL)
}

#' Update market data quality status
#' @param status One of: "full", "partial", "unavailable"
#' @param missing_games Vector of game_ids without market data
update_market_quality <- function(status, missing_games = character(0)) {
  valid_statuses <- c("full", "partial", "unavailable")
  if (!status %in% valid_statuses) {
    warning(sprintf("Invalid market status '%s'; using 'unknown'", status))
    status <- "unknown"
  }
  DATA_QUALITY$market_status <- status
  DATA_QUALITY$market_games_missing <- as.character(missing_games)
  DATA_QUALITY$timestamp <- Sys.time()
  invisible(NULL)
}

#' Update calibration status
#' @param method Calibration method used
#' @param leakage_free TRUE if calibration is leakage-free
update_calibration_quality <- function(method, leakage_free = FALSE) {
  DATA_QUALITY$calibration_method <- as.character(method)
  DATA_QUALITY$calibration_leakage_free <- isTRUE(leakage_free)
  DATA_QUALITY$timestamp <- Sys.time()
  invisible(NULL)
}

#' Get current data quality summary
#' @return List with all quality indicators
get_data_quality <- function() {
  list(
    timestamp = DATA_QUALITY$timestamp,
    injury = list(
      status = DATA_QUALITY$injury_status,
      missing_seasons = DATA_QUALITY$injury_seasons_missing
    ),
    weather = list(
      status = DATA_QUALITY$weather_status,
      fallback_games = DATA_QUALITY$weather_games_fallback
    ),
    market = list(
      status = DATA_QUALITY$market_status,
      missing_games = DATA_QUALITY$market_games_missing
    ),
    calibration = list(
      method = DATA_QUALITY$calibration_method,
      leakage_free = DATA_QUALITY$calibration_leakage_free
    ),
    overall = compute_overall_quality()
  )
}

#' Compute overall quality grade
#' @return String: "HIGH", "MEDIUM", "LOW", "CRITICAL"
compute_overall_quality <- function() {
  issues <- 0
  critical <- FALSE

  # Check injury status
  if (DATA_QUALITY$injury_status == "unavailable") {
    issues <- issues + 2
  } else if (DATA_QUALITY$injury_status == "partial") {
    issues <- issues + 1
  }

  # Check weather status
  if (DATA_QUALITY$weather_status == "all_fallback") {
    issues <- issues + 1
  }

  # Check market status
  if (DATA_QUALITY$market_status == "unavailable") {
    issues <- issues + 2
    critical <- TRUE
  } else if (DATA_QUALITY$market_status == "partial") {
    issues <- issues + 1
  }

  # Check calibration
  if (!DATA_QUALITY$calibration_leakage_free) {
    issues <- issues + 1
  }

  if (critical) return("CRITICAL")
  if (issues >= 4) return("LOW")
  if (issues >= 2) return("MEDIUM")
  return("HIGH")
}

#' Print data quality summary to console
print_data_quality_summary <- function() {
  quality <- get_data_quality()

  cat("\n")
  cat("╔════════════════════════════════════════════════════════════════════╗\n")
  cat("║                    DATA QUALITY SUMMARY                            ║\n")
  cat("╚════════════════════════════════════════════════════════════════════╝\n")
  cat(sprintf("  Overall Quality: %s\n", quality$overall))
  cat(sprintf("  Timestamp: %s\n\n", format(quality$timestamp, "%Y-%m-%d %H:%M:%S")))

  # Injury status
  injury_icon <- switch(quality$injury$status,
    "full" = "✓",
    "partial" = "⚠",
    "unavailable" = "✗",
    "?"
  )
  cat(sprintf("  [%s] Injuries: %s\n", injury_icon, toupper(quality$injury$status)))
  if (length(quality$injury$missing_seasons) > 0) {
    cat(sprintf("      Missing seasons: %s\n", paste(quality$injury$missing_seasons, collapse = ", ")))
  }

  # Weather status
  weather_icon <- switch(quality$weather$status,
    "full" = "✓",
    "partial_fallback" = "⚠",
    "all_fallback" = "✗",
    "?"
  )
  cat(sprintf("  [%s] Weather: %s\n", weather_icon, toupper(quality$weather$status)))
  if (length(quality$weather$fallback_games) > 0) {
    n_fallback <- length(quality$weather$fallback_games)
    cat(sprintf("      %d game(s) using default weather\n", n_fallback))
  }

  # Market status
  market_icon <- switch(quality$market$status,
    "full" = "✓",
    "partial" = "⚠",
    "unavailable" = "✗",
    "?"
  )
  cat(sprintf("  [%s] Market Data: %s\n", market_icon, toupper(quality$market$status)))
  if (length(quality$market$missing_games) > 0) {
    n_missing <- length(quality$market$missing_games)
    cat(sprintf("      %d game(s) without market odds\n", n_missing))
  }

  # Calibration status
  calib_icon <- if (quality$calibration$leakage_free) "✓" else "⚠"
  cat(sprintf("  [%s] Calibration: %s (%s)\n",
    calib_icon,
    quality$calibration$method,
    if (quality$calibration$leakage_free) "leakage-free" else "DIAGNOSTIC ONLY"
  ))

  cat("\n")

  # Warnings for critical issues
  if (quality$overall %in% c("LOW", "CRITICAL")) {
    cat("  ⚠ WARNING: Data quality issues may affect prediction reliability.\n")
    cat("    Review the above issues before using predictions for decisions.\n\n")
  }

  invisible(quality)
}

#' Generate HTML data quality badge
#' @return HTML string for embedding in reports
generate_quality_badge_html <- function() {
  quality <- get_data_quality()

  badge_color <- switch(quality$overall,
    "HIGH" = "#28a745",
    "MEDIUM" = "#ffc107",
    "LOW" = "#fd7e14",
    "CRITICAL" = "#dc3545",
    "#6c757d"
  )

  issues <- character(0)
  if (quality$injury$status != "full") {
    issues <- c(issues, sprintf("Injury data: %s", quality$injury$status))
  }
  if (quality$weather$status != "full") {
    issues <- c(issues, sprintf("Weather: %s", quality$weather$status))
  }
  if (quality$market$status != "full") {
    issues <- c(issues, sprintf("Market data: %s", quality$market$status))
  }
  if (!quality$calibration$leakage_free) {
    issues <- c(issues, "Calibration: not leakage-free")
  }

  issues_html <- if (length(issues) > 0) {
    sprintf("<ul style='margin:5px 0;padding-left:20px;'><li>%s</li></ul>",
            paste(issues, collapse = "</li><li>"))
  } else {
    "<p style='margin:5px 0;'>All data sources complete.</p>"
  }

  sprintf(
    "<div style='border:2px solid %s;border-radius:8px;padding:10px;margin:10px 0;background:%s20;'>
      <strong style='color:%s;'>Data Quality: %s</strong>
      %s
    </div>",
    badge_color, badge_color, badge_color, quality$overall, issues_html
  )
}
