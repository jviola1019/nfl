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
