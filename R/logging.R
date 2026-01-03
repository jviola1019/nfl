# =============================================================================
# NFL Prediction Model - Logging Module
# =============================================================================
# Centralized logging utilities to replace cat() with structured logging.
# Supports log levels, timestamps, and optional file output.
# =============================================================================

#' Log levels
LOG_LEVELS <- list(
  DEBUG = 1L,
  INFO = 2L,
  WARN = 3L,
  ERROR = 4L,
  FATAL = 5L
)

#' Current log level (default: INFO)
.nfl_log_level <- LOG_LEVELS$INFO

#' Log file path (NULL = console only)
.nfl_log_file <- NULL

#' Set logging level
#' @param level One of "DEBUG", "INFO", "WARN", "ERROR", "FATAL"
set_log_level <- function(level) {
  if (is.character(level)) {
    level <- toupper(level)
    if (!level %in% names(LOG_LEVELS)) {
      stop(sprintf("Invalid log level: %s. Use one of: %s",
                   level, paste(names(LOG_LEVELS), collapse = ", ")))
    }
    level <- LOG_LEVELS[[level]]
  }
  assign(".nfl_log_level", level, envir = parent.env(environment()))
  invisible(level)
}

#' Set log file
#' @param path File path or NULL for console only
set_log_file <- function(path) {
  assign(".nfl_log_file", path, envir = parent.env(environment()))
  invisible(path)
}

#' Internal log function
#' @param level Log level integer
#' @param level_name Log level name
#' @param msg Message to log
#' @param ... Additional arguments for sprintf
.log_internal <- function(level, level_name, msg, ...) {
  if (level < .nfl_log_level) return(invisible(NULL))

  # Format message
  if (length(list(...)) > 0) {
    msg <- sprintf(msg, ...)
  }

  # Add timestamp and level
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  formatted <- sprintf("[%s] [%s] %s", timestamp, level_name, msg)

  # Output to console
  if (level >= LOG_LEVELS$WARN) {
    message(formatted)
  } else {
    cat(formatted, "\n", sep = "")
  }

  # Output to file if configured
  if (!is.null(.nfl_log_file)) {
    tryCatch({
      cat(formatted, "\n", file = .nfl_log_file, append = TRUE, sep = "")
    }, error = function(e) {
      warning(sprintf("Failed to write to log file: %s", conditionMessage(e)))
    })
  }

  invisible(formatted)
}

#' Log debug message
#' @param msg Message format string
#' @param ... Arguments for sprintf
log_debug <- function(msg, ...) {
  .log_internal(LOG_LEVELS$DEBUG, "DEBUG", msg, ...)
}

#' Log info message
#' @param msg Message format string
#' @param ... Arguments for sprintf
log_info <- function(msg, ...) {
  .log_internal(LOG_LEVELS$INFO, "INFO", msg, ...)
}

#' Log warning message
#' @param msg Message format string
#' @param ... Arguments for sprintf
log_warn <- function(msg, ...) {
  .log_internal(LOG_LEVELS$WARN, "WARN", msg, ...)
}

#' Log error message
#' @param msg Message format string
#' @param ... Arguments for sprintf
log_error <- function(msg, ...) {
  .log_internal(LOG_LEVELS$ERROR, "ERROR", msg, ...)
}

#' Log fatal message and stop
#' @param msg Message format string
#' @param ... Arguments for sprintf
log_fatal <- function(msg, ...) {
  .log_internal(LOG_LEVELS$FATAL, "FATAL", msg, ...)
  stop(sprintf(msg, ...), call. = FALSE)
}

#' Log a section header
#' @param title Section title
#' @param char Border character
log_section <- function(title, char = "=") {
  border <- paste(rep(char, 70), collapse = "")
  log_info(border)
  log_info(title)
  log_info(border)
}

#' Log progress for iterative operations
#' @param current Current iteration
#' @param total Total iterations
#' @param task Task description
#' @param interval How often to log (default every 10%)
log_progress <- function(current, total, task = "Processing", interval = 0.1) {
  pct <- current / total
  prev_pct <- (current - 1) / total
  threshold <- floor(pct / interval) * interval
  prev_threshold <- floor(prev_pct / interval) * interval

  if (threshold > prev_threshold || current == 1 || current == total) {
    log_info("%s: %d/%d (%.0f%%)", task, current, total, pct * 100)
  }
}

#' Create a timer for performance logging
#' @param name Timer name
#' @return Timer object
create_timer <- function(name) {
  structure(
    list(
      name = name,
      start = Sys.time(),
      checkpoints = list()
    ),
    class = "nfl_timer"
  )
}

#' Add checkpoint to timer
#' @param timer Timer object
#' @param label Checkpoint label
#' @return Updated timer
checkpoint <- function(timer, label) {
  timer$checkpoints[[length(timer$checkpoints) + 1]] <- list(
    label = label,
    time = Sys.time()
  )
  timer
}

#' Log timer results
#' @param timer Timer object
log_timer <- function(timer) {
  total <- as.numeric(difftime(Sys.time(), timer$start, units = "secs"))
  log_info("Timer '%s': %.2f seconds total", timer$name, total)

  if (length(timer$checkpoints) > 0) {
    prev_time <- timer$start
    for (cp in timer$checkpoints) {
      delta <- as.numeric(difftime(cp$time, prev_time, units = "secs"))
      log_debug("  - %s: %.2f seconds", cp$label, delta)
      prev_time <- cp$time
    }
  }
}
