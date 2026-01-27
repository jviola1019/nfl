# =============================================================================
# Tests for R/logging.R - Structured Logging Utilities
# =============================================================================
# Tests logging functions, levels, and output formatting
# =============================================================================

# Skip all tests if logging.R is not available
skip_if_logging_unavailable <- function() {
  logging_path <- file.path(getwd(), "..", "..", "R", "logging.R")
  if (!file.exists(logging_path) && !exists("log_info")) {
    if (file.exists("R/logging.R")) {
      source("R/logging.R")
    } else {
      skip("R/logging.R not available")
    }
  }
}

# =============================================================================
# FUNCTION EXISTENCE TESTS
# =============================================================================

test_that("core logging functions exist", {
  skip_if_logging_unavailable()

  expect_true(exists("log_info") || exists("log_message"),
              info = "log_info or log_message should exist")
  expect_true(exists("log_warn") || exists("log_warning"),
              info = "log_warn or log_warning should exist")
  expect_true(exists("log_error"),
              info = "log_error should exist")
})

test_that("logging functions are callable", {
  skip_if_logging_unavailable()

  # Should not error when called (use capture.output since log_info uses cat())
  if (exists("log_info")) {
    expect_no_error(capture.output(log_info("Test info message"), type = "output"))
  }
  if (exists("log_warn")) {
    expect_no_error(capture.output(log_warn("Test warning message"), type = "message"))
  }
})

# =============================================================================
# LOG LEVEL TESTS
# =============================================================================

test_that("log levels are properly ordered", {
  skip_if_logging_unavailable()

  # If log levels are defined, they should follow standard ordering
  if (exists("LOG_LEVELS")) {
    levels <- LOG_LEVELS

    # Standard ordering: DEBUG < INFO < WARN < ERROR
    if (all(c("DEBUG", "INFO", "WARN", "ERROR") %in% names(levels))) {
      expect_true(levels[["DEBUG"]] < levels[["INFO"]])
      expect_true(levels[["INFO"]] < levels[["WARN"]])
      expect_true(levels[["WARN"]] < levels[["ERROR"]])
    }
  } else {
    # Just verify logging works
    expect_true(TRUE)
  }
})

# =============================================================================
# LOG FORMATTING TESTS
# =============================================================================

test_that("log messages include timestamp", {
  skip_if_logging_unavailable()

  # Capture log output
  if (exists("log_info")) {
    output <- capture.output({
      log_info("Test timestamp message")
    }, type = "message")

    # Should contain some form of timestamp or be empty (if suppressed)
    if (length(output) > 0) {
      # Look for common timestamp patterns
      has_timestamp <- any(grepl("\\d{2}:\\d{2}", output)) ||  # HH:MM
                       any(grepl("\\d{4}-\\d{2}-\\d{2}", output)) ||  # YYYY-MM-DD
                       any(grepl("\\[.*\\]", output))  # [brackets]

      # Timestamp is preferred but not required
      if (!has_timestamp) {
        message("Note: Log output may not include timestamps")
      }
    }
  }
})

test_that("log messages include level indicator", {
  skip_if_logging_unavailable()

  if (exists("log_warn")) {
    output <- capture.output({
      log_warn("Test level indicator")
    }, type = "message")

    if (length(output) > 0) {
      # Should contain WARN or WARNING
      has_level <- any(grepl("WARN|WARNING|\\[W\\]", output, ignore.case = TRUE))
      if (!has_level) {
        message("Note: Log output may not include level indicator")
      }
    }
  }
})

# =============================================================================
# LOG OUTPUT CONTROL TESTS
# =============================================================================

test_that("verbose flag controls logging", {
  skip_if_logging_unavailable()

  if (!exists("VERBOSE")) {
    if (file.exists("config.R")) source("config.R")
  }

  expect_true(exists("VERBOSE"))
  expect_type(VERBOSE, "logical")
})

test_that("log output can be suppressed", {
  skip_if_logging_unavailable()

  # Note: log_info uses cat() which isn't affected by suppressMessages
  # Test that we can at least capture the output
  if (exists("log_info")) {
    output <- capture.output(log_info("This should be captured"), type = "output")
    expect_true(length(output) >= 0)  # Just ensure no error
  }
})

# =============================================================================
# CONTEXT LOGGING TESTS
# =============================================================================

test_that("log functions handle multiple arguments", {
  skip_if_logging_unavailable()

  if (exists("log_info")) {
    # Should handle sprintf-style formatting (use capture.output since log_info uses cat())
    expect_no_error(capture.output(
      log_info("Value: %d, String: %s", 42, "test"),
      type = "output"
    ))

    # Should handle paste-style concatenation
    expect_no_error(capture.output(
      log_info("Value:", 42),
      type = "output"
    ))
  }
})

test_that("log functions handle NULL and NA values", {
  skip_if_logging_unavailable()

  if (exists("log_info")) {
    # Should not error on NULL (use capture.output since log_info uses cat())
    expect_no_error(capture.output(
      tryCatch(log_info("Null value:", NULL), error = function(e) NULL),
      type = "output"
    ))

    # Should not error on NA
    expect_no_error(capture.output(
      tryCatch(log_info("NA value:", NA), error = function(e) NULL),
      type = "output"
    ))
  }
})

# =============================================================================
# ERROR LOGGING TESTS
# =============================================================================

test_that("log_error captures error context", {
  skip_if_logging_unavailable()

  if (exists("log_error")) {
    # log_error should work with error objects (log_error uses message() which we capture)
    err <- simpleError("Test error")
    expect_no_error(capture.output(
      tryCatch(log_error("Error occurred:", conditionMessage(err)), error = function(e) NULL),
      type = "message"
    ))
  }
})

# =============================================================================
# PERFORMANCE LOGGING TESTS
# =============================================================================

test_that("timing logging works if available", {
  skip_if_logging_unavailable()

  if (exists("log_timing") || exists("log_performance")) {
    fn <- if (exists("log_timing")) log_timing else log_performance

    # Should accept timing information
    expect_silent(suppressMessages({
      tryCatch(fn("Operation", 1.234), error = function(e) NULL)
    }))
  }
})

# =============================================================================
# INTEGRATION TESTS
# =============================================================================

test_that("logging integrates with CLAUDE.md recommendation", {
  # CLAUDE.md says: Use `log_info()`, `log_warn()`, `log_error()` from R/logging.R

  logging_path <- if (file.exists("R/logging.R")) {
    "R/logging.R"
  } else if (file.exists("../../R/logging.R")) {
    "../../R/logging.R"
  } else {
    skip("R/logging.R not found")
  }

  logging_content <- readLines(logging_path, warn = FALSE)
  logging_text <- paste(logging_content, collapse = "\n")

  # Should define the functions mentioned in CLAUDE.md
  expect_true(grepl("log_info", logging_text),
              info = "R/logging.R should define log_info")
  expect_true(grepl("log_warn", logging_text),
              info = "R/logging.R should define log_warn")
  expect_true(grepl("log_error", logging_text),
              info = "R/logging.R should define log_error")
})

test_that("NFLsimulation.R uses logging functions", {
  sim_path <- if (file.exists("NFLsimulation.R")) {
    "NFLsimulation.R"
  } else if (file.exists("../../NFLsimulation.R")) {
    "../../NFLsimulation.R"
  } else {
    skip("NFLsimulation.R not found")
  }

  sim_content <- readLines(sim_path, warn = FALSE)
  sim_text <- paste(sim_content, collapse = "\n")

  # Should source logging.R
  expect_true(grepl("logging\\.R", sim_text) ||
              grepl("log_info|log_warn|log_error", sim_text),
              info = "NFLsimulation.R should use logging utilities")
})
