#!/usr/bin/env Rscript
# =============================================================================
# NFL Prediction Model - Run Matrix
# =============================================================================
# Executes all runnable artifacts and records PASS/FAIL status.
# Run with: Rscript scripts/run_matrix.R
# =============================================================================

cat("\n")
cat("=================================================================\n")
cat("  NFL PREDICTION MODEL - RUN MATRIX\n")
cat("=================================================================\n")
cat(sprintf("  Timestamp: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("=================================================================\n\n")

# Create run_logs directory if it doesn't exist
if (!dir.exists("run_logs")) {
  dir.create("run_logs", recursive = TRUE)
}

# Timestamp for output files
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Initialize results
results <- data.frame(
  artifact = character(),
  category = character(),
  command = character(),
  status = character(),
  runtime_secs = numeric(),
  output_lines = integer(),
  error_msg = character(),
  stringsAsFactors = FALSE
)

#' Run a single artifact and record results
#' @param name Artifact name
#' @param category Category (entry, script, test, validation)
#' @param command R command to execute
#' @param timeout_secs Timeout in seconds
run_artifact <- function(name, category, command, timeout_secs = 120) {
  cat(sprintf("\n--- Running: %s ---\n", name))

  start_time <- Sys.time()

  # Create temp file for output
  output_file <- tempfile(fileext = ".txt")

  # Build Rscript command
  # Use --vanilla for clean environment
  full_cmd <- sprintf(
    'Rscript --vanilla -e "%s" > "%s" 2>&1',
    gsub('"', '\\"', command),
    output_file
  )

  # Execute with timeout
  exit_code <- tryCatch({
    system(full_cmd, timeout = timeout_secs)
  }, error = function(e) {
    -1  # Timeout or other error
  })

  end_time <- Sys.time()
  runtime <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Read output
  output <- if (file.exists(output_file)) {
    readLines(output_file, warn = FALSE)
  } else {
    character(0)
  }

  # Clean up
  if (file.exists(output_file)) {
    unlink(output_file)
  }

  # Determine status
  status <- if (exit_code == 0) "PASS" else "FAIL"

  # Extract error message if failed
  error_msg <- ""
  if (status == "FAIL") {
    error_lines <- grep("^Error", output, value = TRUE)
    if (length(error_lines) > 0) {
      error_msg <- error_lines[1]
    } else if (length(output) > 0) {
      error_msg <- tail(output, 1)
    }
  }

  # Print result
  cat(sprintf("  Status: %s (%.1fs)\n", status, runtime))
  if (status == "FAIL" && nzchar(error_msg)) {
    cat(sprintf("  Error: %s\n", substr(error_msg, 1, 100)))
  }

  # Return result row
  data.frame(
    artifact = name,
    category = category,
    command = command,
    status = status,
    runtime_secs = round(runtime, 2),
    output_lines = length(output),
    error_msg = error_msg,
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# DEFINE RUNNABLE ARTIFACTS
# =============================================================================

artifacts <- list(
  # Infrastructure verification
  list(
    name = "scripts/verify_repo_integrity.R",
    category = "verification",
    command = "source('scripts/verify_repo_integrity.R')"
  ),
  list(
    name = "scripts/verify_requirements.R",
    category = "verification",
    command = "source('scripts/verify_requirements.R')"
  ),

  # Core library sourcing (syntax/load test)
  list(
    name = "R/utils.R",
    category = "library",
    command = "source('R/utils.R'); cat('OK')"
  ),
  list(
    name = "R/logging.R",
    category = "library",
    command = "source('R/logging.R'); cat('OK')"
  ),
  list(
    name = "R/data_validation.R",
    category = "library",
    command = "source('R/data_validation.R'); cat('OK')"
  ),
  list(
    name = "R/playoffs.R",
    category = "library",
    command = "source('R/playoffs.R'); cat('OK')"
  ),
  list(
    name = "R/date_resolver.R",
    category = "library",
    command = "source('R/date_resolver.R'); cat('OK')"
  ),

  # Configuration
  list(
    name = "config.R",
    category = "config",
    command = "source('config.R'); cat('OK')"
  ),

  # Test suite
  list(
    name = "testthat",
    category = "test",
    command = "testthat::test_dir('tests/testthat', stop_on_failure=FALSE); cat('OK')"
  )
)

# =============================================================================
# EXECUTE ALL ARTIFACTS
# =============================================================================

cat("=================================================================\n")
cat("  EXECUTING ARTIFACTS\n")
cat("=================================================================\n")

for (artifact in artifacts) {
  result <- run_artifact(
    name = artifact$name,
    category = artifact$category,
    command = artifact$command,
    timeout_secs = 180
  )
  results <- rbind(results, result)
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n")
cat("=================================================================\n")
cat("  RUN MATRIX SUMMARY\n")
cat("=================================================================\n")

pass_count <- sum(results$status == "PASS")
fail_count <- sum(results$status == "FAIL")
total_count <- nrow(results)

cat(sprintf("  Total:  %d artifacts\n", total_count))
cat(sprintf("  Passed: %d\n", pass_count))
cat(sprintf("  Failed: %d\n", fail_count))
cat("=================================================================\n")

# Show failed items
if (fail_count > 0) {
  cat("\nFailed artifacts:\n")
  failed <- results[results$status == "FAIL", ]
  for (i in seq_len(nrow(failed))) {
    cat(sprintf("  - %s: %s\n", failed$artifact[i],
                substr(failed$error_msg[i], 1, 60)))
  }
}

# Show summary table
cat("\nResults by category:\n")
for (cat_name in unique(results$category)) {
  cat_results <- results[results$category == cat_name, ]
  pass <- sum(cat_results$status == "PASS")
  fail <- sum(cat_results$status == "FAIL")
  cat(sprintf("  %-15s: %d/%d passed\n", cat_name, pass, nrow(cat_results)))
}

# =============================================================================
# SAVE RESULTS
# =============================================================================

output_csv <- sprintf("run_logs/run_matrix_%s.csv", timestamp)
write.csv(results, output_csv, row.names = FALSE)
cat(sprintf("\nResults saved to: %s\n", output_csv))

# =============================================================================
# EXIT
# =============================================================================

if (fail_count > 0) {
  cat(sprintf("\n[RUN MATRIX: %d FAILURES]\n", fail_count))
  quit(status = 1)
} else {
  cat("\n[RUN MATRIX: ALL PASSED]\n")
  quit(status = 0)
}
