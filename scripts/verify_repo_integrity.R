#!/usr/bin/env Rscript
# =============================================================================
# NFL Prediction Model - Repository Integrity Verification
# =============================================================================
# Validates schema contracts, logical invariants, and data integrity.
# Run with: Rscript scripts/verify_repo_integrity.R
# =============================================================================

cat("\n")
cat("=================================================================\n")
cat("  NFL PREDICTION MODEL - REPOSITORY INTEGRITY CHECK\n")
cat("=================================================================\n\n")

# Track results
results <- list(
  schema_pass = 0,
  schema_fail = 0,
  invariant_pass = 0,
  invariant_fail = 0,
  issues = character(0)
)

pass <- function(category, test_name) {
  cat(sprintf("  [PASS] %s\n", test_name))
  if (category == "schema") {
    results$schema_pass <<- results$schema_pass + 1
  } else {
    results$invariant_pass <<- results$invariant_pass + 1
  }
}

fail <- function(category, test_name, reason = "") {
  msg <- if (nzchar(reason)) sprintf("%s: %s", test_name, reason) else test_name
  cat(sprintf("  [FAIL] %s\n", msg))
  results$issues <<- c(results$issues, msg)
  if (category == "schema") {
    results$schema_fail <<- results$schema_fail + 1
  } else {
    results$invariant_fail <<- results$invariant_fail + 1
  }
}

# =============================================================================
# SCHEMA VALIDATION: Required Files
# =============================================================================

cat("--- Schema: Required Files ---\n")

required_files <- c(
  "config.R",
  "run_week.R",
  "NFLsimulation.R",
  "NFLmarket.R",
  "NFLbrier_logloss.R",
  "R/utils.R",
  "R/data_validation.R",
  "R/logging.R",
  "R/playoffs.R",
  "R/date_resolver.R",
  "DESCRIPTION",
  "README.md",
  "CLAUDE.md"
)

for (f in required_files) {
  if (file.exists(f)) {
    pass("schema", sprintf("File exists: %s", f))
  } else {
    fail("schema", sprintf("Missing required file: %s", f))
  }
}

# =============================================================================
# SCHEMA VALIDATION: R File Syntax
# =============================================================================

cat("\n--- Schema: R Syntax Validation ---\n")

r_files <- c(
  list.files("R", pattern = "\\.R$", full.names = TRUE),
  list.files(".", pattern = "\\.R$", full.names = TRUE),
  list.files("scripts", pattern = "\\.R$", full.names = TRUE),
  list.files("validation", pattern = "\\.R$", full.names = TRUE)
)

r_files <- unique(r_files[file.exists(r_files)])

syntax_errors <- 0
for (f in r_files) {
  tryCatch({
    parse(file = f)
  }, error = function(e) {
    fail("schema", sprintf("Syntax error in %s", f), conditionMessage(e))
    syntax_errors <<- syntax_errors + 1
  })
}

if (syntax_errors == 0) {
  pass("schema", sprintf("All %d R files parse successfully", length(r_files)))
}

# =============================================================================
# SCHEMA VALIDATION: Test Infrastructure
# =============================================================================

cat("\n--- Schema: Test Infrastructure ---\n")

if (file.exists("tests/testthat/setup.R")) {
  pass("schema", "Test setup.R exists")
} else {
  fail("schema", "Missing tests/testthat/setup.R")
}

test_files <- list.files("tests/testthat", pattern = "^test-.*\\.R$", full.names = TRUE)
if (length(test_files) >= 4) {
  pass("schema", sprintf("%d test files found", length(test_files)))
} else {
  fail("schema", "Expected at least 4 test files", sprintf("found %d", length(test_files)))
}

# =============================================================================
# INVARIANT: Configuration Parameters
# =============================================================================

cat("\n--- Invariant: Configuration Parameters ---\n")

tryCatch({
  source("config.R")

  # Required parameters
  required_params <- c("SEASON", "WEEK_TO_SIM", "N_TRIALS", "SEED", "SHRINKAGE", "KELLY_FRACTION")

  for (param in required_params) {
    if (exists(param)) {
      pass("invariant", sprintf("Config: %s defined", param))
    } else {
      fail("invariant", sprintf("Config: %s not defined", param))
    }
  }

  # Value constraints
  if (exists("SHRINKAGE") && SHRINKAGE >= 0 && SHRINKAGE <= 1) {
    pass("invariant", "SHRINKAGE in valid range [0, 1]")
  } else {
    fail("invariant", "SHRINKAGE out of range")
  }

  if (exists("KELLY_FRACTION") && KELLY_FRACTION > 0 && KELLY_FRACTION <= 0.5) {
    pass("invariant", "KELLY_FRACTION in valid range (0, 0.5]")
  } else {
    fail("invariant", "KELLY_FRACTION out of range")
  }

  if (exists("N_TRIALS") && N_TRIALS >= 1000) {
    pass("invariant", sprintf("N_TRIALS = %s (sufficient)", format(N_TRIALS, big.mark = ",")))
  } else {
    fail("invariant", "N_TRIALS too low (< 1000)")
  }

}, error = function(e) {
  fail("invariant", "Failed to source config.R", conditionMessage(e))
})

# =============================================================================
# INVARIANT: Utility Functions
# =============================================================================

cat("\n--- Invariant: Utility Function Contracts ---\n")

tryCatch({
  source("R/utils.R")

  # Test probability clamping
  if (clamp_probability(0) > 0 && clamp_probability(1) < 1) {
    pass("invariant", "clamp_probability enforces epsilon boundaries")
  } else {
    fail("invariant", "clamp_probability boundary failure")
  }

  # Test odds conversion roundtrip
  test_odds <- c(-200, -110, 100, 150, 300)
  roundtrip_ok <- TRUE
  for (odds in test_odds) {
    prob <- american_to_probability(odds)
    dec <- american_to_decimal(odds)
    if (is.na(prob) || is.na(dec) || prob <= 0 || prob >= 1 || dec <= 1) {
      roundtrip_ok <- FALSE
      break
    }
  }
  if (roundtrip_ok) {
    pass("invariant", "Odds conversion functions work correctly")
  } else {
    fail("invariant", "Odds conversion failure")
  }

  # Test EV calculation (fair bet = 0)
  ev_fair <- expected_value_units(0.5, 100)
  if (abs(ev_fair) < 0.01) {
    pass("invariant", "EV calculation: fair bet = 0 EV")
  } else {
    fail("invariant", "EV calculation: fair bet != 0", sprintf("got %.4f", ev_fair))
  }

  # Test shrinkage
  shrunk <- shrink_probability_toward_market(0.7, 0.5, 0.6)
  expected <- 0.4 * 0.7 + 0.6 * 0.5  # = 0.58
  if (abs(shrunk - expected) < 0.001) {
    pass("invariant", "Shrinkage calculation correct")
  } else {
    fail("invariant", "Shrinkage calculation error")
  }

  # Test Brier score (perfect = 0)
  if (brier_score(c(1, 0), c(1, 0)) == 0) {
    pass("invariant", "Brier score: perfect predictions = 0")
  } else {
    fail("invariant", "Brier score calculation error")
  }

}, error = function(e) {
  fail("invariant", "Failed to load/test R/utils.R", conditionMessage(e))
})

# =============================================================================
# INVARIANT: Data Validation Module
# =============================================================================

cat("\n--- Invariant: Data Validation Module ---\n")

tryCatch({
  source("R/data_validation.R")

  # Test quality tracking (uses nested structure: quality$injury$status)
  reset_data_quality()
  quality <- get_data_quality()

  if (identical(quality$injury$status, "unknown") &&
      identical(quality$market$status, "unknown")) {
    pass("invariant", "Data quality tracking initializes correctly")
  } else {
    fail("invariant", "Data quality tracking initialization")
  }

  # Test quality updates (valid status is "full", not "complete")
  update_injury_quality("full")
  quality <- get_data_quality()
  if (identical(quality$injury$status, "full")) {
    pass("invariant", "Data quality updates work")
  } else {
    fail("invariant", "Data quality update failure")
  }

  # Test overall quality computation
  reset_data_quality()
  update_injury_quality("full")
  update_weather_quality("full")
  update_market_quality("full")
  update_calibration_quality("isotonic_nested_cv", leakage_free = TRUE)

  overall <- compute_overall_quality()
  if (identical(overall, "HIGH")) {
    pass("invariant", "Overall quality computation: 'HIGH' for complete data")
  } else {
    fail("invariant", "Overall quality computation", sprintf("expected 'HIGH', got '%s'", overall))
  }

}, error = function(e) {
  fail("invariant", "Failed to test R/data_validation.R", conditionMessage(e))
})

# =============================================================================
# INVARIANT: No Stale Artifacts
# =============================================================================

cat("\n--- Invariant: No Stale Artifacts ---\n")

# Note: "nul" is a reserved device name on Windows, file.exists("nul") always returns TRUE
# So we check for actual file artifacts, not Windows reserved names
stale_files <- c(".RData", "NFLvsmarket_report.html")
for (f in stale_files) {
  if (!file.exists(f)) {
    pass("invariant", sprintf("No stale artifact: %s", f))
  } else {
    fail("invariant", sprintf("Stale artifact found: %s", f))
  }
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n")
cat("=================================================================\n")
cat("  VERIFICATION SUMMARY\n")
cat("=================================================================\n")
cat(sprintf("  Schema Checks:    %d passed, %d failed\n",
            results$schema_pass, results$schema_fail))
cat(sprintf("  Invariant Checks: %d passed, %d failed\n",
            results$invariant_pass, results$invariant_fail))
cat(sprintf("  Total:            %d passed, %d failed\n",
            results$schema_pass + results$invariant_pass,
            results$schema_fail + results$invariant_fail))
cat("=================================================================\n")

if (length(results$issues) > 0) {
  cat("\nIssues found:\n")
  for (issue in results$issues) {
    cat(sprintf("  - %s\n", issue))
  }
}

# Exit with appropriate code
total_failures <- results$schema_fail + results$invariant_fail
if (total_failures > 0) {
  cat(sprintf("\n[VERIFICATION FAILED: %d issues]\n", total_failures))
  quit(status = 1)
} else {
  cat("\n[VERIFICATION PASSED]\n")
  quit(status = 0)
}
