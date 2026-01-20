#!/usr/bin/env Rscript
# =============================================================================
# NFL Prediction Model - Requirements Verification Script
# =============================================================================
# This script verifies all 20 issues from the audit are properly addressed.
# Run with: Rscript scripts/verify_requirements.R
# =============================================================================

cat("\n")
cat("=================================================================\n")
cat("  NFL PREDICTION MODEL - REQUIREMENTS VERIFICATION\n")
cat("=================================================================\n\n")

# Track results
results <- list(
  passed = character(0),
  failed = character(0),
  skipped = character(0)
)

pass <- function(test_name) {
  cat(sprintf("  [PASS] %s\n", test_name))
  results$passed <<- c(results$passed, test_name)
}

fail <- function(test_name, reason = "") {
  msg <- if (nzchar(reason)) sprintf("%s: %s", test_name, reason) else test_name
  cat(sprintf("  [FAIL] %s\n", msg))
  results$failed <<- c(results$failed, test_name)
}

skip <- function(test_name, reason = "") {
  msg <- if (nzchar(reason)) sprintf("%s: %s", test_name, reason) else test_name
  cat(sprintf("  [SKIP] %s\n", msg))
  results$skipped <<- c(results$skipped, test_name)
}

# =============================================================================
# ISSUE 1: R/utils.R exists and contains canonical functions
# =============================================================================
cat("--- Issue 1: Canonical utils.R module ---\n")
if (file.exists("R/utils.R")) {
  source("R/utils.R")

  # Check required functions exist
  required_fns <- c(
    "american_to_probability", "american_to_decimal", "probability_to_american",
    "clamp_probability", "expected_value_units", "conservative_kelly_stake",
    "shrink_probability_toward_market", "brier_score", "log_loss", "accuracy",
    "standardize_join_keys", "safe_typed_join", "classify_edge_magnitude"
  )

  missing_fns <- required_fns[!sapply(required_fns, function(f) exists(f, mode = "function"))]

  if (length(missing_fns) == 0) {
    pass("All canonical utility functions defined")
  } else {
    fail("Missing utility functions", paste(missing_fns, collapse = ", "))
  }
} else {
  fail("R/utils.R file not found")
}

# =============================================================================
# ISSUE 2: Probability clamping with epsilon
# =============================================================================
cat("\n--- Issue 2: Probability clamping ---\n")
if (exists("clamp_probability", mode = "function")) {
  eps <- 1e-9

  # Test boundary clamping
  if (clamp_probability(0) >= eps && clamp_probability(1) <= (1 - eps)) {
    pass("clamp_probability enforces epsilon boundaries")
  } else {
    fail("clamp_probability boundary enforcement")
  }

  # Test NA handling
  if (is.na(clamp_probability(NA))) {
    pass("clamp_probability handles NA")
  } else {
    fail("clamp_probability NA handling")
  }
} else {
  skip("Probability clamping", "clamp_probability not defined")
}

# =============================================================================
# ISSUE 3: Odds conversion functions
# =============================================================================
cat("\n--- Issue 3: Odds conversion ---\n")
if (exists("american_to_probability", mode = "function")) {
  # Test standard conversions
  p_minus110 <- american_to_probability(-110)
  p_plus100 <- american_to_probability(100)

  if (abs(p_minus110 - 0.524) < 0.01 && abs(p_plus100 - 0.5) < 0.01) {
    pass("american_to_probability correct calculations")
  } else {
    fail("american_to_probability calculations")
  }

  # Test edge cases
  if (is.na(american_to_probability(0)) && is.na(american_to_probability(NA))) {
    pass("american_to_probability edge cases")
  } else {
    fail("american_to_probability edge case handling")
  }
} else {
  skip("Odds conversion", "american_to_probability not defined")
}

# =============================================================================
# ISSUE 4: Expected value calculation
# =============================================================================
cat("\n--- Issue 4: Expected value calculation ---\n")
if (exists("expected_value_units", mode = "function")) {
  # Fair bet at even odds
  ev_fair <- expected_value_units(0.5, 100)
  if (abs(ev_fair) < 0.01) {
    pass("expected_value_units fair bet = 0 EV")
  } else {
    fail("expected_value_units fair bet", sprintf("got %.4f, expected 0", ev_fair))
  }

  # +EV bet
  ev_positive <- expected_value_units(0.55, 100)
  if (ev_positive > 0) {
    pass("expected_value_units positive EV detection")
  } else {
    fail("expected_value_units positive EV")
  }
} else {
  skip("Expected value", "expected_value_units not defined")
}

# =============================================================================
# ISSUE 5: Kelly stake calculation
# =============================================================================
cat("\n--- Issue 5: Kelly stake calculation ---\n")
if (exists("conservative_kelly_stake", mode = "function")) {
  stake <- conservative_kelly_stake(0.8, -110)

  # Should be capped at max_stake (default 0.02)
  if (!is.na(stake) && stake <= 0.02 && stake >= 0) {
    pass("conservative_kelly_stake bounded correctly")
  } else {
    fail("conservative_kelly_stake bounds", sprintf("got %s", stake))
  }

  # Low probability should give minimal/zero stake
  stake_low <- conservative_kelly_stake(0.3, -110)
  if (is.na(stake_low) || stake_low == 0) {
    pass("conservative_kelly_stake handles low probability")
  } else {
    fail("conservative_kelly_stake low probability")
  }
} else {
  skip("Kelly stake", "conservative_kelly_stake not defined")
}

# =============================================================================
# ISSUE 6: Market shrinkage function
# =============================================================================
cat("\n--- Issue 6: Market shrinkage ---\n")
if (exists("shrink_probability_toward_market", mode = "function")) {
  # Test 60% shrinkage (default)
  shrunk <- shrink_probability_toward_market(0.7, 0.5, shrinkage = 0.6)
  expected <- 0.4 * 0.7 + 0.6 * 0.5  # = 0.58

  if (abs(shrunk - expected) < 0.001) {
    pass("shrink_probability_toward_market calculation")
  } else {
    fail("shrink_probability_toward_market", sprintf("got %.4f, expected %.4f", shrunk, expected))
  }
} else {
  skip("Market shrinkage", "shrink_probability_toward_market not defined")
}

# =============================================================================
# ISSUE 7: Validation metrics
# =============================================================================
cat("\n--- Issue 7: Validation metrics ---\n")
if (exists("brier_score", mode = "function") && exists("log_loss", mode = "function")) {
  # Perfect predictions
  bs_perfect <- brier_score(c(1, 0), c(1, 0))
  if (bs_perfect == 0) {
    pass("brier_score perfect predictions = 0")
  } else {
    fail("brier_score perfect predictions")
  }

  # 50% predictions
  bs_half <- brier_score(c(0.5, 0.5), c(1, 0))
  if (abs(bs_half - 0.25) < 0.001) {
    pass("brier_score 50% predictions = 0.25")
  } else {
    fail("brier_score 50% predictions")
  }

  # Log loss ordering
  ll_good <- log_loss(c(0.9, 0.1), c(1, 0))
  ll_bad <- log_loss(c(0.5, 0.5), c(1, 0))
  if (ll_good < ll_bad) {
    pass("log_loss better predictions have lower loss")
  } else {
    fail("log_loss ordering")
  }
} else {
  skip("Validation metrics", "brier_score or log_loss not defined")
}

# =============================================================================
# ISSUE 8: Edge classification
# =============================================================================
cat("\n--- Issue 8: Edge classification ---\n")
if (exists("classify_edge_magnitude", mode = "function")) {
  classifications <- c(
    classify_edge_magnitude(-0.05) == "negative",
    classify_edge_magnitude(0.03) == "realistic",
    classify_edge_magnitude(0.07) == "optimistic",
    classify_edge_magnitude(0.12) == "suspicious",
    classify_edge_magnitude(0.20) == "implausible"
  )

  if (all(classifications)) {
    pass("classify_edge_magnitude correct categories")
  } else {
    fail("classify_edge_magnitude categories")
  }
} else {
  skip("Edge classification", "classify_edge_magnitude not defined")
}

# =============================================================================
# ISSUE 9: Type-safe join keys
# =============================================================================
cat("\n--- Issue 9: Type-safe join keys ---\n")
if (exists("standardize_join_keys", mode = "function")) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    skip("Type-safe joins", "tibble package not available")
  } else {
    test_df <- tibble::tibble(
      gameid = "2024_01_KC_DEN",
      seasonYear = "2024",
      wk = "1"
    )

    result <- standardize_join_keys(test_df)

    # Check renaming
    if ("game_id" %in% names(result) && "season" %in% names(result) && "week" %in% names(result)) {
      pass("standardize_join_keys renames aliases")
    } else {
      fail("standardize_join_keys renaming")
    }

    # Check type coercion
    if (is.character(result$game_id) && is.integer(result$season) && is.integer(result$week)) {
      pass("standardize_join_keys type coercion")
    } else {
      fail("standardize_join_keys type coercion")
    }
  }
} else {
  skip("Type-safe joins", "standardize_join_keys not defined")
}

# =============================================================================
# ISSUE 10: Duplicate key handling
# =============================================================================
cat("\n--- Issue 10: Duplicate key handling ---\n")
if (exists("collapse_by_keys_relaxed", mode = "function") && requireNamespace("tibble", quietly = TRUE)) {
  test_df <- tibble::tibble(
    game_id = c("A", "A", "B"),
    season = c(2024L, 2024L, 2024L),
    value = c(1, NA, 3)
  )

  result <- suppressMessages(collapse_by_keys_relaxed(test_df, c("game_id", "season"), "test"))

  if (nrow(result) == 2 && result$value[result$game_id == "A"] == 1) {
    pass("collapse_by_keys_relaxed handles duplicates")
  } else {
    fail("collapse_by_keys_relaxed duplicate handling")
  }
} else {
  skip("Duplicate key handling", "collapse_by_keys_relaxed not defined or tibble unavailable")
}

# =============================================================================
# ISSUE 11: Unique key enforcement
# =============================================================================
cat("\n--- Issue 11: Unique key enforcement ---\n")
if (exists("ensure_unique_join_keys", mode = "function") && requireNamespace("tibble", quietly = TRUE)) {
  test_df <- tibble::tibble(
    game_id = c("A", "A", "B"),
    season = c(2024L, 2024L, 2024L),
    value = c(1, 2, 3)
  )

  result <- suppressMessages(ensure_unique_join_keys(test_df, c("game_id", "season"), "test"))

  if (nrow(result) == 2) {
    pass("ensure_unique_join_keys removes duplicates")
  } else {
    fail("ensure_unique_join_keys duplicate removal")
  }
} else {
  skip("Unique key enforcement", "ensure_unique_join_keys not defined or tibble unavailable")
}

# =============================================================================
# ISSUE 12: Data validation module exists
# =============================================================================
cat("\n--- Issue 12: Data validation module ---\n")
if (file.exists("R/data_validation.R")) {
  source("R/data_validation.R")

  validation_fns <- c(
    "reset_data_quality", "get_data_quality",
    "update_injury_quality", "update_weather_quality",
    "update_market_quality", "update_calibration_quality",
    "compute_overall_quality", "generate_quality_badge_html"
  )

  missing_fns <- validation_fns[!sapply(validation_fns, function(f) exists(f, mode = "function"))]

  if (length(missing_fns) == 0) {
    pass("Data validation functions defined")
  } else {
    fail("Missing validation functions", paste(missing_fns, collapse = ", "))
  }
} else {
  fail("R/data_validation.R not found")
}

# =============================================================================
# ISSUE 13: Data quality tracking
# =============================================================================
cat("\n--- Issue 13: Data quality tracking ---\n")
if (exists("reset_data_quality", mode = "function")) {
  reset_data_quality()
  quality <- get_data_quality()

  if (quality$injury_status == "unknown" && quality$weather_status == "unknown") {
    pass("reset_data_quality initializes tracking")
  } else {
    fail("reset_data_quality initialization")
  }

  # Test update functions
  update_injury_quality("complete")
  quality <- get_data_quality()
  if (quality$injury_status == "complete") {
    pass("update_injury_quality tracking works")
  } else {
    fail("update_injury_quality tracking")
  }
} else {
  skip("Data quality tracking", "reset_data_quality not defined")
}

# =============================================================================
# ISSUE 14: Overall quality computation
# =============================================================================
cat("\n--- Issue 14: Overall quality computation ---\n")
if (exists("compute_overall_quality", mode = "function")) {
  reset_data_quality()
  update_injury_quality("complete")
  update_weather_quality("api")
  update_market_quality("complete")
  update_calibration_quality("isotonic_nested_cv", leakage_free = TRUE)

  overall <- compute_overall_quality()
  if (overall == "high") {
    pass("compute_overall_quality returns 'high' for complete data")
  } else {
    fail("compute_overall_quality high rating", sprintf("got '%s'", overall))
  }

  # Test degraded quality
  reset_data_quality()
  update_injury_quality("missing", seasons_missing = c("2024", "2025"))
  update_weather_quality("default")
  update_market_quality("missing")
  update_calibration_quality("none")

  overall <- compute_overall_quality()
  if (overall == "low") {
    pass("compute_overall_quality returns 'low' for degraded data")
  } else {
    fail("compute_overall_quality low rating")
  }
} else {
  skip("Overall quality computation", "compute_overall_quality not defined")
}

# =============================================================================
# ISSUE 15: HTML badge generation
# =============================================================================
cat("\n--- Issue 15: HTML badge generation ---\n")
if (exists("generate_quality_badge_html", mode = "function")) {
  reset_data_quality()
  update_injury_quality("complete")
  update_weather_quality("api")
  update_market_quality("complete")
  update_calibration_quality("isotonic_nested_cv", leakage_free = TRUE)

  html <- generate_quality_badge_html()

  if (grepl("<section", html) && grepl("Data Quality", html, ignore.case = TRUE)) {
    pass("generate_quality_badge_html produces valid HTML")
  } else {
    fail("generate_quality_badge_html HTML structure")
  }
} else {
  skip("HTML badge generation", "generate_quality_badge_html not defined")
}

# =============================================================================
# ISSUE 16: Weather fallback defaults
# =============================================================================
cat("\n--- Issue 16: Weather fallback defaults ---\n")
if (exists("get_default_weather_conditions", mode = "function")) {
  defaults <- get_default_weather_conditions(c("game1", "game2"))

  if (nrow(defaults) == 2 && "weather_source" %in% names(defaults)) {
    pass("get_default_weather_conditions returns proper structure")
  } else {
    fail("get_default_weather_conditions structure")
  }

  if (all(defaults$weather_source == "default")) {
    pass("get_default_weather_conditions marks source as default")
  } else {
    fail("get_default_weather_conditions source marking")
  }
} else {
  skip("Weather fallback defaults", "get_default_weather_conditions not defined")
}

# =============================================================================
# ISSUE 17: Injury availability check
# =============================================================================
cat("\n--- Issue 17: Injury availability check ---\n")
if (exists("check_injury_availability", mode = "function")) {
  result <- check_injury_availability(2023)

  if (is.list(result) && "available" %in% names(result) && "season" %in% names(result)) {
    pass("check_injury_availability returns proper structure")
  } else {
    fail("check_injury_availability structure")
  }
} else {
  skip("Injury availability", "check_injury_availability not defined")
}

# =============================================================================
# ISSUE 18: config.R exists with key parameters
# =============================================================================
cat("\n--- Issue 18: Configuration parameters ---\n")
if (file.exists("config.R")) {
  source("config.R")

  required_params <- c("SHRINKAGE", "KELLY_FRACTION", "N_TRIALS", "SEED")
  missing_params <- required_params[!sapply(required_params, exists)]

  if (length(missing_params) == 0) {
    pass("All required config parameters defined")
  } else {
    fail("Missing config parameters", paste(missing_params, collapse = ", "))
  }

  # Verify sensible defaults
  if (exists("SHRINKAGE") && SHRINKAGE >= 0.4 && SHRINKAGE <= 0.8) {
    pass("SHRINKAGE in sensible range (0.4-0.8)")
  } else {
    fail("SHRINKAGE range")
  }
} else {
  fail("config.R not found")
}

# =============================================================================
# ISSUE 19: Test suite exists
# =============================================================================
cat("\n--- Issue 19: Test suite ---\n")
test_files <- c(
  "tests/testthat/test-utils.R",
  "tests/testthat/test-data-validation.R"
)

existing_tests <- test_files[file.exists(test_files)]
missing_tests <- test_files[!file.exists(test_files)]

if (length(missing_tests) == 0) {
  pass("All test files exist")
} else {
  fail("Missing test files", paste(missing_tests, collapse = ", "))
}

# =============================================================================
# ISSUE 20: Documentation files
# =============================================================================
cat("\n--- Issue 20: Documentation ---\n")
doc_files <- c("CLAUDE.md", "AUDIT.md", "README.md")
existing_docs <- doc_files[file.exists(doc_files)]
missing_docs <- doc_files[!file.exists(doc_files)]

if (length(missing_docs) == 0) {
  pass("All documentation files exist")
} else {
  fail("Missing documentation", paste(missing_docs, collapse = ", "))
}

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n")
cat("=================================================================\n")
cat("  VERIFICATION SUMMARY\n")
cat("=================================================================\n")
cat(sprintf("  PASSED:  %d\n", length(results$passed)))
cat(sprintf("  FAILED:  %d\n", length(results$failed)))
cat(sprintf("  SKIPPED: %d\n", length(results$skipped)))
cat("=================================================================\n")

if (length(results$failed) > 0) {
  cat("\nFailed tests:\n")
  for (test in results$failed) {
    cat(sprintf("  - %s\n", test))
  }
}

if (length(results$skipped) > 0) {
  cat("\nSkipped tests:\n")
  for (test in results$skipped) {
    cat(sprintf("  - %s\n", test))
  }
}

# Exit with appropriate code
if (length(results$failed) > 0) {
  cat("\n[VERIFICATION FAILED]\n")
  quit(status = 1)
} else {
  cat("\n[VERIFICATION PASSED]\n")
  quit(status = 0)
}
