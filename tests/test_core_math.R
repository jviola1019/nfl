# =============================================================================
# test_core_math.R - Unit Tests for Core Math Functions
# =============================================================================
# Run with: Rscript tests/test_core_math.R
# Or: source("tests/test_core_math.R") in an R session
# =============================================================================

suppressPackageStartupMessages({
  library(testthat)
})

# Source the functions to test (assumes running from repo root)
if (!exists("american_to_probability", mode = "function")) {
  source("NFLmarket.R")
}

# =============================================================================
# TEST SUITE: american_to_probability()
# =============================================================================
test_that("american_to_probability handles negative odds correctly", {
  # -110 (standard vig line): prob = 110 / (110 + 100) = 0.5238
  expect_equal(american_to_probability(-110), 110 / 210, tolerance = 1e-6)

  # -200 (heavy favorite): prob = 200 / (200 + 100) = 0.6667
  expect_equal(american_to_probability(-200), 200 / 300, tolerance = 1e-6)

  # -150: prob = 150 / (150 + 100) = 0.60
  expect_equal(american_to_probability(-150), 150 / 250, tolerance = 1e-6)

  # -100 (even money): prob = 100 / (100 + 100) = 0.50
  expect_equal(american_to_probability(-100), 0.5, tolerance = 1e-6)
})

test_that("american_to_probability handles positive odds correctly", {
  # +110: prob = 100 / (110 + 100) = 0.4762
  expect_equal(american_to_probability(110), 100 / 210, tolerance = 1e-6)

  # +200 (underdog): prob = 100 / (200 + 100) = 0.3333
  expect_equal(american_to_probability(200), 100 / 300, tolerance = 1e-6)

  # +100 (even money): prob = 100 / (100 + 100) = 0.50
  expect_equal(american_to_probability(100), 0.5, tolerance = 1e-6)
})

test_that("american_to_probability handles edge cases", {
  # NA input -> NA output
  expect_true(is.na(american_to_probability(NA)))

  # Zero odds -> NA (invalid)
  expect_true(is.na(american_to_probability(0)))

  # Inf -> NA
  expect_true(is.na(american_to_probability(Inf)))

  # Vectorized input
  result <- american_to_probability(c(-110, 110, NA))
  expect_equal(length(result), 3)
  expect_false(is.na(result[1]))
  expect_false(is.na(result[2]))
  expect_true(is.na(result[3]))
})

# =============================================================================
# TEST SUITE: american_to_decimal()
# =============================================================================
test_that("american_to_decimal handles negative odds correctly", {
  # -110: decimal = 1 + 100/110 = 1.909
  expect_equal(american_to_decimal(-110), 1 + 100/110, tolerance = 1e-6)

  # -200: decimal = 1 + 100/200 = 1.50
  expect_equal(american_to_decimal(-200), 1.5, tolerance = 1e-6)

  # -100: decimal = 1 + 100/100 = 2.0
  expect_equal(american_to_decimal(-100), 2.0, tolerance = 1e-6)
})

test_that("american_to_decimal handles positive odds correctly", {
  # +110: decimal = 1 + 110/100 = 2.10
  expect_equal(american_to_decimal(110), 2.1, tolerance = 1e-6)

  # +200: decimal = 1 + 200/100 = 3.0
  expect_equal(american_to_decimal(200), 3.0, tolerance = 1e-6)

  # +100: decimal = 1 + 100/100 = 2.0
  expect_equal(american_to_decimal(100), 2.0, tolerance = 1e-6)
})

test_that("american_to_decimal handles edge cases", {
  expect_true(is.na(american_to_decimal(NA)))
  expect_true(is.na(american_to_decimal(0)))
  expect_true(is.na(american_to_decimal(Inf)))
})

# =============================================================================
# TEST SUITE: expected_value_units()
# Formula: EV = prob * decimal_odds - 1 = prob * (dec - 1) - (1 - prob)
#        = prob * b - (1 - prob) where b = dec - 1
# =============================================================================
test_that("expected_value_units calculates EV correctly", {
  # Test case 1: 55% prob at -110 odds
  # dec = 1.909, b = 0.909
  # EV = 0.55 * 0.909 - 0.45 = 0.49995 - 0.45 = 0.04995
  prob <- 0.55
  dec <- american_to_decimal(-110)
  expected_ev <- prob * dec - 1
  result <- expected_value_units(0.55, -110)
  expect_equal(result, expected_ev, tolerance = 1e-4)

  # Test case 2: 50% prob at -110 (should be negative due to vig)
  prob <- 0.50
  dec <- american_to_decimal(-110)
  expected_ev <- prob * dec - 1
  result <- expected_value_units(0.50, -110)
  expect_equal(result, expected_ev, tolerance = 1e-4)
  expect_true(result < 0)  # Should be negative (house edge)

  # Test case 3: 40% prob at +200 odds
  # dec = 3.0, EV = 0.40 * 3.0 - 1 = 0.20
  prob <- 0.40
  dec <- american_to_decimal(200)
  expected_ev <- prob * dec - 1
  result <- expected_value_units(0.40, 200)
  expect_equal(result, expected_ev, tolerance = 1e-4)
  expect_equal(result, 0.20, tolerance = 1e-4)
})

test_that("expected_value_units handles edge cases", {
  # NA probability
  expect_true(is.na(expected_value_units(NA, -110)))

  # NA odds
  expect_true(is.na(expected_value_units(0.55, NA)))

  # Invalid odds
  expect_true(is.na(expected_value_units(0.55, 0)))
})

test_that("expected_value_units boundary: break-even at implied prob", {
  # At the market-implied probability, EV should be 0 (ignoring vig)
  # For +100 (even money), implied prob = 50%, EV should be 0
  result <- expected_value_units(0.50, 100)
  expect_equal(result, 0.0, tolerance = 1e-4)

  # For -200, implied prob = 66.67%
  implied_prob <- american_to_probability(-200)
  result <- expected_value_units(implied_prob, -200)
  expect_equal(result, 0.0, tolerance = 1e-4)
})

# =============================================================================
# TEST SUITE: shrink_probability_toward_market()
# =============================================================================
test_that("shrink_probability_toward_market blends correctly", {
  # shrinkage = 0.6 means 60% market, 40% model
  # model = 0.70, market = 0.50
  # shrunk = 0.40 * 0.70 + 0.60 * 0.50 = 0.28 + 0.30 = 0.58
  result <- shrink_probability_toward_market(0.70, 0.50, shrinkage = 0.6)
  expect_equal(result, 0.58, tolerance = 1e-4)

  # shrinkage = 0.0 (no shrinkage) -> return model prob
  result <- shrink_probability_toward_market(0.70, 0.50, shrinkage = 0.0)
  expect_equal(result, 0.70, tolerance = 1e-4)

  # shrinkage = 1.0 (full shrinkage) -> return market prob
  result <- shrink_probability_toward_market(0.70, 0.50, shrinkage = 1.0)
  expect_equal(result, 0.50, tolerance = 1e-4)
})

test_that("shrink_probability_toward_market clamps output", {
  # Result should always be in (0, 1)
  result <- shrink_probability_toward_market(0.99, 0.99, shrinkage = 0.5)
  expect_true(result > 0 && result < 1)

  result <- shrink_probability_toward_market(0.01, 0.01, shrinkage = 0.5)
  expect_true(result > 0 && result < 1)
})

# =============================================================================
# TEST SUITE: conservative_kelly_stake()
# =============================================================================
test_that("conservative_kelly_stake applies fractional Kelly", {
  # Full Kelly for 55% at -110:
  # b = 0.909, kelly = (0.55 * 0.909 - 0.45) / 0.909 = 0.0545
  # With 1/8 Kelly (0.125): stake = 0.0545 * 0.125 = 0.0068

  result <- conservative_kelly_stake(0.55, -110, kelly_fraction = 0.125)
  expect_true(!is.na(result))
  expect_true(result >= 0)
  expect_true(result <= 0.02)  # Should be capped at max_stake
})

test_that("conservative_kelly_stake respects max_stake cap", {
  # Even with high edge, should not exceed max_stake
  result <- conservative_kelly_stake(0.90, 100, kelly_fraction = 1.0, max_stake = 0.02)
  expect_true(result <= 0.02)
})

test_that("conservative_kelly_stake returns 0 for negative EV", {
  # At 45% prob with -110 odds, EV is negative
  result <- conservative_kelly_stake(0.45, -110)
  expect_true(is.na(result) || result == 0)
})

test_that("conservative_kelly_stake handles edge cases", {
  expect_true(is.na(conservative_kelly_stake(NA, -110)))
  expect_true(is.na(conservative_kelly_stake(0.55, NA)))
  expect_true(is.na(conservative_kelly_stake(0.55, 0)))
})

# =============================================================================
# TEST SUITE: devig_2way (from NFLbrier_logloss.R)
# =============================================================================
# The devig function should remove vig and normalize to sum = 1
test_that("devig removes vig correctly", {
  # If we have a local devig_2way function available
  if (exists("devig_2way", mode = "function")) {
    # -110/-110 line: raw probs sum to 1.047
    p_home_raw <- american_to_probability(-110)
    p_away_raw <- american_to_probability(-110)

    result <- devig_2way(p_home_raw, p_away_raw)

    # After devigging, should sum to 1
    expect_equal(
      result$p_home_mkt_2w + result$p_away_mkt_2w,
      1.0,
      tolerance = 1e-6
    )

    # Both should be 0.5 for even line
    expect_equal(result$p_home_mkt_2w, 0.5, tolerance = 1e-4)
    expect_equal(result$p_away_mkt_2w, 0.5, tolerance = 1e-4)
  }
})

# Similar test for the NFLmarket.R version
test_that("devig_two_way_probabilities removes vig correctly", {
  if (exists("devig_two_way_probabilities", mode = "function")) {
    p_home_raw <- american_to_probability(-110)
    p_away_raw <- american_to_probability(-110)

    result <- devig_two_way_probabilities(p_home_raw, p_away_raw)

    expect_equal(result$p_home + result$p_away, 1.0, tolerance = 1e-6)
  }
})

# =============================================================================
# TEST SUITE: clamp_probability()
# =============================================================================
test_that("clamp_probability enforces bounds", {
  # Values in range stay unchanged
  expect_equal(clamp_probability(0.5), 0.5, tolerance = 1e-6)

  # Values > 1 get clamped to just under 1
  result <- clamp_probability(1.5)
  expect_true(result < 1)
  expect_true(result > 0.999)

  # Values < 0 get clamped to just above 0
  result <- clamp_probability(-0.5)
  expect_true(result > 0)
  expect_true(result < 0.001)

  # Exactly 0 and 1 get nudged
  expect_true(clamp_probability(0) > 0)
  expect_true(clamp_probability(1) < 1)
})

test_that("clamp_probability handles NA", {
  expect_true(is.na(clamp_probability(NA)))
})

# =============================================================================
# TEST SUITE: classify_edge_magnitude()
# =============================================================================
test_that("classify_edge_magnitude categorizes correctly", {
  if (exists("classify_edge_magnitude", mode = "function")) {
    expect_equal(classify_edge_magnitude(-0.05), "negative")
    expect_equal(classify_edge_magnitude(0.03), "realistic")
    expect_equal(classify_edge_magnitude(0.07), "optimistic")
    expect_equal(classify_edge_magnitude(0.12), "suspicious")
    expect_equal(classify_edge_magnitude(0.20), "implausible")
    expect_true(is.na(classify_edge_magnitude(NA)))
  }
})

# =============================================================================
# INTEGRATION TEST: EV calculation chain
# =============================================================================
test_that("full EV calculation chain is consistent", {
  # Start with American odds -150
  odds <- -150

  # Convert to decimal
  dec <- american_to_decimal(odds)
  expect_equal(dec, 1 + 100/150, tolerance = 1e-6)

  # Get implied probability
  implied_prob <- american_to_probability(odds)
  expect_equal(implied_prob, 150/250, tolerance = 1e-6)

  # EV at implied prob should be 0
  ev_at_implied <- expected_value_units(implied_prob, odds)
  expect_equal(ev_at_implied, 0, tolerance = 1e-4)

  # EV with edge should be positive
  model_prob <- implied_prob + 0.05  # 5% edge
  ev_with_edge <- expected_value_units(model_prob, odds)
  expect_true(ev_with_edge > 0)

  # After shrinkage toward market, EV should be reduced
  if (exists("shrink_probability_toward_market", mode = "function")) {
    shrunk_prob <- shrink_probability_toward_market(model_prob, implied_prob, 0.6)
    ev_shrunk <- expected_value_units(shrunk_prob, odds)
    expect_true(ev_shrunk < ev_with_edge)
    expect_true(ev_shrunk > ev_at_implied)
  }
})

# =============================================================================
# RUN TESTS
# =============================================================================
cat("\n=== Running Core Math Unit Tests ===\n\n")

# Run all tests
test_results <- testthat::test_file(
  "tests/test_core_math.R",
  reporter = testthat::SummaryReporter,
  stop_on_failure = FALSE
)

# Print summary
cat("\n=== Test Summary ===\n")
if (length(test_results) > 0) {
  n_pass <- sum(sapply(test_results, function(x) x$passed))
  n_fail <- sum(sapply(test_results, function(x) x$failed))
  n_skip <- sum(sapply(test_results, function(x) x$skipped))

  cat(sprintf("Passed: %d\n", n_pass))
  cat(sprintf("Failed: %d\n", n_fail))
  cat(sprintf("Skipped: %d\n", n_skip))

  if (n_fail > 0) {
    cat("\nSome tests failed. Review output above.\n")
  } else {
    cat("\nAll tests passed!\n")
  }
}
