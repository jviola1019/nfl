# =============================================================================
# FILE: tests/testthat/test-vig-calculation.R
# PURPOSE: Tests for vig (juice) application to model moneylines
#
# VERSION: 2.9.0
# LAST UPDATED: 2026-02-03
#
# TEST COVERAGE:
#   - apply_model_vig() function accuracy
#   - devig_american_odds() function accuracy
#   - Total implied probability verification
#   - Edge cases and boundary conditions
# =============================================================================

library(testthat)

# Source utils
utils_path <- file.path(getwd(), "R", "utils.R")
if (file.exists(utils_path)) {
  source(utils_path, local = FALSE)
}

# =============================================================================
# APPLY_MODEL_VIG TESTS
# =============================================================================

test_that("apply_model_vig returns correct odds for 50% probability", {
  skip_if_not(exists("apply_model_vig"), "apply_model_vig not loaded")

  # 50% with 10% vig should be -110
  result <- apply_model_vig(0.50, 0.10)
  expect_equal(result, -110, tolerance = 5)
})

test_that("apply_model_vig returns correct odds for favorites", {
  skip_if_not(exists("apply_model_vig"), "apply_model_vig not loaded")

  # 60% true prob with 10% vig
  # Vigged prob = 0.60 * 1.05 = 0.63
  # American odds = -170 (approximately)
  result_60 <- apply_model_vig(0.60, 0.10)
  expect_true(result_60 < -130)  # Should be decent favorite

  # 70% true prob
  result_70 <- apply_model_vig(0.70, 0.10)
  expect_true(result_70 < result_60)  # Bigger favorite = more negative
})

test_that("apply_model_vig returns correct odds for underdogs", {
  skip_if_not(exists("apply_model_vig"), "apply_model_vig not loaded")

  # 40% true prob with 10% vig
  result_40 <- apply_model_vig(0.40, 0.10)
  expect_true(result_40 > 0)  # Should be positive (underdog)

  # 30% true prob
  result_30 <- apply_model_vig(0.30, 0.10)
  expect_true(result_30 > result_40)  # Bigger underdog = more positive
})

test_that("vigged moneylines sum to >100% implied probability", {
  skip_if_not(exists("apply_model_vig"), "apply_model_vig not loaded")
  skip_if_not(exists("american_to_probability"), "american_to_probability not loaded")

  # Test various probability scenarios
  probs <- c(0.30, 0.40, 0.50, 0.55, 0.60, 0.70)

  for (p in probs) {
    home_ml <- apply_model_vig(p, 0.10)
    away_ml <- apply_model_vig(1 - p, 0.10)

    home_implied <- american_to_probability(home_ml)
    away_implied <- american_to_probability(away_ml)

    # Total implied should be approximately 1.10 (10% vig)
    total_implied <- home_implied + away_implied
    expect_equal(total_implied, 1.10, tolerance = 0.03,
                 info = sprintf("Failed for prob=%s", p))
  }
})

test_that("apply_model_vig handles extreme probabilities", {
  skip_if_not(exists("apply_model_vig"), "apply_model_vig not loaded")

  # Very high probability (capped at 99%)
  result_high <- apply_model_vig(0.95, 0.10)
  expect_true(is.finite(result_high))
  expect_true(result_high < 0)  # Should be heavy favorite

  # Very low probability
  result_low <- apply_model_vig(0.05, 0.10)
  expect_true(is.finite(result_low))
  expect_true(result_low > 0)  # Should be heavy underdog
})

test_that("apply_model_vig handles zero and no vig", {
  skip_if_not(exists("apply_model_vig"), "apply_model_vig not loaded")
  skip_if_not(exists("probability_to_american"), "probability_to_american not loaded")

  # Zero vig should return fair odds
  fair_odds <- probability_to_american(0.55)
  vigged_zero <- apply_model_vig(0.55, 0.00)
  expect_equal(vigged_zero, fair_odds, tolerance = 2)
})

# =============================================================================
# DEVIG_AMERICAN_ODDS TESTS
# =============================================================================

test_that("devig_american_odds returns correct true probabilities", {
  skip_if_not(exists("devig_american_odds"), "devig_american_odds not loaded")

  # Standard -110/-110 line (10% vig)
  result <- devig_american_odds(-110, -110)

  expect_equal(result$home_prob, 0.50, tolerance = 0.01)
  expect_equal(result$away_prob, 0.50, tolerance = 0.01)
  expect_true(abs(result$overround - 0.10) < 0.05)
})

test_that("devig_american_odds handles asymmetric lines", {
  skip_if_not(exists("devig_american_odds"), "devig_american_odds not loaded")

  # -150/+130 line
  result <- devig_american_odds(-150, 130)

  # Should normalize to sum = 1
  expect_equal(result$home_prob + result$away_prob, 1.0, tolerance = 0.001)

  # Favorite should have higher probability
  expect_true(result$home_prob > result$away_prob)
})

test_that("devig_american_odds calculates correct overround", {
  skip_if_not(exists("devig_american_odds"), "devig_american_odds not loaded")
  skip_if_not(exists("american_to_probability"), "american_to_probability not loaded")

  # Calculate manually
  home_odds <- -200
  away_odds <- 180

  home_implied <- american_to_probability(home_odds)
  away_implied <- american_to_probability(away_odds)

  expected_overround <- home_implied + away_implied - 1

  result <- devig_american_odds(home_odds, away_odds)

  expect_equal(result$overround, expected_overround, tolerance = 0.01)
})

# =============================================================================
# INTEGRATION TESTS
# =============================================================================

test_that("apply_model_vig produces realistic market-like odds", {
  skip_if_not(exists("apply_model_vig"), "apply_model_vig not loaded")

  # Real-world scenarios

  # ~55% favorite: should be around -130/-133
  home_ml <- apply_model_vig(0.55, 0.10)
  expect_true(home_ml >= -145 && home_ml <= -120)

  # ~65% favorite: should be around -200/-210
  home_ml_big <- apply_model_vig(0.65, 0.10)
  expect_true(home_ml_big >= -230 && home_ml_big <= -170)

  # Pick 'em with vig: -110/-110
  home_ml_even <- apply_model_vig(0.50, 0.10)
  away_ml_even <- apply_model_vig(0.50, 0.10)
  expect_equal(home_ml_even, away_ml_even)
  expect_true(abs(home_ml_even - (-110)) < 10)
})

test_that("vigged odds are more extreme than fair odds", {
  skip_if_not(exists("apply_model_vig"), "apply_model_vig not loaded")
  skip_if_not(exists("probability_to_american"), "probability_to_american not loaded")

  prob <- 0.60

  fair <- probability_to_american(prob)
  vigged <- apply_model_vig(prob, 0.10)

  # Vigged odds should be more negative (more extreme) for favorites
  expect_true(vigged < fair)

  # For underdogs, vigged odds should be less positive
  fair_dog <- probability_to_american(0.40)
  vigged_dog <- apply_model_vig(0.40, 0.10)
  expect_true(vigged_dog < fair_dog)
})

# =============================================================================
# EDGE CASES
# =============================================================================

test_that("apply_model_vig handles NA input", {
  skip_if_not(exists("apply_model_vig"), "apply_model_vig not loaded")

  result <- apply_model_vig(NA, 0.10)
  expect_true(is.na(result))
})

test_that("vig functions handle vector input", {
  skip_if_not(exists("apply_model_vig"), "apply_model_vig not loaded")

  probs <- c(0.40, 0.50, 0.60)
  results <- apply_model_vig(probs, 0.10)

  expect_length(results, 3)
  expect_true(all(is.finite(results)))
  expect_true(results[3] < results[2])  # Higher prob = more negative odds
  expect_true(results[2] < results[1])
})

# =============================================================================
# CONFIG PARAMETER TESTS
# =============================================================================

test_that("MODEL_VIG_PCT is properly configured", {
  skip_if_not(exists("MODEL_VIG_PCT"), "MODEL_VIG_PCT not loaded")

  # Should be between 5% and 15%
  expect_true(MODEL_VIG_PCT >= 0.05)
  expect_true(MODEL_VIG_PCT <= 0.15)

  # Default should be 10%
  expect_equal(MODEL_VIG_PCT, 0.10, tolerance = 0.02)
})

test_that("VIG config matches MODEL_VIG_PCT", {
  skip_if_not(exists("VIG"), "VIG not loaded")
  skip_if_not(exists("MODEL_VIG_PCT"), "MODEL_VIG_PCT not loaded")

  # Both vig parameters should be consistent
  expect_equal(VIG, MODEL_VIG_PCT, tolerance = 0.02)
})
