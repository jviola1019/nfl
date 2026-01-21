# =============================================================================
# Tests for R/utils.R - Core Utility Functions
# =============================================================================
# Note: R/utils.R is loaded by tests/testthat/setup.R
# =============================================================================

# =============================================================================
# ODDS CONVERSION TESTS
# =============================================================================

test_that("american_to_probability handles standard cases", {
  # Favorites (negative odds)
  expect_equal(american_to_probability(-100), 0.5, tolerance = 0.001)
  expect_equal(american_to_probability(-110), 0.524, tolerance = 0.001)
  expect_equal(american_to_probability(-200), 0.667, tolerance = 0.001)
  expect_equal(american_to_probability(-300), 0.75, tolerance = 0.001)

  # Underdogs (positive odds)
  expect_equal(american_to_probability(100), 0.5, tolerance = 0.001)
  expect_equal(american_to_probability(110), 0.476, tolerance = 0.001)
  expect_equal(american_to_probability(200), 0.333, tolerance = 0.001)
  expect_equal(american_to_probability(300), 0.25, tolerance = 0.001)
})

test_that("american_to_probability handles edge cases", {
  # Invalid odds
  expect_true(is.na(american_to_probability(0)))
  expect_true(is.na(american_to_probability(NA)))
  expect_true(is.na(american_to_probability(Inf)))
  expect_true(is.na(american_to_probability(-Inf)))

  # Character coercion
  expect_equal(american_to_probability("-110"), 0.524, tolerance = 0.001)
})

test_that("american_to_decimal handles standard cases", {
  # Favorites
  expect_equal(american_to_decimal(-100), 2.0, tolerance = 0.01)
  expect_equal(american_to_decimal(-110), 1.909, tolerance = 0.01)
  expect_equal(american_to_decimal(-200), 1.5, tolerance = 0.01)

  # Underdogs
  expect_equal(american_to_decimal(100), 2.0, tolerance = 0.01)
  expect_equal(american_to_decimal(150), 2.5, tolerance = 0.01)
  expect_equal(american_to_decimal(200), 3.0, tolerance = 0.01)
})

test_that("american_to_decimal handles edge cases", {
  expect_true(is.na(american_to_decimal(0)))
  expect_true(is.na(american_to_decimal(NA)))
})

test_that("probability_to_american inverts correctly", {
  probs <- c(0.5, 0.6, 0.7, 0.8, 0.4, 0.3, 0.25)
  for (p in probs) {
    odds <- probability_to_american(p)
    prob_back <- american_to_probability(odds)
    expect_equal(prob_back, p, tolerance = 0.02, label = sprintf("prob=%.2f", p))
  }
})

# =============================================================================
# PROBABILITY CLAMPING TESTS
# =============================================================================

test_that("clamp_probability clamps to valid range", {
  eps <- 1e-9

  # Values within range
  expect_equal(clamp_probability(0.5), 0.5)
  expect_equal(clamp_probability(0.1), 0.1)

  # Values at boundaries
  expect_equal(clamp_probability(0), eps)
  expect_equal(clamp_probability(1), 1 - eps)

  # Values outside range
  expect_equal(clamp_probability(-0.5), eps)
  expect_equal(clamp_probability(1.5), 1 - eps)

  # NA handling
  expect_true(is.na(clamp_probability(NA)))

  # Character coercion
  expect_equal(clamp_probability("0.5"), 0.5)
})

# =============================================================================
# BETTING FUNCTION TESTS
# =============================================================================

test_that("expected_value_units calculates correctly", {
  # Fair bet (50% at even odds)
  ev <- expected_value_units(0.5, 100)
  expect_equal(ev, 0, tolerance = 0.001)

  # Profitable bet (55% at even odds)
  ev <- expected_value_units(0.55, 100)
  expect_equal(ev, 0.1, tolerance = 0.001)

  # Negative EV bet
  ev <- expected_value_units(0.45, 100)
  expect_equal(ev, -0.1, tolerance = 0.001)

  # With vig (-110 odds, need ~52.4% to break even)
  ev <- expected_value_units(0.524, -110)
  expect_equal(ev, 0, tolerance = 0.01)
})

test_that("expected_value_units handles edge cases", {
  expect_true(is.na(expected_value_units(0.5, 0)))
  expect_true(is.na(expected_value_units(NA, -110)))
  expect_true(is.na(expected_value_units(0.5, NA)))
})

test_that("shrink_probability_toward_market blends correctly", {
  # 60% shrinkage (default)
  shrunk <- shrink_probability_toward_market(0.7, 0.5, shrinkage = 0.6)
  # 0.4 * 0.7 + 0.6 * 0.5 = 0.28 + 0.30 = 0.58
  expect_equal(shrunk, 0.58, tolerance = 0.001)

  # No shrinkage
  shrunk <- shrink_probability_toward_market(0.7, 0.5, shrinkage = 0)
  expect_equal(shrunk, 0.7, tolerance = 0.001)

  # Full shrinkage
  shrunk <- shrink_probability_toward_market(0.7, 0.5, shrinkage = 1)
  expect_equal(shrunk, 0.5, tolerance = 0.001)
})

test_that("classify_edge_magnitude categorizes correctly", {
  expect_equal(classify_edge_magnitude(-0.05), "negative")
  expect_equal(classify_edge_magnitude(0.03), "realistic")
  expect_equal(classify_edge_magnitude(0.07), "optimistic")
  expect_equal(classify_edge_magnitude(0.12), "suspicious")
  expect_equal(classify_edge_magnitude(0.20), "implausible")
  expect_true(is.na(classify_edge_magnitude(NA)))
})

test_that("conservative_kelly_stake is bounded", {
  # High probability should still be capped
  stake <- conservative_kelly_stake(0.8, -110)
  expect_true(stake <= 0.02)  # Max stake cap

  # Low probability should give zero stake
  stake <- conservative_kelly_stake(0.3, -110)
  expect_true(is.na(stake) || stake == 0)

  # Edge cases
  expect_true(is.na(conservative_kelly_stake(0.5, 0)))
  expect_true(is.na(conservative_kelly_stake(NA, -110)))
})

# =============================================================================
# VALIDATION METRIC TESTS
# =============================================================================

test_that("brier_score calculates correctly", {
  # Perfect predictions
  expect_equal(brier_score(c(1, 0), c(1, 0)), 0)

  # Worst predictions
  expect_equal(brier_score(c(0, 1), c(1, 0)), 1)

  # 50% predictions
  expect_equal(brier_score(c(0.5, 0.5), c(1, 0)), 0.25)

  # NA handling
  expect_equal(brier_score(c(0.5, NA), c(1, 0)), 0.25)
})

test_that("log_loss calculates correctly", {
  # Good predictions should have lower loss
  ll_good <- log_loss(c(0.9, 0.1), c(1, 0))
  ll_bad <- log_loss(c(0.5, 0.5), c(1, 0))
  expect_true(ll_good < ll_bad)

  # Perfect predictions (with epsilon)
  expect_true(log_loss(c(1, 0), c(1, 0)) < 0.01)
})

test_that("accuracy calculates correctly", {
  # All correct
  expect_equal(accuracy(c(0.6, 0.4), c(1, 0)), 1)

  # All wrong
  expect_equal(accuracy(c(0.4, 0.6), c(1, 0)), 0)

  # Half correct
  expect_equal(accuracy(c(0.6, 0.6), c(1, 0)), 0.5)
})

# =============================================================================
# JOIN UTILITY TESTS
# =============================================================================

test_that("standardize_join_keys renames and coerces types", {
  df <- tibble::tibble(
    gameid = c("2024_01_KC_DEN", "2024_01_SF_NYG"),
    seasonYear = c("2024", "2024"),
    wk = c("1", "1"),
    other = c("a", "b")
  )

  result <- standardize_join_keys(df)

  # Check renaming
 expect_true("game_id" %in% names(result))
  expect_true("season" %in% names(result))
  expect_true("week" %in% names(result))

  # Check type coercion
  expect_type(result$game_id, "character")
  expect_type(result$season, "integer")
  expect_type(result$week, "integer")

  # Check values
  expect_equal(result$season[1], 2024L)
  expect_equal(result$week[1], 1L)
})

test_that("standardize_join_keys handles NULL/empty input", {
  expect_null(standardize_join_keys(NULL))
  expect_equal(nrow(standardize_join_keys(tibble::tibble())), 0)
})

test_that("collapse_by_keys_relaxed handles duplicates", {
  df <- tibble::tibble(
    game_id = c("A", "A", "B"),
    season = c(2024L, 2024L, 2024L),
    value = c(1, NA, 3)
  )

  result <- collapse_by_keys_relaxed(df, c("game_id", "season"), "test")
  expect_equal(nrow(result), 2)  # Collapsed to 2 rows
  expect_equal(result$value[result$game_id == "A"], 1)  # First non-NA value
})

test_that("ensure_unique_join_keys removes duplicates", {
  df <- tibble::tibble(
    game_id = c("A", "A", "B"),
    season = c(2024L, 2024L, 2024L),
    value = c(1, 2, 3)
  )

  result <- ensure_unique_join_keys(df, c("game_id", "season"), "test")
  expect_equal(nrow(result), 2)  # Only unique keys
})

# =============================================================================
# DATA FRAME UTILITY TESTS
# =============================================================================

test_that("first_non_missing_typed finds first valid value", {
  expect_equal(first_non_missing_typed(c(NA, 2, 3)), 2)
  expect_equal(first_non_missing_typed(c(1, NA, 3)), 1)
  expect_equal(first_non_missing_typed(c(NA, NA, NA)), NA)
  expect_equal(first_non_missing_typed(c("a", "b")), "a")
})

test_that("select_first_column finds matching column", {
  df <- tibble::tibble(col_a = 1, col_b = 2, col_c = 3)

  expect_equal(select_first_column(df, c("col_x", "col_a")), "col_a")
  expect_equal(select_first_column(df, c("col_b", "col_a")), "col_b")
  expect_true(is.na(select_first_column(df, c("col_x", "col_y"))))
})

test_that("ensure_columns_with_defaults adds missing columns", {
  df <- tibble::tibble(a = 1:3)
  defaults <- list(b = 0, c = NA_character_)

  result <- ensure_columns_with_defaults(df, defaults)
  expect_true("b" %in% names(result))
  expect_true("c" %in% names(result))
  expect_equal(result$b, c(0, 0, 0))
})
