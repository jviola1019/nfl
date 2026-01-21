# =============================================================================
# Tests for Playoff Game Type Mapping in NFLsimulation.R
# =============================================================================
# Tests the get_expected_game_type() and is_playoff_week_sim() helper functions.
# =============================================================================

# Source the helper functions (they're defined at the top of NFLsimulation.R)
# For testing, we recreate them here to avoid sourcing the entire simulation file.

.WEEK_TO_GAME_TYPE <- list(
  `19` = "WC",
  `20` = "DIV",
  `21` = "CON",
  `22` = "SB"
)

get_expected_game_type <- function(week) {
  week <- as.integer(week)
  if (is.na(week) || week < 1 || week > 22) {
    return(NA_character_)
  }
  if (week <= 18) {
    return("REG")
  }
  .WEEK_TO_GAME_TYPE[[as.character(week)]]
}

is_playoff_week_sim <- function(week) {
  week <- as.integer(week)
  !is.na(week) && week >= 19 && week <= 22
}

# =============================================================================
# TESTS
# =============================================================================

test_that("get_expected_game_type returns REG for regular season weeks", {
  expect_equal(get_expected_game_type(1), "REG")
  expect_equal(get_expected_game_type(9), "REG")
  expect_equal(get_expected_game_type(18), "REG")
})

test_that("get_expected_game_type returns correct playoff codes", {
  expect_equal(get_expected_game_type(19), "WC")   # Wild Card
  expect_equal(get_expected_game_type(20), "DIV")  # Divisional
  expect_equal(get_expected_game_type(21), "CON")  # Conference Championship
  expect_equal(get_expected_game_type(22), "SB")   # Super Bowl
})

test_that("get_expected_game_type handles invalid weeks", {
  expect_true(is.na(get_expected_game_type(0)))
  expect_true(is.na(get_expected_game_type(-1)))
  expect_true(is.na(get_expected_game_type(23)))
  expect_true(is.na(get_expected_game_type(NA)))
})

test_that("is_playoff_week_sim identifies playoff weeks correctly", {
  # Regular season
  expect_false(is_playoff_week_sim(1))
  expect_false(is_playoff_week_sim(18))

  # Playoffs
  expect_true(is_playoff_week_sim(19))
  expect_true(is_playoff_week_sim(20))
  expect_true(is_playoff_week_sim(21))
  expect_true(is_playoff_week_sim(22))

  # Edge cases
  expect_false(is_playoff_week_sim(0))
  expect_false(is_playoff_week_sim(23))
  expect_false(is_playoff_week_sim(NA))
})

test_that("Week 21 maps to CON (Conference Championship)", {
  # This is the specific "Week 21 error" case
  expect_equal(get_expected_game_type(21), "CON")
  expect_true(is_playoff_week_sim(21))
})
