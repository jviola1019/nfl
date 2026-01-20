# =============================================================================
# Tests for R/playoffs.R - Playoff Module
# =============================================================================

# Load the playoffs module
source(file.path(dirname(dirname(dirname(testthat::test_path()))), "R", "playoffs.R"))

# =============================================================================
# PLAYOFF ROUND DEFINITIONS TESTS
# =============================================================================

test_that("PLAYOFF_ROUNDS contains all four rounds", {
  expect_true("wild_card" %in% names(PLAYOFF_ROUNDS))
  expect_true("divisional" %in% names(PLAYOFF_ROUNDS))
  expect_true("conference" %in% names(PLAYOFF_ROUNDS))
  expect_true("super_bowl" %in% names(PLAYOFF_ROUNDS))
})

test_that("PLAYOFF_ROUNDS has correct game counts", {
  expect_equal(PLAYOFF_ROUNDS$wild_card$expected_games, 6)
  expect_equal(PLAYOFF_ROUNDS$divisional$expected_games, 4)
  expect_equal(PLAYOFF_ROUNDS$conference$expected_games, 2)
  expect_equal(PLAYOFF_ROUNDS$super_bowl$expected_games, 1)
})

test_that("PLAYOFF_ROUNDS has correct week numbers", {
  expect_equal(PLAYOFF_ROUNDS$wild_card$week, 19)
  expect_equal(PLAYOFF_ROUNDS$divisional$week, 20)
  expect_equal(PLAYOFF_ROUNDS$conference$week, 21)
  expect_equal(PLAYOFF_ROUNDS$super_bowl$week, 22)
})

test_that("Super Bowl has no home field advantage", {
  expect_false(PLAYOFF_ROUNDS$super_bowl$home_field)
  expect_true(PLAYOFF_ROUNDS$wild_card$home_field)
  expect_true(PLAYOFF_ROUNDS$divisional$home_field)
  expect_true(PLAYOFF_ROUNDS$conference$home_field)
})

# =============================================================================
# ROUND DETECTION TESTS
# =============================================================================

test_that("derive_playoff_round_from_week maps correctly", {
  expect_equal(derive_playoff_round_from_week(19), "wild_card")
  expect_equal(derive_playoff_round_from_week(20), "divisional")
  expect_equal(derive_playoff_round_from_week(21), "conference")
  expect_equal(derive_playoff_round_from_week(22), "super_bowl")
  expect_true(is.na(derive_playoff_round_from_week(1)))
  expect_true(is.na(derive_playoff_round_from_week(18)))
})

test_that("is_playoff_week identifies playoff weeks", {
  # Regular season
  expect_false(is_playoff_week(1))
  expect_false(is_playoff_week(18))

  # Playoffs
  expect_true(is_playoff_week(19))
  expect_true(is_playoff_week(20))
  expect_true(is_playoff_week(21))
  expect_true(is_playoff_week(22))

  # Edge cases
  expect_false(is_playoff_week(NA))
  expect_false(is_playoff_week(0))
  expect_false(is_playoff_week(23))
})

test_that("get_phase_from_week returns correct phase", {
  # Regular season
  expect_equal(get_phase_from_week(1), "regular_season")
  expect_equal(get_phase_from_week(10), "regular_season")
  expect_equal(get_phase_from_week(18), "regular_season")

  # Playoffs
  expect_equal(get_phase_from_week(19), "playoffs")
  expect_equal(get_phase_from_week(22), "playoffs")

  # Edge cases
  expect_equal(get_phase_from_week(0), "unknown")
  expect_equal(get_phase_from_week(23), "unknown")
  expect_equal(get_phase_from_week(NA), "unknown")
})

test_that("derive_playoff_round handles schedule rows", {
  # Test with game_type column
  wc_row <- tibble::tibble(game_type = "WC", week = 19)
  expect_equal(derive_playoff_round(wc_row), "wild_card")

  div_row <- tibble::tibble(game_type = "DIV", week = 20)
  expect_equal(derive_playoff_round(div_row), "divisional")

  con_row <- tibble::tibble(game_type = "CON", week = 21)
  expect_equal(derive_playoff_round(con_row), "conference")

  sb_row <- tibble::tibble(game_type = "SB", week = 22)
  expect_equal(derive_playoff_round(sb_row), "super_bowl")

  # Regular season
  reg_row <- tibble::tibble(game_type = "REG", week = 10)
  expect_true(is.na(derive_playoff_round(reg_row)))

  # Week-based fallback
  week_only <- tibble::tibble(week = 19)
  expect_equal(derive_playoff_round(week_only), "wild_card")
})

# =============================================================================
# PLAYOFF FEATURES TESTS
# =============================================================================

test_that("PLAYOFF_FEATURES has correct shrinkage values", {
  expect_equal(PLAYOFF_FEATURES$shrinkage$wild_card, 0.65)
  expect_equal(PLAYOFF_FEATURES$shrinkage$divisional, 0.70)
  expect_equal(PLAYOFF_FEATURES$shrinkage$conference, 0.70)
  expect_equal(PLAYOFF_FEATURES$shrinkage$super_bowl, 0.75)
})

test_that("PLAYOFF_FEATURES has correct HFA multipliers", {
  expect_equal(PLAYOFF_FEATURES$hfa_multiplier$wild_card, 1.15)
  expect_equal(PLAYOFF_FEATURES$hfa_multiplier$divisional, 1.20)
  expect_equal(PLAYOFF_FEATURES$hfa_multiplier$conference, 1.25)
  expect_equal(PLAYOFF_FEATURES$hfa_multiplier$super_bowl, 1.00)  # Neutral
})

test_that("get_playoff_shrinkage returns correct values", {
  expect_equal(get_playoff_shrinkage("wild_card"), 0.65)
  expect_equal(get_playoff_shrinkage("super_bowl"), 0.75)
  expect_equal(get_playoff_shrinkage(NA), 0.60)  # Default
  expect_equal(get_playoff_shrinkage("invalid"), 0.60)  # Default
})

test_that("get_playoff_hfa_multiplier returns correct values", {
  expect_equal(get_playoff_hfa_multiplier("wild_card"), 1.15)
  expect_equal(get_playoff_hfa_multiplier("super_bowl"), 1.00)
  expect_equal(get_playoff_hfa_multiplier(NA), 1.0)  # Default
  expect_equal(get_playoff_hfa_multiplier("invalid"), 1.0)  # Default
})

# =============================================================================
# PLAYOFF ADJUSTMENT TESTS
# =============================================================================

test_that("calculate_playoff_adjustments returns correct structure", {
  adj <- calculate_playoff_adjustments("wild_card", "KC", "MIA")

  expect_type(adj, "list")
  expect_true("round" %in% names(adj))
  expect_true("hfa_multiplier" %in% names(adj))
  expect_true("home_rest_bonus" %in% names(adj))
  expect_true("away_rest_bonus" %in% names(adj))
  expect_true("shrinkage" %in% names(adj))
  expect_true("injury_variance" %in% names(adj))
})

test_that("calculate_playoff_adjustments handles bye teams", {
  # Divisional round with bye team at home
  adj <- calculate_playoff_adjustments("divisional", "KC", "HOU",
                                       home_had_bye = TRUE, away_had_bye = FALSE)

  expect_equal(adj$home_rest_bonus, PLAYOFF_FEATURES$bye_rest_bonus$points)
  expect_equal(adj$away_rest_bonus, 0)

  # No bye bonus in wild card
  adj_wc <- calculate_playoff_adjustments("wild_card", "KC", "MIA",
                                          home_had_bye = TRUE, away_had_bye = FALSE)
  expect_equal(adj_wc$home_rest_bonus, 0)
})

test_that("calculate_playoff_adjustments handles NA round", {
  adj <- calculate_playoff_adjustments(NA, "KC", "MIA")

  expect_equal(adj$hfa_multiplier, 1.0)
  expect_equal(adj$shrinkage, 0.60)
  expect_equal(adj$injury_variance, 1.0)
})

# =============================================================================
# ROUND VALIDATION TESTS
# =============================================================================

test_that("validate_playoff_round_games validates correctly", {
  # Correct game count
  correct_schedule <- tibble::tibble(game_id = 1:6)
  result <- validate_playoff_round_games(correct_schedule, "wild_card")
  expect_true(result$valid)
  expect_equal(result$expected, 6)
  expect_equal(result$actual, 6)

  # Incorrect game count
  wrong_schedule <- tibble::tibble(game_id = 1:4)
  result <- validate_playoff_round_games(wrong_schedule, "wild_card")
  expect_false(result$valid)
  expect_equal(result$expected, 6)
  expect_equal(result$actual, 4)

  # Empty schedule
  result <- validate_playoff_round_games(tibble::tibble(), "wild_card")
  expect_false(result$valid)

  # Unknown round
  result <- validate_playoff_round_games(correct_schedule, "invalid_round")
  expect_false(result$valid)
})

# =============================================================================
# SCHEDULE FILTERING TESTS
# =============================================================================

test_that("filter_playoff_schedule filters by game_type", {
  schedule <- tibble::tibble(
    game_id = 1:10,
    game_type = c(rep("REG", 5), "WC", "WC", "DIV", "CON", "SB")
  )

  playoffs <- filter_playoff_schedule(schedule)
  expect_equal(nrow(playoffs), 5)
  expect_true(all(playoffs$game_type %in% c("WC", "DIV", "CON", "SB")))
})

test_that("filter_playoff_schedule filters by week", {
  schedule <- tibble::tibble(
    game_id = 1:10,
    week = c(1:6, 19, 20, 21, 22)
  )

  playoffs <- filter_playoff_schedule(schedule)
  expect_equal(nrow(playoffs), 4)
  expect_true(all(playoffs$week >= 19))
})

test_that("get_playoff_slate filters to specific round", {
  schedule <- tibble::tibble(
    game_id = 1:10,
    week = c(1, 2, 3, 18, 19, 19, 20, 20, 21, 22)
  )

  wc <- get_playoff_slate(schedule, "wild_card")
  expect_equal(nrow(wc), 2)
  expect_true(all(wc$week == 19))

  div <- get_playoff_slate(schedule, "divisional")
  expect_equal(nrow(div), 2)
  expect_true(all(div$week == 20))
})

# =============================================================================
# REPORT GENERATION TESTS
# =============================================================================

test_that("generate_playoff_report_header produces valid HTML", {
  html <- generate_playoff_report_header("wild_card", 2024)

  expect_true(grepl("<header", html))
  expect_true(grepl("Wild Card", html))
  expect_true(grepl("2024", html))
  expect_true(grepl("6 Games", html))
})

test_that("generate_round_sanity_html produces valid HTML", {
  # Valid result
  valid <- list(valid = TRUE, message = "Test passed")
  html <- generate_round_sanity_html(valid)
  expect_true(grepl("sanity-pass", html))
  expect_true(grepl("Test passed", html))

  # Invalid result
  invalid <- list(valid = FALSE, message = "Test failed")
  html <- generate_round_sanity_html(invalid)
  expect_true(grepl("sanity-fail", html))
  expect_true(grepl("Test failed", html))
})

test_that("generate_adjustments_html produces valid HTML", {
  adj <- calculate_playoff_adjustments("divisional", "KC", "MIA",
                                       home_had_bye = TRUE)
  html <- generate_adjustments_html(adj)

  expect_true(grepl("<div", html))
  expect_true(grepl("Divisional", html))
  expect_true(grepl("Market Shrinkage", html))

  # NA round should return empty
  adj_na <- calculate_playoff_adjustments(NA, "KC", "MIA")
  html_na <- generate_adjustments_html(adj_na)
  expect_equal(html_na, "")
})

# =============================================================================
# INTEGRATION TEST - HISTORICAL POSTSEASON
# =============================================================================

test_that("2023 postseason round detection works correctly", {
  # Skip if nflreadr not available
  skip_if_not_installed("nflreadr")

  tryCatch({
    schedule <- nflreadr::load_schedules(seasons = 2023)

    # Filter to playoffs
    playoffs <- schedule %>%
      dplyr::filter(.data$game_type %in% c("WC", "DIV", "CON", "SB"))

    if (nrow(playoffs) > 0) {
      # Check Wild Card (Week 19, 6 games)
      wc <- playoffs %>% dplyr::filter(.data$week == 19)
      expect_equal(nrow(wc), 6, label = "Wild Card should have 6 games")

      # Check Divisional (Week 20, 4 games)
      div <- playoffs %>% dplyr::filter(.data$week == 20)
      expect_equal(nrow(div), 4, label = "Divisional should have 4 games")

      # Check Conference (Week 21, 2 games)
      con <- playoffs %>% dplyr::filter(.data$week == 21)
      expect_equal(nrow(con), 2, label = "Conference should have 2 games")

      # Check Super Bowl (Week 22, 1 game)
      sb <- playoffs %>% dplyr::filter(.data$week == 22)
      expect_equal(nrow(sb), 1, label = "Super Bowl should have 1 game")

      # Test round derivation for actual games
      first_wc <- wc[1, ]
      expect_equal(derive_playoff_round(first_wc), "wild_card")

      first_sb <- sb[1, ]
      expect_equal(derive_playoff_round(first_sb), "super_bowl")
    }
  }, error = function(e) {
    skip(sprintf("Could not load 2023 schedule: %s", e$message))
  })
})
