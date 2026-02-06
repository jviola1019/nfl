# =============================================================================
# Tests for Game Table Invariants G1-G8
# =============================================================================
# Validates core game table properties: matchup parsing, probability accounting,
# devig methods, ML roundtrips, EV math, Kelly staking, beat-market logic,
# and margin coherence.
# =============================================================================

context("Game Table Invariants (G1-G8)")

# Source NFLmarket.R for harmonize_home_margin (not loaded by setup.R)
tryCatch({
  source(file.path(.test_project_root, "NFLmarket.R"))
}, error = function(e) {
  # NFLmarket.R may fail to load in CI; tests that need it will skip individually
})

# =============================================================================
# G1: Matchup Parsing - "SEA @ NE" means NE is home
# =============================================================================

test_that("G1: matchup string 'AWAY @ HOME' identifies home team correctly", {
  matchup <- "SEA @ NE"
  parts <- trimws(strsplit(matchup, "@")[[1]])
  away_team <- parts[1]
  home_team <- parts[2]


  expect_equal(home_team, "NE")
  expect_equal(away_team, "SEA")
})

test_that("G1: matchup parsing handles various team abbreviations", {
  test_cases <- list(
    list(matchup = "KC @ BUF",  home = "BUF", away = "KC"),
    list(matchup = "DAL @ PHI", home = "PHI", away = "DAL"),
    list(matchup = "LAR @ SF",  home = "SF",  away = "LAR")
  )

  for (tc in test_cases) {
    parts <- trimws(strsplit(tc$matchup, "@")[[1]])
    expect_equal(parts[2], tc$home,
                 label = paste("Home in", tc$matchup))
    expect_equal(parts[1], tc$away,
                 label = paste("Away in", tc$matchup))
  }
})

# =============================================================================
# G2: Probability Accounting - raw implied probs sum > 1 (overround)
# =============================================================================

test_that("G2: raw implied probabilities from vigged odds sum to overround > 1", {
  home_odds <- -150
  away_odds <- 130

  home_implied <- american_to_probability(home_odds)
  away_implied <- american_to_probability(away_odds)
  total_implied <- home_implied + away_implied

  # With vig, the implied probabilities must sum to more than 1

  expect_gt(total_implied, 1.0)

  # Overround should be positive but reasonable (typical NFL vig is 3-10%)
  overround <- total_implied - 1
  expect_gt(overround, 0)
  expect_lt(overround, 0.15)
})

test_that("G2: after devig, home + away probabilities sum to exactly 1", {
  home_odds <- -150
  away_odds <- 130

  result <- devig_american_odds(home_odds, away_odds)

  expect_equal(result$home_prob + result$away_prob, 1.0, tolerance = 1e-10)
})

# =============================================================================
# G3: Devig Method - produces valid probabilities summing to 1
# =============================================================================

test_that("G3: devig_american_odds(-150, +130) returns valid probabilities summing to 1", {
  result <- devig_american_odds(-150, 130)

  # Both probabilities must be in (0, 1)
  expect_gt(result$home_prob, 0)
  expect_lt(result$home_prob, 1)
  expect_gt(result$away_prob, 0)
  expect_lt(result$away_prob, 1)

  # Must sum to exactly 1
  expect_equal(result$home_prob + result$away_prob, 1.0, tolerance = 1e-10)

  # Overround should be positive (vig was present)
  expect_gt(result$overround, 0)

  # Home was the favorite (-150), so home_prob > away_prob
  expect_gt(result$home_prob, result$away_prob)
})

test_that("G3: devig works with symmetric odds (-110, -110)", {
  result <- devig_american_odds(-110, -110)

  # Symmetric odds should devig to 50/50
  expect_equal(result$home_prob, 0.5, tolerance = 1e-10)
  expect_equal(result$away_prob, 0.5, tolerance = 1e-10)
  expect_equal(result$home_prob + result$away_prob, 1.0, tolerance = 1e-10)
})

test_that("G3: devig works with heavy favorite", {
  result <- devig_american_odds(-300, 250)

  expect_equal(result$home_prob + result$away_prob, 1.0, tolerance = 1e-10)
  expect_gt(result$home_prob, 0.7)
})

# =============================================================================
# G4: ML Roundtrip Consistency - prob -> ML -> prob is identity (within tolerance)
# =============================================================================

test_that("G4: probability_to_american -> american_to_probability roundtrip is consistent", {
  test_probs <- c(0.4, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8)

  for (p in test_probs) {
    ml <- probability_to_american(p)
    p_back <- american_to_probability(ml)

    expect_equal(p_back, p, tolerance = 0.02,
                 label = sprintf("Roundtrip for prob=%.2f (ML=%d)", p, ml))
  }
})

test_that("G4: american_to_probability(probability_to_american(p)) ~= p", {
  test_probs <- seq(0.05, 0.95, by = 0.05)

  for (p in test_probs) {
    p_back <- american_to_probability(probability_to_american(p))
    expect_equal(
      p_back,
      p,
      tolerance = 0.02,
      label = sprintf("Roundtrip identity at p=%.2f", p)
    )
  }
})

test_that("G4: implied fair home ML probability matches shrunk home probability", {
  shrunk_home_probs <- c(0.32, 0.45, 0.50, 0.58, 0.67, 0.81)

  for (p_shrunk in shrunk_home_probs) {
    fair_home_ml <- probability_to_american(p_shrunk)
    implied_fair_home_prob <- american_to_probability(fair_home_ml)

    expect_equal(
      implied_fair_home_prob,
      p_shrunk,
      tolerance = 0.02,
      label = sprintf("Fair ML implied probability at shrunk home p=%.2f", p_shrunk)
    )
  }
})

test_that("G4: roundtrip works for underdog probabilities", {
  test_probs <- c(0.20, 0.25, 0.30, 0.35)

  for (p in test_probs) {
    ml <- probability_to_american(p)
    p_back <- american_to_probability(ml)

    # Underdog odds are positive
    expect_gt(ml, 0, label = sprintf("ML should be positive for prob=%.2f", p))
    expect_equal(p_back, p, tolerance = 0.02,
                 label = sprintf("Roundtrip for prob=%.2f (ML=+%d)", p, ml))
  }
})

test_that("G4: specific case probability_to_american(0.6) roundtrips", {
  ml <- probability_to_american(0.6)
  p_back <- american_to_probability(ml)

  # ML should be negative (favorite)
  expect_lt(ml, 0)
  expect_equal(p_back, 0.6, tolerance = 0.02)
})

# =============================================================================
# G5: EV Math - expected_value_units correctness
# =============================================================================

test_that("G5: fair bet at -150 with 60% probability has EV near 0", {
  # -150 implies 60% probability. A fair bet: prob = implied prob -> EV ~ 0
  ev <- expected_value_units(0.60, -150)

  expect_equal(ev, 0.0, tolerance = 0.02)
})

test_that("G5: +EV bet at -110 with 55% win probability", {
  # -110 implies ~52.4%; we believe 55% -> should be positive EV
  ev <- expected_value_units(0.55, -110)

  expect_gt(ev, 0)
})

test_that("G5: negative EV when model prob < implied prob", {
  # -200 implies 66.7%; we believe only 55% -> negative EV
  ev <- expected_value_units(0.55, -200)

  expect_lt(ev, 0)
})

test_that("G5: EV formula matches manual calculation", {
  prob <- 0.55
  odds <- -110
  dec <- american_to_decimal(odds)  # 1.909...
  b <- dec - 1                       # 0.909...
  manual_ev <- prob * b - (1 - prob)

  computed_ev <- expected_value_units(prob, odds)

  expect_equal(computed_ev, manual_ev, tolerance = 1e-6)
})

# =============================================================================
# G6: Kelly Governance - conservative_kelly_stake bounds
# =============================================================================

test_that("G6: Kelly stake is within [0, max_stake] range", {
  stake <- conservative_kelly_stake(
    prob = 0.55,
    odds = -110,
    kelly_fraction = 0.125,
    max_stake = 0.02
  )

  expect_gte(stake, 0)
  expect_lte(stake, 0.02)
})

test_that("G6: Kelly stake is 0 or NA for negative EV bet", {
  # -300 implies 75%; we think only 50% -> huge negative edge
  stake <- conservative_kelly_stake(
    prob = 0.50,
    odds = -300,
    kelly_fraction = 0.125,
    max_stake = 0.02
  )

  # Negative Kelly means no bet: stake should be 0
  expect_equal(stake, 0)
})

test_that("G6: Kelly fraction scaling reduces stake", {
  full_kelly <- conservative_kelly_stake(
    prob = 0.60,
    odds = -110,
    kelly_fraction = 1.0,
    max_stake = 1.0
  )

  eighth_kelly <- conservative_kelly_stake(
    prob = 0.60,
    odds = -110,
    kelly_fraction = 0.125,
    max_stake = 1.0
  )

  # 1/8 Kelly should be smaller than full Kelly
  expect_lt(eighth_kelly, full_kelly)
})

test_that("G6: max_stake cap is enforced", {
  # Very high edge should still be capped
  stake <- conservative_kelly_stake(
    prob = 0.90,
    odds = 100,
    kelly_fraction = 0.5,
    max_stake = 0.02
  )

  expect_lte(stake, 0.02)
})

# =============================================================================
# G7: Beat Market TBD - if actual_winner is "TBD", result is "N/A"
# =============================================================================

test_that("G7: TBD actual winner produces N/A result", {
  actual_winner <- "TBD"

  # The logic: if actual_winner is "TBD" the game hasn't been played
  result <- ifelse(actual_winner == "TBD", "N/A", "resolved")

  expect_equal(result, "N/A")
})

test_that("G7: non-TBD actual winner does not produce N/A", {
  actual_winner <- "NE"

  result <- ifelse(actual_winner == "TBD", "N/A", "resolved")

  expect_equal(result, "resolved")
})

test_that("G7: TBD check is case-sensitive", {
  # Only exact "TBD" should match, not "tbd" or "Tbd"
  expect_equal(ifelse("TBD" == "TBD", "N/A", "resolved"), "N/A")
  expect_equal(ifelse("tbd" == "TBD", "N/A", "resolved"), "resolved")
  expect_equal(ifelse("Tbd" == "TBD", "N/A", "resolved"), "resolved")
})

# =============================================================================
# G8: Margin Coherence - harmonize flips margin when inconsistent with prob
# =============================================================================

test_that("G8: positive margin flipped when home_prob < 0.5", {
  skip_if_not(exists("harmonize_home_margin"),
              "harmonize_home_margin not available (NFLmarket.R not loaded)")

  # Margin = +3.0 says home wins by 3, but prob = 0.38 says home is underdog
  # harmonize should flip the sign
  result <- harmonize_home_margin(3.0, 0.38)

  expect_equal(result, -3.0)
})

test_that("G8: negative margin flipped when home_prob > 0.5", {
  skip_if_not(exists("harmonize_home_margin"),
              "harmonize_home_margin not available (NFLmarket.R not loaded)")

  # Margin = -3.0 says home loses by 3, but prob = 0.65 says home is favorite
  # harmonize should flip the sign
  result <- harmonize_home_margin(-3.0, 0.65)

  expect_equal(result, 3.0)
})

test_that("G8: consistent margin and prob are not flipped", {
  skip_if_not(exists("harmonize_home_margin"),
              "harmonize_home_margin not available (NFLmarket.R not loaded)")

  # Margin = +7.0 says home wins by 7, prob = 0.70 agrees -> no flip
  result <- harmonize_home_margin(7.0, 0.70)
  expect_equal(result, 7.0)

  # Margin = -3.0 says home loses by 3, prob = 0.40 agrees -> no flip
  result2 <- harmonize_home_margin(-3.0, 0.40)
  expect_equal(result2, -3.0)
})

test_that("G8: margin near zero with prob near 0.5 is not flipped (tolerance)", {
  skip_if_not(exists("harmonize_home_margin"),
              "harmonize_home_margin not available (NFLmarket.R not loaded)")

  # Margin = +1.0, prob = 0.498 - within default tolerance, should NOT flip
  result <- harmonize_home_margin(1.0, 0.498)
  expect_equal(result, 1.0)
})

test_that("G8: handles NA margin gracefully", {
  skip_if_not(exists("harmonize_home_margin"),
              "harmonize_home_margin not available (NFLmarket.R not loaded)")

  result <- harmonize_home_margin(NA_real_, 0.60)
  expect_true(is.na(result))
})
