context("Bet governance ordering and determinism")

test_that("missing odds always passes first with canonical reason", {
  g <- apply_bet_governance(
    ev = 0.08,
    prob = 0.60,
    odds = NA_real_,
    min_stake = 0.01,
    kelly_fraction = 0.125,
    max_stake = 0.02,
    is_placeholder_odds = TRUE
  )

  expect_equal(g$recommendation, "Pass")
  expect_equal(g$pass_reason, "Market odds missing/placeholder")
  expect_equal(g$final_stake_pct, 0)
})

test_that("negative EV passes before stake checks", {
  g <- apply_bet_governance(
    ev = -0.01,
    prob = 0.40,
    odds = -110,
    min_stake = 0.01,
    kelly_fraction = 0.125,
    max_stake = 0.02
  )

  expect_equal(g$recommendation, "Pass")
  expect_equal(g$pass_reason, "Negative EV")
  expect_equal(g$final_stake_pct, 0)
})

test_that("positive EV uses 1/8 Kelly then 2% cap then min stake", {
  g <- apply_bet_governance(
    ev = 0.03,
    prob = 0.55,
    odds = -110,
    min_stake = 0.01,
    kelly_fraction = 0.125,
    max_stake = 0.02
  )

  expect_true(is.finite(g$raw_kelly_pct))
  expect_true(g$capped_stake_pct <= 0.02)
  expect_equal(g$recommendation, "Pass")
  expect_equal(g$pass_reason, "Stake below minimum")
  expect_equal(g$final_stake_pct, 0)
})

test_that("SEA @ NE contradiction pattern resolves deterministically", {
  # Pattern: placeholder odds + positive EV + plausible prob should still PASS for missing odds.
  # This reproduces contradictory states where EV looked positive but no market odds existed.
  matchup <- "SEA @ NE"
  expect_equal(trimws(strsplit(matchup, "@")[[1]])[2], "NE")

  g <- apply_bet_governance(
    ev = 0.12,
    prob = 0.58,
    odds = NA_real_,
    min_stake = 0.01,
    kelly_fraction = 0.125,
    max_stake = 0.02,
    is_placeholder_odds = TRUE
  )

  expect_equal(g$recommendation, "Pass")
  expect_equal(g$pass_reason, "Market odds missing/placeholder")
  expect_equal(g$final_stake_pct, 0)
  expect_true(is.na(g$raw_kelly_pct))
  expect_true(is.na(g$capped_stake_pct))
})

test_that("max edge gate forces pass with explicit reason", {
  g <- apply_bet_governance(
    ev = 0.15,
    prob = 0.55,
    odds = -110,
    min_stake = 0.01,
    kelly_fraction = 0.125,
    max_stake = 0.02,
    max_edge = 0.10
  )

  expect_equal(g$recommendation, "Pass")
  expect_equal(g$pass_reason, "Edge too large (>10%)")
  expect_equal(g$final_stake_pct, 0)
})
