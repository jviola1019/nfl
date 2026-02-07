# =============================================================================
# Anytime TD Negative Binomial probability tests
# =============================================================================

library(testthat)


if (!exists("simulate_correlated_prop", mode = "function")) {
  tryCatch({
    source(file.path(.test_project_root, "R", "correlated_props.R"))
  }, error = function(e) NULL)
}

test_that("anytime TD probability matches analytic NB p0", {
  skip_if_not(exists("simulate_correlated_prop", mode = "function"),
              "simulate_correlated_prop() not available")

  set.seed(123)
  baseline <- 0.4
  overdispersion <- if (exists("TD_OVERDISPERSION")) TD_OVERDISPERSION else 1.5
  size <- baseline / (overdispersion - 1)
  size <- pmax(size, 0.1)

  sims <- simulate_correlated_prop(
    baseline = baseline,
    sd = baseline * 0.5,
    z_game = NULL,
    rho = 0,
    n_trials = 50000,
    distribution = "negbin"
  )

  p_anytime <- mean(sims >= 1)
  p0 <- (size / (size + baseline))^size
  expected <- 1 - p0

  expect_equal(p_anytime, expected, tolerance = 0.02)
})
