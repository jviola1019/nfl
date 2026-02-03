# =============================================================================
# FILE: core/simulation_engine.R
# PURPOSE: Sport-agnostic Monte Carlo simulation engine
#
# VERSION: 2.7.0
# LAST UPDATED: 2026-02-02
#
# DESCRIPTION:
#   Generic simulation framework that can be extended for any sport.
#   Uses negative binomial distributions with Gaussian copula correlation.
#   This is the blueprint module for multi-sport prediction systems.
#
# EXPORTS:
#   - simulate_game(): Generic game simulation
#   - nb_size_from_musd(): NB size parameter calculation
#   - gaussian_copula_sample(): Correlated uniform generation
# =============================================================================

suppressPackageStartupMessages({
  if (!requireNamespace("randtoolbox", quietly = TRUE)) {
    stop("Package 'randtoolbox' required for simulation engine")
  }
})

#' Calculate negative binomial size parameter from mean and SD
#'
#' Converts mean and standard deviation to the size parameter
#' used by R's negative binomial distribution functions.
#'
#' @param mu Expected value (mean)
#' @param sd Standard deviation
#' @return Size parameter for rnbinom/qnbinom
#'
#' @details
#' For negative binomial: var = mu + mu^2/size
#' Solving for size: size = mu^2 / (var - mu)
#'
#' @export
nb_size_from_musd <- function(mu, sd) {
  var <- sd^2
  if (var <= mu) {
    # Variance too low for NB, use Poisson-like large size
    return(1e6)
  }
  mu^2 / (var - mu)
}

#' Generate correlated uniform samples via Gaussian copula
#'
#' Creates pairs of correlated uniform random variables using
#' Gaussian copula with Sobol quasi-random sequences.
#'
#' @param n Number of samples
#' @param rho Correlation parameter (-1 to 1)
#' @param seed Random seed for reproducibility
#' @return Matrix with 2 columns of correlated uniforms [0,1]
#'
#' @details
#' Uses Sobol QMC sequences for faster convergence than pseudo-random.
#' Applies antithetic variates for variance reduction.
#'
#' @export
gaussian_copula_sample <- function(n, rho, seed = 12345L) {
  n_half <- ceiling(n / 2)

  # Sobol quasi-random sequence
  U <- randtoolbox::sobol(n = n_half, dim = 2, scrambling = 0, seed = seed, normal = FALSE)

  # Inverse normal transform
  Z1 <- qnorm(U[, 1])
  Z2 <- qnorm(U[, 2])

  # Apply correlation via Cholesky
  Z2_corr <- rho * Z1 + sqrt(1 - rho^2) * Z2

  # Back to uniforms via normal CDF
  U1 <- pnorm(Z1)
  U2 <- pnorm(Z2_corr)

  # Antithetic variates (1 - U gives another valid sample)
  U1_anti <- 1 - U1
  U2_anti <- 1 - U2

  # Combine original and antithetic
  U1_full <- c(U1, U1_anti)[1:n]
  U2_full <- c(U2, U2_anti)[1:n]

  cbind(U1_full, U2_full)
}

#' Generic Monte Carlo game simulation
#'
#' Sport-agnostic simulation engine that generates correlated score
#' distributions using specified statistical distributions.
#'
#' @param home_expected Expected home team score
#' @param away_expected Expected away team score
#' @param home_variance Home score variance (or SD, specify with variance_is_sd)
#' @param away_variance Away score variance (or SD, specify with variance_is_sd)
#' @param score_correlation Correlation between scores (-1 to 1)
#' @param n_trials Number of Monte Carlo iterations
#' @param distribution Score distribution: "negbin", "poisson", or "normal"
#' @param variance_is_sd If TRUE, variance params are standard deviations
#' @param score_cap Maximum score cap
#' @param seed Random seed
#'
#' @return List containing:
#'   \item{home_scores}{Vector of simulated home scores}
#'   \item{away_scores}{Vector of simulated away scores}
#'   \item{p_home_win}{Home win probability}
#'   \item{p_away_win}{Away win probability}
#'   \item{p_tie}{Tie probability (before any tiebreaker)}
#'   \item{margin_mean}{Mean margin (home - away)}
#'   \item{margin_sd}{SD of margin}
#'   \item{total_mean}{Mean combined score}
#'   \item{total_sd}{SD of combined score}
#'
#' @examples
#' \dontrun{
#'   # NFL game simulation
#'   result <- simulate_game(
#'     home_expected = 24.5,
#'     away_expected = 21.0,
#'     home_variance = 110,  # variance
#'     away_variance = 100,
#'     score_correlation = -0.15,
#'     distribution = "negbin"
#'   )
#'
#'   # NBA game simulation (normal distribution for high-scoring)
#'   result <- simulate_game(
#'     home_expected = 112,
#'     away_expected = 108,
#'     home_variance = 12,  # SD
#'     away_variance = 12,
#'     score_correlation = -0.10,
#'     distribution = "normal",
#'     variance_is_sd = TRUE
#'   )
#' }
#'
#' @export
simulate_game <- function(home_expected,
                          away_expected,
                          home_variance,
                          away_variance,
                          score_correlation = -0.15,
                          n_trials = 100000L,
                          distribution = c("negbin", "poisson", "normal"),
                          variance_is_sd = FALSE,
                          score_cap = 100L,
                          seed = 12345L) {

  distribution <- match.arg(distribution)

  # Convert SD to variance if needed
  if (variance_is_sd) {
    home_sd <- home_variance
    away_sd <- away_variance
    home_variance <- home_sd^2
    away_variance <- away_sd^2
  } else {
    home_sd <- sqrt(home_variance)
    away_sd <- sqrt(away_variance)
  }

  # Input validation
  if (!is.finite(home_expected) || home_expected <= 0) {
    warning("simulate_game: invalid home_expected, using 21.5")
    home_expected <- 21.5
  }
  if (!is.finite(away_expected) || away_expected <= 0) {
    warning("simulate_game: invalid away_expected, using 21.5")
    away_expected <- 21.5
  }

  # Generate correlated uniforms
  U <- gaussian_copula_sample(n_trials, score_correlation, seed)

  # Transform to scores based on distribution
  if (distribution == "negbin") {
    size_home <- nb_size_from_musd(home_expected, home_sd)
    size_away <- nb_size_from_musd(away_expected, away_sd)

    home_scores <- qnbinom(U[, 1], size = size_home, mu = home_expected)
    away_scores <- qnbinom(U[, 2], size = size_away, mu = away_expected)

  } else if (distribution == "poisson") {
    home_scores <- qpois(U[, 1], lambda = home_expected)
    away_scores <- qpois(U[, 2], lambda = away_expected)

  } else if (distribution == "normal") {
    home_scores <- round(qnorm(U[, 1], mean = home_expected, sd = home_sd))
    away_scores <- round(qnorm(U[, 2], mean = away_expected, sd = away_sd))
    home_scores <- pmax(0, home_scores)  # No negative scores
    away_scores <- pmax(0, away_scores)
  }

  # Apply score cap
  home_scores <- pmin(home_scores, score_cap)
  away_scores <- pmin(away_scores, score_cap)

  # Calculate probabilities and statistics
  margin <- home_scores - away_scores
  total <- home_scores + away_scores

  n_valid <- length(margin)
  p_home_win <- sum(margin > 0) / n_valid
  p_away_win <- sum(margin < 0) / n_valid
  p_tie <- sum(margin == 0) / n_valid

  list(
    home_scores = home_scores,
    away_scores = away_scores,
    p_home_win = p_home_win,
    p_away_win = p_away_win,
    p_tie = p_tie,
    margin_mean = mean(margin),
    margin_sd = sd(margin),
    total_mean = mean(total),
    total_sd = sd(total),
    n_trials = n_valid,
    distribution = distribution
  )
}

#' Calculate expected value for a bet
#'
#' Sport-agnostic EV calculation.
#'
#' @param prob Estimated win probability
#' @param decimal_odds Decimal odds (e.g., 2.5 for +150)
#' @return Expected value as decimal (0.10 = 10% edge)
#' @export
calculate_ev <- function(prob, decimal_odds) {
  prob * decimal_odds - 1
}

#' Convert American odds to decimal
#'
#' @param american American odds (e.g., -150, +200)
#' @return Decimal odds
#' @export
american_to_decimal <- function(american) {
  ifelse(american >= 0,
         1 + american / 100,
         1 + 100 / abs(american))
}

#' Calculate Kelly criterion stake
#'
#' @param prob Win probability
#' @param decimal_odds Decimal odds
#' @param kelly_fraction Fraction of Kelly (default: 0.125 = 1/8)
#' @param max_stake Maximum stake as fraction of bankroll
#' @return Recommended stake
#' @export
kelly_stake <- function(prob, decimal_odds, kelly_fraction = 0.125, max_stake = 0.02) {
  b <- decimal_odds - 1
  q <- 1 - prob

  if (b <= 0 || !is.finite(b)) return(0)

  kelly_full <- (prob * b - q) / b
  if (kelly_full <= 0) return(0)

  min(kelly_full * kelly_fraction, max_stake)
}
