# =============================================================================
# Advanced Calibration Refinement Methods
# Multiple calibration approaches to improve probability predictions
# R Version: 4.5.1+
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(isotone)
  library(boot)
  library(mgcv)
  library(gbm)
})

# R 4.5.1 compatibility
if (getRversion() >= "4.5.0") {
  suppressWarnings(RNGversion("4.5.0"))
}
set.seed(471)

cat("\n")
cat("================================================================================\n")
cat("ADVANCED CALIBRATION REFINEMENT\n")
cat("Multiple methods to improve probability predictions\n")
cat("================================================================================\n\n")

# =============================================================================
# SECTION 1: Load Predictions and Outcomes
# =============================================================================

cat("SECTION 1: Loading Data for Calibration\n")
cat("----------------------------------------\n\n")

#' Load historical predictions and outcomes
#'
#' @param seasons Seasons to load
#' @return Data frame with predictions and outcomes
load_calibration_data <- function(seasons = c(2022, 2023)) {

  cat("Loading historical data for calibration...\n")

  # Load schedules
  sched <- tryCatch({
    load_schedules(seasons = seasons)
  }, error = function(e) {
    cat("Warning: Could not load schedules, creating mock data\n")
    return(create_mock_calibration_data(seasons))
  })

  # Filter to completed regular season games
  data <- sched %>%
    filter(game_type == "REG",
           !is.na(if("home_score" %in% names(.)) home_score else score_home)) %>%
    transmute(
      season = season,
      week = week,
      game_id = game_id,
      home_team = if("home_team" %in% names(.)) home_team else team_home,
      away_team = if("away_team" %in% names(.)) away_team else team_away,
      home_score = if("home_score" %in% names(.)) home_score else score_home,
      away_score = if("away_score" %in% names(.)) away_score else score_away,
      home_win = as.integer(home_score > away_score),
      # Simulate model predictions (in practice, load from NFLsimulation.R)
      raw_win_prob = pnorm((home_score - away_score + rnorm(n(), 0, 8)) / 13),
      raw_win_prob = pmin(pmax(raw_win_prob, 0.01), 0.99)
    )

  cat(sprintf("Loaded %d games for calibration training\n\n", nrow(data)))

  return(data)
}

#' Create mock calibration data
create_mock_calibration_data <- function(seasons) {
  teams <- c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE",
             "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC",
             "LAC", "LAR", "LV", "MIA", "MIN", "NE", "NO", "NYG",
             "NYJ", "PHI", "PIT", "SEA", "SF", "TB", "TEN", "WAS")

  n_games <- 500

  tibble(
    season = sample(seasons, n_games, replace = TRUE),
    week = sample(1:18, n_games, replace = TRUE),
    game_id = paste0("mock_", 1:n_games),
    home_team = sample(teams, n_games, replace = TRUE),
    away_team = sample(teams, n_games, replace = TRUE),
    home_score = round(rnorm(n_games, mean = 23, sd = 10)),
    away_score = round(rnorm(n_games, mean = 21, sd = 10)),
    home_win = as.integer(home_score > away_score),
    raw_win_prob = rbeta(n_games, 2, 2)  # Uncalibrated probabilities
  ) %>%
    filter(home_team != away_team) %>%
    mutate(
      home_score = pmax(home_score, 0),
      away_score = pmax(away_score, 0),
      # Make probabilities somewhat predictive
      raw_win_prob = 0.3 * raw_win_prob + 0.7 * home_win
    )
}

cal_data <- load_calibration_data(seasons = c(2022, 2023))

# =============================================================================
# SECTION 2: Method 1 - Isotonic Regression (Current Method)
# =============================================================================

cat("SECTION 2: Method 1 - Isotonic Regression\n")
cat("-------------------------------------------\n\n")

#' Apply isotonic regression calibration
#'
#' @param train_data Training data with predictions and outcomes
#' @param test_data Test data to calibrate
#' @return Calibrated probabilities
calibrate_isotonic <- function(train_data, test_data = NULL) {

  cat("Fitting isotonic regression...\n")

  # Fit isotonic regression
  iso_fit <- isoreg(train_data$raw_win_prob, train_data$home_win)

  # Function to predict
  predict_isotonic <- function(probs) {
    # Linear interpolation for isotonic fit
    approx(iso_fit$x, iso_fit$yf, xout = probs,
           yleft = min(iso_fit$yf), yright = max(iso_fit$yf))$y
  }

  # Calibrate training data
  train_cal <- train_data %>%
    mutate(
      iso_prob = predict_isotonic(raw_win_prob),
      iso_prob = pmin(pmax(iso_prob, 0.01), 0.99),  # Bound to [0.01, 0.99]
      method = "isotonic"
    )

  # Calculate metrics
  brier_raw <- mean((train_cal$home_win - train_cal$raw_win_prob)^2)
  brier_cal <- mean((train_cal$home_win - train_cal$iso_prob)^2)

  cat(sprintf("  Raw Brier score: %.4f\n", brier_raw))
  cat(sprintf("  Calibrated Brier: %.4f\n", brier_cal))
  cat(sprintf("  Improvement: %.4f (%.1f%% reduction)\n\n",
              brier_raw - brier_cal,
              100 * (brier_raw - brier_cal) / brier_raw))

  # Calibrate test data if provided
  if (!is.null(test_data)) {
    test_cal <- test_data %>%
      mutate(
        iso_prob = predict_isotonic(raw_win_prob),
        iso_prob = pmin(pmax(iso_prob, 0.01), 0.99)
      )
    return(list(train = train_cal, test = test_cal, model = iso_fit))
  }

  return(list(train = train_cal, model = iso_fit))
}

iso_results <- calibrate_isotonic(cal_data)

# =============================================================================
# SECTION 3: Method 2 - Platt Scaling (Logistic Calibration)
# =============================================================================

cat("SECTION 3: Method 2 - Platt Scaling\n")
cat("-------------------------------------\n\n")

#' Apply Platt scaling (logistic calibration)
#'
#' @param train_data Training data
#' @param test_data Test data
#' @return Calibrated probabilities
calibrate_platt <- function(train_data, test_data = NULL) {

  cat("Fitting Platt scaling (logistic calibration)...\n")

  # Convert probabilities to logits
  train_data <- train_data %>%
    mutate(
      raw_logit = log(raw_win_prob / (1 - raw_win_prob))
    )

  # Fit logistic regression
  platt_model <- glm(home_win ~ raw_logit, data = train_data, family = binomial)

  cat("  Calibration parameters:\n")
  cat(sprintf("    Intercept: %.4f\n", coef(platt_model)[1]))
  cat(sprintf("    Slope: %.4f\n", coef(platt_model)[2]))

  # Ideal calibration: intercept ≈ 0, slope ≈ 1
  if (abs(coef(platt_model)[1]) < 0.1 && abs(coef(platt_model)[2] - 1.0) < 0.1) {
    cat("    Assessment: Already well-calibrated!\n")
  } else if (coef(platt_model)[2] < 1.0) {
    cat("    Assessment: Model is overconfident (slope < 1)\n")
  } else {
    cat("    Assessment: Model is underconfident (slope > 1)\n")
  }

  # Calibrate training data
  train_cal <- train_data %>%
    mutate(
      platt_prob = predict(platt_model, newdata = ., type = "response"),
      platt_prob = pmin(pmax(platt_prob, 0.01), 0.99)
    )

  # Calculate metrics
  brier_raw <- mean((train_cal$home_win - train_cal$raw_win_prob)^2)
  brier_platt <- mean((train_cal$home_win - train_cal$platt_prob)^2)

  cat(sprintf("\n  Raw Brier score: %.4f\n", brier_raw))
  cat(sprintf("  Platt Brier: %.4f\n", brier_platt))
  cat(sprintf("  Improvement: %.4f (%.1f%% reduction)\n\n",
              brier_raw - brier_platt,
              100 * (brier_raw - brier_platt) / brier_raw))

  # Calibrate test data if provided
  if (!is.null(test_data)) {
    test_cal <- test_data %>%
      mutate(
        raw_logit = log(raw_win_prob / (1 - raw_win_prob)),
        platt_prob = predict(platt_model, newdata = ., type = "response"),
        platt_prob = pmin(pmax(platt_prob, 0.01), 0.99)
      )
    return(list(train = train_cal, test = test_cal, model = platt_model))
  }

  return(list(train = train_cal, model = platt_model))
}

platt_results <- calibrate_platt(cal_data)

# =============================================================================
# SECTION 4: Method 3 - Beta Calibration
# =============================================================================

cat("SECTION 4: Method 3 - Beta Calibration\n")
cat("----------------------------------------\n\n")

#' Apply beta calibration
#'
#' @param train_data Training data
#' @param test_data Test data
#' @return Calibrated probabilities
calibrate_beta <- function(train_data, test_data = NULL) {

  cat("Fitting beta calibration...\n")

  # Transform to beta distribution parameters
  # Beta calibration: fit a + b*log(p/(1-p)) + c*log((1-p)/p)
  train_data <- train_data %>%
    mutate(
      logit_p = log(raw_win_prob / (1 - raw_win_prob)),
      logit_1mp = log((1 - raw_win_prob) / raw_win_prob)
    )

  # Fit beta calibration model
  beta_model <- glm(home_win ~ logit_p + logit_1mp,
                    data = train_data,
                    family = binomial)

  cat("  Calibration parameters:\n")
  cat(sprintf("    Intercept: %.4f\n", coef(beta_model)[1]))
  cat(sprintf("    Beta_a (logit_p): %.4f\n", coef(beta_model)[2]))
  cat(sprintf("    Beta_b (logit_1-p): %.4f\n", coef(beta_model)[3]))

  # Calibrate training data
  train_cal <- train_data %>%
    mutate(
      beta_prob = predict(beta_model, newdata = ., type = "response"),
      beta_prob = pmin(pmax(beta_prob, 0.01), 0.99)
    )

  # Calculate metrics
  brier_raw <- mean((train_cal$home_win - train_cal$raw_win_prob)^2)
  brier_beta <- mean((train_cal$home_win - train_cal$beta_prob)^2)

  cat(sprintf("\n  Raw Brier score: %.4f\n", brier_raw))
  cat(sprintf("  Beta Brier: %.4f\n", brier_beta))
  cat(sprintf("  Improvement: %.4f (%.1f%% reduction)\n\n",
              brier_raw - brier_beta,
              100 * (brier_raw - brier_beta) / brier_raw))

  # Calibrate test data if provided
  if (!is.null(test_data)) {
    test_cal <- test_data %>%
      mutate(
        logit_p = log(raw_win_prob / (1 - raw_win_prob)),
        logit_1mp = log((1 - raw_win_prob) / raw_win_prob),
        beta_prob = predict(beta_model, newdata = ., type = "response"),
        beta_prob = pmin(pmax(beta_prob, 0.01), 0.99)
      )
    return(list(train = train_cal, test = test_cal, model = beta_model))
  }

  return(list(train = train_cal, model = beta_model))
}

beta_results <- calibrate_beta(cal_data)

# =============================================================================
# SECTION 5: Method 4 - Spline Calibration (GAM)
# =============================================================================

cat("SECTION 5: Method 4 - Spline Calibration (GAM)\n")
cat("------------------------------------------------\n\n")

#' Apply spline calibration using GAM
#'
#' @param train_data Training data
#' @param test_data Test data
#' @return Calibrated probabilities
calibrate_spline <- function(train_data, test_data = NULL) {

  cat("Fitting spline calibration (GAM)...\n")

  # Fit GAM with spline
  spline_model <- gam(home_win ~ s(raw_win_prob, bs = "cs", k = 10),
                      data = train_data,
                      family = binomial)

  cat(sprintf("  Model deviance explained: %.1f%%\n",
              summary(spline_model)$dev.expl * 100))

  # Calibrate training data
  train_cal <- train_data %>%
    mutate(
      spline_prob = predict(spline_model, newdata = ., type = "response"),
      spline_prob = pmin(pmax(spline_prob, 0.01), 0.99)
    )

  # Calculate metrics
  brier_raw <- mean((train_cal$home_win - train_cal$raw_win_prob)^2)
  brier_spline <- mean((train_cal$home_win - train_cal$spline_prob)^2)

  cat(sprintf("\n  Raw Brier score: %.4f\n", brier_raw))
  cat(sprintf("  Spline Brier: %.4f\n", brier_spline))
  cat(sprintf("  Improvement: %.4f (%.1f%% reduction)\n\n",
              brier_raw - brier_spline,
              100 * (brier_raw - brier_spline) / brier_raw))

  # Calibrate test data if provided
  if (!is.null(test_data)) {
    test_cal <- test_data %>%
      mutate(
        spline_prob = predict(spline_model, newdata = ., type = "response"),
        spline_prob = pmin(pmax(spline_prob, 0.01), 0.99)
      )
    return(list(train = train_cal, test = test_cal, model = spline_model))
  }

  return(list(train = train_cal, model = spline_model))
}

spline_results <- calibrate_spline(cal_data)

# =============================================================================
# SECTION 6: Method 5 - Ensemble Calibration
# =============================================================================

cat("SECTION 6: Method 5 - Ensemble Calibration\n")
cat("--------------------------------------------\n\n")

#' Ensemble calibration (weighted average of methods)
#'
#' @param iso_data Isotonic calibrated data
#' @param platt_data Platt calibrated data
#' @param beta_data Beta calibrated data
#' @param spline_data Spline calibrated data
#' @return Ensemble calibrated probabilities
calibrate_ensemble <- function(iso_data, platt_data, beta_data, spline_data) {

  cat("Creating ensemble calibration...\n")

  # Combine all methods
  ensemble <- iso_data$train %>%
    select(game_id, home_win, raw_win_prob, iso_prob) %>%
    left_join(platt_data$train %>% select(game_id, platt_prob), by = "game_id") %>%
    left_join(beta_data$train %>% select(game_id, beta_prob), by = "game_id") %>%
    left_join(spline_data$train %>% select(game_id, spline_prob), by = "game_id")

  # Calculate weights based on inverse Brier scores
  brier_iso <- mean((ensemble$home_win - ensemble$iso_prob)^2, na.rm = TRUE)
  brier_platt <- mean((ensemble$home_win - ensemble$platt_prob)^2, na.rm = TRUE)
  brier_beta <- mean((ensemble$home_win - ensemble$beta_prob)^2, na.rm = TRUE)
  brier_spline <- mean((ensemble$home_win - ensemble$spline_prob)^2, na.rm = TRUE)

  # Inverse Brier weights (better models get more weight)
  total_inv_brier <- (1/brier_iso + 1/brier_platt + 1/brier_beta + 1/brier_spline)

  w_iso <- (1/brier_iso) / total_inv_brier
  w_platt <- (1/brier_platt) / total_inv_brier
  w_beta <- (1/brier_beta) / total_inv_brier
  w_spline <- (1/brier_spline) / total_inv_brier

  cat("  Ensemble weights (based on inverse Brier):\n")
  cat(sprintf("    Isotonic: %.3f\n", w_iso))
  cat(sprintf("    Platt: %.3f\n", w_platt))
  cat(sprintf("    Beta: %.3f\n", w_beta))
  cat(sprintf("    Spline: %.3f\n", w_spline))

  # Create weighted ensemble
  ensemble <- ensemble %>%
    mutate(
      ensemble_prob = w_iso * iso_prob +
                     w_platt * platt_prob +
                     w_beta * beta_prob +
                     w_spline * spline_prob,
      ensemble_prob = pmin(pmax(ensemble_prob, 0.01), 0.99)
    )

  # Calculate metrics
  brier_raw <- mean((ensemble$home_win - ensemble$raw_win_prob)^2)
  brier_ensemble <- mean((ensemble$home_win - ensemble$ensemble_prob)^2)

  cat(sprintf("\n  Raw Brier score: %.4f\n", brier_raw))
  cat(sprintf("  Ensemble Brier: %.4f\n", brier_ensemble))
  cat(sprintf("  Improvement: %.4f (%.1f%% reduction)\n\n",
              brier_raw - brier_ensemble,
              100 * (brier_raw - brier_ensemble) / brier_raw))

  weights <- c(iso = w_iso, platt = w_platt, beta = w_beta, spline = w_spline)

  return(list(data = ensemble, weights = weights))
}

ensemble_results <- calibrate_ensemble(iso_results, platt_results,
                                       beta_results, spline_results)

# =============================================================================
# SECTION 7: Compare All Methods
# =============================================================================

cat("SECTION 7: Comparing All Calibration Methods\n")
cat("----------------------------------------------\n\n")

#' Compare all calibration methods
compare_calibration_methods <- function(ensemble_data) {

  # Calculate metrics for all methods
  results <- tibble(
    method = c("Raw (Uncalibrated)", "Isotonic", "Platt Scaling",
               "Beta Calibration", "Spline (GAM)", "Ensemble"),

    brier_score = c(
      mean((ensemble_data$home_win - ensemble_data$raw_win_prob)^2, na.rm = TRUE),
      mean((ensemble_data$home_win - ensemble_data$iso_prob)^2, na.rm = TRUE),
      mean((ensemble_data$home_win - ensemble_data$platt_prob)^2, na.rm = TRUE),
      mean((ensemble_data$home_win - ensemble_data$beta_prob)^2, na.rm = TRUE),
      mean((ensemble_data$home_win - ensemble_data$spline_prob)^2, na.rm = TRUE),
      mean((ensemble_data$home_win - ensemble_data$ensemble_prob)^2, na.rm = TRUE)
    ),

    log_loss = c(
      -mean(ensemble_data$home_win * log(ensemble_data$raw_win_prob + 1e-15) +
            (1 - ensemble_data$home_win) * log(1 - ensemble_data$raw_win_prob + 1e-15), na.rm = TRUE),
      -mean(ensemble_data$home_win * log(ensemble_data$iso_prob + 1e-15) +
            (1 - ensemble_data$home_win) * log(1 - ensemble_data$iso_prob + 1e-15), na.rm = TRUE),
      -mean(ensemble_data$home_win * log(ensemble_data$platt_prob + 1e-15) +
            (1 - ensemble_data$home_win) * log(1 - ensemble_data$platt_prob + 1e-15), na.rm = TRUE),
      -mean(ensemble_data$home_win * log(ensemble_data$beta_prob + 1e-15) +
            (1 - ensemble_data$home_win) * log(1 - ensemble_data$beta_prob + 1e-15), na.rm = TRUE),
      -mean(ensemble_data$home_win * log(ensemble_data$spline_prob + 1e-15) +
            (1 - ensemble_data$home_win) * log(1 - ensemble_data$spline_prob + 1e-15), na.rm = TRUE),
      -mean(ensemble_data$home_win * log(ensemble_data$ensemble_prob + 1e-15) +
            (1 - ensemble_data$home_win) * log(1 - ensemble_data$ensemble_prob + 1e-15), na.rm = TRUE)
    )
  ) %>%
    mutate(
      brier_improvement = brier_score[1] - brier_score,
      brier_pct_improvement = 100 * brier_improvement / brier_score[1],
      rank = rank(brier_score)
    ) %>%
    arrange(rank)

  cat("=== CALIBRATION METHOD COMPARISON ===\n\n")

  print(results %>%
         mutate(across(c(brier_score, log_loss, brier_improvement),
                      ~sprintf("%.5f", .)),
               brier_pct_improvement = sprintf("%.2f%%", brier_pct_improvement),
               rank = as.integer(rank)))
  cat("\n")

  return(results)
}

comparison <- compare_calibration_methods(ensemble_results$data)

# =============================================================================
# SECTION 8: Recommendations
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("CALIBRATION REFINEMENT SUMMARY\n")
cat("================================================================================\n\n")

best_method <- comparison$method[which.min(comparison$brier_score)]
best_improvement <- max(comparison$brier_pct_improvement)

cat(sprintf("BEST METHOD: %s\n", best_method))
cat(sprintf("Improvement over raw: %.2f%% Brier reduction\n\n", best_improvement))

cat("RECOMMENDATIONS:\n\n")

if (best_method == "Ensemble") {
  cat("  1. ✓ IMPLEMENT ensemble calibration for production\n")
  cat("     Combines strengths of all methods\n")
  cat("     Weights:\n")
  for (m in names(ensemble_results$weights)) {
    cat(sprintf("       %s: %.3f\n", m, ensemble_results$weights[m]))
  }
} else if (best_method == "Isotonic") {
  cat("  1. ✓ CONTINUE using isotonic regression (current method)\n")
  cat("     Already optimal for this data\n")
} else {
  cat(sprintf("  1. → SWITCH to %s for better calibration\n", best_method))
  cat(sprintf("     Provides %.2f%% improvement over current method\n", best_improvement))
}

cat("\n  2. → Apply calibration in nested CV to prevent overfitting\n")
cat("     (Already implemented in model_validation.R)\n")

cat("\n  3. → Monitor calibration curves weekly during 2025 season\n")
cat("     Recalibrate if systematic deviations emerge\n")

cat("\n  4. → Consider context-specific calibration:\n")
cat("     - Separate calibration for favorites vs underdogs\n")
cat("     - Division games vs non-division\n")
cat("     - Early season vs late season\n")

cat("\n  5. → Test temperature scaling for overconfidence:\n")
cat("     If model consistently too confident (slope < 1 in Platt)\n\n")

cat("IMPLEMENTATION NOTE:\n")
cat("  Current NFLsimulation.R uses isotonic regression.\n")
cat(sprintf("  Consider upgrading to %s for %.2f%% Brier improvement.\n\n",
            best_method, best_improvement))

cat("================================================================================\n")
cat("CALIBRATION ANALYSIS COMPLETE\n")
cat("================================================================================\n\n")

# Save results
CALIBRATION_RESULTS <- list(
  comparison = comparison,
  best_method = best_method,
  ensemble_weights = ensemble_results$weights,
  isotonic_model = iso_results$model,
  platt_model = platt_results$model,
  beta_model = beta_results$model,
  spline_model = spline_results$model,
  timestamp = Sys.time(),
  recommendation = sprintf("Use %s calibration for %.2f%% improvement",
                          best_method, best_improvement)
)

saveRDS(CALIBRATION_RESULTS, "calibration_refinement_results.rds")
cat("Results saved to: calibration_refinement_results.rds\n")
cat("To load: results <- readRDS('calibration_refinement_results.rds')\n\n")
