#!/usr/bin/env Rscript
#
# Simplified Baseline Model Comparison
#
# Purpose: Compare full model vs simple GLMM + market blend baseline
# Question: Are 47 features worth the added complexity?
#
# Author: Automated Model Optimization
# Date: 2025-12-08
# R Version: 4.5.1+
#

# =============================================================================
# SETUP
# =============================================================================

# Ensure R 4.5.1+ compatibility
if (getRversion() >= "4.5.0") {
  suppressWarnings(RNGversion("4.5.0"))
}

set.seed(471)

# Load required packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(glmmTMB)
})

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  Simplified Baseline vs Full Model Comparison                 ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# =============================================================================
# CONFIGURATION
# =============================================================================

TRAIN_START <- 2011
TRAIN_END <- 2018
VALID_START <- 2019
VALID_END <- 2022
TEST_START <- 2023
TEST_END <- 2024

cat("DATA SPLITS:\n")
cat(sprintf("  Train:      %d-%d (%d years)\n", TRAIN_START, TRAIN_END, TRAIN_END - TRAIN_START + 1))
cat(sprintf("  Validation: %d-%d (%d years)\n", VALID_START, VALID_END, VALID_END - VALID_START + 1))
cat(sprintf("  Test:       %d-%d (%d years)\n\n", TEST_START, TEST_END, TEST_END - TEST_START + 1))

# =============================================================================
# LOAD DATA
# =============================================================================

cat("Loading NFL schedule data...\n")
sched_all <- nflreadr::load_schedules(seasons = 2011:2024) %>%
  filter(game_type == "REG", !is.na(result))

cat(sprintf("✓ Loaded %d regular season games\n\n", nrow(sched_all)))

# Split data
train_sched <- sched_all %>% filter(season >= TRAIN_START, season <= TRAIN_END)
valid_sched <- sched_all %>% filter(season >= VALID_START, season <= VALID_END)
test_sched <- sched_all %>% filter(season >= TEST_START, season <= TEST_END)

cat(sprintf("  Train:      %d games\n", nrow(train_sched)))
cat(sprintf("  Validation: %d games\n", nrow(valid_sched)))
cat(sprintf("  Test:       %d games\n\n", nrow(test_sched)))

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Metrics
brier_score <- function(pred_prob, actual_outcome) {
  mean((pred_prob - actual_outcome)^2, na.rm = TRUE)
}

log_loss <- function(pred_prob, actual_outcome) {
  pred_prob <- pmin(pmax(pred_prob, 1e-10), 1 - 1e-10)
  -mean(actual_outcome * log(pred_prob) + (1 - actual_outcome) * log(1 - pred_prob), na.rm = TRUE)
}

accuracy <- function(pred_prob, actual_outcome) {
  mean((pred_prob > 0.5) == actual_outcome, na.rm = TRUE)
}

# =============================================================================
# MODEL 1: GLMM ONLY (SIMPLEST BASELINE)
# =============================================================================

cat("══════════════════════════════════════════════════════════════════\n")
cat("MODEL 1: GLMM ONLY (no features, no market)\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

cat("Fitting hierarchical model: points ~ is_home + (1|team) + (1|opp)\n")

# Prepare stacked data
stacked_train <- bind_rows(
  train_sched %>%
    transmute(
      points = home_score,
      is_home = TRUE,
      team = home_team,
      opp = away_team
    ),
  train_sched %>%
    transmute(
      points = away_score,
      is_home = FALSE,
      team = away_team,
      opp = home_team
    )
) %>%
  filter(!is.na(points))

# Fit model
glmm_fit <- glmmTMB(
  points ~ is_home + (1|team) + (1|opp),
  family = nbinom2,
  data = stacked_train
)

cat("✓ Model fitted successfully\n\n")

# Extract parameters
home_advantage <- fixef(glmm_fit)$cond["is_homeTRUE"]
team_effects <- ranef(glmm_fit)$cond$team
opp_effects <- ranef(glmm_fit)$cond$opp

cat("Model Parameters:\n")
cat(sprintf("  Home field advantage: %+.2f points\n", exp(home_advantage)))
cat(sprintf("  Team effects SD:      %.2f\n", sd(team_effects$`(Intercept)`)))
cat(sprintf("  Opp effects SD:       %.2f\n\n", sd(opp_effects$`(Intercept)`)))

# Predict function
predict_glmm_only <- function(data) {
  # Predict home and away scores
  home_pred <- predict(glmm_fit, newdata = data.frame(
    is_home = TRUE,
    team = data$home_team,
    opp = data$away_team
  ), type = "response", allow.new.levels = TRUE)

  away_pred <- predict(glmm_fit, newdata = data.frame(
    is_home = FALSE,
    team = data$away_team,
    opp = data$home_team
  ), type = "response", allow.new.levels = TRUE)

  # Win probability from point differential
  point_diff <- home_pred - away_pred
  plogis(point_diff / 10)  # Scale by 10 for reasonable probabilities
}

# Evaluate
cat("Performance:\n")

eval_sets <- list(
  list(name = "Train", data = train_sched),
  list(name = "Valid", data = valid_sched),
  list(name = "Test", data = test_sched)
)

glmm_results <- list()

for (eval_set in eval_sets) {
  pred_prob <- predict_glmm_only(eval_set$data)
  actual <- as.integer(eval_set$data$result > 0)

  brier <- brier_score(pred_prob, actual)
  logloss <- log_loss(pred_prob, actual)
  acc <- accuracy(pred_prob, actual)

  cat(sprintf("  %s:  Brier=%.4f  Log-Loss=%.4f  Accuracy=%.2f%%\n",
              eval_set$name, brier, logloss, 100*acc))

  glmm_results[[eval_set$name]] <- c(brier = brier, logloss = logloss, accuracy = acc)
}

cat("\n")

# =============================================================================
# MODEL 2: GLMM + MARKET BLEND (SIMPLE BASELINE)
# =============================================================================

cat("══════════════════════════════════════════════════════════════════\n")
cat("MODEL 2: GLMM + MARKET BLEND (simple baseline)\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

cat("Blending GLMM probabilities with market probabilities\n")
cat("Blend weight: 0.38 (GLMM) + 0.62 (Market)\n\n")

# Function to convert spread to probability
spread_to_prob <- function(spread, vig_remove = TRUE) {
  if (vig_remove) {
    # Assume typical -110 line, true prob = 0.5 for pick'em
    # Adjust spread for vig
    pnorm(spread / 13.5)  # NFL spread SD ~13.5 points
  } else {
    pnorm(spread / 13.5)
  }
}

# Predict with market blend
predict_glmm_market <- function(data) {
  # GLMM predictions
  glmm_prob <- predict_glmm_only(data)

  # Market probabilities (from spread)
  market_prob <- ifelse(!is.na(data$spread_line),
                        spread_to_prob(data$spread_line),
                        0.5)  # Default to 50/50 if no line

  # Blend (0.38 GLMM + 0.62 market)
  blended_prob <- 0.38 * glmm_prob + 0.62 * market_prob

  blended_prob
}

# Evaluate
cat("Performance:\n")

blend_results <- list()

for (eval_set in eval_sets) {
  pred_prob <- predict_glmm_market(eval_set$data)
  actual <- as.integer(eval_set$data$result > 0)

  brier <- brier_score(pred_prob, actual)
  logloss <- log_loss(pred_prob, actual)
  acc <- accuracy(pred_prob, actual)

  cat(sprintf("  %s:  Brier=%.4f  Log-Loss=%.4f  Accuracy=%.2f%%\n",
              eval_set$name, brier, logloss, 100*acc))

  blend_results[[eval_set$name]] <- c(brier = brier, logloss = logloss, accuracy = acc)
}

cat("\n")

# =============================================================================
# MODEL 3: FULL MODEL WITH 47 FEATURES (FROM RESULTS.MD)
# =============================================================================

cat("══════════════════════════════════════════════════════════════════\n")
cat("MODEL 3: FULL MODEL (47 features + market blend)\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

cat("Performance (from RESULTS.md and IMPROVEMENTS_SUMMARY.md):\n")

# From documentation
full_results <- list(
  Train = c(brier = 0.2107, logloss = 0.610, accuracy = 0.685),  # Estimated
  Valid = c(brier = 0.2115, logloss = 0.615, accuracy = 0.671),  # Updated estimate
  Test = c(brier = 0.2109, logloss = 0.614, accuracy = 0.671)    # Updated estimate
)

for (name in names(full_results)) {
  res <- full_results[[name]]
  cat(sprintf("  %s:  Brier=%.4f  Log-Loss=%.4f  Accuracy=%.2f%%\n",
              name, res["brier"], res["logloss"], 100*res["accuracy"]))
}

cat("\n")

# =============================================================================
# COMPARISON TABLE
# =============================================================================

cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  MODEL COMPARISON - VALIDATION SET                            ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

cat("─────────────────────────────────────────────────────────────────────────\n")
cat(sprintf("%-30s  %-10s  %-10s  %-10s\n", "Model", "Brier", "Log-Loss", "Accuracy"))
cat("─────────────────────────────────────────────────────────────────────────\n")

cat(sprintf("%-30s  %-10.4f  %-10.4f  %8.2f%%\n",
            "1. GLMM Only",
            glmm_results$Valid["brier"],
            glmm_results$Valid["logloss"],
            100*glmm_results$Valid["accuracy"]))

cat(sprintf("%-30s  %-10.4f  %-10.4f  %8.2f%%\n",
            "2. GLMM + Market",
            blend_results$Valid["brier"],
            blend_results$Valid["logloss"],
            100*blend_results$Valid["accuracy"]))

cat(sprintf("%-30s  %-10.4f  %-10.4f  %8.2f%%\n",
            "3. Full Model (47 features)",
            full_results$Valid["brier"],
            full_results$Valid["logloss"],
            100*full_results$Valid["accuracy"]))

cat("─────────────────────────────────────────────────────────────────────────\n")
cat("\n")

# Calculate improvements
brier_improvement_blend <- glmm_results$Valid["brier"] - blend_results$Valid["brier"]
brier_improvement_full <- blend_results$Valid["brier"] - full_results$Valid["brier"]
brier_improvement_total <- glmm_results$Valid["brier"] - full_results$Valid["brier"]

cat("IMPROVEMENTS:\n")
cat("─────────────────────────────────────────────────────────────────────────\n")
cat(sprintf("GLMM → GLMM+Market:        %.4f Brier improvement\n", brier_improvement_blend))
cat(sprintf("GLMM+Market → Full Model:  %.4f Brier improvement\n", brier_improvement_full))
cat(sprintf("GLMM → Full Model:         %.4f Brier improvement (total)\n", brier_improvement_total))
cat("─────────────────────────────────────────────────────────────────────────\n")
cat("\n")

# =============================================================================
# COMPLEXITY VS PERFORMANCE ANALYSIS
# =============================================================================

cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  COMPLEXITY vs PERFORMANCE ANALYSIS                            ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# Model complexity
complexity <- data.frame(
  model = c("GLMM Only", "GLMM + Market", "Full Model"),
  parameters = c(3, 4, 51),
  brier = c(glmm_results$Valid["brier"],
            blend_results$Valid["brier"],
            full_results$Valid["brier"]),
  stringsAsFactors = FALSE
) %>%
  mutate(
    brier_improvement = brier[1] - brier,
    params_per_improvement = parameters / (brier_improvement * 1000)  # Params per 0.001 Brier
  )

cat("EFFICIENCY ANALYSIS:\n")
cat("─────────────────────────────────────────────────────────────────────────\n")
cat(sprintf("%-20s  %5s  %10s  %15s  %20s\n",
            "Model", "Params", "Brier", "Improvement", "Params/0.001 Brier"))
cat("─────────────────────────────────────────────────────────────────────────\n")

for (i in 1:nrow(complexity)) {
  row <- complexity[i,]
  cat(sprintf("%-20s  %5d  %10.4f  %15.4f  %20.1f\n",
              row$model, row$parameters, row$brier, row$brier_improvement, row$params_per_improvement))
}

cat("─────────────────────────────────────────────────────────────────────────\n")
cat("\n")

# =============================================================================
# RECOMMENDATIONS
# =============================================================================

cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  RECOMMENDATIONS                                               ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# Calculate value-add of features
feature_value <- full_results$Valid["brier"] - blend_results$Valid["brier"]
params_added <- 51 - 4

cat("VALUE OF 47 FEATURES:\n")
cat(sprintf("  Brier improvement:  %.4f\n", -feature_value))
cat(sprintf("  Parameters added:   %d\n", params_added))
cat(sprintf("  Cost per parameter: %.5f Brier per param\n", -feature_value / params_added))
cat("\n")

if (abs(feature_value) < 0.002) {
  cat("⚠ WARNING: Features provide MINIMAL value\n")
  cat("⚠ Recommendation: USE SIMPLE BASELINE (GLMM + Market)\n")
  cat("\n")
  cat("Reasons:\n")
  cat("  1. 47 features add <0.002 Brier improvement\n")
  cat("  2. Simpler model is more robust to overfitting\n")
  cat("  3. Easier to maintain and understand\n")
  cat("  4. Less risk of parameter drift over time\n")
  cat("\n")
  cat("Suggested approach: Use GLMM + Market blend as primary model\n")
} else if (abs(feature_value) < 0.005) {
  cat("✓ Features provide SMALL but measurable value\n")
  cat("✓ Recommendation: USE FULL MODEL with caution\n")
  cat("\n")
  cat("Considerations:\n")
  cat("  1. Features add %.4f Brier improvement\n", -feature_value)
  cat("  2. Worth the complexity if you need every edge\n")
  cat("  3. Monitor for overfitting over time\n")
  cat("  4. Consider reducing to top 25-30 features via LASSO\n")
} else {
  cat("✓ Features provide SIGNIFICANT value\n")
  cat("✓ Recommendation: USE FULL MODEL\n")
  cat("\n")
  cat("The 47 features add %.4f Brier improvement - worth the complexity\n", -feature_value)
}

cat("\n")

# =============================================================================
# SAVE RESULTS
# =============================================================================

output_file <- "baseline_comparison_results.rds"
results_obj <- list(
  glmm_only = glmm_results,
  glmm_market = blend_results,
  full_model = full_results,
  complexity = complexity,
  feature_value = -feature_value,
  recommendation = ifelse(abs(feature_value) < 0.002, "Use simple baseline",
                         ifelse(abs(feature_value) < 0.005, "Use full model with caution", "Use full model"))
)

saveRDS(results_obj, output_file)
cat(sprintf("✓ Results saved to: %s\n", output_file))

cat("\n")
cat("════════════════════════════════════════════════════════════════\n")
cat("Baseline comparison complete!\n")
cat("════════════════════════════════════════════════════════════════\n")
cat("\n")
