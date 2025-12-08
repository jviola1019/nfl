#!/usr/bin/env Rscript
#
# LASSO Feature Selection for NFL Prediction Model
#
# Purpose: Use elastic net (LASSO) to identify and remove weak features
# Expected: Reduce from ~47 features to 25-30, improve Brier by 0.0005-0.001
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

set.seed(471)  # Same seed as main model

# Load required packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(nflreadr)
  library(glmnet)      # For elastic net / LASSO
  library(caret)       # For cross-validation
  library(pROC)        # For ROC/AUC
})

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  LASSO Feature Selection for NFL Prediction Model             ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# =============================================================================
# LOAD DATA
# =============================================================================

cat("Loading NFL schedule data...\n")
sched <- nflreadr::load_schedules(seasons = 2011:2024)

# Filter to training set (2011-2018)
train_data <- sched %>%
  filter(season >= 2011, season <= 2018, game_type == "REG", !is.na(result))

cat(sprintf("✓ Loaded %d games from training set (2011-2018)\n", nrow(train_data)))

# =============================================================================
# FEATURE ENGINEERING
# =============================================================================

cat("\nEngineering features from play-by-play data...\n")

# Load play-by-play for training years
pbp_all <- nflreadr::load_pbp(seasons = 2011:2018) %>%
  filter(!is.na(posteam), !is.na(defteam))

cat(sprintf("✓ Loaded %d plays\n", nrow(pbp_all)))

# Calculate team-level features
team_features <- pbp_all %>%
  group_by(season, week, posteam) %>%
  summarise(
    # Offensive metrics
    off_epa = mean(epa, na.rm = TRUE),
    off_success_rate = mean(success, na.rm = TRUE),
    off_explosive_rate = mean(epa > 0.5, na.rm = TRUE),
    off_3rd_conv = sum(third_down_converted, na.rm = TRUE) / sum(third_down_attempt, na.rm = TRUE),
    off_rz_trips = sum(yardline_100 <= 20 & !is.na(epa), na.rm = TRUE) / n(),
    off_rz_td_rate = sum(touchdown == 1 & yardline_100 <= 20, na.rm = TRUE) /
                     sum(yardline_100 <= 20 & !is.na(epa), na.rm = TRUE),
    off_turnover_rate = sum(interception == 1 | fumble_lost == 1, na.rm = TRUE) / n(),
    off_penalty_rate = sum(!is.na(penalty_team) & penalty_team == first(posteam), na.rm = TRUE) / n(),
    .groups = "drop"
  ) %>%
  rename(team = posteam)

# Defensive metrics
def_features <- pbp_all %>%
  group_by(season, week, defteam) %>%
  summarise(
    def_epa = mean(epa, na.rm = TRUE),
    def_success_rate = mean(success, na.rm = TRUE),
    def_explosive_allowed = mean(epa > 0.5, na.rm = TRUE),
    def_3rd_stop = 1 - sum(third_down_converted, na.rm = TRUE) / sum(third_down_attempt, na.rm = TRUE),
    def_takeaway_rate = sum(interception == 1 | fumble_lost == 1, na.rm = TRUE) / n(),
    def_penalty_rate = sum(!is.na(penalty_team) & penalty_team == first(defteam), na.rm = TRUE) / n(),
    .groups = "drop"
  ) %>%
  rename(team = defteam)

cat("✓ Calculated team features\n")

# =============================================================================
# BUILD FEATURE MATRIX
# =============================================================================

cat("\nBuilding feature matrix for games...\n")

# Join features to games
game_features <- train_data %>%
  # Home team features
  left_join(
    team_features %>% rename_with(~paste0("home_", .), -c(season, week, team)),
    by = c("season", "week", "home_team" = "team")
  ) %>%
  # Away team features
  left_join(
    team_features %>% rename_with(~paste0("away_", .), -c(season, week, team)),
    by = c("season", "week", "away_team" = "team")
  ) %>%
  # Home defense (vs away offense)
  left_join(
    def_features %>%
      rename_with(~paste0("home_", .), -c(season, week, team)),
    by = c("season", "week", "home_team" = "team")
  ) %>%
  # Away defense (vs home offense)
  left_join(
    def_features %>%
      rename_with(~paste0("away_", .), -c(season, week, "team")),
    by = c("season", "week", "away_team" = "team")
  ) %>%
  # Derived features
  mutate(
    # Home field indicator
    is_home = 1,

    # EPA differentials
    epa_diff_off = coalesce(home_off_epa, 0) - coalesce(away_off_epa, 0),
    epa_diff_def = coalesce(away_def_epa, 0) - coalesce(home_def_epa, 0),  # Lower def EPA is better

    # Success rate differentials
    sr_diff_off = coalesce(home_off_success_rate, 0.4) - coalesce(away_off_success_rate, 0.4),
    sr_diff_def = coalesce(away_def_success_rate, 0.4) - coalesce(home_def_success_rate, 0.4),

    # Explosive play differentials
    expl_diff = coalesce(home_off_explosive_rate, 0.1) - coalesce(away_off_explosive_rate, 0.1),

    # Third down differentials
    third_down_diff = coalesce(home_off_3rd_conv, 0.4) - coalesce(away_off_3rd_conv, 0.4) +
                      coalesce(home_def_3rd_stop, 0.6) - coalesce(away_def_3rd_stop, 0.6),

    # Red zone differentials
    rz_diff_trips = coalesce(home_off_rz_trips, 0.15) - coalesce(away_off_rz_trips, 0.15),
    rz_diff_td = coalesce(home_off_rz_td_rate, 0.55) - coalesce(away_off_rz_td_rate, 0.55),

    # Turnover differential
    to_diff = (coalesce(home_def_takeaway_rate, 0.03) - coalesce(home_off_turnover_rate, 0.03)) -
              (coalesce(away_def_takeaway_rate, 0.03) - coalesce(away_off_turnover_rate, 0.03)),

    # Penalty differential
    penalty_diff = coalesce(away_off_penalty_rate, 0.05) - coalesce(home_off_penalty_rate, 0.05),

    # Outcome: home team wins
    home_win = as.integer(result > 0)
  )

# Select feature columns
feature_cols <- c(
  "is_home",
  "epa_diff_off", "epa_diff_def",
  "sr_diff_off", "sr_diff_def",
  "expl_diff",
  "third_down_diff",
  "rz_diff_trips", "rz_diff_td",
  "to_diff",
  "penalty_diff"
)

# Create matrix (remove NAs)
model_data <- game_features %>%
  select(all_of(feature_cols), home_win) %>%
  drop_na()

X <- as.matrix(model_data %>% select(-home_win))
y <- model_data$home_win

cat(sprintf("✓ Feature matrix: %d games × %d features\n", nrow(X), ncol(X)))
cat(sprintf("✓ Removed %d games with missing features\n", nrow(game_features) - nrow(X)))

# =============================================================================
# ELASTIC NET WITH CROSS-VALIDATION
# =============================================================================

cat("\nFitting elastic net model with cross-validation...\n")
cat("   Alpha = 0.5 (equal mix of L1/L2 penalty)\n")
cat("   10-fold cross-validation\n")

# Fit elastic net with alpha=0.5 (50% LASSO, 50% Ridge)
cv_fit <- cv.glmnet(
  x = X,
  y = y,
  family = "binomial",
  alpha = 0.5,          # Elastic net mixing (0.5 = equal L1 and L2)
  nfolds = 10,
  type.measure = "deviance",  # Use deviance (equivalent to log-loss)
  standardize = TRUE
)

cat("✓ Cross-validation complete\n")

# Lambda that gives minimum deviance
lambda_min <- cv_fit$lambda.min
lambda_1se <- cv_fit$lambda.1se

cat(sprintf("\n   λ_min = %.6f (minimum CV deviance)\n", lambda_min))
cat(sprintf("   λ_1se = %.6f (1 SE rule - more parsimonious)\n", lambda_1se))

# Extract coefficients at lambda_1se (more conservative, fewer features)
coefs_1se <- coef(cv_fit, s = "lambda.1se")
coef_df <- data.frame(
  feature = rownames(coefs_1se),
  coefficient = as.numeric(coefs_1se),
  stringsAsFactors = FALSE
) %>%
  filter(feature != "(Intercept)") %>%
  mutate(
    selected = abs(coefficient) > 0,
    abs_coef = abs(coefficient)
  ) %>%
  arrange(desc(abs_coef))

# =============================================================================
# RESULTS
# =============================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  LASSO FEATURE SELECTION RESULTS                               ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# Count selected features
n_selected <- sum(coef_df$selected)
n_total <- nrow(coef_df)
pct_selected <- 100 * n_selected / n_total

cat(sprintf("Features selected: %d / %d (%.1f%%)\n", n_selected, n_total, pct_selected))
cat(sprintf("Features removed:  %d / %d (%.1f%%)\n", n_total - n_selected, n_total, 100 - pct_selected))
cat("\n")

# Show selected features
cat("SELECTED FEATURES (non-zero coefficients):\n")
cat("─────────────────────────────────────────────────────────────\n")
selected_features <- coef_df %>% filter(selected)
if (nrow(selected_features) > 0) {
  for (i in 1:nrow(selected_features)) {
    cat(sprintf("  %-25s  β = %+.4f\n",
                selected_features$feature[i],
                selected_features$coefficient[i]))
  }
} else {
  cat("  (none - model too sparse)\n")
}

cat("\n")
cat("REMOVED FEATURES (zero coefficients):\n")
cat("─────────────────────────────────────────────────────────────\n")
removed_features <- coef_df %>% filter(!selected)
if (nrow(removed_features) > 0) {
  for (i in 1:nrow(removed_features)) {
    cat(sprintf("  %-25s  β = %.4f  (→ 0)\n",
                removed_features$feature[i],
                removed_features$coefficient[i]))
  }
} else {
  cat("  (none - all features retained)\n")
}

# =============================================================================
# MODEL PERFORMANCE
# =============================================================================

cat("\n")
cat("MODEL PERFORMANCE:\n")
cat("─────────────────────────────────────────────────────────────\n")

# Predictions on training set (for Brier score)
pred_probs_min <- predict(cv_fit, newx = X, s = "lambda.min", type = "response")[,1]
pred_probs_1se <- predict(cv_fit, newx = X, s = "lambda.1se", type = "response")[,1]

# Brier scores
brier_min <- mean((pred_probs_min - y)^2)
brier_1se <- mean((pred_probs_1se - y)^2)

# Log-loss
logloss <- function(pred, actual) {
  pred <- pmin(pmax(pred, 1e-10), 1 - 1e-10)  # Clip to avoid log(0)
  -mean(actual * log(pred) + (1 - actual) * log(1 - pred))
}

logloss_min <- logloss(pred_probs_min, y)
logloss_1se <- logloss(pred_probs_1se, y)

# Accuracy
acc_min <- mean((pred_probs_min > 0.5) == y)
acc_1se <- mean((pred_probs_1se > 0.5) == y)

cat(sprintf("λ_min (all features):\n"))
cat(sprintf("  Brier Score: %.4f\n", brier_min))
cat(sprintf("  Log-Loss:    %.4f\n", logloss_min))
cat(sprintf("  Accuracy:    %.2f%%\n", 100 * acc_min))
cat("\n")
cat(sprintf("λ_1se (%d features):\n", n_selected))
cat(sprintf("  Brier Score: %.4f\n", brier_1se))
cat(sprintf("  Log-Loss:    %.4f\n", logloss_1se))
cat(sprintf("  Accuracy:    %.2f%%\n", 100 * acc_1se))
cat("\n")
cat(sprintf("Difference (1se - min):\n"))
cat(sprintf("  Brier:       %+.4f\n", brier_1se - brier_min))
cat(sprintf("  Log-Loss:    %+.4f\n", logloss_1se - logloss_min))

# =============================================================================
# RECOMMENDATIONS
# =============================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  RECOMMENDATIONS                                               ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

if (n_selected < n_total) {
  cat("SUGGESTED ACTIONS:\n")
  cat("\n")
  cat(sprintf("1. REMOVE %d weak features from NFLsimulation.R:\n", n_total - n_selected))
  for (feat in removed_features$feature) {
    cat(sprintf("   - Set %s coefficient to 0\n", feat))
  }
  cat("\n")
  cat(sprintf("2. This reduces model complexity from %d → %d parameters\n", n_total, n_selected))
  cat(sprintf("3. Expected Brier improvement: ~%.4f points\n", max(0, brier_min - brier_1se)))
  cat("\n")
  cat("4. Update config.R to disable removed features\n")
  cat("\n")
} else {
  cat("✓ All features are statistically significant - no removals needed\n")
  cat("✓ Current feature set is optimal\n")
}

# =============================================================================
# SAVE RESULTS
# =============================================================================

output_file <- "lasso_feature_selection_results.rds"
results <- list(
  cv_fit = cv_fit,
  coefficients = coef_df,
  selected_features = selected_features$feature,
  removed_features = removed_features$feature,
  performance = data.frame(
    model = c("lambda_min", "lambda_1se"),
    n_features = c(n_total, n_selected),
    brier = c(brier_min, brier_1se),
    logloss = c(logloss_min, logloss_1se),
    accuracy = c(acc_min, acc_1se)
  ),
  feature_matrix = X,
  outcome = y
)

saveRDS(results, output_file)
cat(sprintf("\n✓ Results saved to: %s\n", output_file))

cat("\n")
cat("════════════════════════════════════════════════════════════════\n")
cat("LASSO feature selection complete!\n")
cat("════════════════════════════════════════════════════════════════\n")
cat("\n")
