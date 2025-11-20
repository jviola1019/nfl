# =============================================================================
# NFL Model Validation Script
# K-Fold Cross-Validation and Statistical Significance Testing
# R Version: 4.5.1+
# =============================================================================

# Required packages with R 4.5.1 compatibility
required_packages <- c(
  "tidyverse",      # Data manipulation
  "glmmTMB",        # GLMM models
  "caret",          # Cross-validation framework
  "lmtest",         # Statistical tests
  "car",            # ANOVA tests
  "boot",           # Bootstrap methods
  "pROC",           # ROC analysis
  "broom.mixed",    # Tidy model outputs
  "performance",    # Model performance metrics
  "see",            # Visualization
  "foreach",        # Parallel processing
  "doParallel"      # Parallel backend
)

# Check and install missing packages
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing package: ", pkg)
    install.packages(pkg, repos = "https://cloud.r-project.org/")
  }
}

# Load packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(glmmTMB)
  library(caret)
  library(lmtest)
  library(car)
  library(boot)
  library(pROC)
  library(broom.mixed)
  library(performance)
  library(foreach)
  library(doParallel)
})

# Set seed for reproducibility
set.seed(471)

# =============================================================================
# SECTION 1: Data Loading and Preparation
# =============================================================================

message("\n===== SECTION 1: Data Loading and Preparation =====\n")

# Source the main simulation file to get data
# We'll load required data without running the full simulation
source_data <- function() {
  # Check if nflreadr is available
  if (!requireNamespace("nflreadr", quietly = TRUE)) {
    install.packages("nflreadr", repos = "https://cloud.r-project.org/")
  }

  library(nflreadr)

  # Load schedule data
  SEASON <- year(Sys.Date())
  sched <- load_schedules(seasons = c(SEASON - 2, SEASON - 1, SEASON))

  # Determine column names
  home_team_col <- if ("home_team" %in% names(sched)) "home_team" else "team_home"
  away_team_col <- if ("away_team" %in% names(sched)) "away_team" else "team_away"
  home_pts_col  <- if ("home_score" %in% names(sched)) "home_score" else "score_home"
  away_pts_col  <- if ("away_score" %in% names(sched)) "away_score" else "score_away"

  # Filter completed regular season games
  df_hist <- sched %>%
    filter(game_type == "REG", !is.na(.data[[home_pts_col]]), !is.na(.data[[away_pts_col]])) %>%
    transmute(
      game_id = game_id,
      season = season,
      week = week,
      home = .data[[home_team_col]],
      away = .data[[away_team_col]],
      y_home = .data[[home_pts_col]],
      y_away = .data[[away_pts_col]]
    )

  # Create stacked data format
  stacked <- bind_rows(
    df_hist %>% transmute(game_id, season, week, team = home, opp = away, is_home = 1L, points = y_home),
    df_hist %>% transmute(game_id, season, week, team = away, opp = home, is_home = 0L, points = y_away)
  )

  return(list(
    df_hist = df_hist,
    stacked = stacked,
    sched = sched
  ))
}

message("Loading NFL data...")
data_obj <- source_data()
df_hist <- data_obj$df_hist
stacked <- data_obj$stacked
sched <- data_obj$sched

message(sprintf("Loaded %d games (%d team-game observations)",
                nrow(df_hist), nrow(stacked)))

# =============================================================================
# SECTION 2: K-Fold Cross-Validation
# =============================================================================

message("\n===== SECTION 2: K-Fold Cross-Validation =====\n")

#' Perform k-fold cross-validation on the base GLMM model
#'
#' @param data Stacked data frame with team-game observations
#' @param k Number of folds (default: 10)
#' @param metric Evaluation metric ("rmse", "mae", "log_loss")
#' @return List containing CV results and fold performance
perform_kfold_cv <- function(data, k = 10, metric = "rmse") {

  message(sprintf("Performing %d-fold cross-validation...", k))

  # Create folds stratified by points (to ensure balance)
  set.seed(471)
  data <- data %>%
    mutate(
      points_bin = cut(points, breaks = quantile(points, probs = seq(0, 1, 0.2)),
                       include.lowest = TRUE, labels = FALSE),
      fold_id = NA_integer_
    )

  # Assign folds within each bin
  for (bin in unique(data$points_bin)) {
    idx <- which(data$points_bin == bin)
    data$fold_id[idx] <- sample(rep(1:k, length.out = length(idx)))
  }

  # Initialize results storage
  fold_results <- tibble(
    fold = integer(),
    n_train = integer(),
    n_test = integer(),
    rmse = numeric(),
    mae = numeric(),
    log_loss = numeric(),
    brier_score = numeric(),
    convergence = logical()
  )

  predictions_all <- tibble()

  # Perform cross-validation
  for (fold in 1:k) {
    message(sprintf("  Processing fold %d/%d...", fold, k))

    # Split data
    train_data <- data %>% filter(fold_id != fold)
    test_data <- data %>% filter(fold_id == fold)

    # Fit model on training data
    fit <- try(
      glmmTMB(
        points ~ is_home + (1|team) + (1|opp),
        family = nbinom2,
        data = train_data,
        control = glmmTMBControl(optimizer = nlminb, optCtrl = list(iter.max = 1000))
      ),
      silent = TRUE
    )

    if (inherits(fit, "try-error")) {
      message("    Warning: Model failed to converge for fold ", fold)
      fold_results <- fold_results %>%
        add_row(
          fold = fold,
          n_train = nrow(train_data),
          n_test = nrow(test_data),
          rmse = NA_real_,
          mae = NA_real_,
          log_loss = NA_real_,
          brier_score = NA_real_,
          convergence = FALSE
        )
      next
    }

    # Make predictions on test data
    test_data <- test_data %>%
      mutate(
        pred_points = as.numeric(predict(fit, newdata = test_data, type = "response")),
        pred_points = pmax(pred_points, 0)  # Ensure non-negative
      )

    # Calculate metrics
    rmse <- sqrt(mean((test_data$points - test_data$pred_points)^2, na.rm = TRUE))
    mae <- mean(abs(test_data$points - test_data$pred_points), na.rm = TRUE)

    # Log loss (using negative binomial likelihood)
    size_param <- sigma(fit)  # Overdispersion parameter
    log_likelihood <- sum(dnbinom(test_data$points,
                                  mu = test_data$pred_points,
                                  size = size_param,
                                  log = TRUE))
    log_loss <- -log_likelihood / nrow(test_data)

    # Brier score (for win probability)
    test_data <- test_data %>%
      mutate(
        actual_win = ifelse(points > 20, 1, 0),  # Simple threshold
        pred_win_prob = pnbinom(20, mu = pred_points, size = size_param, lower.tail = FALSE)
      )
    brier <- mean((test_data$actual_win - test_data$pred_win_prob)^2, na.rm = TRUE)

    # Store results
    fold_results <- fold_results %>%
      add_row(
        fold = fold,
        n_train = nrow(train_data),
        n_test = nrow(test_data),
        rmse = rmse,
        mae = mae,
        log_loss = log_loss,
        brier_score = brier,
        convergence = TRUE
      )

    predictions_all <- bind_rows(
      predictions_all,
      test_data %>% select(fold_id, game_id, team, points, pred_points)
    )
  }

  # Summary statistics
  summary_stats <- fold_results %>%
    filter(convergence) %>%
    summarise(
      n_folds = n(),
      mean_rmse = mean(rmse, na.rm = TRUE),
      sd_rmse = sd(rmse, na.rm = TRUE),
      mean_mae = mean(mae, na.rm = TRUE),
      sd_mae = sd(mae, na.rm = TRUE),
      mean_log_loss = mean(log_loss, na.rm = TRUE),
      sd_log_loss = sd(log_loss, na.rm = TRUE),
      mean_brier = mean(brier_score, na.rm = TRUE),
      sd_brier = sd(brier_score, na.rm = TRUE)
    )

  # 95% confidence intervals
  ci_rmse <- t.test(fold_results$rmse)$conf.int
  ci_mae <- t.test(fold_results$mae)$conf.int

  message("\n--- Cross-Validation Results ---")
  message(sprintf("RMSE: %.3f ± %.3f (95%% CI: [%.3f, %.3f])",
                  summary_stats$mean_rmse, summary_stats$sd_rmse, ci_rmse[1], ci_rmse[2]))
  message(sprintf("MAE:  %.3f ± %.3f (95%% CI: [%.3f, %.3f])",
                  summary_stats$mean_mae, summary_stats$sd_mae, ci_mae[1], ci_mae[2]))
  message(sprintf("Log Loss: %.3f ± %.3f",
                  summary_stats$mean_log_loss, summary_stats$sd_log_loss))
  message(sprintf("Brier Score: %.3f ± %.3f",
                  summary_stats$mean_brier, summary_stats$sd_brier))

  return(list(
    fold_results = fold_results,
    summary_stats = summary_stats,
    predictions = predictions_all,
    ci_rmse = ci_rmse,
    ci_mae = ci_mae
  ))
}

# Run 10-fold CV
cv_results <- perform_kfold_cv(stacked, k = 10)

# Save CV results
saveRDS(cv_results, "model_validation_cv_results.rds")

# =============================================================================
# SECTION 3: Statistical Significance Testing
# =============================================================================

message("\n===== SECTION 3: Statistical Significance Testing =====\n")

#' Test statistical significance of model components
#'
#' @param data Stacked data frame
#' @return Data frame with test results
test_model_significance <- function(data) {

  message("Fitting full model for significance testing...")

  # Fit the full model
  full_model <- glmmTMB(
    points ~ is_home + (1|team) + (1|opp),
    family = nbinom2,
    data = data,
    control = glmmTMBControl(optimizer = nlminb, optCtrl = list(iter.max = 1000))
  )

  if (!inherits(full_model, "glmmTMB")) {
    stop("Full model failed to converge")
  }

  message("Full model fitted successfully\n")

  # Extract fixed effects summary
  fixed_effects <- summary(full_model)$coefficients$cond

  message("--- Fixed Effects Significance ---")
  print(fixed_effects)

  # Likelihood ratio tests for random effects
  message("\n--- Random Effects Significance (LRT) ---\n")

  # Test team random effect
  model_no_team <- try(
    glmmTMB(points ~ is_home + (1|opp), family = nbinom2, data = data),
    silent = TRUE
  )

  if (!inherits(model_no_team, "try-error")) {
    lrt_team <- anova(full_model, model_no_team)
    message("Team random effect test:")
    print(lrt_team)
    team_pval <- lrt_team$`Pr(>Chisq)`[2]
  } else {
    message("Team RE test: Model failed to converge")
    team_pval <- NA_real_
  }

  # Test opponent random effect
  model_no_opp <- try(
    glmmTMB(points ~ is_home + (1|team), family = nbinom2, data = data),
    silent = TRUE
  )

  if (!inherits(model_no_opp, "try-error")) {
    lrt_opp <- anova(full_model, model_no_opp)
    message("\nOpponent random effect test:")
    print(lrt_opp)
    opp_pval <- lrt_opp$`Pr(>Chisq)`[2]
  } else {
    message("Opponent RE test: Model failed to converge")
    opp_pval <- NA_real_
  }

  # Test is_home fixed effect
  model_no_home <- try(
    glmmTMB(points ~ (1|team) + (1|opp), family = nbinom2, data = data),
    silent = TRUE
  )

  if (!inherits(model_no_home, "try-error")) {
    lrt_home <- anova(full_model, model_no_home)
    message("\nHome field advantage test:")
    print(lrt_home)
    home_pval <- lrt_home$`Pr(>Chisq)`[2]
  } else {
    message("Home effect test: Model failed to converge")
    home_pval <- NA_real_
  }

  # Compile results
  significance_results <- tibble(
    component = c("is_home (fixed)", "team (random)", "opp (random)"),
    p_value = c(home_pval, team_pval, opp_pval),
    significant = p_value < 0.05,
    interpretation = case_when(
      p_value < 0.001 ~ "Highly significant (p < 0.001)",
      p_value < 0.01 ~ "Very significant (p < 0.01)",
      p_value < 0.05 ~ "Significant (p < 0.05)",
      p_value < 0.10 ~ "Marginally significant (p < 0.10)",
      TRUE ~ "Not significant (p >= 0.10)"
    )
  )

  message("\n--- Significance Summary ---")
  print(significance_results)

  return(list(
    full_model = full_model,
    fixed_effects = fixed_effects,
    significance_results = significance_results
  ))
}

# Run significance tests
sig_results <- test_model_significance(stacked)

# Save significance results
saveRDS(sig_results, "model_validation_significance_results.rds")

# =============================================================================
# SECTION 4: Permutation Variable Importance
# =============================================================================

message("\n===== SECTION 4: Permutation Variable Importance =====\n")

#' Test importance of predictor adjustments using permutation
#'
#' This tests the adjustments applied in the full simulation
#' (e.g., rest, injuries, weather, etc.)
test_adjustment_importance <- function() {

  message("Testing importance of predictor variable adjustments...")
  message("Note: This analyzes the adjustment variables from the full model\n")

  # The adjustments that need statistical testing:
  adjustments_to_test <- tibble(
    adjustment = c(
      "Rest adjustments (short/long/bye)",
      "Injury adjustments (skill/trench/secondary/front7)",
      "Travel adjustments (timezone, early games)",
      "Division game adjustments",
      "Conference game adjustments",
      "Weather adjustments (dome/wind/cold/rain)",
      "Pass protection vs rush mismatch",
      "Explosive play rate differential",
      "Red zone efficiency metrics",
      "Home/away split performance",
      "Third down conversion rates",
      "Turnover metrics",
      "QB impact adjustments",
      "Recent form (EPA-based)",
      "Schedule strength adjustments"
    ),
    recommended_test = c(
      "Bootstrap resampling with/without adjustment",
      "Permutation test on injury severity",
      "Compare games with/without travel",
      "Division vs non-division game comparison",
      "Conference vs non-conference comparison",
      "Weather game vs neutral game comparison",
      "Correlation with point differential",
      "Correlation with scoring",
      "Red zone trip vs TD conversion correlation",
      "Home/away EPA differential significance",
      "3rd down rate vs win probability",
      "Turnover rate vs points allowed",
      "QB availability impact analysis",
      "Recent form decay rate optimization",
      "SoS weighted vs unweighted performance"
    ),
    data_required = c(
      "Schedule with rest days",
      "Injury report data",
      "Game location and timezone data",
      "Team division information",
      "Team conference information",
      "Weather data (temp, wind, precipitation)",
      "Pass protection and rush rate stats",
      "Explosive play (20+ yard) rates",
      "Red zone play-by-play data",
      "Home/away split statistics",
      "Third down conversion data",
      "Turnover data by game",
      "QB availability flags",
      "Historical EPA by recency",
      "Opponent strength ratings"
    ),
    priority = c(
      "HIGH", "HIGH", "MEDIUM", "MEDIUM", "LOW",
      "HIGH", "MEDIUM", "MEDIUM", "HIGH", "HIGH",
      "MEDIUM", "MEDIUM", "HIGH", "HIGH", "MEDIUM"
    )
  )

  message("--- Adjustment Variables to Test ---")
  print(adjustments_to_test %>% arrange(desc(priority)))

  # Recommendation message
  message("\n--- Recommendations for Next Steps ---")
  message("1. HIGH PRIORITY: Test rest, injury, weather, QB, recent form, and RZ adjustments")
  message("2. MEDIUM PRIORITY: Test travel, pass pro/rush, explosive plays, 3rd down, TO, SoS")
  message("3. LOW PRIORITY: Test conference game adjustments")
  message("\nFor each adjustment:")
  message("  - Run the full model with and without the adjustment")
  message("  - Compare Brier score and log loss on held-out data")
  message("  - Use bootstrap or permutation tests for p-values")
  message("  - If p > 0.10 and no practical improvement, consider removing")

  return(adjustments_to_test)
}

adjustment_tests <- test_adjustment_importance()
saveRDS(adjustment_tests, "model_validation_adjustment_tests.rds")

# =============================================================================
# SECTION 5: Practical Significance Testing (Effect Sizes)
# =============================================================================

message("\n===== SECTION 5: Practical Significance (Effect Sizes) =====\n")

#' Calculate effect sizes for model components
#'
#' @param model Fitted glmmTMB model
#' @param data Data used to fit the model
#' @return Effect size summary
calculate_effect_sizes <- function(model, data) {

  message("Calculating effect sizes...")

  # Extract coefficients
  coefs <- fixef(model)$cond

  # Home field advantage effect
  home_effect <- coefs["is_home"]

  # Extract random effects standard deviations
  re_sd <- as.data.frame(VarCorr(model)$cond)
  team_sd <- re_sd$sdcor[re_sd$grp == "team"]
  opp_sd <- re_sd$sdcor[re_sd$grp == "opp"]

  # Calculate ICC (Intraclass Correlation Coefficient)
  # Proportion of variance explained by team/opponent effects
  total_re_var <- team_sd^2 + opp_sd^2
  residual_var <- sigma(model)^2  # Residual variance

  icc_team <- team_sd^2 / (total_re_var + residual_var)
  icc_opp <- opp_sd^2 / (total_re_var + residual_var)
  icc_total <- total_re_var / (total_re_var + residual_var)

  # Effect size summary
  effect_summary <- tibble(
    component = c("Home field advantage", "Team quality", "Opponent quality"),
    estimate = c(home_effect, team_sd, opp_sd),
    metric = c("Points", "SD (points)", "SD (points)"),
    interpretation = c(
      sprintf("Home teams score %.2f more points on average", home_effect),
      sprintf("Team quality varies by ±%.2f points (1 SD)", team_sd),
      sprintf("Opponent strength varies by ±%.2f points (1 SD)", opp_sd)
    )
  )

  icc_summary <- tibble(
    component = c("Team effects", "Opponent effects", "Total structured variance"),
    icc = c(icc_team, icc_opp, icc_total),
    percentage = sprintf("%.1f%%", icc * 100),
    interpretation = c(
      "Variance due to team offensive quality",
      "Variance due to opponent defensive quality",
      "Total variance explained by team structure"
    )
  )

  message("\n--- Effect Size Summary ---")
  print(effect_summary)

  message("\n--- Variance Decomposition (ICC) ---")
  print(icc_summary)

  return(list(
    effect_summary = effect_summary,
    icc_summary = icc_summary
  ))
}

effect_sizes <- calculate_effect_sizes(sig_results$full_model, stacked)
saveRDS(effect_sizes, "model_validation_effect_sizes.rds")

# =============================================================================
# SECTION 6: Model Diagnostics
# =============================================================================

message("\n===== SECTION 6: Model Diagnostics =====\n")

#' Perform diagnostic checks on the model
#'
#' @param model Fitted glmmTMB model
#' @param data Data used to fit the model
diagnostic_checks <- function(model, data) {

  message("Running diagnostic checks...")

  # Check convergence
  convergence <- model$sdr$pdHess
  message(sprintf("Model convergence: %s", ifelse(convergence, "SUCCESS", "FAILED")))

  # Calculate residuals
  data <- data %>%
    mutate(
      fitted = predict(model, type = "response"),
      residual = points - fitted,
      std_residual = residual / sd(residual)
    )

  # Check for outliers (|residual| > 3 SD)
  outliers <- data %>%
    filter(abs(std_residual) > 3)

  message(sprintf("\nOutliers detected: %d (%.2f%% of observations)",
                  nrow(outliers), 100 * nrow(outliers) / nrow(data)))

  if (nrow(outliers) > 0) {
    message("\nTop 10 outliers:")
    print(outliers %>%
            arrange(desc(abs(std_residual))) %>%
            select(game_id, team, points, fitted, residual) %>%
            head(10))
  }

  # Residual normality test (Shapiro-Wilk on sample)
  if (nrow(data) > 5000) {
    sample_residuals <- sample(data$std_residual, 5000)
  } else {
    sample_residuals <- data$std_residual
  }

  shapiro_test <- shapiro.test(sample_residuals)
  message(sprintf("\nShapiro-Wilk normality test: W = %.4f, p = %.4e",
                  shapiro_test$statistic, shapiro_test$p.value))

  # Homoscedasticity check (Breusch-Pagan test would require lm, so visual check)
  # Group by fitted value bins
  data <- data %>%
    mutate(fitted_bin = cut(fitted, breaks = 10, labels = FALSE))

  variance_by_bin <- data %>%
    group_by(fitted_bin) %>%
    summarise(
      mean_fitted = mean(fitted),
      var_residual = var(residual),
      n = n(),
      .groups = "drop"
    )

  message("\n--- Residual Variance by Fitted Value ---")
  print(variance_by_bin)

  # Test for heteroscedasticity (increasing variance with fitted values)
  if (nrow(variance_by_bin) >= 3) {
    cor_test <- cor.test(variance_by_bin$mean_fitted, variance_by_bin$var_residual)
    message(sprintf("\nCorrelation between fitted values and residual variance: %.3f (p = %.4f)",
                    cor_test$estimate, cor_test$p.value))

    if (cor_test$p.value < 0.05) {
      message("WARNING: Significant heteroscedasticity detected")
    }
  }

  return(list(
    convergence = convergence,
    outliers = outliers,
    shapiro_test = shapiro_test,
    variance_by_bin = variance_by_bin,
    residual_data = data
  ))
}

diagnostics <- diagnostic_checks(sig_results$full_model, stacked)
saveRDS(diagnostics, "model_validation_diagnostics.rds")

# =============================================================================
# SECTION 7: R 4.5.1 Compatibility Check
# =============================================================================

message("\n===== SECTION 7: R 4.5.1 Compatibility Check =====\n")

#' Check package compatibility with R 4.5.1
check_r_compatibility <- function() {

  r_version <- R.version.string
  message(sprintf("Current R version: %s\n", r_version))

  # Check if running R 4.5.1 or later
  r_version_numeric <- as.numeric(paste0(R.version$major, ".", R.version$minor))

  if (r_version_numeric >= 4.5) {
    message("✓ Running R 4.5.0 or later - GOOD\n")
  } else {
    message("✗ Running R < 4.5.0 - May have compatibility issues\n")
  }

  # Check critical packages
  critical_packages <- c("glmmTMB", "nflreadr", "tidyverse", "randtoolbox",
                         "nnet", "glmnet", "isotone")

  package_status <- tibble(
    package = character(),
    installed = logical(),
    version = character(),
    compatible = character()
  )

  for (pkg in critical_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      pkg_version <- as.character(packageVersion(pkg))

      # Check for known compatibility issues
      compatible <- case_when(
        pkg == "glmmTMB" && packageVersion(pkg) >= "1.1.0" ~ "✓ Compatible",
        pkg == "nflreadr" && packageVersion(pkg) >= "1.3.0" ~ "✓ Compatible",
        pkg == "tidyverse" && packageVersion(pkg) >= "2.0.0" ~ "✓ Compatible",
        pkg == "randtoolbox" && packageVersion(pkg) >= "2.0.0" ~ "✓ Compatible",
        TRUE ~ "? Unknown - recommend updating"
      )

      package_status <- package_status %>%
        add_row(
          package = pkg,
          installed = TRUE,
          version = pkg_version,
          compatible = compatible
        )
    } else {
      package_status <- package_status %>%
        add_row(
          package = pkg,
          installed = FALSE,
          version = "Not installed",
          compatible = "✗ Not installed"
        )
    }
  }

  message("--- Package Compatibility Status ---")
  print(package_status)

  # Test critical functionality
  message("\n--- Testing Critical Functionality ---")

  # Test glmmTMB
  test_data <- data.frame(
    y = rpois(100, lambda = 10),
    x = rnorm(100),
    grp = rep(letters[1:10], each = 10)
  )

  test_result <- try({
    test_model <- glmmTMB(y ~ x + (1|grp), family = poisson, data = test_data)
    TRUE
  }, silent = TRUE)

  if (inherits(test_result, "try-error")) {
    message("✗ glmmTMB test FAILED - Check package installation")
  } else {
    message("✓ glmmTMB test PASSED")
  }

  # Test nflreadr (if installed)
  if ("nflreadr" %in% installed.packages()[, "Package"]) {
    test_nfl <- try({
      nflreadr::load_schedules(seasons = year(Sys.Date()))
      TRUE
    }, silent = TRUE)

    if (inherits(test_nfl, "try-error")) {
      message("✗ nflreadr test FAILED")
    } else {
      message("✓ nflreadr test PASSED")
    }
  }

  return(package_status)
}

compatibility_results <- check_r_compatibility()
saveRDS(compatibility_results, "model_validation_compatibility.rds")

# =============================================================================
# SECTION 8: Final Report and Recommendations
# =============================================================================

message("\n===== SECTION 8: Final Report and Recommendations =====\n")

#' Generate final validation report
generate_final_report <- function(cv_res, sig_res, effects, diags) {

  message("\n" , paste(rep("=", 70), collapse = ""))
  message("NFL MODEL VALIDATION REPORT")
  message(paste(rep("=", 70), collapse = ""))

  message("\n1. CROSS-VALIDATION PERFORMANCE")
  message(paste(rep("-", 70), collapse = ""))
  message(sprintf("  RMSE:        %.3f ± %.3f points",
                  cv_res$summary_stats$mean_rmse,
                  cv_res$summary_stats$sd_rmse))
  message(sprintf("  MAE:         %.3f ± %.3f points",
                  cv_res$summary_stats$mean_mae,
                  cv_res$summary_stats$sd_mae))
  message(sprintf("  Log Loss:    %.3f ± %.3f",
                  cv_res$summary_stats$mean_log_loss,
                  cv_res$summary_stats$sd_log_loss))
  message(sprintf("  Brier Score: %.3f ± %.3f",
                  cv_res$summary_stats$mean_brier,
                  cv_res$summary_stats$sd_brier))

  message("\n  INTERPRETATION:")
  message(sprintf("  - Typical prediction error: ~%.1f points",
                  cv_res$summary_stats$mean_rmse))
  message(sprintf("  - Model precision is %s",
                  ifelse(cv_res$summary_stats$mean_rmse < 11, "GOOD",
                         ifelse(cv_res$summary_stats$mean_rmse < 13, "ACCEPTABLE", "NEEDS IMPROVEMENT"))))

  message("\n2. STATISTICAL SIGNIFICANCE")
  message(paste(rep("-", 70), collapse = ""))

  for (i in 1:nrow(sig_res$significance_results)) {
    row <- sig_res$significance_results[i, ]
    message(sprintf("  %s: %s", row$component, row$interpretation))
  }

  message("\n  KEY FINDINGS:")
  n_sig <- sum(sig_res$significance_results$significant, na.rm = TRUE)
  message(sprintf("  - %d out of 3 core model components are statistically significant", n_sig))

  if (n_sig == 3) {
    message("  - ALL components should be RETAINED in the model ✓")
  } else {
    message("  - Review non-significant components for potential removal")
  }

  message("\n3. EFFECT SIZES")
  message(paste(rep("-", 70), collapse = ""))

  for (i in 1:nrow(effects$effect_summary)) {
    row <- effects$effect_summary[i, ]
    message(sprintf("  %s: %s", row$component, row$interpretation))
  }

  message("\n  VARIANCE EXPLAINED:")
  for (i in 1:nrow(effects$icc_summary)) {
    row <- effects$icc_summary[i, ]
    message(sprintf("  %s: %s", row$component, row$percentage))
  }

  message("\n4. MODEL DIAGNOSTICS")
  message(paste(rep("-", 70), collapse = ""))
  message(sprintf("  Convergence: %s",
                  ifelse(diags$convergence, "SUCCESS ✓", "FAILED ✗")))
  message(sprintf("  Outliers: %d observations (%.2f%%)",
                  nrow(diags$outliers),
                  100 * nrow(diags$outliers) / nrow(stacked)))
  message(sprintf("  Residual normality: p = %.4f %s",
                  diags$shapiro_test$p.value,
                  ifelse(diags$shapiro_test$p.value < 0.05, "(non-normal)", "(approximately normal)")))

  message("\n5. RECOMMENDATIONS")
  message(paste(rep("-", 70), collapse = ""))

  recommendations <- c()

  # Recommendation based on significance
  if (all(sig_res$significance_results$significant, na.rm = TRUE)) {
    recommendations <- c(recommendations,
                         "✓ All core model components are significant - RETAIN ALL")
  } else {
    non_sig <- sig_res$significance_results %>%
      filter(!significant) %>%
      pull(component)
    recommendations <- c(recommendations,
                         sprintf("⚠ Consider removing: %s", paste(non_sig, collapse = ", ")))
  }

  # Recommendation based on CV performance
  if (cv_res$summary_stats$mean_rmse < 11) {
    recommendations <- c(recommendations,
                         "✓ Model precision is GOOD - Continue current approach")
  } else if (cv_res$summary_stats$mean_rmse < 13) {
    recommendations <- c(recommendations,
                         "⚠ Model precision is ACCEPTABLE - Consider adding more predictors")
  } else {
    recommendations <- c(recommendations,
                         "✗ Model precision NEEDS IMPROVEMENT - Review feature engineering")
  }

  # Recommendation based on diagnostics
  if (nrow(diags$outliers) / nrow(stacked) > 0.05) {
    recommendations <- c(recommendations,
                         sprintf("⚠ High outlier rate (%.1f%%) - Investigate extreme predictions",
                                 100 * nrow(diags$outliers) / nrow(stacked)))
  }

  # Adjustment testing recommendations
  recommendations <- c(recommendations,
                       "→ NEXT STEPS: Test adjustment variables (see Section 4)",
                       "→ Priority 1: Rest, injuries, weather, QB status, recent form",
                       "→ Priority 2: Travel, pass protection, explosive plays",
                       "→ Use bootstrap resampling to test each adjustment",
                       "→ Remove adjustments with p > 0.10 and no practical benefit")

  for (i in seq_along(recommendations)) {
    message(sprintf("  %d. %s", i, recommendations[i]))
  }

  message("\n" , paste(rep("=", 70), collapse = ""))
  message("END OF REPORT")
  message(paste(rep("=", 70), collapse = ""))

  return(recommendations)
}

final_report <- generate_final_report(cv_results, sig_results, effect_sizes, diagnostics)

# =============================================================================
# Save all results
# =============================================================================

message("\n===== Saving Results =====\n")

all_results <- list(
  cv_results = cv_results,
  significance_results = sig_results,
  effect_sizes = effect_sizes,
  diagnostics = diagnostics,
  adjustment_tests = adjustment_tests,
  compatibility = compatibility_results,
  recommendations = final_report,
  timestamp = Sys.time(),
  r_version = R.version.string
)

saveRDS(all_results, "model_validation_full_results.rds")
message("All results saved to: model_validation_full_results.rds")

message("\n✓ Model validation complete!")
message("\nTo view results later, run: readRDS('model_validation_full_results.rds')")
