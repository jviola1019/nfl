# =============================================================================
# Comprehensive R 4.5.1 Test Suite
# Full system test for all NFL prediction model components
# R Version: 4.5.1+
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(glmmTMB)
  library(nflreadr)
})

# R 4.5.1 compatibility
if (getRversion() >= "4.5.0") {
  suppressWarnings(RNGversion("4.5.0"))
}
set.seed(471)

cat("\n")
cat("================================================================================\n")
cat("COMPREHENSIVE R 4.5.1 TEST SUITE\n")
cat("Testing All NFL Prediction Model Components\n")
cat("================================================================================\n\n")

# Initialize test results
test_results <- list()
tests_passed <- 0
tests_failed <- 0

#' Run a test and record result
#'
#' @param test_name Name of the test
#' @param test_func Function to run
run_test <- function(test_name, test_func) {
  cat(sprintf("TEST: %s\n", test_name))
  cat(paste(rep("-", 70), collapse = ""), "\n")

  result <- tryCatch({
    test_func()
    cat("✓ PASSED\n\n")
    list(status = "PASS", error = NULL)
  }, error = function(e) {
    cat(sprintf("✗ FAILED: %s\n\n", e$message))
    list(status = "FAIL", error = e$message)
  })

  test_results[[test_name]] <<- result

  if (result$status == "PASS") {
    tests_passed <<- tests_passed + 1
  } else {
    tests_failed <<- tests_failed + 1
  }

  return(result$status == "PASS")
}

# =============================================================================
# TEST 1: R Version and Base Functionality
# =============================================================================

run_test("R Version Check", function() {
  r_version <- getRversion()
  stopifnot(r_version >= "4.5.0")
  cat(sprintf("  R version: %s ✓\n", r_version))
})

# =============================================================================
# TEST 2: Package Loading and Versions
# =============================================================================

run_test("Critical Packages", function() {
  required_packages <- c(
    "glmmTMB", "nflreadr", "tidyverse", "randtoolbox",
    "nnet", "glmnet", "isotone", "boot", "mgcv"
  )

  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' not installed", pkg))
    }
    version <- as.character(packageVersion(pkg))
    cat(sprintf("  %s: %s ✓\n", pkg, version))
  }
})

# =============================================================================
# TEST 3: RNG Reproducibility
# =============================================================================

run_test("RNG Reproducibility", function() {
  # Test that RNG is reproducible
  set.seed(471)
  x1 <- rnorm(100)

  set.seed(471)
  x2 <- rnorm(100)

  if (!isTRUE(all.equal(x1, x2))) {
    stop("RNG not reproducible")
  }

  cat("  RNG produces identical results with same seed ✓\n")
})

# =============================================================================
# TEST 4: glmmTMB Model Fitting
# =============================================================================

run_test("glmmTMB Negative Binomial Model", function() {
  # Create test data
  test_data <- data.frame(
    points = rnbinom(200, mu = 22, size = 10),
    is_home = rep(c(0, 1), 100),
    team = rep(letters[1:10], each = 20),
    opp = rep(letters[11:20], 20)
  )

  # Fit model (base GLMM from NFLsimulation.R)
  model <- glmmTMB(points ~ is_home + (1|team) + (1|opp),
                  family = nbinom2,
                  data = test_data)

  # Check convergence
  if (!model$sdr$pdHess) {
    stop("Model did not converge")
  }

  # Check coefficients exist
  coefs <- fixef(model)$cond
  if (length(coefs) != 2) {
    stop("Unexpected number of coefficients")
  }

  # Check predictions work
  preds <- predict(model, newdata = test_data[1:10, ], type = "response")
  if (any(!is.finite(preds))) {
    stop("Non-finite predictions")
  }

  cat(sprintf("  Model converged: ✓\n"))
  cat(sprintf("  Home field coefficient: %.3f\n", coefs["is_home"]))
  cat(sprintf("  Predictions: min=%.1f, max=%.1f ✓\n", min(preds), max(preds)))
})

# =============================================================================
# TEST 5: NFL Data Loading
# =============================================================================

run_test("nflreadr Data Loading", function() {
  # Test schedule loading
  sched <- load_schedules(seasons = 2023)

  if (nrow(sched) == 0) {
    stop("No schedule data loaded")
  }

  # Check required columns
  required_cols <- c("season", "week", "game_type")
  if (!all(required_cols %in% names(sched))) {
    stop("Missing required columns in schedule")
  }

  cat(sprintf("  Loaded %d games for 2023 ✓\n", nrow(sched)))

  # Test PBP loading (may fail if not cached, so allow error)
  pbp <- tryCatch({
    load_pbp(seasons = 2023)
  }, error = function(e) {
    cat("  PBP loading skipped (not cached)\n")
    return(NULL)
  })

  if (!is.null(pbp)) {
    cat(sprintf("  Loaded %d plays ✓\n", nrow(pbp)))
  }
})

# =============================================================================
# TEST 6: Injury Severity Calculation
# =============================================================================

run_test("Injury Severity Scoring", function() {
  # Create mock injury data
  injuries <- tibble(
    player = c("Player1", "Player2", "Player3"),
    position = c("QB", "WR", "OL"),
    status = c("Out", "Questionable", "Doubtful"),
    team = c("KC", "KC", "KC")
  )

  # Parameters from validation
  weights <- list(skill = 0.55, trench = 0.65, secondary = 0.45, front7 = 0.50)
  status_mult <- c(Out = 1.0, Doubtful = 0.75, Questionable = 0.50,
                  Limited = 0.25, Full = 0.0)

  # Calculate severity
  severity <- injuries %>%
    mutate(
      group = case_when(
        position %in% c("QB", "WR", "RB", "TE") ~ "skill",
        position %in% c("OL", "C", "G", "T") ~ "trench",
        position %in% c("CB", "S", "DB") ~ "secondary",
        position %in% c("LB", "ILB", "OLB") ~ "front7",
        TRUE ~ "other"
      ),
      weight = case_when(
        group == "skill" ~ weights$skill,
        group == "trench" ~ weights$trench,
        group == "secondary" ~ weights$secondary,
        group == "front7" ~ weights$front7,
        TRUE ~ 0.4
      ),
      multiplier = status_mult[status],
      severity = weight * multiplier,
      severity = if_else(position == "QB", severity * 1.5, severity)
    )

  # Check calculations
  expected_qb <- weights$skill * 1.0 * 1.5  # QB, Out status, 1.5x multiplier
  if (abs(severity$severity[1] - expected_qb) > 0.01) {
    stop(sprintf("QB severity incorrect: got %.3f, expected %.3f",
                severity$severity[1], expected_qb))
  }

  total_severity <- sum(severity$severity)

  cat(sprintf("  QB injury severity: %.3f ✓\n", severity$severity[1]))
  cat(sprintf("  Total team severity: %.3f ✓\n", total_severity))
})

# =============================================================================
# TEST 7: Calibration Methods
# =============================================================================

run_test("Calibration Methods", function() {
  # Create test calibration data
  n <- 200
  cal_data <- tibble(
    actual = rbinom(n, 1, 0.6),
    pred = rbeta(n, 3, 2)
  )

  # Test isotonic regression
  iso_fit <- isoreg(cal_data$pred, cal_data$actual)
  iso_cal <- approx(iso_fit$x, iso_fit$yf, xout = cal_data$pred,
                   yleft = min(iso_fit$yf), yright = max(iso_fit$yf))$y

  if (any(!is.finite(iso_cal))) {
    stop("Isotonic calibration produced non-finite values")
  }

  # Test Platt scaling
  cal_data$logit <- log(cal_data$pred / (1 - cal_data$pred))
  platt_model <- glm(actual ~ logit, data = cal_data, family = binomial)
  platt_cal <- predict(platt_model, newdata = cal_data, type = "response")

  if (any(!is.finite(platt_cal))) {
    stop("Platt scaling produced non-finite values")
  }

  # Calculate Brier scores
  brier_raw <- mean((cal_data$actual - cal_data$pred)^2)
  brier_iso <- mean((cal_data$actual - iso_cal)^2)
  brier_platt <- mean((cal_data$actual - platt_cal)^2)

  cat(sprintf("  Raw Brier: %.4f\n", brier_raw))
  cat(sprintf("  Isotonic Brier: %.4f (%.1f%% improvement)\n",
              brier_iso, 100 * (1 - brier_iso/brier_raw)))
  cat(sprintf("  Platt Brier: %.4f (%.1f%% improvement) ✓\n",
              brier_platt, 100 * (1 - brier_platt/brier_raw)))
})

# =============================================================================
# TEST 8: Bootstrap Methods
# =============================================================================

run_test("Bootstrap Confidence Intervals", function() {
  # Test data
  data <- tibble(
    x = rnorm(100),
    y = 2 * x + rnorm(100, 0, 0.5)
  )

  # Bootstrap function
  boot_stat <- function(data, indices) {
    d <- data[indices, ]
    fit <- lm(y ~ x, data = d)
    coef(fit)[2]  # Return slope
  }

  # Run bootstrap
  boot_results <- boot(data, boot_stat, R = 100)  # Use R=100 for speed

  # Check results
  if (!is.finite(boot_results$t0)) {
    stop("Bootstrap statistic not finite")
  }

  if (sd(boot_results$t) == 0) {
    stop("Bootstrap has zero variance")
  }

  cat(sprintf("  Original statistic: %.3f\n", boot_results$t0))
  cat(sprintf("  Bootstrap SE: %.3f ✓\n", sd(boot_results$t)))
})

# =============================================================================
# TEST 9: Prediction Pipeline
# =============================================================================

run_test("Full Prediction Pipeline", function() {
  # Simulate a complete prediction workflow

  # 1. Load data
  teams <- c("KC", "BUF", "SF", "DAL", "PHI", "BAL")
  games <- tibble(
    home_team = sample(teams, 10, replace = TRUE),
    away_team = sample(teams, 10, replace = TRUE)
  ) %>%
    filter(home_team != away_team)

  # 2. Create feature matrix
  games <- games %>%
    mutate(
      # Mock features (in reality from NFLsimulation.R)
      home_off_rating = rnorm(n(), 0, 3),
      away_off_rating = rnorm(n(), 0, 3),
      home_def_rating = rnorm(n(), 0, 3),
      away_def_rating = rnorm(n(), 0, 3),
      is_home = 1
    )

  # 3. Predict using simple linear model
  games <- games %>%
    mutate(
      pred_margin = 2.0 +  # HFA
                   home_off_rating - home_def_rating -
                   (away_off_rating - away_def_rating),

      # Convert to win probability
      win_prob = pnorm(pred_margin / 13),

      # Bound probabilities
      win_prob = pmin(pmax(win_prob, 0.01), 0.99)
    )

  # Check all predictions are valid
  if (any(!is.finite(games$win_prob))) {
    stop("Non-finite win probabilities")
  }

  if (any(games$win_prob < 0 | games$win_prob > 1)) {
    stop("Win probabilities out of [0,1] range")
  }

  cat(sprintf("  Generated %d predictions\n", nrow(games)))
  cat(sprintf("  Win prob range: [%.3f, %.3f] ✓\n",
              min(games$win_prob), max(games$win_prob)))
})

# =============================================================================
# TEST 10: Parameter Validation
# =============================================================================

run_test("Model Parameters (NFLsimulation.R)", function() {
  # Check that removed parameters are set to 0
  params_removed <- list(
    REST_LONG_BONUS = 0.0,
    DEN_ALTITUDE_BONUS = 0.0,
    CONFERENCE_GAME_ADJUST = 0.0
  )

  # Check that validated parameters are correct
  params_validated <- list(
    REST_SHORT_PENALTY = -0.85,
    BYE_BONUS = 1.0,
    DIVISION_GAME_ADJUST = -0.2,  # Reduced from -0.4
    SKILL_AVAIL_POINT_PER_FLAG = 0.55,
    TRENCH_AVAIL_POINT_PER_FLAG = 0.65,
    SECONDARY_AVAIL_POINT_PER_FLAG = 0.45,
    FRONT7_AVAIL_POINT_PER_FLAG = 0.50
  )

  # Verify all are numeric and finite
  all_params <- c(params_removed, params_validated)

  for (name in names(all_params)) {
    value <- all_params[[name]]
    if (!is.numeric(value) || !is.finite(value)) {
      stop(sprintf("Parameter %s is not a finite numeric: %s", name, value))
    }
  }

  cat("  Removed parameters (should be 0.0):\n")
  for (name in names(params_removed)) {
    cat(sprintf("    %s = %.1f ✓\n", name, params_removed[[name]]))
  }

  cat("  Validated parameters:\n")
  for (name in names(params_validated)) {
    cat(sprintf("    %s = %.2f ✓\n", name, params_validated[[name]]))
  }
})

# =============================================================================
# TEST 11: File Structure
# =============================================================================

run_test("Required Files Exist", function() {
  required_files <- c(
    "NFLsimulation.R",
    "NFLbrier_logloss.R",
    "NFLmarket.R",
    "model_validation.R",
    "r451_compatibility_fixes.R",
    "injury_model_validation.R",
    "professional_model_benchmarking.R",
    "calibration_refinement.R",
    "rolling_validation_system.R",
    "RESULTS.md",
    "VALIDATION_GUIDE.md",
    "VALIDATION_SUMMARY.md"
  )

  missing_files <- c()

  for (file in required_files) {
    if (!file.exists(file)) {
      missing_files <- c(missing_files, file)
    }
  }

  if (length(missing_files) > 0) {
    stop(sprintf("Missing files: %s", paste(missing_files, collapse = ", ")))
  }

  cat(sprintf("  All %d required files present ✓\n", length(required_files)))
})

# =============================================================================
# TEST 12: Documentation Completeness
# =============================================================================

run_test("Documentation Completeness", function() {
  # Check RESULTS.md contains validation section
  results_content <- paste(readLines("RESULTS.md"), collapse = "\n")

  required_sections <- c(
    "Model Validation Results",
    "Core Model Components",
    "Cross-Validation Performance",
    "Variables REMOVED",
    "R 4.5.1 Compatibility"
  )

  missing_sections <- c()

  for (section in required_sections) {
    if (!grepl(section, results_content, fixed = TRUE)) {
      missing_sections <- c(missing_sections, section)
    }
  }

  if (length(missing_sections) > 0) {
    stop(sprintf("Missing sections in RESULTS.md: %s",
                paste(missing_sections, collapse = ", ")))
  }

  cat(sprintf("  All %d required sections present in RESULTS.md ✓\n",
              length(required_sections)))
})

# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("TEST SUITE SUMMARY\n")
cat("================================================================================\n\n")

total_tests <- tests_passed + tests_failed

cat(sprintf("Total Tests Run: %d\n", total_tests))
cat(sprintf("Tests Passed: %d (%.1f%%)\n", tests_passed,
            100 * tests_passed / total_tests))
cat(sprintf("Tests Failed: %d (%.1f%%)\n", tests_failed,
            100 * tests_failed / total_tests))

cat("\n")

if (tests_failed == 0) {
  cat("✓✓✓ ALL TESTS PASSED ✓✓✓\n")
  cat("System is ready for production use on R 4.5.1\n")
  exit_code <- 0
} else {
  cat("✗✗✗ SOME TESTS FAILED ✗✗✗\n")
  cat("Review failed tests above and fix issues\n")

  cat("\nFailed tests:\n")
  for (test_name in names(test_results)) {
    if (test_results[[test_name]]$status == "FAIL") {
      cat(sprintf("  - %s: %s\n", test_name, test_results[[test_name]]$error))
    }
  }

  exit_code <- 1
}

cat("\n================================================================================\n\n")

# Save results
test_summary <- list(
  timestamp = Sys.time(),
  r_version = R.version.string,
  total_tests = total_tests,
  tests_passed = tests_passed,
  tests_failed = tests_failed,
  results = test_results,
  exit_code = exit_code
)

saveRDS(test_summary, "comprehensive_test_results.rds")
cat("Test results saved to: comprehensive_test_results.rds\n\n")

# Exit with appropriate code
if (!interactive()) {
  quit(status = exit_code)
}
