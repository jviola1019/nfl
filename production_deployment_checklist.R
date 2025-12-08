#!/usr/bin/env Rscript
#
# Production Deployment Checklist for NFL Prediction Model
#
# Purpose: Automated verification before deploying to production
# Ensures: Code quality, R 4.5.1 compatibility, performance standards
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

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  PRODUCTION DEPLOYMENT CHECKLIST                               ║\n")
cat("║  NFL Prediction Model v2.0                                     ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# Results tracking
checks_passed <- 0
checks_failed <- 0
checks_warnings <- 0

# Helper functions
pass_check <- function(msg) {
  cat(sprintf("✓ %s\n", msg))
  checks_passed <<- checks_passed + 1
}

fail_check <- function(msg) {
  cat(sprintf("✗ %s\n", msg))
  checks_failed <<- checks_failed + 1
}

warn_check <- function(msg) {
  cat(sprintf("⚠ %s\n", msg))
  checks_warnings <<- checks_warnings + 1
}

# =============================================================================
# CHECK 1: R VERSION COMPATIBILITY
# =============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("CHECK 1: R Version Compatibility\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

r_version <- getRversion()
cat(sprintf("R version: %s\n\n", r_version))

if (r_version >= "4.5.1") {
  pass_check(sprintf("R version %s >= 4.5.1", r_version))
} else {
  fail_check(sprintf("R version %s < 4.5.1 (REQUIRED)", r_version))
}

cat("\n")

# =============================================================================
# CHECK 2: REQUIRED PACKAGES
# =============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("CHECK 2: Required Packages\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

required_packages <- c(
  "tidyverse", "nflreadr", "lubridate", "glmmTMB", "nnet",
  "randtoolbox", "glmnet", "purrr", "scales", "digest", "httr2"
)

for (pkg in required_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    version <- packageVersion(pkg)
    pass_check(sprintf("%-20s  v%s", pkg, version))
  } else {
    fail_check(sprintf("%-20s  NOT INSTALLED", pkg))
  }
}

cat("\n")

# =============================================================================
# CHECK 3: CORE FILES EXIST
# =============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("CHECK 3: Core Files Exist\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

core_files <- c(
  "config.R",
  "NFLsimulation.R",
  "NFLmarket.R",
  "NFLbrier_logloss.R"
)

for (file in core_files) {
  if (file.exists(file)) {
    size_kb <- file.size(file) / 1024
    pass_check(sprintf("%-30s  (%.1f KB)", file, size_kb))
  } else {
    fail_check(sprintf("%-30s  MISSING", file))
  }
}

cat("\n")

# =============================================================================
# CHECK 4: DOCUMENTATION FILES
# =============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("CHECK 4: Documentation Files\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

doc_files <- c(
  "README.md",
  "GETTING_STARTED.md",
  "DOCUMENTATION.md",
  "UPDATES.md",
  "RESULTS.md",
  "IMPROVEMENTS_SUMMARY.md"
)

for (file in doc_files) {
  if (file.exists(file)) {
    lines <- length(readLines(file, warn = FALSE))
    pass_check(sprintf("%-30s  (%d lines)", file, lines))
  } else {
    warn_check(sprintf("%-30s  MISSING", file))
  }
}

cat("\n")

# =============================================================================
# CHECK 5: R 4.5.1 COMPATIBILITY (CODE PATTERNS)
# =============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("CHECK 5: R 4.5.1 Code Compatibility\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Check NFLsimulation.R for compatibility issues
if (file.exists("NFLsimulation.R")) {
  code <- readLines("NFLsimulation.R", warn = FALSE)

  # Check 1: lag() with vector defaults (should have scalar defaults)
  lag_lines <- grep("lag\\(.*default\\s*=", code, value = FALSE)
  vector_defaults <- grep("lag\\(.*default\\s*=\\s*[^0-9NA]", code[lag_lines], value = FALSE)

  if (length(vector_defaults) == 0) {
    pass_check("All lag() calls use scalar defaults (R 4.5.1 compliant)")
  } else {
    fail_check(sprintf("Found %d lag() calls with possible vector defaults", length(vector_defaults)))
  }

  # Check 2: pivot_longer() duplicate column prevention
  pivot_lines <- grep("pivot_longer", code, value = FALSE)
  if (length(pivot_lines) > 0) {
    # Check if there's a select(-any_of("location")) before each pivot_longer
    has_protection <- sapply(pivot_lines, function(i) {
      prev_5 <- code[max(1, i-5):i]
      any(grepl("select.*-any_of", prev_5))
    })

    if (all(has_protection)) {
      pass_check("All pivot_longer() calls have duplicate column protection")
    } else {
      warn_check(sprintf("%d pivot_longer() calls may need duplicate protection", sum(!has_protection)))
    }
  } else {
    pass_check("No pivot_longer() calls found")
  }

  # Check 3: No deprecated functions
  deprecated_patterns <- c(
    "aes_string", "funs\\(", "gather\\(", "spread\\("
  )

  found_deprecated <- FALSE
  for (pattern in deprecated_patterns) {
    if (any(grepl(pattern, code))) {
      fail_check(sprintf("Found deprecated pattern: %s", pattern))
      found_deprecated <- TRUE
    }
  }

  if (!found_deprecated) {
    pass_check("No deprecated functions found")
  }

} else {
  fail_check("NFLsimulation.R not found - cannot check compatibility")
}

cat("\n")

# =============================================================================
# CHECK 6: PARAMETER VALIDATION
# =============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("CHECK 6: Parameter Validation Documentation\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

if (file.exists("config.R")) {
  config_code <- readLines("config.R", warn = FALSE)

  # Check for validation documentation
  validation_lines <- grep("@validation", config_code)

  if (length(validation_lines) > 0) {
    pass_check(sprintf("Found %d parameters with @validation documentation", length(validation_lines)))

    # Check for p-values
    pvalue_lines <- grep("p\\s*[=<>]", config_code[validation_lines])
    if (length(pvalue_lines) > 0) {
      pass_check(sprintf("Found %d parameters with p-value documentation", length(pvalue_lines)))
    } else {
      warn_check("No p-values found in validation documentation")
    }

    # Check for dataset provenance
    provenance_lines <- grep("@validated_on", config_code)
    if (length(provenance_lines) > 0) {
      pass_check(sprintf("Found %d parameters with dataset provenance", length(provenance_lines)))
    } else {
      warn_check("No @validated_on tags found - unclear which dataset was used")
    }
  } else {
    warn_check("No @validation documentation found in config.R")
  }
} else {
  fail_check("config.R not found")
}

cat("\n")

# =============================================================================
# CHECK 7: OVERFITTING INDICATORS
# =============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("CHECK 7: Overfitting Risk Assessment\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Check RESULTS.md for performance metrics
if (file.exists("RESULTS.md")) {
  results_content <- paste(readLines("RESULTS.md", warn = FALSE), collapse = "\n")

  # Extract Brier scores from documentation
  train_brier_match <- regmatches(results_content, regexpr("TRAIN.*?0\\.21[0-9]{2}", results_content))
  valid_brier_match <- regmatches(results_content, regexpr("VALIDATION.*?0\\.21[0-9]{2}", results_content))

  if (length(train_brier_match) > 0 && length(valid_brier_match) > 0) {
    # Extract numbers
    train_brier <- as.numeric(sub(".*?(0\\.21[0-9]{2}).*", "\\1", train_brier_match))
    valid_brier <- as.numeric(sub(".*?(0\\.21[0-9]{2}).*", "\\1", valid_brier_match))

    gap <- valid_brier - train_brier
    gap_pct <- 100 * gap / train_brier

    cat(sprintf("Train Brier:      %.4f\n", train_brier))
    cat(sprintf("Validation Brier: %.4f\n", valid_brier))
    cat(sprintf("Gap:              %.4f (%.2f%%)\n\n", gap, gap_pct))

    if (gap_pct < 1.0) {
      pass_check(sprintf("Train→Validation gap %.2f%% < 1%% (excellent)", gap_pct))
    } else if (gap_pct < 2.0) {
      pass_check(sprintf("Train→Validation gap %.2f%% < 2%% (good)", gap_pct))
    } else if (gap_pct < 5.0) {
      warn_check(sprintf("Train→Validation gap %.2f%% - some overfitting", gap_pct))
    } else {
      fail_check(sprintf("Train→Validation gap %.2f%% > 5%% - significant overfitting", gap_pct))
    }
  } else {
    warn_check("Could not extract Brier scores from RESULTS.md")
  }
} else {
  warn_check("RESULTS.md not found - cannot check overfitting")
}

cat("\n")

# =============================================================================
# CHECK 8: PERFORMANCE STANDARDS
# =============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("CHECK 8: Performance Standards\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Performance thresholds
BRIER_MAX <- 0.215
RMSE_MAX <- 11.0
ACCURACY_MIN <- 0.65

cat("Performance thresholds:\n")
cat(sprintf("  Brier Score:   < %.3f\n", BRIER_MAX))
cat(sprintf("  RMSE:          < %.1f points\n", RMSE_MAX))
cat(sprintf("  Accuracy:      > %.1f%%\n\n", 100*ACCURACY_MIN))

# Check if performance metrics meet standards (from documentation)
if (file.exists("IMPROVEMENTS_SUMMARY.md")) {
  content <- paste(readLines("IMPROVEMENTS_SUMMARY.md", warn = FALSE), collapse = "\n")

  # Check Brier
  if (grepl("0\\.210[0-9]", content)) {
    pass_check(sprintf("Brier score ~0.210x < %.3f threshold", BRIER_MAX))
  } else {
    warn_check("Could not verify Brier score from documentation")
  }

  # Check RMSE
  if (grepl("10\\.[0-9]{2}", content)) {
    pass_check(sprintf("RMSE ~10.x < %.1f threshold", RMSE_MAX))
  } else {
    warn_check("Could not verify RMSE from documentation")
  }

  # Check accuracy
  if (grepl("67\\.[0-9]%", content)) {
    pass_check(sprintf("Accuracy ~67%% > %.0f%% threshold", 100*ACCURACY_MIN))
  } else {
    warn_check("Could not verify accuracy from documentation")
  }
} else {
  warn_check("IMPROVEMENTS_SUMMARY.md not found - cannot verify performance")
}

cat("\n")

# =============================================================================
# CHECK 9: GIT STATUS
# =============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("CHECK 9: Git Repository Status\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

git_status <- system("git status --porcelain 2>&1", intern = TRUE)

if (length(git_status) == 0 || all(git_status == "")) {
  pass_check("Working directory is clean (no uncommitted changes)")
} else {
  warn_check(sprintf("Found %d uncommitted changes", length(git_status)))
  if (length(git_status) <= 5) {
    for (line in git_status) {
      cat(sprintf("  %s\n", line))
    }
  }
}

cat("\n")

# =============================================================================
# CHECK 10: RECENT VALIDATION
# =============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("CHECK 10: Recent Validation\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Check for recent validation results
validation_files <- list.files(pattern = "validation.*\\.rds", ignore.case = TRUE)

if (length(validation_files) > 0) {
  # Get most recent
  file_times <- file.mtime(validation_files)
  most_recent <- validation_files[which.max(file_times)]
  days_ago <- as.numeric(difftime(Sys.time(), max(file_times), units = "days"))

  cat(sprintf("Most recent validation: %s\n", most_recent))
  cat(sprintf("Age: %.1f days ago\n\n", days_ago))

  if (days_ago < 7) {
    pass_check("Validation results are recent (<7 days)")
  } else if (days_ago < 30) {
    warn_check("Validation results are >7 days old - consider re-running")
  } else {
    warn_check("Validation results are >30 days old - should re-run before deployment")
  }
} else {
  warn_check("No validation result files found (.rds)")
}

cat("\n")

# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("═══════════════════════════════════════════════════════════════\n")
cat("DEPLOYMENT CHECKLIST SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

total_checks <- checks_passed + checks_failed + checks_warnings

cat(sprintf("Total checks:   %d\n", total_checks))
cat(sprintf("✓ Passed:       %d (%.0f%%)\n", checks_passed, 100*checks_passed/total_checks))
cat(sprintf("✗ Failed:       %d (%.0f%%)\n", checks_failed, 100*checks_failed/total_checks))
cat(sprintf("⚠ Warnings:     %d (%.0f%%)\n\n", checks_warnings, 100*checks_warnings/total_checks))

if (checks_failed == 0 && checks_warnings == 0) {
  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║  ✓✓✓ READY FOR PRODUCTION DEPLOYMENT ✓✓✓                      ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n")
  cat("\n")
  cat("All checks passed! Model is ready for production use.\n")
  cat("\n")
  deployment_status <- "APPROVED"
} else if (checks_failed == 0) {
  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║  ⚠ READY WITH WARNINGS ⚠                                       ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n")
  cat("\n")
  cat("Model can be deployed, but review warnings above.\n")
  cat("\n")
  deployment_status <- "APPROVED_WITH_WARNINGS"
} else {
  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║  ✗✗✗ NOT READY FOR DEPLOYMENT ✗✗✗                             ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n")
  cat("\n")
  cat("Critical issues found. Fix failed checks before deploying.\n")
  cat("\n")
  deployment_status <- "REJECTED"
}

# Save results
results <- list(
  timestamp = Sys.time(),
  r_version = as.character(r_version),
  checks_passed = checks_passed,
  checks_failed = checks_failed,
  checks_warnings = checks_warnings,
  status = deployment_status
)

saveRDS(results, "deployment_checklist_results.rds")
cat("✓ Results saved to: deployment_checklist_results.rds\n")

cat("\n")
cat("════════════════════════════════════════════════════════════════\n")
cat("Deployment checklist complete!\n")
cat("════════════════════════════════════════════════════════════════\n")
cat("\n")

# Exit with appropriate code
if (deployment_status == "REJECTED") {
  quit(status = 1)
} else {
  quit(status = 0)
}
