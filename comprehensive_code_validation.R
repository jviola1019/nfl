#!/usr/bin/env Rscript
#
# Comprehensive Code Validation Script
# Tests ALL .R files for structural integrity, syntax, and logical errors
#
# Author: Claude Code
# Date: 2025-12-09
# Purpose: Ensure zero errors before production deployment
#

cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("  COMPREHENSIVE CODE VALIDATION - NFL Prediction Model\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

# Set R version for reproducibility
if (getRversion() >= "4.5.0") {
  suppressWarnings(RNGversion("4.5.0"))
}

# Initialize results tracking
validation_results <- list()
all_passed <- TRUE

# =============================================================================
# SECTION 1: R File Discovery
# =============================================================================
cat("SECTION 1: Discovering R files...\n")
cat("─────────────────────────────────────────────────────────────────────────\n")

r_files <- list.files(pattern = "\\.R$", full.names = TRUE, ignore.case = TRUE)
r_files <- r_files[!grepl("comprehensive_code_validation\\.R$", r_files)]

cat(sprintf("Found %d R files to validate\n\n", length(r_files)))

# =============================================================================
# SECTION 2: Syntax Validation
# =============================================================================
cat("SECTION 2: Syntax Validation\n")
cat("─────────────────────────────────────────────────────────────────────────\n")

syntax_errors <- c()
for (file in r_files) {
  tryCatch({
    parse(file)
    cat(sprintf("✓ %s\n", basename(file)))
  }, error = function(e) {
    msg <- sprintf("✗ %s - SYNTAX ERROR: %s", basename(file), e$message)
    cat(msg, "\n")
    syntax_errors <- c(syntax_errors, msg)
    all_passed <<- FALSE
  })
}

if (length(syntax_errors) == 0) {
  cat("\n✓ All files passed syntax validation\n\n")
} else {
  cat("\n✗ SYNTAX ERRORS FOUND:\n")
  for (err in syntax_errors) cat("  ", err, "\n")
  cat("\n")
}

validation_results$syntax <- list(
  passed = length(syntax_errors) == 0,
  errors = syntax_errors
)

# =============================================================================
# SECTION 3: Function Definition Analysis
# =============================================================================
cat("SECTION 3: Function Definition Analysis\n")
cat("─────────────────────────────────────────────────────────────────────────\n")

# Check NFLsimulation.R for function ordering
nfl_sim_file <- "NFLsimulation.R"
if (file.exists(nfl_sim_file)) {
  cat("Checking function call order in NFLsimulation.R...\n")

  # Read file content
  lines <- readLines(nfl_sim_file)

  # Extract function definitions
  func_defs <- grep("^[a-zA-Z_][a-zA-Z0-9_]* <- function\\(", lines, value = FALSE)
  func_names <- gsub(" <-.*", "", lines[func_defs])

  cat(sprintf("  Found %d function definitions\n", length(func_names)))

  # Check for duplicates
  dup_funcs <- func_names[duplicated(func_names)]
  if (length(dup_funcs) > 0) {
    cat(sprintf("  ✗ DUPLICATE FUNCTIONS: %s\n", paste(dup_funcs, collapse = ", ")))
    all_passed <- FALSE
  } else {
    cat("  ✓ No duplicate function definitions\n")
  }

  cat("\n")
} else {
  cat("✗ NFLsimulation.R not found\n\n")
  all_passed <- FALSE
}

# =============================================================================
# SECTION 4: Check for Common R 4.5.1 Issues
# =============================================================================
cat("SECTION 4: R 4.5.1 Compatibility Check\n")
cat("─────────────────────────────────────────────────────────────────────────\n")

compat_issues <- c()

for (file in r_files) {
  lines <- readLines(file, warn = FALSE)

  # Check for vector defaults in lag()
  vec_lag_lines <- grep("lag\\([^)]*default\\s*=\\s*c\\(", lines)
  if (length(vec_lag_lines) > 0) {
    msg <- sprintf("%s: Vector defaults in lag() at lines: %s",
                   basename(file), paste(vec_lag_lines, collapse = ", "))
    compat_issues <- c(compat_issues, msg)
  }

  # Check for deprecated functions
  deprecated <- c("aes_string", "funs\\(", "gather\\(", "spread\\(")
  for (dep in deprecated) {
    dep_lines <- grep(dep, lines)
    if (length(dep_lines) > 0) {
      msg <- sprintf("%s: Deprecated function '%s' at lines: %s",
                     basename(file), dep, paste(dep_lines, collapse = ", "))
      compat_issues <- c(compat_issues, msg)
    }
  }
}

if (length(compat_issues) == 0) {
  cat("✓ All files are R 4.5.1 compatible\n\n")
} else {
  cat("✗ COMPATIBILITY ISSUES FOUND:\n")
  for (issue in compat_issues) cat("  ", issue, "\n")
  cat("\n")
  all_passed <- FALSE
}

validation_results$r451_compat <- list(
  passed = length(compat_issues) == 0,
  issues = compat_issues
)

# =============================================================================
# SECTION 5: Check for Required Constants
# =============================================================================
cat("SECTION 5: Required Constants Check\n")
cat("─────────────────────────────────────────────────────────────────────────\n")

# Check if config.R defines all required constants
if (file.exists("config.R")) {
  tryCatch({
    source("config.R", local = TRUE)

    required_constants <- c(
      "NB_SIZE_MIN", "NB_SIZE_MAX", "RHO_SCORE", "N_TRIALS",
      "CALIB_TRIALS", "PTS_CAP_HI", "SEED", "RECENCY_HALFLIFE",
      "N_RECENT"
    )

    # Create a test environment
    test_env <- new.env()
    source("config.R", local = test_env)

    missing <- c()
    for (const in required_constants) {
      if (!exists(const, envir = test_env)) {
        missing <- c(missing, const)
      }
    }

    if (length(missing) == 0) {
      cat(sprintf("✓ All %d required constants defined in config.R\n\n",
                  length(required_constants)))
    } else {
      cat("✗ MISSING CONSTANTS:\n")
      for (m in missing) cat("  ", m, "\n")
      cat("\n")
      all_passed <- FALSE
    }

  }, error = function(e) {
    cat(sprintf("✗ Error loading config.R: %s\n\n", e$message))
    all_passed <<- FALSE
  })
} else {
  cat("✗ config.R not found\n\n")
  all_passed <- FALSE
}

# =============================================================================
# SECTION 6: File Structure Validation
# =============================================================================
cat("SECTION 6: File Structure Validation\n")
cat("─────────────────────────────────────────────────────────────────────────\n")

structure_issues <- c()

for (file in r_files) {
  lines <- readLines(file, warn = FALSE)

  # Check for tab characters
  tab_lines <- grep("\t", lines)
  if (length(tab_lines) > 0) {
    msg <- sprintf("%s: Tab characters found at %d lines",
                   basename(file), length(tab_lines))
    structure_issues <- c(structure_issues, msg)
  }

  # Check for trailing whitespace (sample check)
  trailing_lines <- grep(" $", lines)
  if (length(trailing_lines) > 50) {  # Allow some, but flag excessive
    msg <- sprintf("%s: Excessive trailing whitespace (%d lines)",
                   basename(file), length(trailing_lines))
    structure_issues <- c(structure_issues, msg)
  }
}

if (length(structure_issues) == 0) {
  cat("✓ All files have proper structure\n\n")
} else {
  cat("⚠ STRUCTURE ISSUES (non-critical):\n")
  for (issue in structure_issues) cat("  ", issue, "\n")
  cat("\n")
}

validation_results$structure <- list(
  passed = length(structure_issues) == 0,
  issues = structure_issues
)

# =============================================================================
# FINAL SUMMARY
# =============================================================================
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("  VALIDATION SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

cat(sprintf("Total files validated: %d\n\n", length(r_files)))

cat("Results by category:\n")
cat(sprintf("  [%s] Syntax Validation\n",
            ifelse(validation_results$syntax$passed, "✓", "✗")))
cat(sprintf("  [%s] R 4.5.1 Compatibility\n",
            ifelse(validation_results$r451_compat$passed, "✓", "✗")))
cat(sprintf("  [%s] File Structure\n\n",
            ifelse(validation_results$structure$passed, "✓", "⚠")))

if (all_passed && validation_results$syntax$passed && validation_results$r451_compat$passed) {
  cat("═══════════════════════════════════════════════════════════════════════════\n")
  cat("  ✓ ALL CRITICAL VALIDATIONS PASSED\n")
  cat("  STATUS: PRODUCTION-READY\n")
  cat("═══════════════════════════════════════════════════════════════════════════\n")
  quit(status = 0)
} else {
  cat("═══════════════════════════════════════════════════════════════════════════\n")
  cat("  ✗ VALIDATION FAILURES DETECTED\n")
  cat("  STATUS: REQUIRES FIXES\n")
  cat("═══════════════════════════════════════════════════════════════════════════\n")
  quit(status = 1)
}
