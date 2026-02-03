# NFL Prediction Model - Claude Code Agent Guide

**This file is the SINGLE SOURCE OF TRUTH for how Claude Code should operate in this repository.**

---

## 1. REPO CONTEXT

### What This Project Does

An NFL game prediction model using Monte Carlo simulation with:
- Negative Binomial score distributions with Gaussian copula correlation
- Spline calibration (GAM with smoothing penalty, -6.9% Brier improvement)
- 60% market shrinkage for probability estimates
- 1/8 Kelly staking with edge skepticism
- Strength-of-schedule, injury, and coaching change adjustments

### Primary Entrypoints

| File | Purpose | Run Command |
|------|---------|-------------|
| `run_week.R` | Weekly predictions | `source("run_week.R")` |
| `config.R` | Configuration | Edit `SEASON`, `WEEK_TO_SIM` |
| `scripts/verify_repo_integrity.R` | Integrity check | `Rscript scripts/verify_repo_integrity.R` |
| `scripts/run_matrix.R` | Run all artifacts | `Rscript scripts/run_matrix.R` |

### Expected Outputs

When `run_week.R` completes successfully:
1. **HTML Report**: `NFLvsmarket_report.html` with game predictions
2. **Run Logs**: `run_logs/config_*.rds`, `run_logs/final_*.rds`
3. **Console**: Data quality badge, simulation progress, calibration status

### What "Correct" Looks Like

- `scripts/verify_repo_integrity.R`: 35/35 checks pass
- `scripts/run_matrix.R`: 9/9 artifacts pass
- `testthat::test_dir("tests/testthat")`: ~575 tests pass (some skips OK)
- `run_week.R`: Completes without exit code 1

---

## 2. AGENT OPERATING RULES

### ALWAYS Do This

1. **Start in READ-ONLY audit mode** - Never modify code before understanding the failure
2. **Reproduce errors before fixing** - Run the failing command first
3. **Prefer minimal diffs** - Small, reversible changes only
4. **Run verification after changes** - `Rscript scripts/verify_repo_integrity.R`
5. **No silent fallbacks** - Every data issue must be logged and flagged
6. **No unverifiable claims** - Don't claim metrics without running scripts
7. **Use canonical API** - Check R/data_validation.R for correct function signatures

### NEVER Do This

1. **Guess at fixes** without understanding root cause
2. **Add features** while fixing bugs
3. **Refactor** unrelated code
4. **Delete files** without verifying they're unused via run_matrix
5. **Commit untested changes**
6. **Use hardcoded values** - Put in config.R instead

### Data Quality API Reference

```r
# CORRECT status values (memorize these!)
update_injury_quality("full" | "partial" | "unavailable", missing_seasons = c(...))
update_weather_quality("full" | "partial_fallback" | "all_fallback", fallback_games = c(...))
update_market_quality("full" | "partial" | "unavailable", missing_games = c(...))
update_calibration_quality(method = "...", leakage_free = TRUE/FALSE)

# Access quality (nested structure!)
quality <- get_data_quality()
quality$injury$status      # NOT quality$injury_status
quality$weather$status     # NOT quality$weather_status
quality$market$status      # NOT quality$market_status
quality$calibration$method # NOT quality$calibration_method

# Overall quality returns uppercase
compute_overall_quality()  # Returns "HIGH", "MEDIUM", "LOW", "CRITICAL"
```

---

## 3. STANDARD AGENT PROMPTS

### Audit Agent (Use First)

```
Read the entire repo. Do not edit any files.
Identify:
1. Runtime failures (run run_week.R with traceback)
2. Schema mismatches (check data_validation.R API calls)
3. Join risks (search for *_join without standardize_join_keys)
4. Data quality issues (silent fallbacks, missing error handling)

Output a diagnosis report before any fixes.
```

### Fix Agent (After Audit)

```
Implement the minimal fix for the identified root cause.
Rules:
1. Change only the lines needed to fix the issue
2. Add regression test if applicable
3. No refactors, no feature additions
4. Run verify_repo_integrity.R after fix
5. Document the change in CHANGELOG.md
```

### Validation Agent (After Fixes)

```
Re-run all verification:
1. Rscript scripts/verify_repo_integrity.R (must show 35/35 pass)
2. Rscript scripts/run_matrix.R (must show 9/9 pass)
3. testthat::test_dir("tests/testthat") (check for regressions)
4. Verify HTML report generates if run_week.R was changed

Report: PASS/FAIL with evidence.
```

### Docs Agent (When Requested)

```
Update documentation:
1. README.md - Verify file inventory is complete
2. CHANGELOG.md - Add dated entry for changes made
3. CLAUDE.md - Update if agent rules need clarification
4. Verify all run commands work as documented
```

---

## 4. STOP / CONTINUE CHECKPOINT RULES

### STOP and Ask for Confirmation When:

1. **Deleting any file** (even if appears unused)
2. **Changing config.R defaults** (affects all users)
3. **Modifying NFLsimulation.R or NFLmarket.R** (core engine, high risk)
4. **Adding new dependencies** (affects renv.lock)
5. **Changing API signatures** in R/data_validation.R
6. **Exit code 1 persists** after first fix attempt

### Continue Automatically When:

1. Fixing obvious typos or syntax errors
2. Updating test expectations to match correct behavior
3. Adding log/debug statements
4. Updating documentation only
5. Running verification scripts
6. Creating new test files

---

## 5. COMMON FAILURE PLAYBOOK

### Exit Code 1 After Injury Loading

**Symptom**: Script crashes after "Loaded X injury records for season YYYY"

**Diagnosis**:
```r
options(error=function(){traceback(2); quit(status=1)})
source("run_week.R")
```

**Common Causes**:
1. Wrong API call: `update_injury_quality("complete")` should be `"full"`
2. Wrong parameter: `seasons_missing` should be `missing_seasons`
3. 2025 injury data 404 (expected, should be handled gracefully)

**Fix**: Check NFLsimulation.R lines 3144-3150 for correct API calls.

### Invalid Week Error

**Symptom**: "No games found for Week X"

**Diagnosis**: Check config.R `WEEK_TO_SIM` value against valid weeks.

**Valid Weeks**:
- Regular season: 1-18
- Wild Card: 19
- Divisional: 20
- Conference: 21
- Super Bowl: 22

### Empty Join Results

**Symptom**: `build_moneyline_comparison_table()` returns 0 rows

**Diagnosis**:
```r
# Check key types before join
str(schedule[c("game_id", "season", "week")])
str(predictions[c("game_id", "season", "week")])
```

**Fix**: Apply `standardize_join_keys()` to both tables before joining.

### lintr normalizePath("") Error

**Symptom**: VS Code R extension crashes or shows normalizePath errors

**Fix**: Ensure `.lintr` file exists and excludes problematic directories:
```
exclusions: list("renv" = Inf, "run_logs" = Inf)
```

### HTML Report Not Generated

**Symptom**: run_week.R completes but no HTML file

**Diagnosis**: Check for earlier errors; report generation is near end of pipeline.

**Common Causes**:
1. gt package not installed
2. Previous step produced empty results
3. File write permission issue

### Script Hangs After "Snap-weighted injury impacts enabled"

**Symptom**: Script stops after injury loading, shows "✓ Snap-weighted injury impacts enabled"

**Diagnosis**: Network timeout when loading snap participation data

**Root Cause**:
- `nflreadr::load_participation()` hangs when data unavailable for current season
- The function has no timeout protection

**Fix**:
```r
# Verify in config.R:
USE_SNAP_WEIGHTED_INJURIES <- FALSE  # Must be FALSE
```

**Important**: Disabling snap weighting does NOT affect model accuracy:
- Position-level injury weights remain active (validated p < 0.001)
- Snap weighting had no empirical evidence of improving Brier/log-loss
- The feature was disabled in v2.6.7 as the default

### knitr/xfun GitHub Actions Crash

**Symptom**: GitHub Actions fail with knitr or xfun namespace errors

**Fix**:
1. Run `renv::snapshot()` to capture xfun dependency
2. Verify knitr is in DESCRIPTION Imports (not just Suggests)
3. Push updated renv.lock

---

## 6. FILE INVENTORY (Quick Reference)

### Core Pipeline (DO NOT DELETE)
- `run_week.R` - Entry point
- `config.R` - Configuration
- `NFLsimulation.R` - Simulation engine (8300+ lines)
- `NFLmarket.R` - Market analysis (3900+ lines)
- `NFLbrier_logloss.R` - Metrics

### R/ Library (Canonical Source)
- `R/utils.R` - Core utilities (SINGLE SOURCE OF TRUTH)
- `R/data_validation.R` - Data quality tracking
- `R/logging.R` - Structured logging
- `R/playoffs.R` - Playoff logic
- `R/date_resolver.R` - Date resolution
- `R/sleeper_api.R` - Sleeper fantasy API integration
- `R/red_zone_data.R` - Red zone efficiency metrics
- `R/coaching_adjustments.R` - Coaching change adjustments
- `R/simulation_helpers.R` - Simulation utility functions
- `R/model_diagnostics.R` - Calibration diagnostics

### Scripts
- `scripts/verify_repo_integrity.R` - 35-check verification
- `scripts/verify_requirements.R` - 20-issue audit verification
- `scripts/run_matrix.R` - Execute all artifacts

### Tests
- `tests/testthat/setup.R` - Test infrastructure
- `tests/testthat/test-*.R` - Unit tests

### Documentation
- `README.md` - Project overview
- `GETTING_STARTED.md` - Setup guide
- `DOCUMENTATION.md` - Technical reference
- `CLAUDE.md` - This file (agent guide)
- `AUDIT.md` - Repository audit report
- `CHANGELOG.md` - Change log

---

## 7. VERIFICATION COMMANDS (Run These)

```bash
# 1. Basic integrity (should show 35/35 pass)
Rscript scripts/verify_repo_integrity.R

# 2. Full artifact matrix (should show 9/9 pass)
Rscript scripts/run_matrix.R

# 3. Unit tests (575+ tests, some skips OK)
Rscript -e "testthat::test_dir('tests/testthat')"

# 4. Run weekly simulation (use valid week!)
# Edit config.R first: WEEK_TO_SIM <- 16; SEASON <- 2024
Rscript -e "source('run_week.R')"
```

---

## 8. WHEN IN DOUBT

1. Run `scripts/verify_repo_integrity.R` first
2. Read error messages carefully - they usually tell you exactly what's wrong
3. Check this file's API reference section
4. Search for similar patterns in test files
5. Ask for clarification rather than guessing

---

*Last updated: 2026-01-29*
*Version: 2.6.7*
