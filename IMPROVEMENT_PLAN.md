# NFL Prediction Model - Comprehensive Improvement Plan

## Date: 2026-01-30
## Version: 2.6.8 → 2.7.0

---

## PART 1: STATISTICAL VALIDATION STANDARDS

### 1.1 Brier Score Validation Requirements

Based on [professional research](https://pmc.ncbi.nlm.nih.gov/articles/PMC12463883/):

| Metric | Current Value | Target | Status |
|--------|---------------|--------|--------|
| Brier Score | 0.211 | < 0.220 | ✓ VALID |
| 95% CI | (0.205, 0.217) | Width < 0.02 | ✓ VALID |
| vs Market | +0.004 | Within ±0.01 | ✓ ACCEPTABLE |
| Calibration | Spline (-6.9%) | Documented improvement | ✓ VALID |

### 1.2 Required Statistical Documentation

Every validated parameter must include:
```r
#' @param PARAMETER_NAME Description
#' @default value
#' @range [min, max]
#' @validation Method used (bootstrap, cross-validation, etc.)
#' @p_value < 0.05 for significance
#' @brier_impact +/- X% change
#' @validated_on Training set (years), test set (years)
```

### 1.3 Calibration Requirements

Per [PMC research](https://pmc.ncbi.nlm.nih.gov/articles/PMC3575184/):
- Must include reliability diagrams
- Must report Expected Calibration Error (ECE)
- Must decompose Brier into: Uncertainty, Reliability, Resolution

---

## PART 2: CODE CONSOLIDATION

### 2.1 Duplicate Functions to Consolidate

| Function | Current Locations | Canonical Location |
|----------|-------------------|-------------------|
| `standardize_join_keys` | R/utils.R, NFLmarket.R | R/utils.R (add type coercion) |
| `ensure_columns_with_defaults` | R/utils.R, NFLmarket.R | R/utils.R (use robust version) |
| `ensure_unique_join_keys` | R/utils.R, NFLmarket.R | R/utils.R |
| `select_first_column` | R/utils.R, NFLmarket.R | R/utils.R |
| `coerce_numeric_safely` | R/utils.R, NFLmarket.R | R/utils.R |
| `collapse_by_keys_relaxed` | R/utils.R, NFLmarket.R | R/utils.R |
| `probability_to_american` | R/utils.R, NFLmarket.R | R/utils.R |
| `brier_score` | R/utils.R, rolling_*.R | R/utils.R |
| `log_loss` | R/utils.R, rolling_*.R | R/utils.R |
| `accuracy` | R/utils.R, rolling_*.R | R/utils.R |

### 2.2 NFLmarket.R Cleanup

Lines to modify:
- Remove duplicate function definitions (lines 322-378)
- Add `source("R/utils.R")` dependency check
- Keep NFLmarket.R focused on market analysis only

---

## PART 3: DEAD CODE REMOVAL

### 3.1 Files to Archive

| File | Lines | Reason |
|------|-------|--------|
| calibration_refinement.R | 573 | Superseded by ensemble_calibration_implementation.R |
| comprehensive_code_validation.R | 264 | Functionality in scripts/verify_* |
| comprehensive_r451_test_suite.R | 530 | R 4.5.1 compatibility no longer needed |
| final_verification_checklist.R | 681 | Superseded by scripts/verify_repo_integrity.R |
| injury_model_validation.R | 668 | Superseded by injury_scalp.R |
| lasso_feature_selection.R | 305 | Feature selection not used in production |
| model_validation.R | 935 | Superseded by validation_pipeline.R |
| production_deployment_checklist.R | 466 | Functionality in scripts/verify_* |
| r451_compatibility_fixes.R | 495 | R 4.5.1 compatibility applied |
| rolling_window_validation.R | 406 | Duplicate of rolling_validation_system.R |
| run_validation_example.R | 259 | Example only, not production |

**Total: 5,582 lines to archive**

---

## PART 4: DATA-DRIVEN VARIABLE NAMING

### 4.1 Naming Convention

| Prefix | Meaning | Example |
|--------|---------|---------|
| `mu_` | Expected value/mean | `mu_home_score`, `mu_away_score` |
| `sd_` | Standard deviation | `sd_home_score`, `sd_away_score` |
| `p_` | Probability | `p_home_win`, `p_over` |
| `n_` | Count | `n_trials`, `n_games` |
| `is_` | Boolean | `is_home`, `is_playoff` |
| `adj_` | Adjustment factor | `adj_weather`, `adj_injury` |
| `raw_` | Uncalibrated | `raw_prob` |
| `cal_` | Calibrated | `cal_prob` |
| `wt_` | Weight | `wt_skill`, `wt_trench` |

### 4.2 Config Parameters to Document

All parameters in config.R need:
- Description
- Default value
- Valid range
- Statistical validation (if applicable)
- Source/reference

---

## PART 5: IMPLEMENTATION ORDER

### Phase 1: Fix Duplicate Functions (CRITICAL)
1. Update NFLmarket.R standardize_join_keys with type coercion
2. Replace R/utils.R ensure_columns_with_defaults with robust version
3. Remove duplicate definitions from NFLmarket.R

### Phase 2: Archive Dead Code
1. Create archive/ directory
2. Move 11 unused files
3. Update .gitignore

### Phase 3: Add Statistical Documentation
1. Document all config.R parameters with validation info
2. Add Brier decomposition to NFLbrier_logloss.R
3. Add ECE calculation

### Phase 4: Final Verification
1. Run all tests
2. Run run_week.R
3. Verify HTML output
4. Run integrity checks

---

## VERIFICATION CRITERIA

### Must Pass:
- [ ] `run_week.R` completes without errors
- [ ] HTML report generated
- [ ] 35/35 integrity checks (excluding generated artifacts)
- [ ] All duplicate functions removed from NFLmarket.R
- [ ] Archive directory contains unused files
- [ ] Statistical documentation complete for key parameters

---

*Plan created: 2026-01-30*
