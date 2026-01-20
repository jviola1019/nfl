# NFL Prediction Model - Audit Report

This document provides a comprehensive summary of all 20 issues identified and fixed in the NFL prediction model codebase.

## 20-Issue Verification Table

| # | Issue | File(s) | How to Verify | Test Function |
|---|-------|---------|---------------|---------------|
| 1 | Canonical utils.R module | R/utils.R | Check all 13+ core functions defined | `test-utils.R::test_that("american_to_probability...")` |
| 2 | Probability clamping with epsilon | R/utils.R | `clamp_probability(0) >= 1e-9` | `test-utils.R::test_that("clamp_probability...")` |
| 3 | Odds conversion functions | R/utils.R | `-110` odds → `0.524` probability | `test-utils.R::test_that("american_to_probability...")` |
| 4 | Expected value calculation | R/utils.R | Fair bet (50% @ +100) = 0 EV | `test-utils.R::test_that("expected_value_units...")` |
| 5 | Kelly stake calculation | R/utils.R | Stake capped at `max_stake` (0.02) | `test-utils.R::test_that("conservative_kelly_stake...")` |
| 6 | Market shrinkage function | R/utils.R | 60% shrinkage: `0.4*model + 0.6*market` | `test-utils.R::test_that("shrink_probability_toward_market...")` |
| 7 | Validation metrics (Brier, log-loss) | R/utils.R | Perfect predictions → Brier = 0 | `test-utils.R::test_that("brier_score...")` |
| 8 | Edge classification | R/utils.R | 15%+ edge → "suspicious/implausible" | `test-utils.R::test_that("classify_edge_magnitude...")` |
| 9 | Type-safe join keys | R/utils.R | season/week coerced to integer | `test-utils.R::test_that("standardize_join_keys...")` |
| 10 | Duplicate key handling | R/utils.R | `collapse_by_keys_relaxed()` merges | `test-utils.R::test_that("collapse_by_keys_relaxed...")` |
| 11 | Unique key enforcement | R/utils.R | `ensure_unique_join_keys()` dedupes | `test-utils.R::test_that("ensure_unique_join_keys...")` |
| 12 | Data validation module | R/data_validation.R | 8+ validation functions defined | `test-data-validation.R::test_that("reset_data_quality...")` |
| 13 | Data quality tracking | R/data_validation.R | `update_*_quality()` functions work | `test-data-validation.R::test_that("update_injury_quality...")` |
| 14 | Overall quality computation | R/data_validation.R | All good → "high", degraded → "low" | `test-data-validation.R::test_that("compute_overall_quality...")` |
| 15 | HTML badge generation | R/data_validation.R | Valid HTML with `<section>` tag | `test-data-validation.R::test_that("generate_quality_badge_html...")` |
| 16 | Weather fallback defaults | R/data_validation.R | `weather_source = "default"` marked | `test-data-validation.R::test_that("get_default_weather_conditions...")` |
| 17 | Injury availability check | R/data_validation.R | Returns list with `available`, `season` | `test-data-validation.R::test_that("check_injury_availability...")` |
| 18 | Configuration parameters | config.R | SHRINKAGE, KELLY_FRACTION, etc. exist | `scripts/verify_requirements.R` |
| 19 | Test suite coverage | tests/testthat/*.R | Both test files exist and pass | `testthat::test_dir("tests/testthat")` |
| 20 | Documentation files | CLAUDE.md, AUDIT.md, README.md | All three files exist | `scripts/verify_requirements.R` |

## Detailed Issue Documentation

### Issue 1: Canonical Utils Module

**Requirement**: Single source of truth for utility functions in `R/utils.R`.

**Implementation**: All shared utility functions defined in `R/utils.R`:
- Odds conversion: `american_to_probability()`, `american_to_decimal()`, `probability_to_american()`
- Probability handling: `clamp_probability()`, `shrink_probability_toward_market()`
- Betting math: `expected_value_units()`, `conservative_kelly_stake()`
- Validation metrics: `brier_score()`, `log_loss()`, `accuracy()`
- Join utilities: `standardize_join_keys()`, `safe_typed_join()`, `validate_join_overlap()`
- Data frame utilities: `first_non_missing_typed()`, `collapse_by_keys_relaxed()`, `ensure_unique_join_keys()`

### Issue 2: Probability Clamping

**Requirement**: Prevent `log(0)` and division by zero with epsilon boundaries.

**Implementation**:
```r
PROB_EPSILON <- 1e-9

clamp_probability <- function(p, eps = PROB_EPSILON) {
  p <- suppressWarnings(as.numeric(p))
  pmin(pmax(p, eps), 1 - eps)
}
```

### Issue 3: Broken Join in build_moneyline_comparison_table (CRITICAL)

**Symptom**: Empty comparison table despite valid data inputs.

**Root Cause**: Type mismatch in join keys - `game_id` sometimes numeric, `season`/`week` sometimes character.

**Fix**: Apply `standardize_join_keys()` to all tables before joining:
```r
# CRITICAL: Standardize scores join keys (type coercion)
scores <- standardize_join_keys(scores)
```

**Location**: NFLmarket.R:1708, NFLsimulation.R:755

### Issue 4: Expected Value Calculation

**Formula**: `EV = prob * (decimal_odds - 1) - (1 - prob)`

**Test**: Fair bet (50% at +100 odds) should return EV = 0.

### Issue 5: Kelly Stake Calculation

**Implementation**: Conservative 1/8 Kelly with edge skepticism and max stake cap.

```r
conservative_kelly_stake <- function(prob, odds,
                                     kelly_fraction = 0.125,  # 1/8 Kelly
                                     max_edge = 0.10,
                                     max_stake = 0.02) {
  # Full Kelly * fraction * edge_penalty, capped at max_stake
}
```

### Issue 6: Market Shrinkage

**Requirement**: Blend model probability toward market consensus.

**Default**: 60% market weight → `shrunk = 0.4*model + 0.6*market`

### Issue 7: Validation Metrics

**Brier Score**: `mean((predicted - actual)^2)` - lower is better.

**Log Loss**: `-mean(actual*log(pred) + (1-actual)*log(1-pred))` - lower is better.

**Accuracy**: `mean((pred > 0.5) == actual)` - higher is better.

### Issue 8: Edge Classification

**Thresholds**:
- `<= 0`: "negative"
- `0-5%`: "realistic"
- `5-10%`: "optimistic"
- `10-15%`: "suspicious"
- `> 15%`: "implausible"

### Issues 9-11: Type-Safe Joins

**Key Type Standards**:
- `game_id`: character
- `season`: integer
- `week`: integer

**Functions**:
- `standardize_join_keys()`: Renames aliases and coerces types
- `collapse_by_keys_relaxed()`: Merges duplicate rows (first non-NA value)
- `ensure_unique_join_keys()`: Removes duplicate key combinations

### Issues 12-17: Data Quality Tracking

**Module**: `R/data_validation.R`

**Functions**:
- `reset_data_quality()` - Initialize tracking environment
- `update_injury_quality(status, seasons_missing)` - Track injury data
- `update_weather_quality(status, games_fallback)` - Track weather data
- `update_market_quality(status, games_missing)` - Track market data
- `update_calibration_quality(method, leakage_free)` - Track calibration method
- `compute_overall_quality()` - Return "high"/"medium"/"low" rating
- `generate_quality_badge_html()` - HTML badge for reports

**Status Values**:
- Injury: "complete", "partial", "missing", "unknown"
- Weather: "api", "partial", "default", "unknown"
- Market: "complete", "missing", "unknown"
- Calibration: "isotonic_nested_cv", "isotonic_global", "none", "unknown"

### Issue 18: Configuration Parameters

**Required in config.R**:
- `SHRINKAGE` (default: 0.6)
- `KELLY_FRACTION` (default: 0.125)
- `N_TRIALS` (default: 100000)
- `SEED` (default: 42)

### Issues 19-20: Test Suite and Documentation

**Test Files**:
- `tests/testthat/test-utils.R` - 20+ tests for utility functions
- `tests/testthat/test-data-validation.R` - 10+ tests for validation module

**Documentation Files**:
- `CLAUDE.md` - Claude Code context and instructions
- `AUDIT.md` - This comprehensive audit report
- `README.md` - Project overview and quick start

## Verification

### Run Full Verification Script
```r
source("scripts/verify_requirements.R")
```

### Run Test Suite
```r
testthat::test_dir("tests/testthat")
```

### Run Weekly Simulation
```r
source("run_week.R")
# Verify: comparison table non-empty, HTML report generated, data quality badge present
```

## Files Modified

| File | Changes |
|------|---------|
| `R/utils.R` | Added type-safe join utilities, edge classification |
| `R/data_validation.R` | Complete data quality tracking infrastructure |
| `NFLsimulation.R` | Fixed join, integrated data quality tracking |
| `NFLmarket.R` | Fixed join, added quality badge to HTML reports |
| `config.R` | Added all tunable parameters |
| `tests/testthat/test-utils.R` | Comprehensive utility tests |
| `tests/testthat/test-data-validation.R` | Data validation tests |

## Known Limitations

1. **2025 Injury Data**: Shows as "missing" until nflverse releases data
2. **Neutral Site Games**: Use league-average weather conditions
3. **New Stadiums**: May need manual coordinate additions

## Calibration Notes

The codebase has two isotonic calibration modes:
- **Nested CV** (lines 5174-5235): Leave-one-week-out, leakage-free, used for production
- **Global** (lines 5141-5168): Uses all data, marked "DIAGNOSTIC ONLY"

The data quality tracker records which method is in use via `update_calibration_quality()`.
