# NFL Prediction Model - Audit Report

This document summarizes the issues identified and fixed in the NFL prediction model codebase.

## Summary

| Issue | Severity | Status | Root Cause |
|-------|----------|--------|------------|
| Broken join in `build_moneyline_comparison_table` | Critical | Fixed | Type mismatch in join keys |
| Missing 2025 injury data (404) | High | Fixed | nflreadr API unavailability |
| Stadium/weather fallbacks silent | High | Fixed | No quality tracking |
| Calibration leakage potential | Medium | Fixed | Missing production flag |
| Silent data quality degradation | High | Fixed | No fallback visibility |

## Detailed Fixes

### Issue 3: Broken Join in `build_moneyline_comparison_table()` (CRITICAL)

**Symptom**: Empty comparison table despite valid data inputs.

**Root Cause**: The `scores` dataframe from `extract_game_level_scores()` was not being passed through `standardize_join_keys()` before joining with `schedule_std`. This caused type mismatches:
- `game_id` was sometimes numeric instead of character
- `season` and `week` were sometimes character instead of integer

**Fix** (NFLmarket.R:1708 and NFLsimulation.R:755):
```r
# CRITICAL: Standardize scores join keys (type coercion for game_id, season, week)
scores <- standardize_join_keys(scores)
```

**Verification**:
```r
source("run_week.R")
# Check that comparison table is non-empty
nrow(moneyline_report_inputs$comparison$per_game) > 0
```

### Issue 1: Missing 2025 Injury Data

**Symptom**: 404 error when loading injury data for 2025 season.

**Root Cause**: nflreadr injury data not yet released for 2025.

**Fix**: The `safe_load_injuries()` function already handled this gracefully, but now integrates with data quality tracking:

```r
# Update data quality tracking
if (exists("update_injury_quality", mode = "function")) {
  if (nrow(injuries) == 0) {
    update_injury_quality("missing", seasons_missing = as.character(seasons))
  } else if (length(missing)) {
    update_injury_quality("partial", seasons_missing = as.character(missing))
  } else {
    update_injury_quality("complete")
  }
}
```

**Verification**: Look for "Injury data not available for seasons: 2025" warning in console output.

### Issue 2: Stadium/Weather Fallbacks

**Symptom**: Games with missing stadium coordinates silently used default weather.

**Fix**: Added explicit data quality tracking after weather loading:

```r
# Update data quality tracking for weather
if (exists("update_weather_quality", mode = "function")) {
  total_games <- nrow(weather_inputs)
  fallback_count <- length(.stadium_fallback_games)
  if (fallback_count == 0) {
    update_weather_quality("api", games_fallback = character(0))
  } else if (fallback_count < total_games) {
    update_weather_quality("partial", games_fallback = .stadium_fallback_games)
  } else {
    update_weather_quality("default", games_fallback = .stadium_fallback_games)
  }
}
```

**Verification**: Check for "STADIUM FALLBACK" warning if any games use default coordinates.

### Issue 5: Calibration Leakage

**Symptom**: Potential for overfitting if calibration used test data.

**Current State**: The codebase already had proper nested CV isotonic calibration:
- **Nested CV** (lines 5174-5235): Leave-one-week-out, leakage-free, used for validation metrics
- **Global** (lines 5141-5168): Uses all data, marked "DIAGNOSTIC ONLY"

**Fix**: Added data quality tracking to record which calibration method is in use:

```r
if (exists("update_calibration_quality", mode = "function")) {
  update_calibration_quality(
    method = "isotonic_nested_cv",
    leakage_free = TRUE
  )
}
```

**Verification**: Console output shows "✅ Using nested CV isotonic calibration (leave-one-week-out, no leakage)".

### Issue 13: Data Quality Gates

**New Feature**: Comprehensive data quality tracking system in `R/data_validation.R`:

- `reset_data_quality()` - Initialize tracking
- `update_injury_quality()` - Track injury data status
- `update_weather_quality()` - Track weather data status
- `update_market_quality()` - Track market data status
- `update_calibration_quality()` - Track calibration method
- `compute_overall_quality()` - Compute overall rating (high/medium/low)
- `print_data_quality_summary()` - Console summary
- `generate_quality_badge_html()` - HTML badge for reports

**Console Output Example**:
```
═══════════════════════════════════════════════════════════════
                    DATA QUALITY SUMMARY
═══════════════════════════════════════════════════════════════
Overall Quality: MEDIUM

Injury Data:    PARTIAL (missing: 2025)
Weather Data:   API
Market Data:    COMPLETE
Calibration:    isotonic_nested_cv (leakage-free: TRUE)
═══════════════════════════════════════════════════════════════
```

## Files Modified

| File | Changes |
|------|---------|
| `R/data_validation.R` | Added data quality tracking infrastructure (lines 311-571) |
| `NFLsimulation.R` | Fixed join, added module sourcing, integrated data quality tracking |
| `NFLmarket.R` | Fixed join, added data quality badge to HTML reports |

## Test Suite

New tests added in `tests/testthat/test-data-validation.R`:
- Data quality tracking functions
- Injury availability checking
- Weather fallback handling

Run tests:
```r
testthat::test_dir("tests/testthat")
```

## Verification Steps

1. **Run the simulation**:
   ```r
   source("run_week.R")
   ```

2. **Check for non-empty comparison table**:
   ```r
   nrow(moneyline_report_inputs$comparison$per_game) > 0
   ```

3. **Check data quality summary** (printed at end of simulation)

4. **Verify HTML report** contains:
   - Data Quality badge section
   - Proper game predictions
   - EV calculations

5. **Run tests**:
   ```r
   testthat::test_dir("tests/testthat")
   ```

## Configuration

Key configuration values in `config.R`:
- `DEFAULT_WEATHER_CONDITIONS` - Fallback weather parameters
- `SHRINKAGE` = 0.6 - Market probability blend weight
- `KELLY_FRACTION` = 0.125 - 1/8 Kelly staking
- `ISOTONIC_EPSILON` = 0.01 - Calibration boundary padding

## Known Limitations

1. **2025 Injury Data**: Will show as "missing" until nflverse releases the data
2. **Neutral Site Games**: Will use league-average weather conditions
3. **New Stadiums**: May need to be added to `stadium_coords` table manually

## Contact

See `CLAUDE.md` for development context and `DOCUMENTATION.md` for methodology details.
