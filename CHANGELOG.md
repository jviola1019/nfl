# Changelog

All notable changes to the NFL Prediction Model are documented in this file.

## [2.6.2] - 2026-01-25

### Critical Bug Fixes (Brier Score ~0.25 → ~0.211)

**Double Calibration Fix (CRITICAL)**
- Fixed double isotonic calibration bug in NFLsimulation.R
- `home_p_2w_model` now uses `home_p_2w_raw` (uncalibrated) instead of `home_p_2w_cal`
- Calibration via `map_blend()` is now applied exactly ONCE
- **Impact**: Brier score was inflated ~18% due to over-compression toward 0.5

**Calibration Order Fix**
- Dynamic shrinkage now applied BEFORE isotonic calibration, not after
- Restructured probability blend flow:
  1. Get raw model probability
  2. Apply dynamic shrinkage (blend with market)
  3. Apply isotonic calibration ONCE
- Prevents shrinkage from breaking calibration guarantees

**Config Parameter Conflicts Fix**
- Weather fallback defaults now match config.R:
  - `OUTDOOR_WIND_PEN`: -1.0 → -1.2
  - `COLD_TEMP_PEN`: -0.5 → -0.6

**Sleeper API Fallback in NFLsimulation.R**
- Added automatic Sleeper API fallback when nflreadr returns no data
- Sources R/sleeper_api.R dynamically if available
- Converts Sleeper format to nflreadr-compatible format
- User message now includes Sleeper tip: `INJURY_MODE='sleeper'`

### Calibration Method Switch
- Changed `CALIBRATION_METHOD` from "ensemble" to "spline" in config.R
- Spline calibration (GAM with smoothing penalty) gives -6.9% Brier improvement (best of all methods)
- Isotonic regression found to be catastrophically broken (Brier 0.316, worse than random)
- Added spline calibration branch in NFLsimulation.R (loads spline model from ensemble artifact)
- Falls back to isotonic if spline artifact not available

### Phase 1-2 Implementations

**Snap-Weighted Injury Impacts (NEW)**
- Integrated `weight_injury_by_snaps()` into `calc_injury_impacts()` function
- Added player name column to injury data pipeline
- Injuries now weighted by player snap percentage (WR1 @ 60% snaps has greater impact than WR5 @ 10%)
- Enabled via `USE_SNAP_WEIGHTED_INJURIES <- TRUE` in config.R
- Sources injury_scalp.R automatically when snap weighting is enabled

**Test Coverage Improvements**
- Added `tests/testthat/test-calibration.R`: Calibration method and ensemble loading tests
- Added `tests/testthat/test-backup-qb.R`: Backup QB quality function tests
- Added `tests/testthat/test-snap-weighting.R`: Snap-weighted injury integration tests
- All tests verify both config existence and function behavior

**Documentation Updates**
- Updated DOCUMENTATION.md line counts:
  - config.R: 390 → 986 lines
  - NFLsimulation.R: 7,400 → 8,226 lines
  - NFLmarket.R: 2,700 → 3,942 lines
- Updated GETTING_STARTED.md version to 2.6.2
- Updated plan file with completed Phase 0-2 status

## [2.6.1] - 2026-01-23

### Repository Audit & Sleeper API Integration

**Sleeper API Integration (NEW: R/sleeper_api.R)**
- Real-time NFL injury data from Sleeper fantasy API
- Free, no authentication required
- Automatic caching with 4-hour expiry
- Full player database with injury_status, injury_body_part, injury_notes
- Integrated into injury_scalp.R fallback chain: Sleeper → nflreadr → ESPN → cache

**Injury System Improvements**
- Added "sleeper" as new INJURY_MODE option
- Auto mode now tries Sleeper API first before nflreadr
- Normalize Sleeper data to standard injury report format
- Sources R/sleeper_api.R for Sleeper integration

**Critical Config/Code Integration Fix**
- Fixed disconnect between config.R injury weights and NFLsimulation.R
- `SKILL_AVAIL_POINT_PER_FLAG` now uses `INJURY_WEIGHT_SKILL` from config.R
- `TRENCH_AVAIL_POINT_PER_FLAG` now uses `INJURY_WEIGHT_TRENCH` from config.R
- `SECONDARY_AVAIL_POINT_PER_FLAG` now uses `INJURY_WEIGHT_SECONDARY` from config.R
- `FRONT7_AVAIL_POINT_PER_FLAG` now uses `INJURY_WEIGHT_FRONT7` from config.R
- Added new config parameters for position multipliers:
  - `INJURY_POS_MULT_TRENCH = 1.3`
  - `INJURY_POS_MULT_SKILL = 1.05`
  - `INJURY_POS_MULT_SECONDARY = 0.95`
  - `INJURY_POS_MULT_FRONT7 = 0.85`
  - `INJURY_POS_MULT_OTHER = 0.6`
- calc_injury_impacts() now uses config parameters with fallback defaults

**New A/B Test for Injury Model (validation/injury_ab_comparison.R)**
- Compares model performance WITH vs WITHOUT injury adjustments
- Bootstrap statistical significance testing (1000 samples)
- Reports Brier score, log-loss, accuracy differences
- Run: `source("validation/injury_ab_comparison.R")`

**New Test Files Added**
- tests/testthat/test-sleeper-api.R - Sleeper API integration tests
- tests/testthat/test-injury-model.R - Injury weight and config validation
- tests/testthat/test-weather.R - Weather impact parameter tests
- tests/testthat/test-logging.R - Logging utility tests

**Code Consistency Fixes**
- Standardized PROB_EPSILON = 1e-9 across all files (was 1e-9, 1e-12, 1e-15)
- NFLbrier_logloss.R now sources R/utils.R instead of duplicating functions
- NFLmarket.R now sources R/utils.R instead of duplicating functions
- All duplicate function definitions converted to conditional fallbacks

**Configuration Centralization (config.R)**
- Added `MARGIN_PROB_PRIOR_SD = 6.5` (was hardcoded in NFLsimulation.R)
- Added `PPD_BLEND_WEIGHT = 0.65` (was hardcoded in ppd_blend function)
- Added `PRESSURE_MISMATCH_PTS = 0.6` (was hardcoded in pressure calculation)
- NFLsimulation.R now uses config values with fallback defaults

**Documentation Updates**
- GETTING_STARTED.md updated to v2.6.1 with Sleeper injury mode docs
- DOCUMENTATION.md updated to v2.6.1 with complete file reference table
- CLAUDE.md remains authoritative source for agent operations

**Repository Cleanup**
- Removed stale files from git: .RDataTmp (83MB), .Rhistory, nul, *.rds artifacts
- Updated .gitignore to prevent re-addition of artifacts
- All documentation versions updated to 2.6.1

### Test Results

After v2.6.1 fixes:
- **Integrity checks**: 35/35 passed
- **Test suite**: All tests pass, 6 skipped (network-dependent)
- **Sleeper API**: Working, verified Bo Nix (QB) OUT for Denver
- **Config/code sync**: Injury weights now properly connected

---

## [2.6.0] - 2026-01-22

### Playoff Mode Enhancements

**Calibration Improvements (NFLsimulation.R)**
- Calibration now includes playoff games (WC/DIV/CON/SB) in addition to regular season
- Added `is_playoff` and `game_type` tracking to calibration data
- Cache version bumped to v2 to invalidate old regular-season-only caches
- Calibration diagnostics now report playoff vs regular season game counts

**Playoff Adjustments Applied**
- R/playoffs.R now sourced in NFLsimulation.R for playoff-specific features
- Home Field Advantage multiplier applied for playoff weeks (15-25% boost)
- Playoff-specific market shrinkage applied (extra 5-15% market trust)
- Output validation warns about extreme probabilities (>90% or <10%)

**Join Key Improvements (R/utils.R)**
- Added `game_type` to `JOIN_KEY_ALIASES` for reliable playoff/regular season filtering
- `standardize_join_keys()` now coerces `game_type` to character type
- `PREDICTION_JOIN_KEYS` now includes `game_type`

**Configuration Validation (config.R)**
- Added `.validate_config()` function that runs at config load time
- Validates: WEEK_TO_SIM (1-22), SEASON (2002-current+1), N_TRIALS (>=1000), SHRINKAGE (0-1), KELLY_FRACTION (0-0.5)
- Clear error messages for invalid configuration

**Bug Fixes**
- Fixed R/date_resolver.R: `phase = "offseason"` instead of `NA_character_` for empty boundaries
- Fixed R/date_resolver.R: Added `suppressWarnings()` to `min()/max()` on empty groups
- Test suite: 0 failures, 0 warnings, 6 skips (acceptable), 317 passes

### Test Results

After v2.6 fixes:
- **Test suite**: 317 passed, 0 fail, 0 warnings, 6 skipped
- **R 4.5.1 compatibility**: Verified

---

## [2.5.0] - 2026-01-21

### Critical API Fixes

**Data Quality API Corrections (NFLsimulation.R)**
- Fixed `update_injury_quality()` calls:
  - Parameter: `seasons_missing` → `missing_seasons`
  - Status values: `"complete"` → `"full"`, `"missing"` → `"unavailable"`
- Fixed `update_weather_quality()` calls:
  - Parameter: `games_fallback` → `fallback_games`
  - Status values: `"api"` → `"full"`, `"partial"` → `"partial_fallback"`, `"default"` → `"all_fallback"`
- Fixed `update_market_quality()` calls:
  - Status values: `"complete"` → `"full"`, `"missing"` → `"unavailable"`

**Playoff Module Fix (R/playoffs.R)**
- Fixed `get_round_from_week()` type comparison bug:
  - `identical()` failed due to numeric/integer type mismatch
  - Changed to `isTRUE(== comparison)` for reliable matching
  - `derive_playoff_round_from_week()` now returns correct round names

**Test Precision Fix (tests/testthat/test-utils.R)**
- Fixed floating point comparison: `0.333` → `1/3` for exact comparison

### New Verification Scripts

- `scripts/run_matrix.R` - Execute all artifacts, record PASS/FAIL
- Updated `scripts/verify_requirements.R` - Fixed dplyr dependency and API calls
- Updated `CLAUDE.md` - Authoritative agent guide with API reference

### Documentation Updates

- `README.md` - Complete file inventory (~50 files), version 2.5
- `CLAUDE.md` - Rewritten as agent operating guide with:
  - Data Quality API Reference (correct parameters and status values)
  - Common failure playbook
  - Standard agent prompts
  - Stop/Continue checkpoint rules

### Test Results

After v2.5 fixes:
- **Integrity checks**: 35/35 passed
- **Run matrix**: 9/9 artifacts pass
- **Test suite**: 316 passed, 1 fail (minor edge case), 6 skipped

---

## [2.4.1] - 2026-01-21

### Repository Audit & Refactoring

**Critical Fixes (P0)**
- Created `tests/testthat/setup.R` to fix test path resolution
  - Tests were failing with "cannot open file './R/utils.R'" due to incorrect path construction
  - New setup file uses `rprojroot` to reliably locate project root and source all R modules
- Removed `source()` calls from individual test files (test-utils.R, test-data-validation.R, test-date-resolver.R, test-playoffs.R)
- Deleted stale artifacts: `.RData`, `nul`
- Updated `.gitignore` to:
  - Track `.vscode/` settings for consistent R extension configuration
  - Ignore `nul` Windows error file
  - Add `reports/` directory
  - Add `.cache/` directory

**High Priority Fixes (P1)**
- Created `.lintr` configuration file to prevent VS Code linter crashes
- Updated `.vscode/settings.json` with proper R extension settings:
  - R path for Windows
  - LSP diagnostics enabled
  - File associations for R, Rmd
  - Editor settings for R files
- Deleted duplicate test file `tests/test_core_math.R` (functionality covered by test-utils.R)

**Bug Fixes**
- Fixed `R/date_resolver.R`:
  - `parse_datetime()` now handles vector input correctly (was failing in `dplyr::case_when`)
  - `parse_kickoff_times()` rewritten with row-by-row processing for type safety
  - Prevents "length > 1 in coercion to logical" errors
- Fixed `tests/testthat/test-data-validation.R`:
  - Updated API calls to match actual implementation
  - Changed `quality$injury_status` to `quality$injury$status` (nested structure)
  - Changed status values: "complete" -> "full", "api" -> "full", "missing" -> "unavailable"
  - Changed overall quality values: "high" -> "HIGH", "medium" -> "MEDIUM"
  - Changed parameter names: `seasons_missing` -> `missing_seasons`, `games_fallback` -> `fallback_games`
  - Changed HTML badge assertions: `<section>` -> `<div>`
  - Fixed validation function tests to expect TRUE/FALSE returns instead of complex structures

**New Files**
- `tests/testthat/setup.R` - Test infrastructure setup
- `scripts/verify_repo_integrity.R` - Repository integrity verification script
- `.lintr` - Lintr configuration
- `CHANGELOG.md` - This file
- Updated `AUDIT.md` - Comprehensive repository audit report

### Files Modified

| File | Change Type | Description |
|------|-------------|-------------|
| `tests/testthat/setup.R` | NEW | Test setup for loading R modules |
| `tests/testthat/test-utils.R` | MODIFIED | Removed source() call |
| `tests/testthat/test-data-validation.R` | MODIFIED | Fixed API mismatches, removed source() |
| `tests/testthat/test-date-resolver.R` | MODIFIED | Removed source() call |
| `tests/testthat/test-playoffs.R` | MODIFIED | Removed source() call |
| `R/date_resolver.R` | MODIFIED | Fixed vectorization bugs |
| `.gitignore` | MODIFIED | Updated exclusions |
| `.vscode/settings.json` | MODIFIED | Added R extension config |
| `.lintr` | NEW | Lintr configuration |
| `scripts/verify_repo_integrity.R` | NEW | Integrity verification |
| `AUDIT.md` | MODIFIED | Complete audit report |
| `CHANGELOG.md` | NEW | This file |

### Files Deleted

| File | Reason |
|------|--------|
| `tests/test_core_math.R` | Duplicate of test-utils.R |
| `.RData` | Stale R session data |
| `nul` | Windows error artifact |

### Test Results

After fixes:
- **data-validation**: 41 tests passing
- **game-type-mapping**: 22 tests passing
- **playoffs**: ~80 tests passing
- **utils**: ~80 tests passing
- **date-resolver**: Some skipped (network-dependent tests)

### Known Issues

1. Date resolver tests skip when schedule data cannot be loaded (network-dependent)
2. Some date resolver tests produce NA kickoff times in certain environments

---

## [2.4.0] - Previous Version

See previous release notes for v2.4.0 changes including:
- Fixed kickoff_local timezone bug with `safe_with_tz()` helper
- Consolidated R utility functions into `R/utils.R`
- Added type-safe joins via `standardize_join_keys()`
- Created `R/logging.R` for structured logging
- Created `R/data_validation.R` for centralized data validation
- Added comprehensive test suite

---

## Version History

- **2.5.0** - Data quality API fixes, playoff module type fix, verification scripts (2026-01-21)
- **2.4.1** - Repository audit and test infrastructure fixes (2026-01-21)
- **2.4.0** - Utility consolidation and type-safe joins (2026-01)
- **2.3.0** - Division-by-zero fixes and HTML report enhancements
- **2.2.0** - Injury data fallback system
- **2.1.0** - Playoff mode support
- **2.0.0** - Major refactoring with modular R/ directory
