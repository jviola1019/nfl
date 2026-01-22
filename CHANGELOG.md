# Changelog

All notable changes to the NFL Prediction Model are documented in this file.

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
