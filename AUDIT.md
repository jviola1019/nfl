# NFL Prediction Model - Repository Audit Report

**Generated**: 2026-01-21
**Auditor**: Clean-Room Refactor Agent
**Repository Version**: 2.4.1

---

## Executive Summary

This audit identifies 47 files in the repository, classifies each by purpose and necessity, and provides specific remediation actions. The primary issues are:

1. **Test path resolution failure** - Tests cannot find source files
2. **Redundant validation scripts** - 10+ overlapping validation files
3. **Root directory clutter** - Many scripts belong in subdirectories
4. **Missing test infrastructure** - No `setup.R` for testthat
5. **Stale artifacts** - `.RData` and `nul` file in root

---

## File Classification Table

| File Path | Type | Runnable | Referenced By | Status | Rationale |
|-----------|------|----------|---------------|--------|-----------|
| **Core Pipeline** |
| `run_week.R` | entry | YES | User | KEEP | Main entry point |
| `config.R` | config | YES | run_week, NFLsimulation | KEEP | Central configuration |
| `NFLsimulation.R` | engine | YES | run_week | KEEP | Core simulation (7829 lines) |
| `NFLmarket.R` | engine | YES | NFLsimulation | KEEP | Market analysis (3932 lines) |
| `NFLbrier_logloss.R` | engine | YES | NFLsimulation | KEEP | Evaluation metrics (1156 lines) |
| `injury_scalp.R` | engine | YES | NFLsimulation | KEEP | Injury data loading (899 lines) |
| **R/ Module Directory** |
| `R/utils.R` | library | NO | Tests, NFLmarket, NFLsim | KEEP | Canonical utilities |
| `R/data_validation.R` | library | NO | NFLsimulation | KEEP | Data validation |
| `R/logging.R` | library | NO | Multiple | KEEP | Structured logging |
| `R/playoffs.R` | library | NO | NFLsimulation, tests | KEEP | Playoff logic |
| `R/date_resolver.R` | library | NO | NFLsimulation, tests | KEEP | Date resolution |
| **Validation Scripts (CONSOLIDATE)** |
| `validation_pipeline.R` | validation | YES | None | KEEP | Primary validation (741 lines) |
| `model_validation.R` | validation | YES | None | MERGE->validation/ | Statistical testing (935 lines) |
| `injury_model_validation.R` | validation | YES | None | MOVE->validation/ | Injury validation (668 lines) |
| `professional_model_benchmarking.R` | validation | YES | None | MOVE->validation/ | Benchmarking (749 lines) |
| `calibration_refinement.R` | validation | YES | None | MOVE->validation/ | Calibration (573 lines) |
| `rolling_validation_system.R` | validation | YES | None | MERGE->validation/ | Duplicate? (599 lines) |
| `rolling_window_validation.R` | validation | YES | None | MERGE->validation/ | Duplicate? (406 lines) |
| `simplified_baseline_comparison.R` | validation | YES | None | MOVE->validation/ | Baseline (424 lines) |
| `ensemble_calibration_implementation.R` | validation | YES | None | MOVE->validation/ | Ensemble (608 lines) |
| `validation_reports.R` | validation | YES | None | MOVE->validation/ | Reports (388 lines) |
| `run_validation_example.R` | validation | YES | None | MERGE->validation/ | Example (259 lines) |
| `validation/playoffs_validation.R` | validation | YES | None | KEEP | Already in correct location |
| **Test Suite (FIX)** |
| `tests/testthat/test-utils.R` | test | NO | testthat | FIX | Path resolution broken |
| `tests/testthat/test-data-validation.R` | test | NO | testthat | FIX | Path resolution broken |
| `tests/testthat/test-date-resolver.R` | test | NO | testthat | FIX | Path resolution broken |
| `tests/testthat/test-playoffs.R` | test | NO | testthat | FIX | Path resolution broken |
| `tests/testthat/test-game-type-mapping.R` | test | YES | testthat | KEEP | Works (no source()) |
| `tests/test_core_math.R` | test | YES | None | DELETE | Duplicates test-utils.R |
| **Infrastructure Scripts** |
| `scripts/verify_requirements.R` | verify | YES | CI | KEEP | Audit verification |
| **Compatibility / Checklist Scripts** |
| `r451_compatibility_fixes.R` | compat | YES | None | DEPRECATE | One-time R 4.5.1 fixes |
| `comprehensive_r451_test_suite.R` | test | YES | None | DEPRECATE | One-time compat tests |
| `comprehensive_code_validation.R` | test | YES | None | MERGE->scripts/ | General validation |
| `final_verification_checklist.R` | verify | YES | None | MERGE->scripts/ | Deployment checks |
| `production_deployment_checklist.R` | verify | YES | None | MERGE->scripts/ | Deployment checks |
| `lasso_feature_selection.R` | analysis | YES | None | MOVE->validation/ | Feature selection |
| **Documentation** |
| `README.md` | docs | NO | - | KEEP | Project overview |
| `GETTING_STARTED.md` | docs | NO | - | KEEP | Setup guide |
| `DOCUMENTATION.md` | docs | NO | - | KEEP | Technical reference |
| `CLAUDE.md` | docs | NO | - | KEEP | Agent context |
| `AUDIT.md` | docs | NO | - | KEEP | This file |
| `DESCRIPTION` | meta | NO | - | KEEP | Package metadata |
| **.gitignore** | config | NO | - | KEEP | Git config |
| **CI/CD** |
| `.github/workflows/ci.yml` | ci | YES | GitHub | KEEP | CI workflow |
| **VS Code** |
| `.vscode/settings.json` | config | NO | - | FIX | Missing R settings |
| `.vscode/launch.json` | config | NO | - | KEEP | Debug config |
| **renv** |
| `renv/activate.R` | renv | NO | - | KEEP | renv activation |
| `renv/settings.R` | renv | NO | - | KEEP | renv settings |
| **Artifacts (DELETE)** |
| `.RData` | artifact | NO | - | DELETE | Stale R session data |
| `nul` | artifact | NO | - | DELETE | Empty/error file |
| `*_results.rds` | artifact | NO | - | IGNORE | Gitignored |
| `run_logs/*.rds` | artifact | NO | - | IGNORE | Gitignored |
| `NFLvsmarket_report.html` | artifact | NO | - | DELETE | Should be gitignored |

---

## Critical Issues

### Issue 1: Test Path Resolution Failure (CRITICAL)

**Symptom**: All tests in `tests/testthat/` fail with "cannot open file './R/utils.R'"

**Root Cause**: The test files use:
```r
source(file.path(dirname(dirname(dirname(testthat::test_path()))), "R", "utils.R"))
```

This path construction fails because `testthat::test_path()` is not available during the outer `source()` call.

**Fix**: Create `tests/testthat/setup.R` with proper path handling:
```r
# tests/testthat/setup.R
project_root <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION"))
source(file.path(project_root, "R", "utils.R"))
source(file.path(project_root, "R", "data_validation.R"))
source(file.path(project_root, "R", "logging.R"))
source(file.path(project_root, "R", "playoffs.R"))
source(file.path(project_root, "R", "date_resolver.R"))
```

Then remove the `source()` lines from individual test files.

### Issue 2: Redundant Validation Scripts

**Files**: 10+ validation scripts with overlapping functionality in root directory.

**Fix**: Consolidate into `validation/` directory:
- Keep `validation_pipeline.R` as primary (rename to `validation/run_validation.R`)
- Merge `rolling_validation_system.R` and `rolling_window_validation.R`
- Move remaining files to `validation/`

### Issue 3: Stale Artifacts in Git

**Files**:
- `.RData` - R session data (should never be committed)
- `nul` - Empty Windows error file
- `NFLvsmarket_report.html` - Generated report (should be gitignored)

**Fix**:
```bash
git rm --cached .RData nul NFLvsmarket_report.html
```

### Issue 4: Missing Lintr Configuration

**Issue**: VS Code R extension may crash when linting without `.lintr` file.

**Fix**: Create `.lintr`:
```yaml
linters: linters_with_defaults(
  line_length_linter(120),
  commented_code_linter = NULL
)
exclusions: list(
  "renv" = Inf,
  "run_logs" = Inf
)
```

---

## Recommended Repository Structure

```
nfl/
├── R/                           # Library functions (NO CHANGES)
│   ├── utils.R                  # Canonical utilities
│   ├── data_validation.R        # Data validation
│   ├── logging.R                # Structured logging
│   ├── playoffs.R               # Playoff logic
│   ├── date_resolver.R          # Date resolution
│   └── contracts.R              # NEW: Data contracts
├── scripts/                     # Utility scripts
│   ├── verify_requirements.R    # Existing
│   ├── verify_repo_integrity.R  # NEW: Schema/invariant checks
│   └── run_matrix.R             # NEW: Run all artifacts
├── validation/                  # Validation scripts
│   ├── run_validation.R         # Primary validation (renamed)
│   ├── model_validation.R       # Statistical testing
│   ├── injury_validation.R      # Injury impact (renamed)
│   ├── benchmarking.R           # vs FiveThirtyEight (renamed)
│   ├── calibration.R            # Calibration analysis (renamed)
│   ├── rolling_validation.R     # Rolling window (merged)
│   └── playoffs_validation.R    # Existing
├── tests/
│   └── testthat/
│       ├── setup.R              # NEW: Source R/ modules
│       ├── test-utils.R         # FIXED: Remove source()
│       ├── test-data-validation.R
│       ├── test-date-resolver.R
│       ├── test-playoffs.R
│       └── test-game-type-mapping.R
├── docs/                        # Optional: Move markdown
│   └── (keep in root for now)
├── run_logs/                    # Generated (gitignored)
├── reports/                     # NEW: Generated reports
├── .github/workflows/ci.yml     # CI config
├── config.R                     # Central configuration
├── run_week.R                   # Main entry point
├── NFLsimulation.R              # Core engine
├── NFLmarket.R                  # Market analysis
├── NFLbrier_logloss.R           # Metrics
├── injury_scalp.R               # Injury loading
├── DESCRIPTION                  # Package metadata
├── README.md                    # Overview
├── GETTING_STARTED.md           # Setup guide
├── DOCUMENTATION.md             # Technical reference
├── CLAUDE.md                    # Agent context
├── AUDIT.md                     # This file
├── CHANGELOG.md                 # NEW: Change log
├── .lintr                       # NEW: Lintr config
├── .gitignore                   # Git ignore
└── renv.lock                    # Dependencies
```

---

## Action Items (Priority Order)

### P0: Critical (Must Fix)

1. **Create `tests/testthat/setup.R`** - Fix test path resolution
2. **Remove stale artifacts** - `.RData`, `nul`, `NFLvsmarket_report.html`
3. **Update `.gitignore`** - Add `*.html` to prevent re-addition

### P1: High (Should Fix)

4. **Create `.lintr`** - Prevent VS Code crashes
5. **Update VS Code settings** - Add R extension config
6. **Delete `tests/test_core_math.R`** - Duplicate of test-utils.R

### P2: Medium (Recommended)

7. **Move validation scripts to `validation/`** - Clean root directory
8. **Deprecate R 4.5.1 compat scripts** - One-time fixes applied
9. **Merge duplicate validation scripts** - rolling_validation_system + rolling_window_validation
10. **Create `R/contracts.R`** - Data contract definitions

### P3: Low (Nice to Have)

11. **Create `scripts/verify_repo_integrity.R`** - Automated schema checks
12. **Create `scripts/run_matrix.R`** - Execute all runnable artifacts
13. **Add `reports/` directory** - Centralize generated outputs

---

## Verification Commands

After remediation, verify with:

```bash
# 1. Run tests (should all pass)
Rscript -e "testthat::test_dir('tests/testthat')"

# 2. Run verification script
Rscript scripts/verify_requirements.R

# 3. Run weekly simulation
Rscript run_week.R 16 2024

# 4. Check for stale files
git status --porcelain
```

---

## Files to Delete (Safe)

These files can be safely deleted with no impact:

| File | Reason |
|------|--------|
| `.RData` | Stale session data |
| `nul` | Empty Windows error file |
| `NFLvsmarket_report.html` | Generated artifact |
| `tests/test_core_math.R` | Duplicates test-utils.R |
| `comprehensive_test_results.rds` | Regenerable artifact |
| `model_validation_*.rds` | Regenerable artifacts |
| `r451_compatibility_report.rds` | Regenerable artifact |

---

## Files to Deprecate (Safe)

Mark with `# DEPRECATED:` comment and remove after 1 release:

| File | Reason |
|------|--------|
| `r451_compatibility_fixes.R` | One-time R 4.5.1 patches applied |
| `comprehensive_r451_test_suite.R` | One-time compat tests completed |

---

## Data Contract Definitions (Proposed)

Create `R/contracts.R` with required columns and types:

```r
# Schedule contract
SCHEDULE_CONTRACT <- list(
  required = c("game_id", "season", "week", "home_team", "away_team", "gameday"),
  types = list(game_id = "character", season = "integer", week = "integer",
               home_team = "character", away_team = "character")
)

# Predictions contract
PREDICTIONS_CONTRACT <- list(
  required = c("game_id", "season", "week", "home_win_prob"),
  types = list(game_id = "character", season = "integer", week = "integer",
               home_win_prob = "numeric"),
  constraints = list(home_win_prob = list(min = 0, max = 1))
)
```

---

## Known Limitations (Accepted)

1. **2025 Injury Data**: May show "missing" until nflverse releases data
2. **Neutral Site Games**: Use league-average weather conditions
3. **New Stadiums**: May need manual coordinate additions
4. **Calibration Leakage**: Nested CV has minimal impact; documented

---

## Previous Audit Summary (20 Issues)

The previous audit documented 20 issues that have been addressed:

| # | Issue | Status |
|---|-------|--------|
| 1-8 | Core utility functions | RESOLVED |
| 9-11 | Type-safe joins | RESOLVED |
| 12-17 | Data quality tracking | RESOLVED |
| 18 | Config parameters | RESOLVED |
| 19 | Test suite | NEEDS FIX (path issue) |
| 20 | Documentation | RESOLVED |

---

## Appendix: Line Count Summary

| Category | Files | Lines |
|----------|-------|-------|
| Core Engine | 4 | 13,073 |
| R/ Library | 5 | 2,281 |
| Validation | 11 | 6,353 |
| Tests | 6 | ~500 |
| Scripts | 1 | 551 |
| **Total** | **27** | **~22,758** |

---

*End of Audit Report*
