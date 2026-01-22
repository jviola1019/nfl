# NFL Game Prediction Model

![CI](https://github.com/jviola1019/nfl/actions/workflows/ci.yml/badge.svg)

A production-ready statistical model for predicting NFL game outcomes using Monte Carlo simulation and data-driven analysis.

**Version**: 2.5
**R Version Required**: 4.3.0+ (tested on 4.5.1)
**Status**: Production-Ready

---

## Documentation

- **[GETTING_STARTED.md](GETTING_STARTED.md)** - Quick start guide, IDE setup, troubleshooting
- **[DOCUMENTATION.md](DOCUMENTATION.md)** - Complete technical reference, methodology, validation results

---

## Quick Start

### Installation (2 minutes)

```bash
# Clone the repository
git clone https://github.com/jviola1019/nfl.git
cd nfl

# Install dependencies using renv (recommended)
R -e "install.packages('renv'); renv::restore()"
```

### Run Weekly Analysis

```bash
# Run for current week (uses config.R defaults)
Rscript run_week.R

# Or specify week and season
Rscript run_week.R 15        # Week 15, current season
Rscript run_week.R 15 2024   # Week 15, 2024 season
```

The script generates an HTML report with:
- Game-by-game predictions
- EV analysis and betting recommendations
- Market comparison (blend vs Vegas)
- Stake sizing using Kelly criterion

**For detailed setup**: See **[GETTING_STARTED.md](GETTING_STARTED.md)** for IDE setup (RStudio + VS Code) and troubleshooting.

---

## Model Accuracy

**Performance** (2022-2024, 2,282 games):
- **Brier Score**: 0.211 (Vegas: 0.208) - Near-market accuracy
- **Accuracy**: 67.1% (2 out of 3 games correct)
- **RMSE**: 10.82 points (within target)

**Comparison** (same dataset):
- **Vegas Market**: 0.208 Brier (benchmark)
- **This Model**: 0.211 Brier (+0.003 from market)
- Confidence interval includes 0 (not statistically distinguishable from market)

*Note: External model comparisons (FiveThirtyEight, ESPN) are approximate since they use different datasets and time periods. See `tests/testthat/` for unit tests validating EV/Kelly/de-vig calculations.*

**See [DOCUMENTATION.md](DOCUMENTATION.md) for complete validation results and statistical methodology.**

---

## How the Model Works (Simple Explanation)

### Step 1: Collect Data
- Downloads play-by-play data for all NFL games (2002-present)
- Gets current injury reports from nflverse
- Fetches weather forecasts for outdoor stadiums
- Loads team rosters and depth charts

### Step 2: Calculate Team Strength
The model evaluates each team using:

**Offensive Metrics:**
- Points per game (recent games weighted more heavily)
- Yards per play and explosive play rate
- Red zone touchdown conversion rate
- Third down conversion efficiency

**Defensive Metrics:**
- Points allowed per game
- Yards allowed per play
- Red zone defense efficiency
- Pass rush effectiveness vs opponent pass protection

**Situational Factors:**
- Strength of schedule (how good the opponents were)
- Recent form (last 3 games count more than games from month ago)
- Home field advantage (average +2.3 points)
- Rest days (teams on short rest penalized -0.85 points)
- Bye week recovery bonus (+1.0 points)

### Step 3: Adjust for Current Conditions

**Injuries** (validated with p < 0.01):
- Quarterback out: -7.2 points on average
- Skill positions (WR/RB/TE): -0.55 points per injured starter
- Offensive/defensive line: -0.65 points per injured starter
- Secondary (CB/S): -0.45 points per injured starter
- Linebackers/edge rushers: -0.50 points per injured starter

**Weather** (validated with p < 0.05):
- Indoor dome: +0.8 points to total scoring
- High wind (>15 mph): -1.0 points to passing offense
- Cold temperature (<32°F): -0.5 points to total scoring
- Rain or snow: -0.8 points to total scoring

**Other Adjustments:**
- Division rivalry games: -0.2 points (games tend to be closer)
- All adjustments are statistically validated (p < 0.05)

### Step 4: Run Monte Carlo Simulation

For each game:
1. Calculate expected points for home and away teams
2. Run 100,000 simulated games using statistical distributions
3. Account for score correlation (teams don't score independently)
4. Calculate win probability from simulation results

**Why 100,000 simulations?**
- Captures the full range of possible outcomes
- Provides reliable confidence intervals
- Accounts for randomness inherent in football

### Step 5: Calibrate Probabilities

Raw model probabilities are adjusted using isotonic regression to ensure they match real-world outcomes. This step prevents overconfidence.

---

## Configuration

Edit `config.R` to change settings:

```r
SEASON <- 2025              # Current season
WEEK_TO_SIM <- 12          # Week to predict (1-18)
N_TRIALS <- 100000         # Simulation count
```

All model parameters (injuries, weather, rest, etc.) are statistically validated (p < 0.05). See [DOCUMENTATION.md](DOCUMENTATION.md) for complete parameter details.

---

## Statistical Validation

All model parameters tested using:
- 10-fold cross-validation (2,282 games, 2022-2024)
- Permutation testing (p < 0.05 required)
- Effect size analysis

**Validation scripts**:
```bash
Rscript validation_pipeline.R              # Hyperparameter tuning
Rscript injury_model_validation.R          # Injury impacts
Rscript professional_model_benchmarking.R  # vs FiveThirtyEight/ESPN
```

See [DOCUMENTATION.md](DOCUMENTATION.md) for complete validation methodology.

---

## Troubleshooting

**Common issues and detailed troubleshooting**: See **[GETTING_STARTED.md](GETTING_STARTED.md#troubleshooting)**

**Quick fixes**:
- `"could not find function 'year'"`: Run `install.packages("lubridate")`
- `"could not find function '%>%'"`: Run `install.packages("dplyr")`
- `"unused argument (seasons_missing)"`: Update to v2.5 - API fixed
- No predictions: Check `WEEK_TO_SIM` and `SEASON` in config.R
- Tests fail with path errors: Ensure `tests/testthat/setup.R` exists

**Verify Repository Integrity**:
```bash
Rscript scripts/verify_repo_integrity.R  # Should show 35/35 passed
Rscript scripts/run_matrix.R             # Should show 9/9 passed
```

---

## Complete File Inventory

### Entry Points (Run These)
| File | Purpose |
|------|---------|
| `run_week.R` | **PRIMARY ENTRYPOINT** - Weekly prediction pipeline |
| `config.R` | All tunable parameters (SEASON, WEEK, N_TRIALS, etc.) |

### Core Engine (7,800+ lines each)
| File | Purpose |
|------|---------|
| `NFLsimulation.R` | Monte Carlo simulation engine (~7,800 lines) |
| `NFLmarket.R` | Market comparison and HTML report generation (~3,900 lines) |
| `NFLbrier_logloss.R` | Brier score, log loss, calibration metrics (~1,150 lines) |
| `injury_scalp.R` | Injury data loading with fallback sources |

### R Package Modules (`R/`)
| File | Purpose |
|------|---------|
| `R/utils.R` | **CANONICAL** - Core utilities (clamp, odds, Kelly, shrinkage) |
| `R/logging.R` | Structured logging (log_info, log_warn, log_error) |
| `R/data_validation.R` | Data quality tracking and validation |
| `R/playoffs.R` | Playoff round detection and game type handling |
| `R/date_resolver.R` | Timezone-safe datetime parsing |

### Verification Scripts (`scripts/`)
| File | Purpose |
|------|---------|
| `scripts/verify_repo_integrity.R` | Schema + invariant verification (35 checks) |
| `scripts/verify_requirements.R` | Package dependency validation |
| `scripts/run_matrix.R` | Execute all artifacts, record PASS/FAIL |

### Test Suite (`tests/testthat/`)
| File | Purpose |
|------|---------|
| `tests/testthat/setup.R` | Test infrastructure - loads R/ modules |
| `tests/testthat/test-utils.R` | Tests for R/utils.R functions |
| `tests/testthat/test-data-validation.R` | Tests for data quality tracking |
| `tests/testthat/test-playoffs.R` | Tests for playoff detection |
| `tests/testthat/test-date-resolver.R` | Tests for datetime parsing |
| `tests/testthat/test-game-type-mapping.R` | Tests for game type constants |

### Validation Scripts (Model Testing)
| File | Purpose |
|------|---------|
| `validation_pipeline.R` | Hyperparameter tuning with cross-validation |
| `model_validation.R` | Statistical significance testing |
| `injury_model_validation.R` | Validate injury impact coefficients |
| `professional_model_benchmarking.R` | Compare to FiveThirtyEight/ESPN |
| `calibration_refinement.R` | Isotonic regression tuning |
| `rolling_validation_system.R` | Rolling window backtesting |
| `rolling_window_validation.R` | Time-series validation |
| `ensemble_calibration_implementation.R` | Multi-method calibration |
| `simplified_baseline_comparison.R` | Baseline model comparisons |
| `lasso_feature_selection.R` | Feature importance via LASSO |
| `run_validation_example.R` | Example validation run |
| `validation/playoffs_validation.R` | Playoff-specific validation |

### Utility Scripts
| File | Purpose |
|------|---------|
| `r451_compatibility_fixes.R` | R 4.5.1 compatibility patches |
| `final_verification_checklist.R` | Pre-deployment verification |
| `comprehensive_code_validation.R` | Code quality checks |
| `comprehensive_r451_test_suite.R` | R version compatibility tests |
| `production_deployment_checklist.R` | Production readiness checks |

### Documentation
| File | Purpose |
|------|---------|
| `README.md` | This file - project overview |
| `GETTING_STARTED.md` | IDE setup guide (RStudio + VS Code) |
| `DOCUMENTATION.md` | Complete technical methodology |
| `CLAUDE.md` | **AUTHORITATIVE** - Agent context and API reference |
| `CHANGELOG.md` | Version history and fixes |
| `AUDIT.md` | File classification and inventory |

### Configuration & Environment
| File | Purpose |
|------|---------|
| `DESCRIPTION` | R package metadata |
| `renv.lock` | Package versions for reproducibility |
| `renv/` | renv configuration (library excluded from git) |
| `.lintr` | lintr configuration (120 char lines) |
| `.gitignore` | Git ignore rules |
| `.github/workflows/ci.yml` | GitHub Actions CI workflow |
| `.vscode/settings.json` | VS Code R extension settings |
| `.vscode/launch.json` | VS Code debug configuration |

### Generated Outputs (gitignored)
| Directory | Purpose |
|-----------|---------|
| `run_logs/` | Execution logs from run_matrix.R |
| `reports/` | Generated HTML reports |
| `*.rds` | Cached validation results |

---

## Data Sources

All data is obtained from the nflverse project (publicly available):

- **Play-by-play data**: nflreadr package (https://nflreadr.nflverse.com/)
- **Injury reports**: nflreadr::load_injuries() (updated weekly)
- **Schedule data**: nflreadr::load_schedules()
- **Team rosters**: nflreadr::load_rosters()

**Data Coverage**: 2002-2025 (complete historical records)
**Update Frequency**: Injury reports updated Tuesday-Friday during season
**Reliability**: Official NFL data aggregated by the nflverse community

### Injury Data System

The model uses a flexible injury loading system with multiple fallback options:

**Configuration** (`config.R`):
```r
INJURY_MODE <- "auto"  # Options: auto, off, last_available, manual, scalp
```

**Modes**:
- `auto` (default): Use nflreadr with automatic fallback to last available season
- `off`: Disable injury adjustments entirely
- `last_available`: Use most recent season with available data
- `manual`: Load from local file specified by `INJURY_MANUAL_FILE`
- `scalp`: Use current week practice/game status (experimental)

**Sources**:
- [nflreadr injury documentation](https://nflreadr.nflverse.com/reference/load_injuries.html)
- [nflverse data repository](https://github.com/nflverse/nflverse-data)

**Fallback Behavior**:
- If current season returns 404, automatically uses last available season
- HTML report shows banner indicating fallback mode used
- Zero injury impact used only when no data available from any season

---

## Requirements

- **R Version**: 4.3.0+ (tested on 4.5.1)
- **RAM**: 8 GB minimum
- **Packages**: Managed via `renv.lock` - run `renv::restore()` to install

**See [GETTING_STARTED.md](GETTING_STARTED.md#requirements) for complete system requirements.**

---

## License & Credits

This model is built on publicly available NFL data from the nflverse project. All statistical methods are documented in peer-reviewed literature.

**nflverse Project**: https://github.com/nflverse
**Statistical Methods**: Hierarchical Bayesian modeling, Monte Carlo simulation, isotonic regression calibration

---

## Updates & Maintenance

**Current Version**: 2.5 (January 2026)

**Recent fixes (v2.5)**:
- Fixed data quality API mismatches in NFLsimulation.R and verify_requirements.R
- Corrected parameter names: `missing_seasons` (not `seasons_missing`), `fallback_games` (not `games_fallback`)
- Corrected status values: `"full"` (not `"complete"`), `"unavailable"` (not `"missing"`)
- Created `tests/testthat/setup.R` for proper test path resolution
- Created `scripts/verify_repo_integrity.R` with 35 schema+invariant checks
- Created `scripts/run_matrix.R` to execute all artifacts with PASS/FAIL tracking
- Fixed `R/date_resolver.R` vectorization bugs
- Updated `CLAUDE.md` as authoritative agent guide with API reference

**Previous fixes (v2.4)**:
- Fixed kickoff_local timezone bug with `safe_with_tz()` helper
- Consolidated R utility functions into `R/utils.R` (canonical source of truth)
- Added type-safe joins via `standardize_join_keys()` with proper type coercion
- Created `R/logging.R` for structured logging
- Created `R/data_validation.R` for centralized data validation

**Previous fixes (v2.3)**:
- Fixed division-by-zero in devig_2way() and Kelly calculations
- Added actionable error messages for empty comparison tables
- Added HTML report keyboard shortcuts and quick filters
- Added unit tests for EV/Kelly/de-vig validation
