# NFL Game Prediction Model

![CI](https://github.com/jviola1019/nfl/actions/workflows/ci.yml/badge.svg)

A production-ready statistical model for predicting NFL game outcomes using Monte Carlo simulation and data-driven analysis.

**Version**: 2.3
**R Version Required**: 4.3.0+ (tested on 4.5.1)
**Status**: Production-Ready

---

## 📚 Documentation

**Choose your guide based on your needs**:

### Core Documentation

1. **🚀 [GETTING_STARTED.md](GETTING_STARTED.md)** - Start here!
   - 5-minute extra detailed quick start
   - IDE setup (RStudio + VS Code)
   - Common workflows and troubleshooting
   - Perfect for beginners

2. **📖 [DOCUMENTATION.md](DOCUMENTATION.md)** - Complete technical reference
   - Full architecture and data pipeline
   - All functions documented
   - Statistical methodology and formulas
   - For developers and statisticians

3. **📝 [UPDATES.md](UPDATES.md)** - Recent changes and fixes
   - Version history and changelog
   - Bug fixes and improvements
   - Migration guides
   - Known issues

### Validation & Results

4. **📊 [RESULTS.md](RESULTS.md)** - Model performance metrics
   - Validation results (Brier, log-loss, accuracy)
   - Comparison to professional models
   - Statistical significance tests
   - Performance across different time periods

5. **🎯 [IMPROVEMENTS_SUMMARY.md](IMPROVEMENTS_SUMMARY.md)** - Recent optimizations
   - Session-by-session improvement tracking
   - Overfitting reduction strategies
   - R 4.5.1 compatibility fixes
   - Parameter validation results

6. **🚀 [PRODUCTION_READY_SUMMARY.md](PRODUCTION_READY_SUMMARY.md)** - Production deployment guide
   - Complete production readiness checklist
   - New validation tools documentation
   - Performance monitoring procedures
   - Best practices for weekly workflow

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

*Note: External model comparisons (FiveThirtyEight, ESPN) are approximate since they use different datasets and time periods. See `tests/test_core_math.R` for unit tests validating EV/Kelly/de-vig calculations.*

**See [RESULTS.md](RESULTS.md) for complete validation results and statistical tests.**

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

All model parameters (injuries, weather, rest, etc.) are statistically validated (p < 0.05). **See [DOCUMENTATION.md](DOCUMENTATION.md) for complete parameter details.**

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

**See [RESULTS.md](RESULTS.md) and [VALIDATION_README.md](VALIDATION_README.md) for complete validation methodology.**

---

## Troubleshooting

**Common issues and detailed troubleshooting**: See **[GETTING_STARTED.md](GETTING_STARTED.md#troubleshooting)**

**Quick fixes**:
- `"could not find function 'year'"`: Install lubridate → `install.packages("lubridate")`
- `"object 'season' not found"`: Fixed in current version (v2.0)
- No predictions: Check `WEEK_TO_SIM` and `SEASON` in config.R

---

## File Structure

### Core Files (Run These)
- **`run_week.R`** - Main entry point for weekly analysis (recommended)
- **`config.R`** - All model parameters and settings
- **`NFLsimulation.R`** - Main prediction engine
- **`NFLmarket.R`** - Market comparison and betting analysis utilities
- **`NFLbrier_logloss.R`** - Model evaluation metrics
- **`injury_scalp.R`** - Injury data loading with multiple fallback sources

### Environment & CI
- **`renv.lock`** - Package versions for reproducibility
- **`renv/`** - renv configuration (library excluded from git)
- **`.github/workflows/ci.yml`** - GitHub Actions CI workflow
- **`.gitignore`** - Git ignore rules

### Validation Files (Test Model Performance)
- **`validation_pipeline.R`** - Hyperparameter tuning and cross-validation
- **`model_validation.R`** - Statistical significance testing
- **`injury_model_validation.R`** - Injury impact validation
- **`professional_model_benchmarking.R`** - Compare to FiveThirtyEight/ESPN
- **`calibration_refinement.R`** - Probability calibration analysis

### Documentation
- **`README.md`** - This file (beginner guide)
- **`GETTING_STARTED.md`** - Further detailed beginner guide for both RStudio and VS Code
- **`DOCUMENTATION.md`** - Detailed code architecture/Validation methodology
- **`UPDATES.md`** - Update logs
- **`RESULTS.md`** - Detailed validation results and statistical tests

### Utility Files
- **`r451_compatibility_fixes.R`** - R 4.5.1 compatibility patches
- **`final_verification_checklist.R`** - Pre-deployment checks

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

**Current Version**: 2.3 (December 2025)

**Recent fixes (v2.3)**:
- **NFLbrier_logloss.R**: Fixed missing library imports (dplyr, tibble, purrr, rlang)
- **NFLbrier_logloss.R**: Fixed division-by-zero bug in devig_2way()
- **NFLmarket.R**: Added actionable error messages for empty comparison table
- **injury_scalp.R**: Fixed ESPN date field overwriting and added CSS selector fallbacks
- **HTML Report**: Added keyboard shortcuts (/ to search, Esc to clear)
- **HTML Report**: Added quick filter buttons (+EV Only, Suspicious Edges, Pass Games)
- **HTML Report**: Added reduced-motion accessibility fallback
- **Unit Tests**: Added `tests/test_core_math.R` for EV/Kelly/de-vig validation

**Previous fixes (v2.2)**:
- Injury data system with `injury_mode` config (auto/off/last_available/manual/scalp)
- Stadium fallback warnings for neutral site games (no longer silently defaults to KC)
- Pre-export validation checks for HTML report (prevents empty tables)
- Nested CV calibration clearly marked as production; global isotonic labeled diagnostic-only
- Redesigned HTML report with concise professional intro section
- Enhanced troubleshooting docs for Windows 00LOCK glmnet issue

**Previous fixes (v2.1)**:
- EV/pick/odds alignment bug fix with invariant checks
- Reproducibility setup with renv
- New `run_week.R` entry script
- GitHub Actions CI workflow
- Color scale warning fixes (`scales::squish`)

**See [UPDATES.md](UPDATES.md) for complete changelog and version history.**
