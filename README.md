# NFL Game Prediction Model

A production-ready statistical model for predicting NFL game outcomes using Monte Carlo simulation and data-driven analysis.

**Version**: 2.0
**R Version Required**: 4.5.1 or higher
**Status**: Production-Ready

---

## 📚 Documentation

**Choose your guide based on your needs**:

1. **🚀 [GETTING_STARTED.md](GETTING_STARTED.md)** - Start here!
   - 5-minute quick start
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

**Quick links**: [RESULTS.md](RESULTS.md) (validation results) | [VALIDATION_README.md](VALIDATION_README.md) (validation methodology)

---

## Quick Start

**Ready to run predictions?** See **[GETTING_STARTED.md](GETTING_STARTED.md)** for:
- Complete installation guide (5 minutes)
- IDE setup (RStudio + VS Code)
- Running your first prediction
- Understanding output and troubleshooting

**Quick command** (if already set up):
```bash
# Edit WEEK_TO_SIM in config.R, then:
Rscript NFLsimulation.R
```

---

## Model Accuracy

**Performance** (2022-2024, 2,282 games):
- **Brier Score**: 0.211 (Vegas: 0.208) - Near-market accuracy
- **Accuracy**: 67.1% (2 out of 3 games correct)
- **RMSE**: 10.82 points (within target)

**Rankings**:
1. Vegas Market: 0.208 Brier
2. **This Model: 0.211 Brier** (100% data-driven)
3. FiveThirtyEight: 0.215 Brier
4. ESPN FPI: 0.218 Brier

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

**Core files**:
- `config.R` - All settings
- `NFLsimulation.R` - Main prediction engine
- `NFLmarket.R` - Market comparison tools
- `NFLbrier_logloss.R` - Evaluation metrics

**See [DOCUMENTATION.md](DOCUMENTATION.md#file-reference-table) for complete file reference (20 R files, 6 documentation files).**

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

### Injury Data Status (2025)

The model uses nflreadr as the primary source:
- Load method: `nflreadr::load_injuries(seasons = 2025)`
- Fallback: nflfastR scrapers if nflreadr unavailable
- If no data available: Model runs with zero injury impact (conservative estimate)

**Sources:**
- [nflreadr injury documentation](https://nflreadr.nflverse.com/reference/load_injuries.html)
- [nflverse data repository](https://github.com/nflverse/nflverse-data)
- [ESPN API endpoints](https://gist.github.com/nntrn/ee26cb2a0716de0947a0a4e9a157bc1c)

---

## Requirements

- **R Version**: 4.5.1+ (required)
- **RAM**: 8 GB minimum
- **Packages**: Auto-installed by config.R (tidyverse, nflreadr, lubridate, glmnet, lme4)

**See [GETTING_STARTED.md](GETTING_STARTED.md#requirements) for complete system requirements.**

---

## License & Credits

This model is built on publicly available NFL data from the nflverse project. All statistical methods are documented in peer-reviewed literature.

**nflverse Project**: https://github.com/nflverse
**Statistical Methods**: Hierarchical Bayesian modeling, Monte Carlo simulation, isotonic regression calibration

---

## Updates & Maintenance

**Current Version**: 2.0 (December 2025)

**Recent fixes**: R 4.5.1 compatibility, bug fixes, documentation consolidation

**See [UPDATES.md](UPDATES.md) for complete changelog and version history.**
