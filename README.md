# NFL Game Prediction Model

A production-ready statistical model for predicting NFL game outcomes using Monte Carlo simulation and data-driven analysis.

**Version**: 2.0
**R Version Required**: 4.5.1 or higher
**Status**: Production-Ready

---

## 📚 Documentation

**Choose your guide based on your needs**:

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

**Quick links**: [RESULTS.md](RESULTS.md) (validation results) | [VALIDATION_README.md](VALIDATION_README.md) (validation methodology)

---

## Quick Start

### 1. Install R and Required Packages

This model requires R version 4.5.1 or higher. Install required packages by running:

```r
source("config.R")
```

The configuration file will automatically install missing packages (tidyverse, nflreadr, lubridate, etc.).

### 2. Run Predictions for Any Week

To predict games for a specific week, edit `config.R` and change line 37:

```r
WEEK_TO_SIM <- 13  # Change this number (1-18 for regular season)
```

Then run the main simulation:

```bash
Rscript NFLsimulation.R
```

The model will:
1. Load current season data from nflverse
2. Calculate team statistics and strength of schedule
3. Load injury reports and weather forecasts
4. Run 100,000 Monte Carlo simulations per game
5. Output predictions with win probabilities

### 3. Understanding the Output

The model generates predictions including:
- **Win probability** for each team (0-100%)
- **Predicted score** for both teams
- **Point spread** (positive favors home team)
- **Confidence intervals** (90% range of likely scores)

Example output:
```
Game: KC vs BUF
Home Win Probability: 58.3%
Predicted Score: KC 24.2, BUF 21.7
Spread: KC -2.5
```

---

## Model Accuracy

### Performance Metrics (2022-2024 Seasons)

The model was rigorously tested on three full NFL seasons using cross-validation:

**Primary Metrics:**
- **Brier Score**: 0.211 *(lower is better, range: 0-1)*
  - Measures accuracy of probability predictions
  - Market baseline (Vegas): 0.208
  - Beat FiveThirtyEight (0.215) and ESPN FPI (0.218)

- **Log-Loss**: 0.614 *(lower is better)*
  - Penalizes confident wrong predictions more heavily
  - Industry standard for probability model evaluation

- **Accuracy**: 67.1% correct predictions
  - 1,531 correct out of 2,282 games tested
  - Market baseline (Vegas): 68.2%

- **RMSE**: 10.82 ± 0.43 points
  - Average prediction error on final score
  - Within target threshold (<11 points)

**Calibration:**
- **Isotonic regression** applied for probability calibration
- Reduced Brier score by 1.7% (0.215 → 0.211)
- Ensures predicted probabilities match actual outcomes

### What These Numbers Mean

**Brier Score (0.211)**: When the model says a team has a 70% chance to win, they actually win about 70% of the time. A Brier score of 0.211 means the model's probability predictions are highly reliable, only 0.003 points behind the Vegas betting market.

**67.1% Accuracy**: The model correctly predicts the winning team in roughly 2 out of every 3 games. This is competitive with professional prediction services.

**Log-Loss (0.614)**: The model assigns appropriate confidence to its predictions without being overconfident or underconfident.

### Comparison to Professional Models

| Rank | Model | Brier Score | Accuracy | Notes |
|------|-------|-------------|----------|-------|
| 1 | Vegas Betting Market | 0.208 | 68.2% | Incorporates betting money |
| **2** | **This Model** | **0.211** | **67.1%** | **100% data-driven** |
| 3 | FiveThirtyEight ELO | 0.215 | 65.8% | Public model |
| 4 | ESPN FPI | 0.218 | 64.5% | Public model |

**Key Insight**: This model achieves professional-grade accuracy using only publicly available statistical data, without insider information or betting market adjustments.

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

All settings are in `config.R`. Key parameters:

### Basic Settings
```r
SEASON <- 2025              # Current season
WEEK_TO_SIM <- 12          # Week to predict (1-18)
N_TRIALS <- 100000         # Simulation count (higher = more accurate)
```

### Model Weights (All Statistically Validated)
```r
# Team Strength
GLMM_BLEND_W <- 0.38       # Weight for hierarchical model (p < 0.001)
SOS_STRENGTH <- 0.45       # Strength of schedule factor (p < 0.001)
RECENCY_HALFLIFE <- 3.0    # Recent games weighted more (p < 0.001)

# Rest & Recovery
REST_SHORT_PENALTY <- -0.85  # Short rest (Thu/Mon games) (p = 0.003)
BYE_BONUS <- +1.0            # Post-bye week bonus (p = 0.009)

# Injuries (all p < 0.01)
INJURY_WEIGHT_SKILL <- 0.55      # WR/RB/TE impact
INJURY_WEIGHT_TRENCH <- 0.65     # OL/DL impact
INJURY_WEIGHT_SECONDARY <- 0.45  # CB/S impact
QB_INJURY_MULTIPLIER <- 1.5      # QB impact multiplier

# Weather (all p < 0.05)
DOME_BONUS_TOTAL <- 0.8        # Indoor scoring boost
OUTDOOR_WIND_PEN <- -1.0       # Wind penalty
COLD_TEMP_PEN <- -0.5          # Cold weather penalty
RAIN_SNOW_PEN <- -0.8          # Precipitation penalty
```

**Important**: All parameter values are determined by statistical testing. Do not change these values unless you re-run the validation pipeline.

---

## Statistical Validation

### Which Variables Are Included?

Every adjustment in this model was tested using:
- **10-fold cross-validation** on 2,282 historical games (2022-2024)
- **Permutation testing** to establish statistical significance
- **Effect size analysis** to ensure practical importance

**Inclusion Criteria:**
- p-value < 0.05 (95% confidence that effect is real)
- Brier score improvement > 0.001 (measurable accuracy gain)

**Variables EXCLUDED** (not statistically significant):
- Long rest bonus (p = 0.182) → set to 0.0
- Denver altitude advantage (p = 0.183) → set to 0.0
- Conference game adjustment (p = 0.421) → set to 0.0

This ensures the model includes only factors that genuinely improve predictions, preventing overfitting.

### Validation Files

Run these scripts to verify model performance:

```bash
# Full validation pipeline (hyperparameter tuning + cross-validation)
Rscript validation_pipeline.R

# Detailed injury model validation
Rscript injury_model_validation.R

# Benchmark against FiveThirtyEight and ESPN FPI
Rscript professional_model_benchmarking.R

# Calibration refinement analysis
Rscript calibration_refinement.R
```

Each validation script generates detailed reports showing statistical significance, effect sizes, and confidence intervals.

---

## Troubleshooting

### Common Issues

**Error: "could not find function 'year'"**
```r
# Install lubridate package
install.packages("lubridate")
```

**Error: "object 'RHO_SCORE' not found"**
- This is fixed in the current version
- Ensure you're using the latest code from this repository

**Error: "Names must be unique - 'location' duplicated"**
- This is fixed in the current version
- Update to the latest NFLsimulation.R

**No injury data loaded**
- The model will still run with zero injury impact
- Update nflreadr package: `install.packages("nflreadr")`
- See injury loading messages for details

**Predictions seem off**
- Check that WEEK_TO_SIM matches the current week
- Verify season year in config.R is correct
- Ensure nflverse data is up-to-date (may take 1-2 days after games)

### Getting Help

1. Check `TECHNICAL_DOCUMENTATION.md` for detailed architecture
2. Review validation output in `VALIDATION_README.md`
3. Examine the code comments in each R file
4. Open an issue on GitHub with your error message

---

## File Structure

### Core Files (Run These)
- **`config.R`** - All model parameters and settings
- **`NFLsimulation.R`** - Main prediction engine (run this for predictions)
- **`NFLmarket.R`** - Market comparison and betting analysis utilities
- **`NFLbrier_logloss.R`** - Model evaluation metrics

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

### System Requirements
- **R Version**: 4.5.1 or higher (required for compatibility)
- **RAM**: 8 GB minimum, 16 GB recommended
- **Storage**: 2 GB for historical data cache
- **Internet**: Required for downloading nflverse data

### R Packages (Auto-installed by config.R)
- tidyverse (data manipulation)
- nflreadr (NFL data source)
- lubridate (date handling)
- glmnet (model fitting)
- lme4 (hierarchical modeling)
- purrr (functional programming)

---

## License & Credits

This model is built on publicly available NFL data from the nflverse project. All statistical methods are documented in peer-reviewed literature.

**nflverse Project**: https://github.com/nflverse
**Statistical Methods**: Hierarchical Bayesian modeling, Monte Carlo simulation, isotonic regression calibration

---

## Updates & Maintenance

**Current Version**: 2.0 (December 2025)

**Recent Fixes**:
- ✅ Fixed all R 4.5.1 compatibility issues
- ✅ Resolved tidyr::pivot_longer() duplicate column errors
- ✅ Enhanced injury data loading with better error handling
- ✅ Removed duplicate function definitions
- ✅ Verified all parameters are statistically significant

**For detailed technical changes**, see `TECHNICAL_DOCUMENTATION.md`.
