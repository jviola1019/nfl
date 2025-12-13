# Getting Started with NFL Prediction Model

A beginner-friendly guide to running NFL game predictions using this statistical model.

**Version**: 2.1
**R Version Required**: 4.3.0+ (tested on 4.5.1)
**Last Updated**: December 2025

---

## Quick Start (5 Minutes)

### 1. Install R (4.3.0+)

Download and install R from [CRAN](https://cran.r-project.org/):
- **Windows**: Download `R-4.x.x-win.exe`
- **Mac**: Download `R-4.x.x-arm64.pkg` (Apple Silicon) or `R-4.x.x-x86_64.pkg` (Intel)
- **Linux**: `sudo apt-get install r-base` (Ubuntu/Debian)

### 2. Install Dependencies (Recommended: renv)

```bash
# Install renv and restore all packages
R -e "install.packages('renv'); renv::restore()"
```

Or install packages manually:
```r
install.packages(c("tidyverse", "nflreadr", "gt", "glmnet", "zoo"))
```

### 3. Run Weekly Predictions

**Recommended method (command line):**
```bash
# Run for current week (uses config.R defaults)
Rscript run_week.R

# Specify week and season
Rscript run_week.R 15        # Week 15, current season
Rscript run_week.R 15 2024   # Week 15, 2024 season
```

**Alternative: Edit config.R first**
```bash
# 1. Edit WEEK_TO_SIM in config.R
# 2. Run the simulation
Rscript NFLsimulation.R
```

### 4. View Results

The script generates an HTML report with:
- Game-by-game predictions with win probabilities
- EV analysis and betting recommendations
- Market comparison (model vs Vegas)
- Stake sizing using Kelly criterion

Output file: `NFLvsmarket_week15_2024.html` (or similar)

---

## IDE Setup (Optional)

### RStudio (Recommended for Beginners)

1. Download from [posit.co/download/rstudio-desktop](https://posit.co/download/rstudio-desktop/)
2. Open project: `File > Open Project` → select the `nfl` folder
3. Run in Console: `source("run_week.R")`

### VS Code

1. Install [R Extension](https://marketplace.visualstudio.com/items?itemName=REditorSupport.r)
2. Open folder and use integrated terminal
3. Run: `Rscript run_week.R 15`

---

## Model Performance

**Validation Results** (2022-2024, 2,282 games):
- **Brier Score**: 0.211 (Vegas: 0.208) - Professional-grade accuracy
- **Accuracy**: 67.1% - Correctly predicts 2 out of 3 games
- **Beats** FiveThirtyEight (0.215) and ESPN FPI (0.218)

**See [RESULTS.md](RESULTS.md) for complete validation results.**

---

## Troubleshooting

### "Error: could not find function 'year'"
```r
install.packages("lubridate")
```

### "No injury data loaded"
The model gracefully handles missing injury data. To check:
```r
library(nflreadr)
injuries <- load_injuries(seasons = 2025)
```

### "Predictions seem incorrect"
1. Verify `WEEK_TO_SIM` and `SEASON` in `config.R`
2. Wait 1-2 days after games for nflverse data updates

### Package installation issues
```r
# Use renv for reproducible package management
renv::restore()
```

---

## Common Workflows

### Weekly Predictions
```bash
# Every Tuesday after Monday Night Football
Rscript run_week.R 16  # Increment week number
```

### Full Season Backtest
```r
source("professional_model_benchmarking.R")
# Tests against FiveThirtyEight and ESPN FPI (15-20 min)
```

### Enable Debug Mode
```r
options(nfl.ev_debug = TRUE)
source("run_week.R")  # Shows EV calculation trace
```

---

## File Structure

**To run predictions**:
- `run_week.R` - Main entry point (recommended)
- `config.R` - Edit settings (WEEK_TO_SIM, SEASON)
- `NFLsimulation.R` - Core prediction engine

**Documentation**:
- `README.md` - Project overview
- `GETTING_STARTED.md` - This guide
- `DOCUMENTATION.md` - Technical reference
- `RESULTS.md` - Validation results

---

## Requirements

- **R**: Version 4.3.0+ (tested on 4.5.1)
- **RAM**: 8 GB minimum
- **Packages**: Managed via `renv.lock`

---

**Ready? Run `Rscript run_week.R` to get started!**
