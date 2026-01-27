# Getting Started with NFL Prediction Model

A beginner-friendly guide to running NFL game predictions using this statistical model.

**Version**: 2.6.2
**R Version Required**: 4.3.0+ (tested on 4.5.1)
**Last Updated**: January 2026

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
install.packages(c("tidyverse", "nflreadr", "gt", "glmnet", "zoo", "lubridate", "randtoolbox"))
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

## IDE Setup

### RStudio (Recommended for Beginners)

1. Download from [posit.co/download/rstudio-desktop](https://posit.co/download/rstudio-desktop/)
2. Open project: `File > Open Project` → select the `nfl` folder
3. Install renv: In Console, run `install.packages("renv")`
4. Restore packages: Run `renv::restore()` in Console
5. Run predictions: `source("run_week.R")`

**Tips for RStudio**:
- Use `Ctrl+Enter` (Windows/Linux) or `Cmd+Enter` (Mac) to run selected code
- The Files pane shows project structure
- Use `Ctrl+Shift+F10` to restart R session if packages fail to load

### VS Code

1. Install [R Extension for Visual Studio Code](https://marketplace.visualstudio.com/items?itemName=REditorSupport.r)
2. Install [R LSP Client](https://marketplace.visualstudio.com/items?itemName=REditorSupport.r-lsp) for IntelliSense
3. Open the `nfl` folder: `File > Open Folder`
4. Open integrated terminal: `Ctrl+`` (backtick)
5. Install dependencies: `R -e "install.packages('renv'); renv::restore()"`
6. Run predictions: `Rscript run_week.R 15`

**Tips for VS Code**:
- Use `Ctrl+Shift+S` to send current line to R terminal
- Install "R Debugger" extension for debugging support
- The R extension provides syntax highlighting and code completion

---

## Model Performance

**Validation Results** (2022-2024, 2,282 games):
- **Brier Score**: 0.211 (Vegas: 0.208) - Professional-grade accuracy
- **Accuracy**: 67.1% - Correctly predicts 2 out of 3 games
- **Competitive with** FiveThirtyEight (0.215) and ESPN FPI (0.218)

See [DOCUMENTATION.md](DOCUMENTATION.md) for complete validation methodology.

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

You can also configure injury mode in `config.R`:
- `INJURY_MODE = "auto"` (default) - Uses nflreadr with fallback
- `INJURY_MODE = "sleeper"` - Uses Sleeper API for real-time injury data
- `INJURY_MODE = "off"` - Disables injury adjustments
- `INJURY_MODE = "last_available"` - Uses most recent available season
- `INJURY_MODE = "scalp"` - Scrapes ESPN for current week data

### "Predictions seem incorrect"
1. Verify `WEEK_TO_SIM` and `SEASON` in `config.R`
2. Wait 1-2 days after games for nflverse data updates

### Package installation issues
```r
# Use renv for reproducible package management
renv::restore()
```

### Windows: "00LOCK" error with glmnet
**Symptom**: `installation of package 'glmnet' had non-zero exit status` or `cannot remove prior installation of package 'glmnet'`

**Cause**: Windows file locking prevents package updates while R session is active.

**Solution**:
```r
# 1. Close all R sessions (including RStudio)
# 2. Delete the lock folder manually:
#    Navigate to: [R_LIBS_USER]/00LOCK-glmnet and delete it
# 3. Restart R and reinstall:
install.packages("glmnet")
```

**Alternative (run as Administrator)**:
```r
# Force removal
unlink(.libPaths()[1], recursive = TRUE, force = TRUE)
# Then reinstall all packages
renv::restore()
```

### "Stadium fallback warning"
This warning appears when venue data is missing for a game:
```
STADIUM FALLBACK: 1 game(s) using league-average weather conditions
```

**This is normal for**:
- International games (London, Mexico City)
- Neutral site games
- New stadiums not yet in database

The model uses moderate outdoor defaults (55°F, 8mph wind). To suppress:
```r
# In config.R
WARN_STADIUM_FALLBACK <- FALSE
```

### Color scale warnings
If you see warnings about values outside domain, they are cosmetic and handled automatically. The model uses `scales::squish` to clamp values.

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
- `DOCUMENTATION.md` - Technical reference and validation results

---

## Requirements

- **R**: Version 4.3.0+ (tested on 4.5.1)
- **RAM**: 8 GB minimum
- **Packages**: Managed via `renv.lock`

---

**Ready? Run `Rscript run_week.R` to get started!**
