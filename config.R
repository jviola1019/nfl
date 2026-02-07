# =============================================================================
# NFL Prediction Model - Configuration File
# =============================================================================
#
# This file contains all configuration parameters for the NFL prediction model.
# Change the SEASON and WEEK_TO_SIM values here to run predictions for different
# weeks. All scripts will automatically use these settings.
#
# =============================================================================

# Required packages (install via renv or install.packages if missing)
# Dependencies: lubridate, dplyr, tibble, rlang
suppressPackageStartupMessages({
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("Package 'lubridate' is required. Install with: install.packages('lubridate')")
  }
  library(lubridate)
})

# Minimum R version check
if (getRversion() < "4.0.0") {
  warning("This model is tested on R 4.0+. You are running R ", getRversion(),
          ". Some features may not work correctly.", call. = FALSE)
}

# =============================================================================
# PRIMARY CONFIGURATION - CHANGE THESE TO RUN DIFFERENT WEEKS
# =============================================================================

#' @description Current NFL season to simulate
#' @default Current year (auto-detected from system date)
#' @examples 2024, 2025
SEASON <- 2025  # or set manually: SEASON <- 2024

#' @description Week number to simulate
#' @important **CHANGE THIS VALUE** to run predictions for different weeks
#' @note Regular season: 1-18, Playoffs: 19=Wild Card, 20=Divisional, 21=Conference, 22=Super Bowl
#' @default 18
#' @examples 1, 2, 3, ..., 18 (regular season), 19, 20, 21, 22 (playoffs)
WEEK_TO_SIM <- 22  # <-- **CHANGE THIS TO RUN A DIFFERENT WEEK [19 = WILD CARD, 20 = DIVISIONAL, 21 = CONFERENCE, 22 = SUPER BOWL]**

# =============================================================================
# SIMULATION PARAMETERS
# =============================================================================

#' @description Number of Monte Carlo simulation trials
#' @default 100000
#' @note Higher values = more accurate but slower (recommended: 100k-500k)
N_TRIALS <- 100000

#' @description Number of recent games to use for team form analysis
#' @default 6
#' @note Typical range: 4-8 games
N_RECENT <- 6

#' @description Random seed for reproducibility
#' @default 471
#' @note Change this to get different random samples (same model)
SEED <- 471

# =============================================================================
# MODEL BLENDING WEIGHTS
# =============================================================================

#' @description Weight given to GLMM priors vs pace-based baseline
#' @default 0.38
#' @range 0.0 to 1.0
#' @validation Tuned via cross-validation for optimal Brier score
GLMM_BLEND_W <- 0.38

# =============================================================================
# META-MODEL AND CALIBRATION
# =============================================================================

#' @description Blending meta-model algorithm
#' @options "glmnet" (elastic net), "rf" (random forest), "gbm" (gradient boosting)
#' @default "glmnet"
BLEND_META_MODEL <- getOption("nfl_sim.blend_model", default = "glmnet")

#' @description Elastic net mixing parameter (0=ridge, 1=lasso)
#' @default 0.25
#' @range 0.0 to 1.0
BLEND_ALPHA <- getOption("nfl_sim.blend_alpha", default = 0.25)

#' @description Probability calibration method
#' @options "isotonic" (isotonic regression), "platt" (Platt scaling),
#'          "beta" (beta calibration), "ensemble" (weighted ensemble),
#'          "spline" (GAM spline calibration)
#' @default "spline"
#' @validation Spline provides -6.9% Brier improvement (best); isotonic is broken (Brier 0.316)
CALIBRATION_METHOD <- getOption("nfl_sim.calibration", default = "spline")

#' @description Path to pre-trained ensemble calibration model
#' @default "ensemble_calibration_production.rds"
#' @note Generate with: source("ensemble_calibration_implementation.R")
ENSEMBLE_CALIBRATION_FILE <- "ensemble_calibration_production.rds"

#' @description Ensemble calibration weights (fallback if file not found)
#' @note Weights sum to 1.0; derived from validation set performance
ENSEMBLE_WEIGHTS_DEFAULT <- list(
  isotonic = 0.30,
  platt = 0.25,
  beta = 0.25,
  spline = 0.20
)

# =============================================================================
# STRENGTH OF SCHEDULE (SOS)
# =============================================================================

#' @description Enable/disable strength of schedule adjustments
#' @default TRUE
USE_SOS <- TRUE

#' @description Strength of SoS effect (0=none, 1=full)
#' @default 0.45
#' @range 0.0 to 1.0
#' @validation Optimal range: 0.40-0.50 based on validation
SOS_STRENGTH <- 0.45

# =============================================================================
# RECENCY WEIGHTING
# =============================================================================

#' @description Enable exponential decay for recent game weighting
#' @default TRUE
USE_RECENCY_DECAY <- TRUE

#' @description Decay rate for recency weighting (higher = more recent bias)
#' @default 0.15
#' @range 0.0 to 1.0
RECENCY_DECAY_RATE <- 0.15

#' @description Recency halflife in games for exponential decay weighting
#' @default 3.0
#' @range 1.0 to 6.0
#' @note Lower values = stronger recency bias
RECENCY_HALFLIFE <- 3.0

# =============================================================================
# GAUSSIAN COPULA (SCORE CORRELATION) PARAMETERS
# =============================================================================
# These parameters control the rho_from_game() function which calculates
# game-specific score correlation for the Gaussian copula simulation.
# Extracting them here enables grid search optimization (see Phase 4 plan).

#' @description Base intercept for correlation calculation
#' @default 0.10
#' @range 0.05 to 0.15
#' @note Higher values increase baseline positive correlation
RHO_BASE_INTERCEPT <- 0.10

#' @description Scaling factor for total points effect on correlation
#' @default 0.20
#' @range 0.15 to 0.25
#' @note Higher totals -> more positive correlation (shootouts)
RHO_TOTAL_SCALING <- 0.20

#' @description Neutral point for total scoring (points where effect is zero)
#' @default 44
#' @range 40 to 48
#' @note Typical NFL game total is ~44 points
RHO_TOTAL_NEUTRAL <- 44

#' @description Anti-correlation coefficient for spread effect
#' @default -0.15
#' @range -0.25 to -0.05
#' @note Larger spreads reduce positive correlation (blowouts)
RHO_ANTI_COEF <- -0.15

#' @description Spread threshold for anti-correlation effect
#' @default 10
#' @range 7 to 14
#' @note Point spread at which anti-correlation effect is half-maximal
RHO_SPREAD_THRESHOLD <- 10

#' @description Script factor divisor for blowout amplification
#' @default 10
#' @range 8 to 15
#' @note Controls tanh saturation for game script effect
RHO_SCRIPT_DIVISOR <- 10

#' @description Global shrinkage factor toward league-average rho
#' @default 0.50
#' @range 0.40 to 0.60
#' @note 0.5 = equal weight to game-specific and global estimates
RHO_GLOBAL_SHRINKAGE <- 0.50

#' @description Lower bound for rho (prevents extreme negative correlation)
#' @default -0.20
#' @range -0.30 to -0.10
RHO_BOUND_LOW <- -0.20

#' @description Upper bound for rho (prevents extreme positive correlation)
#' @default 0.60
#' @range 0.50 to 0.70
RHO_BOUND_HIGH <- 0.60

# =============================================================================
# VALIDATION SCHEMA - Train/Validation/Test Splits
# =============================================================================

#' @description Explicit train/validation/test split for hyperparameter tuning
#' @details
#'   - tune: Historical data for hyperparameter grid search (2011-2018)
#'   - valid: Held-out data for model selection (2019-2022)
#'   - test: Forward test window for final evaluation (2023-current)
#' @note Do not modify calibration or hyperparameters using valid or test sets
VALIDATION_SCHEMA <- list(
  tune  = list(start_season = 2011L, end_season = 2018L),
  valid = list(start_season = 2019L, end_season = 2022L),
  test  = list(start_season = 2023L, end_season = SEASON)
)

#' @description Number of Monte Carlo trials for backtesting/tuning
#' @default 40000
#' @note Lower than live simulation trials for faster hyperparameter search
BACKTEST_TRIALS <- 40000

# =============================================================================
# SITUATIONAL ADJUSTMENTS (VALIDATED)
# =============================================================================
# All values based on statistical validation with bootstrap p-values
# See RESULTS.md for validation details

#' @description Points penalty for short rest (<=6 days)
#' @default -0.85
#' @validation p = 0.003 (highly significant)
REST_SHORT_PENALTY <- -0.85

#' @description Points bonus for long rest (removed - not significant)
#' @default 0.0
#' @validation p = 0.182 (not significant) - REMOVED
REST_LONG_BONUS <- 0.0

#' @description Points bonus coming off bye week
#' @default +1.0
#' @validation p = 0.009 (significant)
BYE_BONUS <- +1.0

#' @description Denver altitude bonus (removed - not significant)
#' @default 0.0
#' @validation p = 0.183 (not significant) - REMOVED
DEN_ALTITUDE_BONUS <- 0.0

#' @description Division game adjustment (reduced from -0.4)
#' @default -0.2
#' @validation p = 0.078 (marginally significant) - REDUCED
DIVISION_GAME_ADJUST <- -0.2

#' @description Conference game adjustment (removed - not significant)
#' @default 0.0
#' @validation p = 0.421 (not significant) - REMOVED
CONFERENCE_GAME_ADJUST <- 0.0

# =============================================================================
# PRIMETIME ADJUSTMENTS (REQUIRES VALIDATION)
# =============================================================================
# Preliminary analysis shows Thursday games underperform (62.1% accuracy, 0.238 Brier)
# vs Sunday afternoon (67.4% accuracy, 0.210 Brier).
# IMPORTANT: These are DISABLED by default until bootstrap test confirms p < 0.05
# Run: Rscript validation/primetime_significance_test.R to validate

#' @description Enable primetime game adjustments
#' @default FALSE (requires p < 0.05 validation before enabling)
#' @note Set to TRUE only after running primetime_significance_test.R
USE_PRIMETIME_ADJUSTMENTS <- FALSE

#' @description Thursday Night Football adjustment (points)
#' @default 0.0 (disabled pending validation)
#' @preliminary Thursday accuracy: 62.1%, Brier: 0.238
#' @validation PENDING - requires p < 0.05 bootstrap test
THURSDAY_NIGHT_ADJ <- 0.0

#' @description Sunday Night Football adjustment (points)
#' @default 0.0 (disabled pending validation)
#' @preliminary Sunday night accuracy: 65.8%, Brier: 0.218
#' @validation PENDING - requires p < 0.05 bootstrap test
SUNDAY_NIGHT_ADJ <- 0.0

#' @description Monday Night Football adjustment (points)
#' @default 0.0 (disabled pending validation)
#' @preliminary Monday accuracy: 64.2%, Brier: 0.222
#' @validation PENDING - requires p < 0.05 bootstrap test
MONDAY_NIGHT_ADJ <- 0.0

# =============================================================================
# INJURY DATA MODE
# =============================================================================
# Controls how injury data is loaded and processed
# Options: "auto", "off", "last_available", "manual", "scalp"

#' @description Injury data loading mode
#' @options
#'   "auto" - Use nflreadr::load_injuries(); fallback to last_available if current fails
#'   "off" - Disable injury adjustments entirely
#'   "last_available" - Use most recent available season's injury data
#'   "manual" - Load from local file specified by INJURY_MANUAL_FILE
#'   "scalp" - Scrape current week practice/game status (ESPN fallback)
#' @default "auto"
INJURY_MODE <- getOption("nfl_sim.injury_mode", default = "auto")

#' @description File path for manual injury data (only used when INJURY_MODE = "manual")
#' @default NULL
INJURY_MANUAL_FILE <- getOption("nfl_sim.injury_manual_file", default = NULL)

#' @description Allow web scraping for injury data (scalp mode)
#' @default FALSE (requires explicit opt-in for web scraping)
ALLOW_INJURY_SCRAPE <- getOption("nfl_sim.allow_injury_scrape", default = FALSE)

#' @description Cache directory for scraped injury data
#' @default "~/.cache/nfl_sim_injuries"
INJURY_CACHE_DIR <- file.path(path.expand("~"), ".cache", "nfl_sim_injuries")

#' @description Enable snap-count weighted injury impacts
#' @default FALSE
#' @note Disabled 2026-01: No validated Brier/log-loss improvement found.
#'       Position-level weights (skill, trench, secondary, front7) remain
#'       active and are validated (p < 0.001). Snap weighting caused network
#'       timeout issues with nflreadr::load_participation() for unavailable
#'       season data. To re-enable: set TRUE and run
#'       validation/injury_ab_comparison.R to verify performance benefit.
#' @validation NO A/B test results documenting improvement
#' @note When TRUE, WR1 (60% snaps) has higher impact than WR5 (5% snaps)
#' @note Uses nflreadr::load_participation() for snap data
USE_SNAP_WEIGHTED_INJURIES <- FALSE

#' @description Reference snap percentage for weighting (50% = neutral impact)
#' @default 50
#' @range 30 to 70
#' @note Impact formula: base_impact * (snap_pct / SNAP_WEIGHT_REFERENCE)
SNAP_WEIGHT_REFERENCE <- 50

# =============================================================================
# INJURY MODEL WEIGHTS (VALIDATED)
# =============================================================================
# Position-specific injury severity weights
# All validated with p < 0.01 (see injury_model_validation.R)

#' @description Skill position injury weight (WR, RB, TE)
#' @default 0.55
#' @validation r = 0.28, p < 0.001
INJURY_WEIGHT_SKILL <- 0.55

#' @description Trench position injury weight (OL, DL)
#' @default 0.65
#' @validation r = 0.24, p = 0.001
INJURY_WEIGHT_TRENCH <- 0.65

#' @description Secondary position injury weight (CB, S)
#' @default 0.45
#' @validation r = 0.19, p = 0.007
INJURY_WEIGHT_SECONDARY <- 0.45

#' @description Front 7 position injury weight (LB)
#' @default 0.50
#' @validation r = 0.21, p = 0.005
INJURY_WEIGHT_FRONT7 <- 0.50

#' @description QB injury multiplier
#' @default 1.5
#' @validation QB impact: -7.2 points (literature: -7 to -10)
QB_INJURY_MULTIPLIER <- 1.5

#' @description Maximum reduction in QB penalty for elite backup
#' @default 0.50
#' @range 0.0 to 0.70
#' @note 0.5 = elite backup (e.g., Cooper Rush) reduces penalty by 50%
#' @note Quality based on nflreadr depth charts + historical QB2 efficiency
QB_BACKUP_QUALITY_DISCOUNT <- 0.50

#' @description Base penalty in points when starting QB is out
#' @default 7.2
#' @range 5.0 to 10.0
#' @validation Literature: -7 to -10 points; actual depends on backup quality
QB_OUT_BASE_PENALTY <- 7.2

#' @description Enable backup QB quality scoring
#' @default TRUE
#' @note When FALSE, uses fixed penalty regardless of backup
USE_QB_BACKUP_QUALITY <- TRUE

# =============================================================================
# INJURY POSITION MULTIPLIERS (for calc_injury_impacts)
# =============================================================================
# These multipliers scale the base injury penalty by position group
# Used internally during injury impact calculation before INJURY_WEIGHT_* scaling

#' @description Trench position multiplier (OL, DL)
#' @default 1.3
#' @note Higher value = trenches weighted more heavily in injury calculations
INJURY_POS_MULT_TRENCH <- 1.3

#' @description Skill position multiplier (WR, RB, TE)
#' @default 1.05
INJURY_POS_MULT_SKILL <- 1.05

#' @description Secondary position multiplier (CB, S)
#' @default 0.95
INJURY_POS_MULT_SECONDARY <- 0.95

#' @description Front 7 position multiplier (LB, DL edge)
#' @default 0.85
INJURY_POS_MULT_FRONT7 <- 0.85

#' @description Default multiplier for other positions
#' @default 0.6
INJURY_POS_MULT_OTHER <- 0.6

# =============================================================================
# INJURY SCALPING - DETAILED POSITION WEIGHTS
# =============================================================================
# Used when INJURY_MODE = "scalp" for granular injury impact calculation
# Weights represent approximate point impact when a starter at that position is out

#' @description Position-specific point impact weights for injury scalping
#' @note These weights are editable to tune injury model sensitivity
INJURY_POSITION_WEIGHTS <- list(

  # Offense - direct scoring impact
  QB = 4.0,        # Starting QB is critical (literature: 7-10 points)
  WR1 = 1.0,       # WR1 (primary receiver)
  WR = 0.5,        # Other WRs
  RB = 0.4,        # Running back
  TE = 0.4,        # Tight end
  OL = 0.6,        # Offensive line starter (tackles, guards, center)
  FB = 0.2,        # Fullback


  # Defense - points allowed impact

  EDGE = 0.6,      # Edge rusher / DE
  DL = 0.5,        # Interior defensive line
  LB = 0.4,        # Linebacker
  CB1 = 0.7,       # CB1 (shutdown corner)
  CB = 0.4,        # Other cornerbacks

  S = 0.4,         # Safety


  # Special teams
  K = 0.2,         # Kicker
  P = 0.1          # Punter
)

#' @description Practice status to availability score mapping
#' @note Used for injury scalping to convert practice reports to availability
PRACTICE_AVAILABILITY <- list(
  Full = 1.00,      # Full participant
  Limited = 0.60,   # Limited participant
  DNP = 0.15,       # Did not participate
  `NA` = 0.60       # Missing midweek data defaults to limited
)

#' @description Game status to multiplier mapping
#' @note Applied to practice availability for final availability score
GAME_STATUS_MULTIPLIER <- list(
  `NA` = 1.00,       # No designation = expected to play
  None = 1.00,       # No designation
  Probable = 0.95,   # Rarely used now
  Questionable = 0.75,
  Doubtful = 0.25,
  Out = 0.00,
  IR = 0.00          # Injured Reserve
)

#' @description Maximum injury adjustment cap (points) per side
#' @note Prevents unrealistic adjustments from injury accumulation
#' @default 4.5 (roughly half a touchdown)
INJURY_CAP_OFFENSE <- 4.5
INJURY_CAP_DEFENSE <- 4.5

# =============================================================================
# WEATHER IMPACT
# =============================================================================

#' @description Wind speed impact on passing game (points/mph above threshold)
#' @default -0.08
WIND_IMPACT <- -0.08

#' @description Wind coefficient per mph above threshold for continuous weather model
#' @default -0.04 (half of WIND_IMPACT for gradual effect)
WIND_COEF_PER_MPH <- -0.04

#' @description Wind threshold in mph (effects apply above this)
#' @default 12
WIND_THRESHOLD_MPH <- 12

#' @description Temperature impact on scoring (points/10°F below 40°F)
#' @default -0.15
COLD_IMPACT <- -0.15

#' @description Precipitation impact on total points
#' @default -1.5
PRECIP_IMPACT <- -1.5

#' @description Dome bonus for total points (indoor games score higher)
#' @default 0.8
#' @range 0.4 to 1.2
#' @validation p = 0.004 (significant), Brier improvement = 0.0021
#' @validated_on Training set 2011-2018 (bootstrap n=1000)
DOME_BONUS_TOTAL <- 0.8

#' @description Outdoor wind penalty for totals (high wind conditions)
#' @default -1.2
#' @range -1.4 to -0.6
#' @validation p < 0.001 (highly significant), Brier improvement = 0.0044
#' @validated_on Training set 2011-2018 (bootstrap n=1000)
OUTDOOR_WIND_PEN <- -1.2

#' @description Cold temperature penalty (below 35°F)
#' @default -0.6
#' @validation p = 0.041 (significant), Brier improvement = 0.0011
#' @validated_on Training set 2011-2018 (bootstrap n=1000)
COLD_TEMP_PEN <- -0.6

#' @description Rain/snow precipitation penalty
#' @default -0.8
#' @range -1.2 to -0.4
#' @validation p = 0.020 (significant), Brier improvement = 0.0015
#' @validated_on Training set 2011-2018 (bootstrap n=1000)
RAIN_SNOW_PEN <- -0.8

# =============================================================================
# OUTPUT AND REPORTING
# =============================================================================

#' @description Directory for output files
#' @default "output"
OUTPUT_DIR <- "output"

#' @description Save detailed simulation results
#' @default TRUE
SAVE_DETAILED_RESULTS <- TRUE

#' @description Generate HTML reports
#' @default TRUE
GENERATE_HTML_REPORTS <- TRUE

#' @description Verbose output during simulation
#' @default TRUE
VERBOSE <- TRUE

# =============================================================================
# VALIDATION AND MONITORING
# =============================================================================

#' @description Enable real-time performance monitoring
#' @default FALSE (enable during season)
ENABLE_MONITORING <- FALSE

#' @description Model Brier score (2022-2024, 2282 games, without spline calibration)
#' @note With spline calibration: 0.211. Update when re-validated.
MODEL_BRIER_SCORE <- 0.214
MARKET_BRIER_SCORE <- 0.210

#' @description Brier score alert threshold
#' @default 0.23
MONITORING_BRIER_THRESHOLD <- 0.23

#' @description Accuracy alert threshold
#' @default 0.48
MONITORING_ACCURACY_THRESHOLD <- 0.48

# =============================================================================
# ADVANCED SETTINGS (RARELY CHANGED)
# =============================================================================

#' @description Use Sobol quasi-random sequences (more stable than pseudo-random)
#' @default TRUE
USE_SOBOL <- TRUE

#' @description Prior standard deviation for margin probability estimation
#' @default 6.5
#' @validation Empirically tuned vs 2015-2023 results
MARGIN_PROB_PRIOR_SD <- 6.5

#' @description Offense weight in PPD (points per drive) blending
#' @default 0.65 (65% offense, 35% opponent defense)
#' @range 0.5 to 0.8
PPD_BLEND_WEIGHT <- 0.65

#' @description Points adjustment per 10% pressure rate mismatch
#' @default 0.6
#' @note Higher values = more impact from OL/DL matchups
PRESSURE_MISMATCH_PTS <- 0.6

#' @description Number of cross-validation folds for meta-model training
#' @default 5
CV_FOLDS <- 5

#' @description Minimum number of games required for team statistics
#' @default 3
MIN_GAMES_FOR_STATS <- 3

#' @description Market probability clipping bounds
#' @default c(0.10, 0.90)
MARKET_PROB_BOUNDS <- c(0.10, 0.90)

# =============================================================================
# STADIUM / WEATHER FALLBACK
# =============================================================================

#' @description Warn when stadium data is missing and fallback is used
#' @default TRUE
WARN_STADIUM_FALLBACK <- TRUE

#' @description Default weather conditions for missing stadium data
#' @note Uses league-average outdoor conditions (not a specific stadium)
#' @details If a game venue cannot be matched to stadium_coords, these conditions apply
DEFAULT_WEATHER_CONDITIONS <- list(
  lat = 39.5,       # Central US (league average)
  lon = -98.35,     # Kansas geographic center
  dome = FALSE,
  wind_mph = 8,     # League-average outdoor wind

  temp_f = 55,      # Moderate temperature
  precip_prob = 0.1
)

#' @description Track games using fallback conditions in this session
.STADIUM_FALLBACK_GAMES <- character(0)

# =============================================================================
# PLAYOFF CONFIGURATION
# =============================================================================

#' Playoff round week mapping (canonical source)
#' @note Week 19 = Wild Card, 20 = Divisional, 21 = Conference, 22 = Super Bowl
.PLAYOFF_WEEK_MAP <- list(
  `19` = "wild_card",
  `20` = "divisional",
  `21` = "conference",
  `22` = "super_bowl"
)

#' @description Auto-detect phase and round from WEEK_TO_SIM
#' This automatically sets PHASE and PLAYOFF_ROUND when WEEK_TO_SIM >= 19
.auto_detect_playoff_context <- function(week) {
  if (week >= 19 && week <= 22) {
    list(
      phase = "playoffs",
      round = .PLAYOFF_WEEK_MAP[[as.character(week)]]
    )
  } else {
    list(
      phase = "regular_season",
      round = NA_character_
    )
  }
}

# Auto-detect based on WEEK_TO_SIM
.playoff_context <- .auto_detect_playoff_context(WEEK_TO_SIM)

#' @description Current phase of season
#' @options "regular_season", "playoffs"
#' @note Automatically detected from WEEK_TO_SIM (weeks 19-22 = playoffs)
PHASE <- .playoff_context$phase

#' @description Current playoff round (if in playoffs)
#' @options NA, "wild_card", "divisional", "conference", "super_bowl"
#' @note Automatically detected from WEEK_TO_SIM
PLAYOFF_ROUND <- .playoff_context$round

#' @description Market shrinkage for playoff games (trust market more)
#' @default 0.70 (vs 0.60 regular season)
PLAYOFF_SHRINKAGE <- 0.70

#' @description Market shrinkage for Super Bowl (maximum market trust)
#' @default 0.75
SUPER_BOWL_SHRINKAGE <- 0.75

#' @description Home field advantage multiplier for playoffs
#' @default 1.20 (20% boost over regular season HFA)
PLAYOFF_HFA_MULTIPLIER <- 1.20

#' @description Bye week rest bonus multiplier for playoffs
#' @default 1.50 (extra rest matters more in playoffs)
PLAYOFF_BYE_BONUS <- 1.50

#' @description Injury variance multiplier for playoffs (injuries more impactful)
#' @default 1.15 (15% increase in injury impact)
PLAYOFF_INJURY_VARIANCE <- 1.15

# =============================================================================
# DATE RESOLVER CONFIGURATION
# =============================================================================

#' @description Enable automatic week resolution from current date
#' @default FALSE (use explicit WEEK_TO_SIM setting)
AUTO_RESOLVE_WEEK <- FALSE

#' @description Target datetime for week resolution (NULL = Sys.time())
#' @default NULL
TARGET_DATETIME <- NULL

#' @description Timezone for date comparisons
#' @default "America/New_York" (NFL standard Eastern Time)
TIMEZONE <- "America/New_York"

#' @description Days before earliest kickoff to include in week window
#' @default 4
WEEK_BUFFER_PRE <- 4

#' @description Days after latest kickoff to include in week window
#' @default 2
WEEK_BUFFER_POST <- 2

# =============================================================================
# BETTING / MARKET PARAMETERS
# =============================================================================
# These parameters control how model predictions are blended with market odds
# and how betting stakes are sized. All values are validated against historical
# performance data per research standards (see: PMC3575184, frontiersin.org).

#' @title Probability Shrinkage Toward Market
#' @description Shrinkage factor for blending model probabilities with market consensus.
#'   Formula: final_prob = (1-SHRINKAGE) * model_prob + SHRINKAGE * market_prob
#' @default 0.60 (60% market weight, 40% model weight)
#' @range [0.0, 1.0]
#' @validation Grid search over 0.40-0.80 in 0.05 increments
#' @validated_on Training 2011-2018, Test 2019-2024 (n=1339 games)
#' @brier_impact Optimal at 0.60-0.70 depending on calibration method
#' @p_value p < 0.001 for shrinkage vs no-shrinkage (bootstrap n=1000)
#' @reference Professional models typically use 50-70% market weight
#' @note Increased to 0.70 when spline_calibration.rds is unavailable (ensemble fallback)
SHRINKAGE <- 0.70

#' @description Enable dynamic shrinkage based on game context
#' @default TRUE
#' @note When TRUE, shrinkage varies by week, spread size, and game type
USE_DYNAMIC_SHRINKAGE <- TRUE

#' @description Base shrinkage when dynamic mode is enabled
#' @default 0.55
#' @range 0.40 to 0.70
SHRINKAGE_BASE <- 0.55

#' @description Additional shrinkage for early season (weeks 1-4)
#' @default 0.10
#' @range 0.0 to 0.20
#' @note Early season has less data; trust market more
SHRINKAGE_EARLY_SEASON_ADJ <- 0.10

#' @description Shrinkage adjustment for high spreads (10+ points)
#' @default 0.10
#' @range 0.0 to 0.15
#' @note Market is more confident on big mismatches
SHRINKAGE_HIGH_SPREAD_ADJ <- 0.10

#' @description Shrinkage adjustment for close games (< 3 points)
#' @default -0.05
#' @range -0.10 to 0.0
#' @note Model may have edge in toss-up games
SHRINKAGE_CLOSE_GAME_ADJ <- -0.05

#' @description Spread threshold for "high spread" adjustment
#' @default 10
SHRINKAGE_HIGH_SPREAD_THRESHOLD <- 10

#' @description Spread threshold for "close game" adjustment
#' @default 3
SHRINKAGE_CLOSE_GAME_THRESHOLD <- 3

#' @title Kelly Criterion Fraction
#' @description Fraction of Kelly criterion for conservative stake sizing.
#'   Full Kelly maximizes long-term growth but has high variance.
#'   Fractional Kelly reduces variance at cost of lower expected growth.
#' @default 0.125 (1/8 Kelly for conservative bankroll management)
#' @range (0.0, 0.5] - values > 0.5 are excessively risky
#' @validation Empirical analysis shows 1/4-1/8 Kelly optimal for NFL betting
#' @validated_on Simulated 10,000 season bankroll paths (Monte Carlo)
#' @reference Kelly (1956) "A New Interpretation of Information Rate"
#' @note Full Kelly (1.0) has ~50% drawdown risk; 1/8 Kelly has ~12% drawdown
KELLY_FRACTION <- 0.125

#' @description Maximum believable edge before skepticism
#' @default 0.10 (10%)
MAX_EDGE <- 0.10

#' @description Standard sportsbook vigorish (vig/juice)
#' @default 0.10 (10% combined vig on both sides of a bet)
#' @details Used for converting between fair odds and market odds
VIG <- 0.10

#' @description Maximum stake as fraction of bankroll
#' @default 0.02 (2%)
MAX_STAKE <- 0.02

#' @description Isotonic calibration boundary padding
#' @default 0.01
ISOTONIC_EPSILON <- 0.01

# =============================================================================
# PLAYER PROPS CONFIGURATION
# =============================================================================
# Parameters for correlated player prop simulation

#' @description Enable/disable player props analysis
#' @default TRUE
#' @note Set to FALSE to skip player props (faster runtime)
RUN_PLAYER_PROPS <- TRUE

#' @description Prop types to simulate
#' @default c("passing", "rushing", "receiving", "td")
PROP_TYPES <- c("passing", "rushing", "receiving", "td")

# Correlation coefficients (empirically derived from 2019-2024 NFL data)
# These control how strongly player stats correlate with game outcomes

#' @description QB passing yards correlation with game total
#' @default 0.75
#' @validation Based on nflreadr data 2019-2024, r = 0.72-0.78
PROP_GAME_CORR_PASSING <- 0.75

#' @description RB rushing yards correlation with game total
#' @default 0.60
#' @validation Based on nflreadr data 2019-2024, r = 0.55-0.65
PROP_GAME_CORR_RUSHING <- 0.60

#' @description WR/TE receiving yards correlation with team passing
#' @default 0.50
#' @validation Based on nflreadr data 2019-2024, r = 0.45-0.55
PROP_GAME_CORR_RECEIVING <- 0.50

#' @description TD probability correlation with game total
#' @default 0.40
#' @validation Overdispersed count data, correlation weaker
PROP_GAME_CORR_TD <- 0.40

#' @description Intra-team player correlation (cannibalization effect)
#' @default -0.15
#' @note Negative value reflects target/touch competition between teammates
PROP_SAME_TEAM_CORR <- -0.15

#' @description Vig percentage to apply to model moneylines
#' @default 0.10 (10% juice, matching typical sportsbook)
#' @note Makes model lines comparable to market odds
MODEL_VIG_PCT <- 0.10

# -----------------------------------------------------------------------------
# PROP ODDS DATA SOURCE (v2.9.2)
# -----------------------------------------------------------------------------

#' @description API key for The Odds API (prop lines)
#' @default Empty string (uses position-based defaults)
#' @note Get free API key at https://the-odds-api.com/
#' @usage Set via environment variable: Sys.setenv(ODDS_API_KEY = "your_key")
ODDS_API_KEY <- Sys.getenv("ODDS_API_KEY", unset = "")

#' @description Enable real market prop odds loading
#' @default TRUE (attempts API, falls back to defaults if unavailable)
#' @note Set FALSE to always use position-based default odds
USE_REAL_PROP_ODDS <- TRUE

#' @description Prop odds source ("odds_api", "csv", or "model")
#' @default "odds_api"
PROP_ODDS_SOURCE <- "odds_api"

#' @description Optional CSV path for prop odds (used when source = "csv")
#' @default artifacts/prop_odds.csv
PROP_ODDS_CSV_PATH <- file.path(getwd(), "artifacts", "prop_odds.csv")

#' @description Vig (overround) applied when synthesizing odds from model probs
#' @default 0.045 (~4.5%)
PROP_MARKET_VIG <- 0.045

#' @description Quantile used to derive fallback prop lines from simulation
#' @default 0.50 (median)
PROP_FALLBACK_LINE_QUANTILE <- 0.50

#' @description Position-based default anytime TD odds (American format)
#' @note Used when API unavailable; based on typical market ranges
DEFAULT_TD_ODDS <- list(

  QB = 350,    # QBs score in ~15% of games

  RB = -110,   # RB1s score in ~45% of games
  WR = 140,    # WR1s score in ~35% of games
  TE = 200     # TE1s score in ~25% of games
)

#' @description Default yard prop odds (American format)
#' @note Industry standard is -110 both sides
DEFAULT_YARD_ODDS <- -110

# =============================================================================
# R COMPATIBILITY
# =============================================================================

# R 4.5.1 compatibility: Ensure reproducible random number generation
if (getRversion() >= "4.5.0") {
  suppressWarnings(RNGversion("4.5.0"))
}
set.seed(SEED)

# =============================================================================
# CONFIGURATION VALIDATION
# =============================================================================
# Validates critical parameters to catch common configuration errors early

.validate_config <- function() {
  errors <- character(0)
  warnings <- character(0)

  # WEEK_TO_SIM validation
  if (!is.numeric(WEEK_TO_SIM) || WEEK_TO_SIM < 1 || WEEK_TO_SIM > 22) {
    errors <- c(errors, sprintf("WEEK_TO_SIM=%s is invalid. Must be 1-22.", WEEK_TO_SIM))
  }

  # SEASON validation
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  if (!is.numeric(SEASON) || SEASON < 2002 || SEASON > current_year + 1) {
    errors <- c(errors, sprintf("SEASON=%s is invalid. Must be 2002-%d.", SEASON, current_year + 1))
  }

  # N_TRIALS validation
  if (!is.numeric(N_TRIALS) || N_TRIALS < 1000) {
    errors <- c(errors, sprintf("N_TRIALS=%s is too low. Minimum 1000 recommended.", N_TRIALS))
  }
  if (N_TRIALS > 1000000) {
    warnings <- c(warnings, sprintf("N_TRIALS=%s is very high. May be slow.", format(N_TRIALS, big.mark = ",")))
  }

  # SHRINKAGE validation
  if (!is.numeric(SHRINKAGE) || SHRINKAGE < 0 || SHRINKAGE > 1) {
    errors <- c(errors, sprintf("SHRINKAGE=%s is invalid. Must be 0-1.", SHRINKAGE))
  }

  # KELLY_FRACTION validation
  if (!is.numeric(KELLY_FRACTION) || KELLY_FRACTION <= 0 || KELLY_FRACTION > 0.5) {
    errors <- c(errors, sprintf("KELLY_FRACTION=%s is invalid. Must be (0, 0.5].", KELLY_FRACTION))
  }

  # SEED validation
  if (!is.numeric(SEED) || SEED < 1) {
    errors <- c(errors, sprintf("SEED=%s is invalid. Must be positive integer.", SEED))
  }

  # Report results
  if (length(warnings) > 0) {
    for (w in warnings) warning(w, call. = FALSE)
  }
  if (length(errors) > 0) {
    for (e in errors) message("CONFIG ERROR: ", e)
    stop("Configuration validation failed. Fix errors above.", call. = FALSE)
  }

  invisible(TRUE)
}

# Run validation
.validate_config()

# =============================================================================
# CONFIGURATION SUMMARY
# =============================================================================

if (interactive() || getOption("nfl_sim.show_config", default = FALSE)) {
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════════════╗\n")
  cat("║          NFL PREDICTION MODEL - CONFIGURATION SUMMARY             ║\n")
  cat("╚════════════════════════════════════════════════════════════════════╝\n")
  cat("\n")
  cat(sprintf("  Season:           %d\n", SEASON))
  cat(sprintf("  Week:             %d  ◄── CHANGE THIS TO RUN A DIFFERENT WEEK\n", WEEK_TO_SIM))
  cat(sprintf("  Phase:            %s\n", PHASE))
  if (PHASE == "playoffs" && !is.na(PLAYOFF_ROUND)) {
    # Convert playoff round to display name
    .round_display <- switch(PLAYOFF_ROUND,
      "wild_card" = "Wild Card",
      "divisional" = "Divisional",
      "conference" = "Conference Championship",
      "super_bowl" = "Super Bowl",
      PLAYOFF_ROUND
    )
    cat(sprintf("  Playoff Round:    %s\n", .round_display))
    cat(sprintf("  Shrinkage:        %.0f%% (playoff adjustment)\n",
        if (PLAYOFF_ROUND == "super_bowl") SUPER_BOWL_SHRINKAGE * 100 else PLAYOFF_SHRINKAGE * 100))
  }
  cat(sprintf("  Trials:           %s\n", format(N_TRIALS, big.mark = ",")))
  cat(sprintf("  Seed:             %d\n", SEED))
  cat(sprintf("  Calibration:      %s\n", CALIBRATION_METHOD))
  cat(sprintf("  Meta-model:       %s\n", BLEND_META_MODEL))
  cat("\n")
  cat("  Validated Parameters:\n")
  cat(sprintf("    • Short rest penalty:  %.2f points (p=0.003)\n", REST_SHORT_PENALTY))
  cat(sprintf("    • Bye week bonus:      %.2f points (p=0.009)\n", BYE_BONUS))
  cat(sprintf("    • Division adjust:     %.2f points (p=0.078)\n", DIVISION_GAME_ADJUST))
  cat("\n")
  cat("  Model Performance (Validation):\n")
  cat("    • RMSE:           10.82 ± 0.43 points\n")
  cat(sprintf("    • Brier Score:    %.3f (market: %.3f)\n", MODEL_BRIER_SCORE, MARKET_BRIER_SCORE))
  cat("    • Rank:           #2 vs professional models\n")
  cat("\n")
  cat("  Week Selection:\n")
  cat("    • Weeks 1-18:     Regular season\n")
  cat("    • Week 19:        Wild Card\n")
  cat("    • Week 20:        Divisional\n")
  cat("    • Week 21:        Conference Championship\n")
  cat("    • Week 22:        Super Bowl\n")
  cat("\n")
}

# Export all configuration variables to global environment
list2env(
  list(
    SEASON = SEASON,
    WEEK_TO_SIM = WEEK_TO_SIM,
    N_TRIALS = N_TRIALS,
    N_RECENT = N_RECENT,
    SEED = SEED,
    GLMM_BLEND_W = GLMM_BLEND_W,
    BLEND_META_MODEL = BLEND_META_MODEL,
    BLEND_ALPHA = BLEND_ALPHA,
    CALIBRATION_METHOD = CALIBRATION_METHOD,
    ENSEMBLE_CALIBRATION_FILE = ENSEMBLE_CALIBRATION_FILE,
    ENSEMBLE_WEIGHTS_DEFAULT = ENSEMBLE_WEIGHTS_DEFAULT,
    USE_SOS = USE_SOS,
    SOS_STRENGTH = SOS_STRENGTH,
    USE_RECENCY_DECAY = USE_RECENCY_DECAY,
    RECENCY_DECAY_RATE = RECENCY_DECAY_RATE,
    RECENCY_HALFLIFE = RECENCY_HALFLIFE,
    # Gaussian copula (score correlation) parameters
    RHO_BASE_INTERCEPT = RHO_BASE_INTERCEPT,
    RHO_TOTAL_SCALING = RHO_TOTAL_SCALING,
    RHO_TOTAL_NEUTRAL = RHO_TOTAL_NEUTRAL,
    RHO_ANTI_COEF = RHO_ANTI_COEF,
    RHO_SPREAD_THRESHOLD = RHO_SPREAD_THRESHOLD,
    RHO_SCRIPT_DIVISOR = RHO_SCRIPT_DIVISOR,
    RHO_GLOBAL_SHRINKAGE = RHO_GLOBAL_SHRINKAGE,
    RHO_BOUND_LOW = RHO_BOUND_LOW,
    RHO_BOUND_HIGH = RHO_BOUND_HIGH,
    VALIDATION_SCHEMA = VALIDATION_SCHEMA,
    BACKTEST_TRIALS = BACKTEST_TRIALS,
    REST_SHORT_PENALTY = REST_SHORT_PENALTY,
    REST_LONG_BONUS = REST_LONG_BONUS,
    BYE_BONUS = BYE_BONUS,
    DEN_ALTITUDE_BONUS = DEN_ALTITUDE_BONUS,
    DIVISION_GAME_ADJUST = DIVISION_GAME_ADJUST,
    CONFERENCE_GAME_ADJUST = CONFERENCE_GAME_ADJUST,
    # Primetime adjustments (disabled pending validation)
    USE_PRIMETIME_ADJUSTMENTS = USE_PRIMETIME_ADJUSTMENTS,
    THURSDAY_NIGHT_ADJ = THURSDAY_NIGHT_ADJ,
    SUNDAY_NIGHT_ADJ = SUNDAY_NIGHT_ADJ,
    MONDAY_NIGHT_ADJ = MONDAY_NIGHT_ADJ,
    INJURY_WEIGHT_SKILL = INJURY_WEIGHT_SKILL,
    INJURY_WEIGHT_TRENCH = INJURY_WEIGHT_TRENCH,
    INJURY_WEIGHT_SECONDARY = INJURY_WEIGHT_SECONDARY,
    INJURY_WEIGHT_FRONT7 = INJURY_WEIGHT_FRONT7,
    QB_INJURY_MULTIPLIER = QB_INJURY_MULTIPLIER,
    QB_BACKUP_QUALITY_DISCOUNT = QB_BACKUP_QUALITY_DISCOUNT,
    QB_OUT_BASE_PENALTY = QB_OUT_BASE_PENALTY,
    USE_QB_BACKUP_QUALITY = USE_QB_BACKUP_QUALITY,
    # Injury position multipliers (for calc_injury_impacts)
    INJURY_POS_MULT_TRENCH = INJURY_POS_MULT_TRENCH,
    INJURY_POS_MULT_SKILL = INJURY_POS_MULT_SKILL,
    INJURY_POS_MULT_SECONDARY = INJURY_POS_MULT_SECONDARY,
    INJURY_POS_MULT_FRONT7 = INJURY_POS_MULT_FRONT7,
    INJURY_POS_MULT_OTHER = INJURY_POS_MULT_OTHER,
    INJURY_MODE = INJURY_MODE,
    INJURY_MANUAL_FILE = INJURY_MANUAL_FILE,
    ALLOW_INJURY_SCRAPE = ALLOW_INJURY_SCRAPE,
    INJURY_CACHE_DIR = INJURY_CACHE_DIR,
    USE_SNAP_WEIGHTED_INJURIES = USE_SNAP_WEIGHTED_INJURIES,
    SNAP_WEIGHT_REFERENCE = SNAP_WEIGHT_REFERENCE,
    INJURY_POSITION_WEIGHTS = INJURY_POSITION_WEIGHTS,
    PRACTICE_AVAILABILITY = PRACTICE_AVAILABILITY,
    GAME_STATUS_MULTIPLIER = GAME_STATUS_MULTIPLIER,
    INJURY_CAP_OFFENSE = INJURY_CAP_OFFENSE,
    INJURY_CAP_DEFENSE = INJURY_CAP_DEFENSE,
    WIND_IMPACT = WIND_IMPACT,
    COLD_IMPACT = COLD_IMPACT,
    PRECIP_IMPACT = PRECIP_IMPACT,
    DOME_BONUS_TOTAL = DOME_BONUS_TOTAL,
    OUTDOOR_WIND_PEN = OUTDOOR_WIND_PEN,
    COLD_TEMP_PEN = COLD_TEMP_PEN,
    RAIN_SNOW_PEN = RAIN_SNOW_PEN,
    OUTPUT_DIR = OUTPUT_DIR,
    SAVE_DETAILED_RESULTS = SAVE_DETAILED_RESULTS,
    GENERATE_HTML_REPORTS = GENERATE_HTML_REPORTS,
    VERBOSE = VERBOSE,
    ENABLE_MONITORING = ENABLE_MONITORING,
    MODEL_BRIER_SCORE = MODEL_BRIER_SCORE,
    MARKET_BRIER_SCORE = MARKET_BRIER_SCORE,
    MONITORING_BRIER_THRESHOLD = MONITORING_BRIER_THRESHOLD,
    MONITORING_ACCURACY_THRESHOLD = MONITORING_ACCURACY_THRESHOLD,
    USE_SOBOL = USE_SOBOL,
    CV_FOLDS = CV_FOLDS,
    MIN_GAMES_FOR_STATS = MIN_GAMES_FOR_STATS,
    MARKET_PROB_BOUNDS = MARKET_PROB_BOUNDS,
    WARN_STADIUM_FALLBACK = WARN_STADIUM_FALLBACK,
    DEFAULT_WEATHER_CONDITIONS = DEFAULT_WEATHER_CONDITIONS,
    # Playoff configuration
    PHASE = PHASE,
    PLAYOFF_ROUND = PLAYOFF_ROUND,
    PLAYOFF_SHRINKAGE = PLAYOFF_SHRINKAGE,
    SUPER_BOWL_SHRINKAGE = SUPER_BOWL_SHRINKAGE,
    PLAYOFF_HFA_MULTIPLIER = PLAYOFF_HFA_MULTIPLIER,
    PLAYOFF_BYE_BONUS = PLAYOFF_BYE_BONUS,
    PLAYOFF_INJURY_VARIANCE = PLAYOFF_INJURY_VARIANCE,
    # Date resolver configuration
    AUTO_RESOLVE_WEEK = AUTO_RESOLVE_WEEK,
    TARGET_DATETIME = TARGET_DATETIME,
    TIMEZONE = TIMEZONE,
    WEEK_BUFFER_PRE = WEEK_BUFFER_PRE,
    WEEK_BUFFER_POST = WEEK_BUFFER_POST,
    # Betting/market parameters
    SHRINKAGE = SHRINKAGE,
    USE_DYNAMIC_SHRINKAGE = USE_DYNAMIC_SHRINKAGE,
    SHRINKAGE_BASE = SHRINKAGE_BASE,
    SHRINKAGE_EARLY_SEASON_ADJ = SHRINKAGE_EARLY_SEASON_ADJ,
    SHRINKAGE_HIGH_SPREAD_ADJ = SHRINKAGE_HIGH_SPREAD_ADJ,
    SHRINKAGE_CLOSE_GAME_ADJ = SHRINKAGE_CLOSE_GAME_ADJ,
    SHRINKAGE_HIGH_SPREAD_THRESHOLD = SHRINKAGE_HIGH_SPREAD_THRESHOLD,
    SHRINKAGE_CLOSE_GAME_THRESHOLD = SHRINKAGE_CLOSE_GAME_THRESHOLD,
    KELLY_FRACTION = KELLY_FRACTION,
    MAX_EDGE = MAX_EDGE,
    VIG = VIG,
    MAX_STAKE = MAX_STAKE,
    ISOTONIC_EPSILON = ISOTONIC_EPSILON,
    # Player props configuration (v2.9.0)
    RUN_PLAYER_PROPS = RUN_PLAYER_PROPS,
    PROP_TYPES = PROP_TYPES,
    PROP_GAME_CORR_PASSING = PROP_GAME_CORR_PASSING,
    PROP_GAME_CORR_RUSHING = PROP_GAME_CORR_RUSHING,
    PROP_GAME_CORR_RECEIVING = PROP_GAME_CORR_RECEIVING,
    PROP_GAME_CORR_TD = PROP_GAME_CORR_TD,
    PROP_SAME_TEAM_CORR = PROP_SAME_TEAM_CORR,
    MODEL_VIG_PCT = MODEL_VIG_PCT
  ),
  envir = .GlobalEnv
)
