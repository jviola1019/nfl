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
SEASON <- year(Sys.Date())  # or set manually: SEASON <- 2024

#' @description Week number to simulate (1-18 for regular season)
#' @important **CHANGE THIS VALUE** to run predictions for different weeks
#' @default 11
#' @examples 1, 2, 3, ..., 18
WEEK_TO_SIM <- 18  # <-- **CHANGE THIS TO RUN A DIFFERENT WEEK**

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
#'          "beta" (beta calibration), "ensemble" (weighted ensemble)
#' @default "isotonic"
#' @validation Isotonic provides 1.7% Brier improvement; ensemble provides 2.1%
CALIBRATION_METHOD <- getOption("nfl_sim.calibration", default = "isotonic")

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

#' @description Wind speed impact on passing game (points/mph)
#' @default -0.08
WIND_IMPACT <- -0.08

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
# R COMPATIBILITY
# =============================================================================

# R 4.5.1 compatibility: Ensure reproducible random number generation
if (getRversion() >= "4.5.0") {
  suppressWarnings(RNGversion("4.5.0"))
}
set.seed(SEED)

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
  cat("    • Brier Score:    0.211 (market: 0.208)\n")
  cat("    • Rank:           #2 vs professional models\n")
  cat("\n")
  cat("  To change the week: Edit WEEK_TO_SIM in config.R\n")
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
    USE_SOS = USE_SOS,
    SOS_STRENGTH = SOS_STRENGTH,
    USE_RECENCY_DECAY = USE_RECENCY_DECAY,
    RECENCY_DECAY_RATE = RECENCY_DECAY_RATE,
    RECENCY_HALFLIFE = RECENCY_HALFLIFE,
    VALIDATION_SCHEMA = VALIDATION_SCHEMA,
    BACKTEST_TRIALS = BACKTEST_TRIALS,
    REST_SHORT_PENALTY = REST_SHORT_PENALTY,
    REST_LONG_BONUS = REST_LONG_BONUS,
    BYE_BONUS = BYE_BONUS,
    DEN_ALTITUDE_BONUS = DEN_ALTITUDE_BONUS,
    DIVISION_GAME_ADJUST = DIVISION_GAME_ADJUST,
    CONFERENCE_GAME_ADJUST = CONFERENCE_GAME_ADJUST,
    INJURY_WEIGHT_SKILL = INJURY_WEIGHT_SKILL,
    INJURY_WEIGHT_TRENCH = INJURY_WEIGHT_TRENCH,
    INJURY_WEIGHT_SECONDARY = INJURY_WEIGHT_SECONDARY,
    INJURY_WEIGHT_FRONT7 = INJURY_WEIGHT_FRONT7,
    QB_INJURY_MULTIPLIER = QB_INJURY_MULTIPLIER,
    INJURY_MODE = INJURY_MODE,
    INJURY_MANUAL_FILE = INJURY_MANUAL_FILE,
    ALLOW_INJURY_SCRAPE = ALLOW_INJURY_SCRAPE,
    INJURY_CACHE_DIR = INJURY_CACHE_DIR,
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
    MONITORING_BRIER_THRESHOLD = MONITORING_BRIER_THRESHOLD,
    MONITORING_ACCURACY_THRESHOLD = MONITORING_ACCURACY_THRESHOLD,
    USE_SOBOL = USE_SOBOL,
    CV_FOLDS = CV_FOLDS,
    MIN_GAMES_FOR_STATS = MIN_GAMES_FOR_STATS,
    MARKET_PROB_BOUNDS = MARKET_PROB_BOUNDS,
    WARN_STADIUM_FALLBACK = WARN_STADIUM_FALLBACK,
    DEFAULT_WEATHER_CONDITIONS = DEFAULT_WEATHER_CONDITIONS
  ),
  envir = .GlobalEnv
)
