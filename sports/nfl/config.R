# =============================================================================
# FILE: sports/nfl/config.R
# PURPOSE: NFL-specific configuration parameters
#
# VERSION: 2.7.0
# LAST UPDATED: 2026-02-02
#
# DESCRIPTION:
#   NFL-specific parameters for the multi-sport prediction framework.
#   These values have been statistically validated on historical data.
# =============================================================================

# Sport identifier
SPORT <- "NFL"

# Score distribution configuration
SCORE_DISTRIBUTION <- "negbin"  # Negative binomial for NFL (overdispersion)
SCORE_CORRELATION <- -0.15      # Slight negative (defensive games vs shootouts)

# Home field advantage (validated)
HOME_FIELD_ADVANTAGE <- 2.5     # Points added to home team expected score

# Monte Carlo settings
DEFAULT_TRIALS <- 100000L       # Standard simulation count
MIN_TRIALS <- 10000L            # Minimum for quick estimates
MAX_TRIALS <- 500000L           # Maximum for high precision

# Scoring distribution parameters
MEAN_NFL_SCORE <- 23.0          # League average points per game
SD_NFL_SCORE <- 10.5            # Typical score standard deviation
SCORE_CAP <- 70L                # Maximum simulated score

# Market shrinkage (validated via k-fold CV)
# 60% market weight = most conservative without sacrificing edge detection
MARKET_SHRINKAGE <- 0.60
MARKET_SHRINKAGE_RANGE <- c(0.40, 0.80)  # Valid range

# Kelly criterion staking
KELLY_FRACTION <- 0.125         # 1/8 Kelly for conservative sizing
MAX_STAKE <- 0.02               # 2% max stake per bet
MAX_EDGE <- 0.10                # 10% max believable edge

# Rest and scheduling adjustments (validated p < 0.05)
REST_SHORT_PENALTY <- -0.85     # Short rest (≤6 days)
REST_LONG_BONUS <- 0.40         # Long rest (≥9 days, not post-bye)
BYE_BONUS <- 1.00               # Post-bye week bonus

# Weather impact parameters (validated p < 0.05)
WIND_THRESHOLD <- 15            # MPH for wind penalty
WIND_PENALTY_PER_10MPH <- -1.0  # Points per 10 mph over threshold
COLD_THRESHOLD <- 32            # Fahrenheit for cold penalty
COLD_PENALTY <- -0.5            # Points when below threshold
PRECIP_PENALTY <- -0.8          # Rain/snow impact
DOME_BONUS <- 0.8               # Indoor game bonus

# Injury impact weights (validated p < 0.001)
# These are multipliers for the base position impact
INJURY_WEIGHTS <- list(
  QB = 7.2,        # Quarterback: highest impact
  RB = 0.55,       # Running back

  WR = 0.55,       # Wide receiver
  TE = 0.55,       # Tight end
  OL = 0.65,       # Offensive line (T, G, C)
  DL = 0.50,       # Defensive line (DE, DT, NT)
  LB = 0.50,       # Linebackers
  DB = 0.45        # Secondary (CB, S)
)

# Injury status probability of missing
INJURY_STATUS_PROBS <- list(
  OUT = 1.00,       # Definitely missing
  DOUBTFUL = 0.75,  # 75% likely to miss
  QUESTIONABLE = 0.25,  # 25% likely to miss
  PROBABLE = 0.05,  # 5% likely to miss
  NONE = 0.00       # Playing
)

# Week configuration
REGULAR_SEASON_WEEKS <- 1:18
PLAYOFF_WEEKS <- list(
  WILD_CARD = 19,
  DIVISIONAL = 20,
  CONFERENCE = 21,
  SUPER_BOWL = 22
)

# Calibration settings
CALIBRATION_METHOD <- "spline"  # "spline", "isotonic", or "platt"
CALIBRATION_SPLINE_K <- 10      # GAM spline basis dimension
CALIBRATION_WINDOW_YEARS <- 4   # Years of data for calibration

# Data sources
DATA_PROVIDER <- "nflreadr"     # Primary data source
INJURY_FALLBACK <- "sleeper"    # Fallback injury source
