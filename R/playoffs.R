# =============================================================================
# NFL Prediction Model - Playoffs Module
# =============================================================================
# Playoff round detection, playoff-specific features, and report generation.
# =============================================================================

# =============================================================================
# PLAYOFF ROUND DEFINITIONS
# =============================================================================

#' @title Playoff Round Configuration
#' @description Canonical playoff round definitions with expected properties
PLAYOFF_ROUNDS <- list(
  wild_card = list(
    name = "Wild Card",
    week = 19,
    expected_games = 6,
    home_field = TRUE,
    description = "Wild Card Weekend"
  ),
  divisional = list(
    name = "Divisional",
    week = 20,
    expected_games = 4,
    home_field = TRUE,
    description = "Divisional Round"
  ),
  conference = list(
    name = "Conference Championship",
    week = 21,
    expected_games = 2,
    home_field = TRUE,
    description = "AFC & NFC Championship Games"
  ),
  super_bowl = list(
    name = "Super Bowl",
    week = 22,
    expected_games = 1,
    home_field = FALSE,  # Neutral site
    description = "Super Bowl"
  )
)

#' @title Playoff Feature Adjustments
#' @description Playoff-specific model adjustments (validated parameters)
PLAYOFF_FEATURES <- list(
  # Home field advantage amplification (playoffs are higher stakes)
  hfa_multiplier = list(
    wild_card = 1.15,
    divisional = 1.20,
    conference = 1.25,
    super_bowl = 1.00  # Neutral site
  ),

  # Rest advantage bonus for bye week teams (divisional round)
  bye_rest_bonus = list(
    points = 1.5,  # Additional points for teams with bye
    applies_to = "divisional"
  ),

  # Market shrinkage adjustments (trust market more in playoffs)
  shrinkage = list(
    wild_card = 0.65,
    divisional = 0.70,
    conference = 0.70,
    super_bowl = 0.75
  ),

  # Injury variance multiplier (injuries more impactful in playoffs)
  injury_variance = list(
    wild_card = 1.10,
    divisional = 1.12,
    conference = 1.15,
    super_bowl = 1.20
  )
)

# =============================================================================
# ROUND DETECTION FUNCTIONS
# =============================================================================

#' Derive playoff round from schedule row
#' @param schedule_row A single row from schedule with game_type and week
#' @return Playoff round name or NA if not playoff
derive_playoff_round <- function(schedule_row) {
  if (is.null(schedule_row) || nrow(schedule_row) == 0) {
    return(NA_character_)
  }

  # Check for game_type column
  game_type <- if ("game_type" %in% names(schedule_row)) {
    schedule_row$game_type[1]
  } else {
    NA_character_
  }

  week <- if ("week" %in% names(schedule_row)) {
    as.integer(schedule_row$week[1])
  } else {
    NA_integer_
  }

  # Regular season check
  if (!is.na(game_type) && game_type == "REG") {
    return(NA_character_)
  }

  # Map game_type to round
  if (!is.na(game_type)) {
    round <- dplyr::case_when(
      game_type == "WC" ~ "wild_card",
      game_type == "DIV" ~ "divisional",
      game_type == "CON" ~ "conference",
      game_type == "SB" ~ "super_bowl",
      TRUE ~ NA_character_
    )
    if (!is.na(round)) return(round)
  }

  # Fallback to week-based detection
  if (!is.na(week)) {
    round <- dplyr::case_when(
      week == 19 ~ "wild_card",
      week == 20 ~ "divisional",
      week == 21 ~ "conference",
      week == 22 ~ "super_bowl",
      TRUE ~ NA_character_
    )
    return(round)
  }

  NA_character_
}

#' Derive playoff round from week number
#' @param week Week number (19-22 for playoffs)
#' @return Playoff round name or NA if not playoff week
derive_playoff_round_from_week <- function(week) {
  week <- as.integer(week)
  dplyr::case_when(
    week == 19 ~ "wild_card",
    week == 20 ~ "divisional",
    week == 21 ~ "conference",
    week == 22 ~ "super_bowl",
    TRUE ~ NA_character_
  )
}

#' Check if week is a playoff week
#' @param week Week number
#' @return TRUE if playoff week, FALSE otherwise
is_playoff_week <- function(week) {
  week <- as.integer(week)
  !is.na(week) && week >= 19 && week <= 22
}

#' Get phase (regular_season or playoffs) from week
#' @param week Week number
#' @return "regular_season" or "playoffs"
get_phase_from_week <- function(week) {
  week <- as.integer(week)
  if (is.na(week)) return("unknown")
  if (week >= 1 && week <= 18) return("regular_season")
  if (week >= 19 && week <= 22) return("playoffs")
  "unknown"
}

# =============================================================================
# ROUND VALIDATION FUNCTIONS
# =============================================================================

#' Validate playoff round game counts
#' @param schedule Schedule data frame
#' @param round Playoff round name
#' @return List with valid (TRUE/FALSE), expected, actual, message
validate_playoff_round_games <- function(schedule, round) {
  if (is.null(schedule) || nrow(schedule) == 0) {
    return(list(
      valid = FALSE,
      expected = NA_integer_,
      actual = 0L,
      message = "Empty schedule"
    ))
  }

  round_config <- PLAYOFF_ROUNDS[[round]]
  if (is.null(round_config)) {
    return(list(
      valid = FALSE,
      expected = NA_integer_,
      actual = NA_integer_,
      message = sprintf("Unknown round: %s", round)
    ))
  }

  expected <- round_config$expected_games
  actual <- nrow(schedule)

  valid <- actual == expected
  message <- if (valid) {
    sprintf("%s: %d games as expected", round_config$name, actual)
  } else {
    sprintf("%s: expected %d games, found %d", round_config$name, expected, actual)
  }

  list(
    valid = valid,
    expected = expected,
    actual = actual,
    message = message
  )
}

#' Validate all playoff rounds in a season
#' @param full_schedule Full season schedule
#' @param season Season year
#' @return List of validation results by round
validate_season_playoffs <- function(full_schedule, season) {
  results <- list()

  for (round_name in names(PLAYOFF_ROUNDS)) {
    round_config <- PLAYOFF_ROUNDS[[round_name]]

    # Filter to this round
    round_schedule <- full_schedule %>%
      dplyr::filter(.data$week == round_config$week | .data$game_type == toupper(substr(round_name, 1, 2)))

    results[[round_name]] <- validate_playoff_round_games(round_schedule, round_name)
  }

  results$all_valid <- all(sapply(results[names(PLAYOFF_ROUNDS)], function(x) x$valid))
  results
}

# =============================================================================
# PLAYOFF ADJUSTMENT FUNCTIONS
# =============================================================================

#' Calculate playoff adjustments for a game
#' @param round Playoff round name
#' @param home_team Home team abbreviation
#' @param away_team Away team abbreviation
#' @param home_had_bye Did home team have first-round bye?
#' @param away_had_bye Did away team have first-round bye?
#' @return List of adjustments
calculate_playoff_adjustments <- function(round, home_team, away_team,
                                         home_had_bye = FALSE, away_had_bye = FALSE) {
  adjustments <- list(
    round = round,
    hfa_multiplier = 1.0,
    home_rest_bonus = 0,
    away_rest_bonus = 0,
    shrinkage = 0.60,  # Default regular season
    injury_variance = 1.0
  )

  if (is.na(round) || !round %in% names(PLAYOFF_ROUNDS)) {
    return(adjustments)
  }

  # Home field advantage multiplier
  adjustments$hfa_multiplier <- PLAYOFF_FEATURES$hfa_multiplier[[round]]

  # Bye week rest bonus (divisional round)
  if (round == "divisional") {
    if (home_had_bye) {
      adjustments$home_rest_bonus <- PLAYOFF_FEATURES$bye_rest_bonus$points
    }
    if (away_had_bye) {
      adjustments$away_rest_bonus <- PLAYOFF_FEATURES$bye_rest_bonus$points
    }
  }

  # Market shrinkage
  adjustments$shrinkage <- PLAYOFF_FEATURES$shrinkage[[round]]

  # Injury variance
  adjustments$injury_variance <- PLAYOFF_FEATURES$injury_variance[[round]]

  adjustments
}

#' Get playoff shrinkage for a round
#' @param round Playoff round name
#' @return Shrinkage factor (0-1)
get_playoff_shrinkage <- function(round) {
  if (is.na(round) || !round %in% names(PLAYOFF_FEATURES$shrinkage)) {
    return(0.60)  # Default
  }
  PLAYOFF_FEATURES$shrinkage[[round]]
}

#' Get HFA multiplier for a round
#' @param round Playoff round name
#' @return HFA multiplier
get_playoff_hfa_multiplier <- function(round) {
  if (is.na(round) || !round %in% names(PLAYOFF_FEATURES$hfa_multiplier)) {
    return(1.0)  # Default
  }
  PLAYOFF_FEATURES$hfa_multiplier[[round]]
}

# =============================================================================
# SCHEDULE FILTERING FUNCTIONS
# =============================================================================
#' Filter schedule to playoff games only
#' @param schedule Full schedule data frame
#' @return Schedule with only playoff games
filter_playoff_schedule <- function(schedule) {
  if (is.null(schedule) || nrow(schedule) == 0) {
    return(schedule)
  }

  # Filter by game_type if available, otherwise by week
  if ("game_type" %in% names(schedule)) {
    schedule %>%
      dplyr::filter(.data$game_type %in% c("WC", "DIV", "CON", "SB"))
  } else if ("week" %in% names(schedule)) {
    schedule %>%
      dplyr::filter(.data$week >= 19 & .data$week <= 22)
  } else {
    schedule  # Return unchanged if no filter columns
  }
}

#' Get playoff slate for a specific round
#' @param schedule Full schedule data frame
#' @param round Playoff round name
#' @return Schedule filtered to specific round
get_playoff_slate <- function(schedule, round) {
  if (is.null(schedule) || nrow(schedule) == 0) {
    return(schedule)
  }

  round_config <- PLAYOFF_ROUNDS[[round]]
  if (is.null(round_config)) {
    warning(sprintf("Unknown playoff round: %s", round))
    return(schedule[0, ])
  }

  # Filter by week
  schedule %>%
    dplyr::filter(.data$week == round_config$week)
}

#' Identify teams with first-round bye
#' @param standings Standings data frame with team, wins, seed columns
#' @return Character vector of team abbreviations with byes
identify_bye_teams <- function(standings) {
  if (is.null(standings) || nrow(standings) == 0) {
    return(character(0))
  }

  # Top seed in each conference gets bye
  bye_teams <- standings %>%
    dplyr::filter(.data$seed == 1) %>%
    dplyr::pull(.data$team)

  bye_teams
}

# =============================================================================
# REPORT GENERATION FUNCTIONS
# =============================================================================

#' Generate playoff report header HTML
#' @param round Playoff round name
#' @param season Season year
#' @return HTML string
generate_playoff_report_header <- function(round, season) {
  round_config <- PLAYOFF_ROUNDS[[round]]

  if (is.null(round_config)) {
    return(sprintf("<h1>%d NFL Playoffs</h1>", season))
  }

  html <- sprintf('
<header class="playoff-header">
  <h1>%d NFL %s</h1>
  <p class="round-description">%s</p>
  <p class="round-info">Week %d | %d Game%s</p>
</header>
',
    season,
    round_config$name,
    round_config$description,
    round_config$week,
    round_config$expected_games,
    if (round_config$expected_games == 1) "" else "s"
  )

  html
}

#' Generate round sanity check HTML
#' @param validation Validation result from validate_playoff_round_games
#' @return HTML string
generate_round_sanity_html <- function(validation) {
  status_class <- if (validation$valid) "sanity-pass" else "sanity-fail"
  status_icon <- if (validation$valid) "&#10003;" else "&#10007;"

  sprintf('
<div class="sanity-check %s">
  <span class="sanity-icon">%s</span>
  <span class="sanity-message">%s</span>
</div>
',
    status_class,
    status_icon,
    validation$message
  )
}

#' Generate playoff adjustments summary HTML
#' @param adjustments Adjustments from calculate_playoff_adjustments
#' @return HTML string
generate_adjustments_html <- function(adjustments) {
  if (is.na(adjustments$round)) {
    return("")
  }

  round_config <- PLAYOFF_ROUNDS[[adjustments$round]]
  round_name <- if (!is.null(round_config)) round_config$name else adjustments$round

  items <- c()

  if (adjustments$hfa_multiplier != 1.0) {
    items <- c(items, sprintf("Home Field: %.0f%% boost", (adjustments$hfa_multiplier - 1) * 100))
  }

  if (adjustments$home_rest_bonus > 0 || adjustments$away_rest_bonus > 0) {
    if (adjustments$home_rest_bonus > 0) {
      items <- c(items, sprintf("Home Bye Bonus: +%.1f pts", adjustments$home_rest_bonus))
    }
    if (adjustments$away_rest_bonus > 0) {
      items <- c(items, sprintf("Away Bye Bonus: +%.1f pts", adjustments$away_rest_bonus))
    }
  }

  items <- c(items, sprintf("Market Shrinkage: %.0f%%", adjustments$shrinkage * 100))

  if (adjustments$injury_variance != 1.0) {
    items <- c(items, sprintf("Injury Impact: %.0f%% boost", (adjustments$injury_variance - 1) * 100))
  }

  sprintf('
<div class="playoff-adjustments">
  <h4>%s Adjustments</h4>
  <ul>
    %s
  </ul>
</div>
',
    round_name,
    paste(sprintf("<li>%s</li>", items), collapse = "\n    ")
  )
}

# =============================================================================
# HISTORICAL PLAYOFF DATA FUNCTIONS
# =============================================================================

#' Load historical playoff results for a season
#' @param season Season year
#' @return Data frame with playoff results or NULL
load_playoff_results <- function(season) {
  tryCatch({
    if (!requireNamespace("nflreadr", quietly = TRUE)) {
      warning("nflreadr not available for playoff data")
      return(NULL)
    }

    # Load schedule which includes playoff games
    schedule <- nflreadr::load_schedules(seasons = season)

    # Filter to completed playoff games
    playoffs <- schedule %>%
      dplyr::filter(.data$game_type %in% c("WC", "DIV", "CON", "SB")) %>%
      dplyr::filter(!is.na(.data$home_score) & !is.na(.data$away_score))

    if (nrow(playoffs) == 0) {
      return(NULL)
    }

    playoffs
  }, error = function(e) {
    warning(sprintf("Failed to load playoff data for %d: %s", season, e$message))
    NULL
  })
}

#' Summarize historical playoff performance
#' @param playoff_results Data frame from load_playoff_results
#' @return Summary statistics
summarize_playoff_results <- function(playoff_results) {
  if (is.null(playoff_results) || nrow(playoff_results) == 0) {
    return(NULL)
  }

  playoff_results %>%
    dplyr::mutate(
      round = derive_playoff_round_from_week(.data$week),
      home_won = .data$home_score > .data$away_score,
      total_points = .data$home_score + .data$away_score,
      point_diff = abs(.data$home_score - .data$away_score)
    ) %>%
    dplyr::group_by(.data$round) %>%
    dplyr::summarise(
      games = dplyr::n(),
      home_win_pct = mean(.data$home_won, na.rm = TRUE),
      avg_total = mean(.data$total_points, na.rm = TRUE),
      avg_margin = mean(.data$point_diff, na.rm = TRUE),
      .groups = "drop"
    )
}
