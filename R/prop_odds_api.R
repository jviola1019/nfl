# =============================================================================
# PROP ODDS API INTEGRATION
# =============================================================================
# File: R/prop_odds_api.R
# Purpose: Load real player prop odds from The Odds API or fallback to defaults
# Version: 2.9.2
#
# API Documentation: https://the-odds-api.com/liveapi/guides/v4/
# Free tier: 500 requests/month
# =============================================================================

#' Load player prop odds from The Odds API
#'
#' Fetches real-time prop betting lines from multiple sportsbooks.
#' Returns NULL if API key not set (graceful fallback to defaults).
#'
#' @param sport Sport key (default: "americanfootball_nfl")
#' @param event_ids Optional vector of event IDs to filter
#' @param markets Markets to fetch (default: player props)
#' @param api_key API key (from ODDS_API_KEY env var)
#' @param bookmakers Bookmakers to include (default: major US books)
#'
#' @return tibble with columns: player, prop_type, line, over_odds, under_odds,
#'         book, event_id, commence_time. Returns NULL if API unavailable.
#'
#' @examples
#' \dontrun{
#'   # Set API key first
#'   Sys.setenv(ODDS_API_KEY = "your_api_key_here")
#'
#'   # Load all NFL prop odds
#'   props <- load_prop_odds()
#'
#'   # Get specific player's line
#'   mahomes_pass <- get_market_prop_line("Patrick Mahomes", "passing_yards", props)
#' }
#'
#' @export
load_prop_odds <- function(
    sport = "americanfootball_nfl",
    event_ids = NULL,
    markets = c("player_pass_yds", "player_rush_yds",
                "player_reception_yds", "player_anytime_td"),
    api_key = Sys.getenv("ODDS_API_KEY"),
    bookmakers = c("draftkings", "fanduel", "betmgm", "caesars", "bovada")
) {

  # Check for API key

if (is.null(api_key) || api_key == "") {
    message("ℹ ODDS_API_KEY not set - using position-based default prop odds")
    message("  Set ODDS_API_KEY environment variable for real market data")
    return(NULL)
  }

  # Check for httr package
  if (!requireNamespace("httr", quietly = TRUE)) {
    warning("httr package required for API calls - using defaults")
    return(NULL)
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    warning("jsonlite package required for API calls - using defaults")
    return(NULL)
  }

  base_url <- "https://api.the-odds-api.com/v4/sports"

  # Map internal prop types to API market keys
  market_map <- c(
    "passing_yards" = "player_pass_yds",
    "rushing_yards" = "player_rush_yds",
    "receiving_yards" = "player_reception_yds",
    "anytime_td" = "player_anytime_td"
  )

  all_props <- tibble::tibble()

  for (market in markets) {
    # Build URL
    url <- sprintf(
      "%s/%s/odds/?apiKey=%s&regions=us&markets=%s&oddsFormat=american&bookmakers=%s",
      base_url, sport, api_key, market,
      paste(bookmakers, collapse = ",")
    )

    # Make API request
    resp <- tryCatch({
      httr::GET(url, httr::timeout(10))
    }, error = function(e) {
      message(sprintf("  API request failed for %s: %s", market, e$message))
      return(NULL)
    })

    if (is.null(resp)) next

    # Check response status
    if (httr::status_code(resp) != 200) {
      status <- httr::status_code(resp)
      if (status == 401) {
        message("  API key invalid or expired")
      } else if (status == 429) {
        message("  API rate limit exceeded")
      } else {
        message(sprintf("  API returned status %d for %s", status, market))
      }
      next
    }

    # Parse response
    content <- tryCatch({
      jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
    }, error = function(e) {
      message(sprintf("  Failed to parse response for %s: %s", market, e$message))
      return(NULL)
    })

    if (is.null(content) || length(content) == 0) next

    # Extract prop lines from each event
    for (event in content) {
      if (is.null(event$bookmakers)) next

      event_id <- event$id
      commence_time <- event$commence_time
      home_team <- event$home_team
      away_team <- event$away_team

      for (book in event$bookmakers) {
        book_name <- book$key

        if (is.null(book$markets)) next

        for (mkt in book$markets) {
          if (is.null(mkt$outcomes)) next

          # Parse outcomes (Over/Under or Yes/No for TD)
          outcomes <- mkt$outcomes

          if (market == "player_anytime_td") {
            # Anytime TD is Yes/No format
            for (outcome in outcomes) {
              if (!is.null(outcome$description)) {
                player_name <- outcome$description
                yes_odds <- if (outcome$name == "Yes") outcome$price else NA_real_
                no_odds <- if (outcome$name == "No") outcome$price else NA_real_

                if (!is.na(yes_odds)) {
                  prop_row <- tibble::tibble(
                    player = player_name,
                    prop_type = "anytime_td",
                    line = NA_real_,
                    over_odds = yes_odds,
                    under_odds = no_odds,
                    book = book_name,
                    event_id = event_id,
                    home_team = home_team,
                    away_team = away_team,
                    commence_time = commence_time
                  )
                  all_props <- dplyr::bind_rows(all_props, prop_row)
                }
              }
            }
          } else {
            # Yard props are Over/Under format
            # Group by player description
            player_outcomes <- list()

            for (outcome in outcomes) {
              if (!is.null(outcome$description)) {
                player_name <- outcome$description
                if (is.null(player_outcomes[[player_name]])) {
                  player_outcomes[[player_name]] <- list(
                    line = outcome$point,
                    over_odds = NA_real_,
                    under_odds = NA_real_
                  )
                }

                if (outcome$name == "Over") {
                  player_outcomes[[player_name]]$over_odds <- outcome$price
                  player_outcomes[[player_name]]$line <- outcome$point
                } else if (outcome$name == "Under") {
                  player_outcomes[[player_name]]$under_odds <- outcome$price
                }
              }
            }

            # Convert internal prop type
            internal_type <- names(market_map)[market_map == market]
            if (length(internal_type) == 0) internal_type <- market

            for (player_name in names(player_outcomes)) {
              po <- player_outcomes[[player_name]]
              prop_row <- tibble::tibble(
                player = player_name,
                prop_type = internal_type,
                line = po$line,
                over_odds = po$over_odds,
                under_odds = po$under_odds,
                book = book_name,
                event_id = event_id,
                home_team = home_team,
                away_team = away_team,
                commence_time = commence_time
              )
              all_props <- dplyr::bind_rows(all_props, prop_row)
            }
          }
        }
      }
    }
  }

  if (nrow(all_props) > 0) {
    message(sprintf("✓ Loaded %d prop lines from %d bookmakers",
                    nrow(all_props), length(unique(all_props$book))))
  } else {
    message("ℹ No prop lines found from API")
  }

  all_props
}

#' Get consensus prop line for a player (median across books)
#'
#' @param player_name Player name to search for
#' @param prop_type Prop type ("passing_yards", "rushing_yards", etc.)
#' @param prop_odds_data Data from load_prop_odds()
#' @param fuzzy_match Use fuzzy matching for player names (default TRUE)
#'
#' @return List with line, over_odds, under_odds, books; NULL if not found
#'
#' @export
get_market_prop_line <- function(player_name, prop_type, prop_odds_data,
                                  fuzzy_match = TRUE) {

  if (is.null(prop_odds_data) || nrow(prop_odds_data) == 0) {
    return(NULL)
  }

  # Filter by prop type
  matches <- prop_odds_data %>%
    dplyr::filter(prop_type == !!prop_type)

  if (nrow(matches) == 0) return(NULL)

  # Match player name
  if (fuzzy_match) {
    # Normalize names for matching
    normalize_name <- function(x) {
      x %>%
        tolower() %>%
        stringr::str_replace_all("[^a-z ]", "") %>%
        stringr::str_squish()
    }

    target_norm <- normalize_name(player_name)
    matches <- matches %>%
      dplyr::mutate(name_norm = normalize_name(player)) %>%
      dplyr::filter(
        name_norm == target_norm |
        stringr::str_detect(name_norm, stringr::fixed(target_norm)) |
        stringr::str_detect(target_norm, name_norm)
      )
  } else {
    matches <- matches %>%
      dplyr::filter(tolower(player) == tolower(player_name))
  }

  if (nrow(matches) == 0) return(NULL)

  # Calculate consensus (median)
  list(
    player = matches$player[1],
    prop_type = prop_type,
    line = stats::median(matches$line, na.rm = TRUE),
    over_odds = round(stats::median(matches$over_odds, na.rm = TRUE)),
    under_odds = round(stats::median(matches$under_odds, na.rm = TRUE)),
    books = unique(matches$book),
    n_books = length(unique(matches$book))
  )
}

#' Get default prop odds when API unavailable
#'
#' Returns position-based realistic odds for props when no market data exists.
#'
#' @param prop_type Type of prop ("passing_yards", "rushing_yards", etc.)
#' @param position Player position ("QB", "RB", "WR", "TE")
#' @param baseline Player's projected baseline value
#'
#' @return List with line, over_odds, under_odds
#'
#' @export
get_default_prop_odds <- function(prop_type, position, baseline) {

  if (prop_type == "anytime_td") {
    # Position-based TD odds (realistic market ranges)
    odds <- switch(position,
      QB = 350,    # QBs score in ~15% of games → +350
      RB = -110,   # RB1s score in ~45% of games → -110
      WR = 140,    # WR1s score in ~35% of games → +140
      TE = 200,    # TE1s score in ~25% of games → +200
      200          # Default
    )

    return(list(
      line = NA_real_,
      over_odds = odds,     # "Yes" odds
      under_odds = NA_real_ # TD props don't have "No" lines typically
    ))
  }

  # Yard props: line at 95% of baseline (typical market placement)
  # -110 both sides is industry standard
  list(
    line = round(baseline * 0.95, 0) + 0.5,
    over_odds = -110,
    under_odds = -110
  )
}

#' Format American odds for display
#'
#' @param odds Numeric American odds value
#' @return Character string with + prefix for positive odds
#'
#' @export
format_american_odds <- function(odds) {
  if (is.na(odds)) return("N/A")
  if (odds >= 0) {
    sprintf("+%d", round(odds))
  } else {
    sprintf("%d", round(odds))
  }
}

#' Calculate implied probability from American odds
#'
#' @param odds American odds (positive or negative)
#' @return Implied probability (0-1)
#'
#' @export
american_odds_to_implied_prob <- function(odds) {
  if (is.na(odds)) return(NA_real_)
  if (odds >= 0) {
    100 / (odds + 100)
  } else {
    abs(odds) / (abs(odds) + 100)
  }
}

#' Calculate decimal odds from American odds
#'
#' @param odds American odds
#' @return Decimal odds
#'
#' @export
american_to_decimal_odds <- function(odds) {
  if (is.na(odds)) return(NA_real_)
  if (odds >= 0) {
    1 + odds / 100
  } else {
    1 + 100 / abs(odds)
  }
}
