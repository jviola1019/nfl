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
#' Returns NULL if API key not set (caller falls back to model-derived odds).
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
    message("INFO: ODDS_API_KEY not set - skipping odds API")
    message("  Set ODDS_API_KEY for real market data or enable PROP_ALLOW_MODEL_ODDS for synthetic odds")
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

# =============================================================================
# SCORESANDODDS MARKET-COMPARISON SCRAPER
# =============================================================================

scoresandodds_market_map <- c(
  passing_yards = "passing yards",
  rushing_yards = "rushing yards",
  receiving_yards = "receiving yards",
  receptions = "receptions",
  anytime_td = "touchdowns"
)

scoresandodds_user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0 Safari/537.36"

scoresandodds_endpoint_map <- c(
  passing_yards = "https://www.scoresandodds.com/nfl/props/passing-yards",
  rushing_yards = "https://www.scoresandodds.com/nfl/props/rushing-yards",
  receiving_yards = "https://www.scoresandodds.com/nfl/props/receiving-yards",
  receptions = "https://www.scoresandodds.com/nfl/props/receptions",
  anytime_td = "https://www.scoresandodds.com/nfl/props/touchdowns"
)

extract_scoresandodds_events <- function(html_text) {
  if (!requireNamespace("xml2", quietly = TRUE)) return(character(0))
  doc <- tryCatch(xml2::read_html(html_text), error = function(e) NULL)
  if (is.null(doc)) return(character(0))
  nodes <- xml2::xml_find_all(doc, "//*[@data-event]")
  events <- unique(xml2::xml_attr(nodes, "data-event"))
  events <- events[!is.na(events) & nzchar(events)]
  events
}

fetch_scoresandodds_market <- function(event_id, market) {
  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) return(NULL)

  base_url <- "https://rga51lus77.execute-api.us-east-1.amazonaws.com/prod/market-comparison"
  url <- sprintf("%s?event=%s&market=%s",
                 base_url,
                 utils::URLencode(event_id, reserved = TRUE),
                 utils::URLencode(market, reserved = TRUE))

  ua <- scoresandodds_user_agent %||% "Mozilla/5.0"
  resp <- tryCatch({
    httr::GET(url, httr::timeout(15), httr::user_agent(ua))
  }, error = function(e) NULL)

  if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)

  content <- tryCatch({
    jsonlite::fromJSON(
      httr::content(resp, "text", encoding = "UTF-8"),
      simplifyVector = FALSE
    )
  }, error = function(e) NULL)

  content
}

parse_scoresandodds_market_json <- function(payload, prop_type) {
  if (is.null(payload) || !is.list(payload) || is.null(payload$markets)) {
    return(tibble::tibble())
  }

  home_team <- payload$event$home$key %||% NA_character_
  away_team <- payload$event$away$key %||% NA_character_
  event_id <- if (!is.null(payload$event) && !is.null(payload$event$sport)) {
    sprintf("%s/%s", payload$event$sport, payload$event$home$id %||% payload$event$away$id %||% "")
  } else {
    NA_character_
  }

  rows <- lapply(payload$markets, function(mkt) {
    if (is.null(mkt) || !is.list(mkt)) return(NULL)
    if (is.null(mkt$comparison)) return(NULL)
    player_name <- NA_character_
    if (!is.null(mkt$player$first_name)) {
      player_name <- paste(mkt$player$first_name, mkt$player$last_name)
    } else if (!is.null(mkt$player$last_name)) {
      player_name <- mkt$player$last_name
    }

    team_key <- mkt$player$team$key %||% NA_character_

    book_rows <- lapply(names(mkt$comparison), function(book_key) {
      bk <- mkt$comparison[[book_key]]
      if (is.null(bk)) return(NULL)
      available <- bk$available
      if (isFALSE(available)) return(NULL)
      tibble::tibble(
        player = player_name,
        prop_type = prop_type,
        line = suppressWarnings(as.numeric(bk$value)),
        line_over = suppressWarnings(as.numeric(bk$value)),
        line_under = suppressWarnings(as.numeric(bk$value)),
        over_odds = suppressWarnings(as.numeric(bk$over)),
        under_odds = suppressWarnings(as.numeric(bk$under)),
        book = book_key,
        event_id = payload$event$id %||% NA_character_,
        home_team = home_team,
        away_team = away_team,
        team = team_key,
        source = "scoresandodds"
      )
    })
    dplyr::bind_rows(book_rows)
  })

  dplyr::bind_rows(rows)
}

#' Load prop odds from ScoresAndOdds market-comparison API
#'
#' @param prop_types Vector of prop types to fetch
#' @param allow_remote Logical to allow remote scraping
#' @param sleep_sec Delay between requests
#' @return Tibble of prop odds or NULL
load_prop_odds_scoresandodds <- function(prop_types = NULL,
                                         allow_remote = TRUE,
                                         sleep_sec = NULL) {

  if (!isTRUE(allow_remote)) {
    message("INFO: ScoresAndOdds scraping disabled (PROP_ODDS_ALLOW_REMOTE_HTML=FALSE)")
    return(NULL)
  }

  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE) ||
      !requireNamespace("xml2", quietly = TRUE)) {
    warning("httr/jsonlite/xml2 packages required for ScoresAndOdds scraping")
    return(NULL)
  }

  if (is.null(prop_types)) prop_types <- names(scoresandodds_market_map)
  prop_types <- intersect(prop_types, names(scoresandodds_market_map))
  if (!length(prop_types)) return(NULL)

  if (is.null(sleep_sec) && exists("PROP_ODDS_SCRAPE_DELAY_SEC")) {
    sleep_sec <- PROP_ODDS_SCRAPE_DELAY_SEC
  }
  if (is.null(sleep_sec) || !is.finite(sleep_sec)) sleep_sec <- 0.4

  all_rows <- list()

  for (prop_type in prop_types) {
    endpoint <- scoresandodds_endpoint_map[[prop_type]]
    market <- scoresandodds_market_map[[prop_type]]
    if (is.null(endpoint) || is.null(market)) next

    html <- tryCatch({
      resp <- httr::GET(endpoint, httr::timeout(15), httr::user_agent(scoresandodds_user_agent))
      if (httr::status_code(resp) != 200) return(NULL)
      httr::content(resp, "text", encoding = "UTF-8")
    }, error = function(e) NULL)

    if (is.null(html)) next
    event_ids <- extract_scoresandodds_events(html)
    if (!length(event_ids)) next

    for (event_id in event_ids) {
      payload <- fetch_scoresandodds_market(event_id, market)
      parsed <- parse_scoresandodds_market_json(payload, prop_type)
      if (!is.null(parsed) && nrow(parsed) > 0) {
        all_rows[[length(all_rows) + 1]] <- parsed
      }
      if (sleep_sec > 0) Sys.sleep(sleep_sec)
    }
  }

  if (!length(all_rows)) return(NULL)
  dplyr::bind_rows(all_rows)
}

#' Get consensus prop line for a player (median across books)
#'
#' @param player_name Player name to search for
#' @param prop_type Prop type ("passing_yards", "rushing_yards", etc.)
#' @param prop_odds_data Data from load_prop_odds()
#' @param fuzzy_match Use fuzzy matching for player names (default TRUE)
#'
#' @param preferred_books Optional vector of book keys to prioritize (e.g., c("draftkings","fanduel"))
#'
#' @return List with line, line_over, line_under, over_odds, under_odds, book, books; NULL if not found
#'
#' @export
get_market_prop_line <- function(player_name, prop_type, prop_odds_data,
                                  fuzzy_match = TRUE,
                                  home_team = NULL,
                                  away_team = NULL,
                                  event_id = NULL,
                                  preferred_books = NULL) {

  if (is.null(prop_odds_data) || nrow(prop_odds_data) == 0) {
    return(NULL)
  }

  player_name <- as.character(player_name %||% "")
  if (!nzchar(player_name) || !grepl("[A-Za-z]", player_name)) {
    return(NULL)
  }

  # Filter by prop type
  matches <- prop_odds_data %>%
    dplyr::filter(prop_type == !!prop_type)

  if (nrow(matches) == 0) return(NULL)

  # Optional: Filter by event or teams when available
  if (!is.null(event_id) && "event_id" %in% names(matches)) {
    matches <- matches %>% dplyr::filter(event_id == !!event_id)
  }

  if (!is.null(home_team) && !is.null(away_team) &&
      all(c("home_team", "away_team") %in% names(matches))) {
    normalize_team <- function(x) {
      tolower(gsub("[^a-z]", "", x %||% ""))
    }
    map_team <- function(abbr) {
      abbr <- toupper(abbr %||% "")
      mapped <- TEAM_ABBR_TO_NAME[abbr]
      mapped[is.na(mapped)] <- abbr[is.na(mapped)]
      as.character(mapped)
    }
    target_home <- normalize_team(map_team(home_team))
    target_away <- normalize_team(map_team(away_team))

    matches <- matches %>%
      dplyr::mutate(
        home_norm = normalize_team(map_team(.data$home_team)),
        away_norm = normalize_team(map_team(.data$away_team))
      ) %>%
      dplyr::filter(
        (home_norm == target_home & away_norm == target_away) |
        (home_norm == target_away & away_norm == target_home)
      ) %>%
      dplyr::select(-home_norm, -away_norm)
  }

  # Match player name
  if (fuzzy_match) {
    # Normalize names for matching
    normalize_name <- function(x) {
      x %>%
        tolower() %>%
        stringr::str_replace_all("[^a-z ]", "") %>%
        stringr::str_squish()
    }
    normalize_name_spaced <- function(x) {
      x %>%
        tolower() %>%
        stringr::str_replace_all("[^a-z ]", " ") %>%
        stringr::str_squish()
    }

    target_norm <- normalize_name(player_name)
    if (is.na(target_norm) || !nzchar(target_norm)) return(NULL)
    target_spaced <- normalize_name_spaced(player_name)
    target_parts <- strsplit(target_spaced, " ")[[1]]
    target_initial <- if (length(target_parts) >= 1) substr(target_parts[1], 1, 1) else ""
    target_last <- if (length(target_parts) >= 2) target_parts[length(target_parts)] else ""

    matches <- matches %>%
      dplyr::mutate(
        name_norm = normalize_name(player),
        name_spaced = normalize_name_spaced(player),
        last_name = stringr::word(name_spaced, -1),
        first_initial = substr(stringr::word(name_spaced, 1), 1, 1)
      ) %>%
      dplyr::filter(
        name_norm == target_norm |
        stringr::str_detect(name_norm, stringr::fixed(target_norm)) |
        stringr::str_detect(target_norm, name_norm) |
        (nzchar(target_last) & last_name == target_last & first_initial == target_initial)
      )
  } else {
    matches <- matches %>%
      dplyr::filter(tolower(player) == tolower(player_name))
  }

  if (nrow(matches) == 0) return(NULL)

  # Apply preferred book filter if available
  if (!is.null(preferred_books)) {
    preferred_books <- tolower(preferred_books)
    matches$book <- tolower(matches$book)
    preferred_matches <- matches %>% dplyr::filter(book %in% preferred_books)
    if (nrow(preferred_matches) > 0) {
      matches <- preferred_matches
    }
  }

  # Choose best book row (min vig for two-way, max price for one-way)
  is_one_way <- all(is.na(matches$under_odds) | !is.finite(matches$under_odds))

  score_row <- function(df) {
    if (nrow(df) == 1) return(df)
    if (is_one_way) {
      dec <- american_to_decimal_odds(df$over_odds)
      df[which.max(dec), , drop = FALSE]
    } else {
      over_p <- american_odds_to_implied_prob(df$over_odds)
      under_p <- american_odds_to_implied_prob(df$under_odds)
      vig <- over_p + under_p
      df[which.min(vig), , drop = FALSE]
    }
  }

  best_row <- score_row(matches)

  as_scalar_num <- function(x) {
    if (is.null(x)) return(NA_real_)
    if (is.list(x)) x <- unlist(x, recursive = TRUE, use.names = FALSE)
    x <- suppressWarnings(as.numeric(x))
    if (!length(x)) return(NA_real_)
    x[1]
  }

  line_over <- if ("line_over" %in% names(best_row)) best_row$line_over else best_row$line
  line_under <- if ("line_under" %in% names(best_row)) best_row$line_under else best_row$line
  line_over <- as_scalar_num(line_over)
  line_under <- as_scalar_num(line_under)

  # Use average line when over/under lines are very close
  line_value <- if (is.finite(line_over) && is.finite(line_under) &&
                    abs(line_over - line_under) <= 0.25) {
    mean(c(line_over, line_under), na.rm = TRUE)
  } else if (is.finite(line_over)) {
    line_over
  } else {
    line_under
  }

  list(
    player = best_row$player[1],
    prop_type = prop_type,
    line = as_scalar_num(line_value),
    line_over = line_over,
    line_under = line_under,
    over_odds = as_scalar_num(best_row$over_odds),
    under_odds = as_scalar_num(best_row$under_odds),
    book = best_row$book[1],
    books = unique(matches$book),
    n_books = length(unique(matches$book))
  )
}

# =============================================================================
# PROP ODDS FALLBACK UTILITIES
# =============================================================================

# Team abbreviation to full name map (for odds providers using full names)
TEAM_ABBR_TO_NAME <- c(
  "ARI" = "Arizona Cardinals",
  "ATL" = "Atlanta Falcons",
  "BAL" = "Baltimore Ravens",
  "BUF" = "Buffalo Bills",
  "CAR" = "Carolina Panthers",
  "CHI" = "Chicago Bears",
  "CIN" = "Cincinnati Bengals",
  "CLE" = "Cleveland Browns",
  "DAL" = "Dallas Cowboys",
  "DEN" = "Denver Broncos",
  "DET" = "Detroit Lions",
  "GB"  = "Green Bay Packers",
  "HOU" = "Houston Texans",
  "IND" = "Indianapolis Colts",
  "JAX" = "Jacksonville Jaguars",
  "KC"  = "Kansas City Chiefs",
  "LAC" = "Los Angeles Chargers",
  "LAR" = "Los Angeles Rams",
  "LV"  = "Las Vegas Raiders",
  "MIA" = "Miami Dolphins",
  "MIN" = "Minnesota Vikings",
  "NE"  = "New England Patriots",
  "NO"  = "New Orleans Saints",
  "NYG" = "New York Giants",
  "NYJ" = "New York Jets",
  "PHI" = "Philadelphia Eagles",
  "PIT" = "Pittsburgh Steelers",
  "SEA" = "Seattle Seahawks",
  "SF"  = "San Francisco 49ers",
  "TB"  = "Tampa Bay Buccaneers",
  "TEN" = "Tennessee Titans",
  "WAS" = "Washington Commanders"
)

probability_to_american_safe <- function(p) {
  if (exists("probability_to_american", mode = "function")) {
    return(probability_to_american(p))
  }
  p <- suppressWarnings(as.numeric(p))
  if (!is.finite(p) || p <= 0 || p >= 1) return(NA_real_)
  if (p == 0.5) return(100)
  if (p > 0.5) {
    return(-as.numeric(round(100 * p / (1 - p))))
  }
  as.numeric(round(100 * (1 - p) / p))
}

round_to_half <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!is.finite(x)) return(NA_real_)
  round(x * 2) / 2
}

apply_two_way_vig <- function(p_over, p_under, vig = 0.045) {
  p_over <- suppressWarnings(as.numeric(p_over))
  p_under <- suppressWarnings(as.numeric(p_under))
  if (!is.finite(p_over) || !is.finite(p_under) || p_over <= 0 || p_under <= 0) {
    return(list(p_over = NA_real_, p_under = NA_real_))
  }
  scale <- (1 + vig) / (p_over + p_under)
  list(
    p_over = pmin(p_over * scale, 0.99),
    p_under = pmin(p_under * scale, 0.99)
  )
}

derive_two_way_odds_from_probs <- function(p_over, p_under, vig = 0.045) {
  vigged <- apply_two_way_vig(p_over, p_under, vig = vig)
  list(
    over_odds = probability_to_american_safe(vigged$p_over),
    under_odds = probability_to_american_safe(vigged$p_under)
  )
}

derive_one_way_odds_from_prob <- function(p, vig = 0.045) {
  p <- suppressWarnings(as.numeric(p))
  if (!is.finite(p) || p <= 0 || p >= 1) return(NA_real_)
  p_vig <- pmin(p * (1 + vig), 0.99)
  probability_to_american_safe(p_vig)
}

derive_prop_market_from_sim <- function(sim_values,
                                        line_quantile = NULL,
                                        vig = NULL) {
  sim_values <- suppressWarnings(as.numeric(sim_values))
  sim_values <- sim_values[is.finite(sim_values)]
  if (!length(sim_values)) {
    return(list(line = NA_real_, over_odds = NA_real_, under_odds = NA_real_))
  }

  if (is.null(line_quantile) && exists("PROP_FALLBACK_LINE_QUANTILE")) {
    line_quantile <- PROP_FALLBACK_LINE_QUANTILE
  }
  if (is.null(line_quantile) || !is.finite(line_quantile)) {
    line_quantile <- 0.50
  }

  if (is.null(vig) && exists("PROP_MARKET_VIG")) {
    vig <- PROP_MARKET_VIG
  }
  if (is.null(vig) || !is.finite(vig)) {
    vig <- 0.045
  }

  line_raw <- stats::quantile(sim_values, probs = line_quantile, names = FALSE, na.rm = TRUE)
  line <- round_to_half(line_raw)

  p_over <- mean(sim_values > line)
  p_under <- mean(sim_values < line)
  p_push <- pmax(1 - p_over - p_under, 0)

  vigged <- apply_two_way_vig(p_over, p_under, vig = vig)
  over_odds <- probability_to_american_safe(vigged$p_over)
  under_odds <- probability_to_american_safe(vigged$p_under)

  list(
    line = line,
    over_odds = over_odds,
    under_odds = under_odds,
    p_over = p_over,
    p_under = p_under,
    p_push = p_push
  )
}

load_prop_odds_csv <- function(path = NULL) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  if (!requireNamespace("readr", quietly = TRUE)) {
    warning("readr package required to load prop odds CSV")
    return(NULL)
  }
  raw <- tryCatch(readr::read_csv(path, show_col_types = FALSE), error = function(e) NULL)
  if (is.null(raw) || nrow(raw) == 0) return(NULL)

  required_cols <- c("player", "prop_type", "over_odds")
  if (!all(required_cols %in% names(raw))) {
    warning("Prop odds CSV missing required columns: player, prop_type, over_odds")
    return(NULL)
  }
  if ("line" %in% names(raw)) raw$line <- suppressWarnings(as.numeric(raw$line))
  if ("line_over" %in% names(raw)) raw$line_over <- suppressWarnings(as.numeric(raw$line_over))
  if ("line_under" %in% names(raw)) raw$line_under <- suppressWarnings(as.numeric(raw$line_under))
  raw$over_odds <- suppressWarnings(as.numeric(raw$over_odds))
  if ("under_odds" %in% names(raw)) raw$under_odds <- suppressWarnings(as.numeric(raw$under_odds))
  raw
}

resolve_prop_odds_cache <- function(source = NULL,
                                    csv_path = NULL,
                                    api_key = Sys.getenv("ODDS_API_KEY")) {
  if (is.null(source) && exists("PROP_ODDS_SOURCE")) source <- PROP_ODDS_SOURCE
  if (is.null(csv_path) && exists("PROP_ODDS_CSV_PATH")) csv_path <- PROP_ODDS_CSV_PATH

  source <- tolower(source %||% "auto")
  cache <- NULL
  cache_source <- "model"

  allow_remote <- if (exists("PROP_ODDS_ALLOW_REMOTE_HTML")) isTRUE(PROP_ODDS_ALLOW_REMOTE_HTML) else FALSE

  if (source %in% c("auto", "scoresandodds", "sao")) {
    cache <- load_prop_odds_scoresandodds(allow_remote = allow_remote)
    if (!is.null(cache) && nrow(cache) > 0) {
      cache_source <- "scoresandodds"
    }
  }

  if ((is.null(cache) || nrow(cache) == 0) && source %in% c("auto", "odds_api")) {
    cache <- load_prop_odds(api_key = api_key)
    if (!is.null(cache) && nrow(cache) > 0) {
      cache_source <- "odds_api"
    }
  }

  if ((is.null(cache) || nrow(cache) == 0) && source %in% c("csv", "file", "local", "auto")) {
    cache <- load_prop_odds_csv(csv_path)
    if (!is.null(cache) && nrow(cache) > 0) {
      cache_source <- "csv"
    }
  }

  if (is.null(cache) || nrow(cache) == 0) {
    cache <- NULL
    cache_source <- "model"
  }

  if (!is.null(cache)) {
    attr(cache, "source") <- cache_source
  }
  cache
}

# NULL-coalescing operator
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
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
  allow_model_odds <- isTRUE(if (exists("PROP_ALLOW_MODEL_ODDS")) PROP_ALLOW_MODEL_ODDS else FALSE)

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
      over_odds = if (allow_model_odds) odds else NA_real_,     # "Yes" odds
      under_odds = NA_real_ # TD props don't have "No" lines typically
    ))
  }

  # Yard props: line at 95% of baseline (typical market placement)
  # -110 both sides is industry standard when synthetic odds are enabled
  list(
    line = round(baseline * 0.95, 0) + 0.5,
    over_odds = if (allow_model_odds) -110 else NA_real_,
    under_odds = if (allow_model_odds) -110 else NA_real_
  )
}

#' Format American odds for display
#'
#' @param odds Numeric American odds value
#' @return Character string with + prefix for positive odds
#'
#' @export
format_american_odds <- function(odds) {
  odds <- suppressWarnings(as.numeric(odds))
  out <- rep("N/A", length(odds))
  valid <- is.finite(odds)
  pos <- valid & odds >= 0
  neg <- valid & odds < 0
  out[pos] <- sprintf("+%d", round(odds[pos]))
  out[neg] <- sprintf("%d", round(odds[neg]))
  if (length(out) == 1) return(out[1])
  out
}

#' Calculate implied probability from American odds
#'
#' @param odds American odds (positive or negative)
#' @return Implied probability (0-1)
#'
#' @export
american_odds_to_implied_prob <- function(odds) {
  odds <- suppressWarnings(as.numeric(odds))
  out <- rep(NA_real_, length(odds))
  valid <- is.finite(odds)
  pos <- valid & odds >= 0
  neg <- valid & odds < 0
  out[pos] <- 100 / (odds[pos] + 100)
  out[neg] <- abs(odds[neg]) / (abs(odds[neg]) + 100)
  out
}

#' Calculate decimal odds from American odds
#'
#' @param odds American odds
#' @return Decimal odds
#'
#' @export
american_to_decimal_odds <- function(odds) {
  odds <- suppressWarnings(as.numeric(odds))
  out <- rep(NA_real_, length(odds))
  valid <- is.finite(odds)
  pos <- valid & odds >= 0
  neg <- valid & odds < 0
  out[pos] <- 1 + odds[pos] / 100
  out[neg] <- 1 + 100 / abs(odds[neg])
  out
}
