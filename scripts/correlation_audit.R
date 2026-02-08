#!/usr/bin/env Rscript
# =============================================================================
# NFL Player Prop Correlation Audit
# =============================================================================
# Computes empirical correlations from nflreadr data to validate
# copula correlation settings used for player props.
#
# Outputs:
#  - Observed correlations and sample sizes
#  - Recommended config values (rounded)
#  - Deviation warnings vs current config
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(nflreadr)
})

if (file.exists("config.R")) {
  source("config.R")
}

current_season <- if (exists("SEASON")) as.integer(SEASON) else as.integer(format(Sys.Date(), "%Y"))
seasons <- (current_season - 2):current_season
seasons <- seasons[seasons >= 2019]

cat(sprintf("Correlation audit seasons: %s\n", paste(seasons, collapse = ", ")))

stats <- nflreadr::load_player_stats(seasons = seasons)
sched <- nflreadr::load_schedules(seasons = seasons) %>%
  dplyr::filter(game_type == "REG") %>%
  dplyr::select(season, week, home_team, away_team, home_score, away_score)

sched_long <- dplyr::bind_rows(
  sched %>% dplyr::transmute(
    season, week,
    team = home_team,
    opponent_team = away_team,
    team_score = home_score,
    opp_score = away_score,
    game_total = home_score + away_score
  ),
  sched %>% dplyr::transmute(
    season, week,
    team = away_team,
    opponent_team = home_team,
    team_score = away_score,
    opp_score = home_score,
    game_total = home_score + away_score
  )
)

stats <- stats %>%
  dplyr::filter(season %in% seasons, season_type == "REG") %>%
  dplyr::select(
    season, week, team, opponent_team, player_id, player_name, position,
    passing_yards, attempts,
    rushing_yards, carries,
    receiving_yards, receptions, targets,
    rushing_tds, receiving_tds
  ) %>%
  dplyr::left_join(sched_long, by = c("season", "week", "team", "opponent_team")) %>%
  dplyr::mutate(
    rush_att = dplyr::coalesce(carries, 0L),
    scoring_tds = dplyr::coalesce(rushing_tds, 0L) + dplyr::coalesce(receiving_tds, 0L)
  )

missing_game <- sum(is.na(stats$game_total))
if (missing_game > 0) {
  cat(sprintf("WARN: %d player rows missing game_total after schedule join\n", missing_game))
}

qb <- stats %>%
  dplyr::filter(position == "QB", attempts >= 10, !is.na(passing_yards), !is.na(game_total))
rb <- stats %>%
  dplyr::filter(position == "RB", rush_att >= 5, !is.na(rushing_yards), !is.na(game_total))

team_pass <- stats %>%
  dplyr::group_by(season, week, team) %>%
  dplyr::summarise(team_passing_yards = sum(passing_yards, na.rm = TRUE), .groups = "drop")

recv <- stats %>%
  dplyr::filter(position %in% c("WR", "TE", "RB"), targets >= 2, !is.na(receiving_yards)) %>%
  dplyr::left_join(team_pass, by = c("season", "week", "team"))

skill <- stats %>%
  dplyr::filter(position %in% c("RB", "WR", "TE"), !is.na(game_total))

cor_qb <- cor(qb$passing_yards, qb$game_total, use = "complete.obs")
cor_rb <- cor(rb$rushing_yards, rb$game_total, use = "complete.obs")
cor_recv <- cor(recv$receiving_yards, recv$team_passing_yards, use = "complete.obs")
cor_recs <- cor(recv$receptions, recv$team_passing_yards, use = "complete.obs")
cor_td <- cor(skill$scoring_tds, skill$game_total, use = "complete.obs")

cat(sprintf("QB passing vs game total: %.3f (n=%d)\n", cor_qb, nrow(qb)))
cat(sprintf("RB rushing vs game total: %.3f (n=%d)\n", cor_rb, nrow(rb)))
cat(sprintf("Receiving yards vs team passing yards: %.3f (n=%d)\n", cor_recv, nrow(recv)))
cat(sprintf("Receptions vs team passing yards: %.3f (n=%d)\n", cor_recs, nrow(recv)))
cat(sprintf("Scoring TDs vs game total: %.3f (n=%d)\n", cor_td, nrow(skill)))

recommended <- list(
  PROP_GAME_CORR_PASSING = round(cor_qb, 2),
  PROP_GAME_CORR_RUSHING = round(cor_rb, 2),
  PROP_GAME_CORR_RECEIVING = round(cor_recv, 2),
  PROP_GAME_CORR_TD = round(cor_td, 2)
)

cat("\nRecommended config values (rounded):\n")
for (nm in names(recommended)) {
  cat(sprintf("  %s = %.2f\n", nm, recommended[[nm]]))
}

if (exists("PROP_GAME_CORR_PASSING") &&
    exists("PROP_GAME_CORR_RUSHING") &&
    exists("PROP_GAME_CORR_RECEIVING") &&
    exists("PROP_GAME_CORR_TD")) {
  current <- c(
    PROP_GAME_CORR_PASSING = PROP_GAME_CORR_PASSING,
    PROP_GAME_CORR_RUSHING = PROP_GAME_CORR_RUSHING,
    PROP_GAME_CORR_RECEIVING = PROP_GAME_CORR_RECEIVING,
    PROP_GAME_CORR_TD = PROP_GAME_CORR_TD
  )
  cat("\nDeviation vs current config:\n")
  for (nm in names(current)) {
    diff <- recommended[[nm]] - current[[nm]]
    flag <- if (abs(diff) >= 0.10) "  <-- consider update" else ""
    cat(sprintf("  %s: current=%.2f | audit=%.2f | diff=%+.2f%s\n",
                nm, current[[nm]], recommended[[nm]], diff, flag))
  }
}
