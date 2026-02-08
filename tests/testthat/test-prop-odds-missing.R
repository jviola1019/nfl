library(testthat)

find_repo_root <- function(start_dir) {
  candidates <- c(
    start_dir,
    dirname(start_dir),
    dirname(dirname(start_dir)),
    dirname(dirname(dirname(start_dir)))
  )
  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "config.R"))) {
      return(normalizePath(candidate, winslash = "/", mustWork = FALSE))
    }
  }
  normalizePath(start_dir, winslash = "/", mustWork = FALSE)
}

start_dir <- getwd()
test_file <- sys.frame(1)$ofile
if (!is.null(test_file)) {
  start_dir <- dirname(test_file)
}
repo_root <- find_repo_root(start_dir)

props_config_path <- file.path(repo_root, "sports", "nfl", "props", "props_config.R")
if (file.exists(props_config_path)) {
  source(props_config_path, local = FALSE)
}

passing_path <- file.path(repo_root, "sports", "nfl", "props", "passing_yards.R")
if (file.exists(passing_path)) {
  source(passing_path, local = FALSE)
}

test_that("missing odds are not synthesized when disabled", {
  skip_if_not(exists("passing_yards_over_under"), "passing_yards_over_under not loaded")

  old_allow <- if (exists("PROP_ALLOW_MODEL_ODDS")) PROP_ALLOW_MODEL_ODDS else NULL
  assign("PROP_ALLOW_MODEL_ODDS", FALSE, envir = .GlobalEnv)
  on.exit({
    if (is.null(old_allow)) {
      if (exists("PROP_ALLOW_MODEL_ODDS", envir = .GlobalEnv)) {
        rm("PROP_ALLOW_MODEL_ODDS", envir = .GlobalEnv)
      }
    } else {
      assign("PROP_ALLOW_MODEL_ODDS", old_allow, envir = .GlobalEnv)
    }
  }, add = TRUE)

  sim <- list(
    simulated_yards = rnorm(2000, mean = 250, sd = 35),
    projection = 250
  )

  res <- passing_yards_over_under(
    simulation = sim,
    line = 250,
    over_odds = NA_real_,
    under_odds = NA_real_
  )

  expect_true(is.na(res$over_odds) || !is.finite(res$over_odds))
  expect_true(is.na(res$under_odds) || !is.finite(res$under_odds))
  expect_true(is.na(res$ev_over) || !is.finite(res$ev_over))
  expect_true(is.na(res$ev_under) || !is.finite(res$ev_under))
  expect_equal(res$recommendation, "PASS")
})
