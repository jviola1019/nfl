# =============================================================================
# FILE: tests/testthat/test-props-module-parse.R
# PURPOSE: Ensure all NFL props modules can be parsed/sourced cleanly
# =============================================================================

library(testthat)

test_that("all NFL props modules parse and source without warnings/errors", {
  props_dir <- file.path(.test_project_root, "sports", "nfl", "props")
  props_files <- list.files(props_dir, pattern = "\\.R$", full.names = TRUE)

  expect_gt(length(props_files), 0)

  for (props_file in props_files) {
    expect_error(parse(file = props_file), NA, info = props_file)
    expect_warning(source(props_file, local = new.env(parent = globalenv()), chdir = TRUE), NA, info = props_file)
  }
})
