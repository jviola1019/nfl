library(testthat)

props_config_path <- file.path(getwd(), "sports", "nfl", "props", "props_config.R")
if (file.exists(props_config_path)) {
  source(props_config_path, local = FALSE)
}

test_that("edge bins are centrally defined and respected", {
  skip_if_not(exists("classify_prop_edge_quality"), "props policy helpers not loaded")

  expect_equal(PROP_EDGE_BIN_OK_MAX, 0.05)
  expect_equal(PROP_EDGE_BIN_HIGH_MAX, 0.10)

  expect_equal(classify_prop_edge_quality(0.04, 0.01, "OVER"), "OK")
  expect_equal(classify_prop_edge_quality(0.07, 0.01, "OVER"), "High")
  expect_equal(classify_prop_edge_quality(0.11, 0.01, "OVER"), "Review")
  expect_equal(classify_prop_edge_quality(-0.20, 0.03, "UNDER"), "OK")
  expect_equal(classify_prop_edge_quality(NA_real_, NA_real_, "MODEL ERROR"), "MODEL ERROR")
})

test_that("recommendation policy blocks >10% edges without model-error labeling", {
  skip_if_not(exists("get_prop_recommendation"), "props policy helpers not loaded")

  expect_equal(get_prop_recommendation(ev_over = 0.03, ev_under = 0.01, model_error = FALSE), "OVER")
  expect_equal(get_prop_recommendation(ev_over = 0.01, ev_under = 0.04, model_error = FALSE), "UNDER")
  expect_equal(get_prop_recommendation(ev_over = 0.052, ev_under = -0.143, model_error = FALSE), "OVER")
  expect_equal(get_prop_recommendation(ev_over = 0.12, ev_under = -0.01, model_error = FALSE), "REVIEW")
  expect_equal(get_prop_recommendation(ev_over = 0.03, ev_under = 0.01, model_error = TRUE), "MODEL ERROR")
})

test_that("model error is reserved for real validation failures", {
  skip_if_not(exists("is_prop_model_error"), "props policy helpers not loaded")

  expect_false(is_prop_model_error(0.54, 0.46, -110, -110, is_two_sided = TRUE))
  expect_true(is_prop_model_error(1.2, -0.2, -110, -110, is_two_sided = TRUE))
  expect_true(is_prop_model_error(0.55, 0.45, -110, NA_real_, is_two_sided = TRUE))
  expect_false(is_prop_model_error(0.28, 0.72, 140, NA_real_, is_two_sided = FALSE))
})

test_that("renderer edge quality mapping matches pipeline policy", {
  skip_if_not(exists("classify_prop_edge_quality"), "props policy helpers not loaded")

  recommendation <- c("PASS", "OVER", "UNDER", "MODEL ERROR")
  ev_over <- c(0.12, 0.04, 0.07, NA_real_)
  ev_under <- c(NA_real_, 0.01, 0.02, NA_real_)

  renderer_edge_quality <- dplyr::case_when(
    recommendation == "PASS" ~ "Pass",
    TRUE ~ mapply(classify_prop_edge_quality, ev_over, ev_under, recommendation, USE.NAMES = FALSE)
  )

  pipeline_edge_quality <- c("Pass", "OK", "High", "MODEL ERROR")

  expect_equal(unname(renderer_edge_quality), pipeline_edge_quality)
})
