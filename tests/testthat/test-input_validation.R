df <- read.csv("test-data/full-data-uk-challenge.csv")
model <- "epiforecasts-EpiExpert"
location <- "GB"

test_that("error messages are triggered as intended", {
  expect_error(
    update_predictions(df,
      methods = "qr", models = model, locations = location,
      return_list = FALSE
    ),
    "qr is not an implemented post processing method."
  )
  expect_error(
    update_predictions(df,
      methods = "cqr", models = c(model, "s"), locations = location,
      return_list = FALSE
    ),
    "At least one of the input models is not contained in the input data frame."
  )
  expect_error(
    update_predictions(
      df,
      methods = "cqr", models = model, locations = "DEU",
      return_list = FALSE
    ),
    "At least one of the input locations is not contained in the input data frame."
  )
  expect_error(
    update_predictions(df,
      methods = "cqr", models = model, locations = location,
      target_types = "Case", return_list = FALSE
    ),
    "At least one of the input target_types is not contained in the input data frame."
  )
  expect_error(
    update_predictions(df,
      methods = "cqr", models = model, locations = location,
      horizons = c(1, 5), return_list = FALSE
    ),
    "At least one of the input horizons is not contained in the input data frame."
  )
  expect_error(
    update_predictions(df,
      methods = "cqr", models = model, locations = location,
      quantiles = c(0, 1), return_list = FALSE
    ),
    "At least one of the input quantiles is not contained in the input data frame."
  )
  expect_error(
    update_predictions(df,
      methods = "cqr", models = model, locations = location,
      return_list = FALSE, cv_init_training = -1
    ),
    "'cv_init_training' must be positive and not greater than the number of unique dates in the data set."
  )
  expect_error(
    update_predictions(df,
      methods = "cqr", models = model, locations = location,
      return_list = FALSE, cv_init_training = 100
    ),
    "'cv_init_training' must be positive and not greater than the number of unique dates in the data set."
  )
})

test_that("cv_init_training parameter can be set as fraction", {
  expect_equal(validate_cv_init(df, cv_init_training = 0.5), 37)
  expect_equal(validate_cv_init(df, cv_init_training = 6.0), 6)
  expect_equal(validate_cv_init(df, cv_init_training = 6), 6)
  expect_null(validate_cv_init(df, cv_init_training = NULL))
})
