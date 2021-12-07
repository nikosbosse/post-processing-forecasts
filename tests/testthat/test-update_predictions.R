df <- read.csv("test-data/full-data-uk-challenge.csv")
tbl <- readr::read_csv("test-data/full-data-uk-challenge.csv")
model <- "epiforecasts-EpiExpert"
location <- "GB"


#   ____________________________________________________________________________
#   Tests for update_subset()                                               ####

test_that("works for cv_init_training = NULL", {
  df_subset <- update_subset(
    df,
    method = cqr, model = model, target_type = "Cases", horizon = 1,
    quantile = 0.01, cv_init_training = NULL
  )

  # dimensions of updated data frame are correct
  expect_equal(dim(df), dim(df_subset))

  # prediction column is updated
  expect_false(all(df$prediction == df_subset$prediction))
})

test_that("works for cv_init_training != NULL", {
  df_subset <- update_subset(
    df,
    method = cqr, model = model, target_type = "Cases", horizon = 1,
    quantile = 0.01, cv_init_training = 5
  )

  expect_equal(dim(df), dim(df_subset))
  expect_false(all(df$prediction == df_subset$prediction))
})



test_that("works for tibble object", {
  tbl_subset <- update_subset(
    tbl,
    method = "cqr", model = model,
    target_type = "Cases", horizon = 1, quantile = 0.01
  )

  expect_equal(dim(tbl), dim(tbl_subset))
})





#   ____________________________________________________________________________
#   Tests for update_predictions()                                          ####

df_updated <- update_predictions(df, method = "cqr", models = model, locations = location, cv_init_training = NULL)

# TODO: refactor tests for equal shape
test_that("old and new data frames have same shape", {
  expect_equal(nrow(df_updated), nrow(df))
})

test_that("columns names of updated data frame are correct", {
  expect_equal(colnames(df_updated), c(colnames(df)))
})

test_that("prediction column is updated", {
  expect_false(all(df$prediction == df_updated$prediction))
})

test_that("works for multiple models", {
  expect_equal(
    nrow(update_predictions(df, method = "cqr", models = c(model, "seb"), locations = location)),
    nrow(df)
  )
})

# TODO: add test for multiple locations

test_that("error message for wrong method is triggered as intended", {
  expect_error(
    update_predictions(df, method = "qr", models = model, locations = location),
    "qr is not an implemented post processing method."
  )
})

test_that("error message for wrong model is triggered as intended", {
  expect_error(
    update_predictions(df, method = "cqr", models = c(model, "s"), locations = location),
    "At least one of the input models is not contained in the input data frame."
  )
})

# TODO: add tests for cv_init_training != NULL



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Tests for collect_predictions()                                         ####

df_combined <- collect_predictions(original = df, cqr = df_updated)

test_that("old and new data frames are vertically stacked", {
  expect_equal(nrow(df_combined), nrow(df) + nrow(df_updated))
})

test_that("columns names of combined data frame are correct", {
  expect_equal(colnames(df_combined), c("method", colnames(df)))
})
