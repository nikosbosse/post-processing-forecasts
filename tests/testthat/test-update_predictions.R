df <- read.csv("test-data/full-data-uk-challenge.csv")
tbl <- readr::read_csv("test-data/full-data-uk-challenge.csv", show_col_types = FALSE)
model <- "epiforecasts-EpiExpert"
location <- "GB"


#   ____________________________________________________________________________
#   Tests for update_subset()                                               ####

test_that("works for cv_init_training = NULL", {
  df_subset <- update_subset(
    df,
    method = "cqr", model, location, target_type = "Cases", horizon = 1,
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
    method = "cqr", model, location, target_type = "Cases", horizon = 1,
    quantile = 0.01, cv_init_training = 5
  )

  expect_equal(dim(df), dim(df_subset))
  expect_false(all(df$prediction == df_subset$prediction))
})


# TODO
# test_that("works for tibble object", {
#   tbl_subset <- update_subset(
#     tbl,
#     method = "cqr", model = model,
#     target_type = "Cases", horizon = 1, quantile = 0.01
#   )
#
#   expect_equal(dim(tbl), dim(tbl_subset))
# })




### ____________________________________________________________________________
### Return Single Updated Data Frame                                        ####


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### update_predictions()                                                    ####

df_updated <- update_predictions(df, methods = "cqr", model, location, cv_init_training = 3, filter_original = FALSE)
df_preprocessed <- preprocess_df(df, model)$df

test_that("updated data frame is downsampled correctly", {
  expect_lt(nrow(df_updated), nrow(df))
  expect_equal(unique(df_updated$model), model)
  expect_equal(dim(df_preprocessed), dim(df_updated))
})

test_that("columns names of updated data frame are correct", {
  expect_equal(colnames(df_updated), c(colnames(df)))
})

test_that("prediction column is updated", {
  expect_false(all(df_updated$prediction %in% df$prediction))
})

test_that("updated data frame has cv_init_training attribute", {
  expect_equal(attr(df_updated, "cv_init_training"), 3)
})


test_that("error message for wrong method is triggered as intended", {
  expect_error(
    update_predictions(df, methods = "qr", models = model, locations = location),
    "qr is not an implemented post processing method."
  )
})

test_that("error message for wrong model is triggered as intended", {
  expect_error(
    update_predictions(df, methods = "cqr", models = c(model, "s"), locations = location),
    "At least one of the input models is not contained in the input data frame."
  )
})



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### collect_predictions()                                                   ####

df_combined_old <- collect_predictions(original = df_preprocessed, cqr = df_updated)

test_that("old and new data frames are vertically stacked", {
  expect_equal(nrow(df_combined_old), 2 * nrow(df_updated))
})

test_that("column names of combined data frame are correct", {
  expect_equal(colnames(df_combined_old), c("method", colnames(df)))
})

test_that("new 'method' column has correct values", {
  expect_equal(unique(df_combined_old$method), c("original", "cqr"))
})

test_that("attributes from updated data frame are transferred", {
  expect_equal(attr(df_combined_old, "cv_init_training"), 3)
})




### ____________________________________________________________________________
### Return List of Old and New Data Frame                                          ####


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### update_predictions()                                                    ####

df_list <- update_predictions(df, methods = "cqr", model, location, cv_init_training = 3, filter_original = TRUE)

test_that("return value is correctly named list", {
  expect_type(df_list, "list")
  expect_named(df_list, c("original", "cqr"))
})

test_that("updated data frame has cv_init_training attribute", {
  expect_equal(attr(df_list$cqr, "cv_init_training"), 3)
})

test_that("preprocessed original and updated data frame have the same shape", {
  expect_equal(dim(df_list$original), dim(df_list$cqr))
})



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### collect_predictions()                                                   ####

df_combined_new <- collect_predictions(df_list)

test_that("preprocessed separate inputs produce same output as list input", {
  expect_true(all(df_combined_old == df_combined_new))
})

test_that("attributes from updated data frame are transferred", {
  expect_equal(attr(df_combined_new, "cv_init_training"), 3)
})

test_that("piping into 'eval_forecasts' works", {
  result <- df_combined_new |>
    scoringutils::eval_forecasts(summarise_by = c("method", "model", "target_type"))

  expect_equal(dim(result), c(4, 10))
  expect_equal(unique(result$method), c("cqr", "original"))
})
