df <- read.csv("test-data/full-data-uk-challenge.csv")
tbl <- readr::read_csv("test-data/full-data-uk-challenge.csv", show_col_types = FALSE)
model <- "epiforecasts-EpiExpert"
location <- "GB"

df_preprocessed <- preprocess_df(df, model)$df
tbl_preprocessed <- preprocess_df(tbl, model)$df

cv_init_training <- 5


#   ____________________________________________________________________________
#   Tests for update_subset_cqr()                                               ####

# input of update_subset_cqr() is preprocessed / filtered data frame

test_that("works for cv_init_training = NULL", {
  df_subset <- update_subset_cqr(
    df_preprocessed,
    model, location, target_type = "Cases", horizon = 1,
    quantile = 0.01, cv_init_training = NULL
  ) |> suppressMessages()

  # dimensions of updated data frame are correct
  expect_equal(dim(df_preprocessed), dim(df_subset))

  # prediction column is updated
  expect_false(all(df_preprocessed$prediction == df_subset$prediction))
})

test_that("works for integer cv_init_training", {
  df_subset <- update_subset_cqr(
    df_preprocessed,
    model, location, target_type = "Cases", horizon = 1,
    quantile = 0.01, cv_init_training = 5
  ) |> suppressMessages()

  expect_equal(dim(df_preprocessed), dim(df_subset))
  expect_false(all(df_preprocessed$prediction == df_subset$prediction))
})

test_that("works for fraction cv_init_training", {
  df_subset <- update_subset_cqr(
    df_preprocessed,
    model, location, target_type = "Cases", horizon = 1,
    quantile = 0.01, cv_init_training = 0.5
  ) |> suppressMessages()

  expect_equal(dim(df_preprocessed), dim(df_subset))
  expect_false(all(df_preprocessed$prediction == df_subset$prediction))
})

test_that("works for tibble object", {
  tbl_subset <- update_subset_cqr(
    tbl_preprocessed,
    model, location, target_type = "Cases", horizon = 1,
    quantile = 0.01, cv_init_training = NULL
  ) |> suppressMessages()

  expect_equal(dim(tbl_preprocessed), dim(tbl_subset))
})




### ____________________________________________________________________________
### Return Single Updated Data Frame                                        ####


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### update_predictions()                                                    ####

df_updated <- update_predictions(df,
  methods = "cqr", model, location, cv_init_training = cv_init_training,
  return_list = FALSE
) |> suppressMessages()

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
  expect_equal(attr(df_updated, "cv_init_training"), cv_init_training)
})

test_that("date columns are transformed to class Date", {
  expect_s3_class(df_updated$forecast_date, "Date")
  expect_s3_class(df_updated$target_end_date, "Date")
})





### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### collect_predictions()                                                   ####

df_combined_old <- collect_predictions(
  original = change_to_date(df_preprocessed, forecast = TRUE, target_end = TRUE),
  cqr = df_updated
) |> suppressMessages()

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
  expect_equal(attr(df_combined_old, "cv_init_training"), cv_init_training)
})




### ____________________________________________________________________________
### Return List of Old and New Data Frame                                          ####


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### update_predictions()                                                    ####

df_list <- update_predictions(
  df,
  methods = "cqr", model, location, cv_init_training = cv_init_training
) |>
  suppressMessages()

test_that("return value is correctly named list", {
  expect_type(df_list, "list")
  expect_named(df_list, c("original", "cqr"))
})

test_that("updated data frame has cv_init_training attribute", {
  expect_equal(attr(df_list$cqr, "cv_init_training"), cv_init_training)
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
  expect_equal(attr(df_combined_new, "cv_init_training"), cv_init_training)
})

test_that("piping into scoringutils::score() works", {
  result <- df_combined_new |>
    scoringutils::score() |>
    scoringutils::summarise_scores(by = c("method", "model", "target_type"))

  expect_equal(dim(result), c(4, 10))
  expect_equal(unique(result$method), c("cqr", "original"))
})


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### extract_training_set() & extract_validation_set()

training_set <- extract_training_set(df_combined_new)
validation_set <- extract_validation_set(df_combined_new)

unique_training_dates <- cv_init_training
unique_validation_dates <- dplyr::n_distinct(df_combined_new$target_end_date) - cv_init_training

test_that("cqr improves the quantiles in the training mode (no cv) for Cases as well as Deaths", {
  dt <- training_set |>
    scoringutils::score() |>
    scoringutils::summarise_scores(by = c("method", "model", "target_type")) |>
    dplyr::arrange(target_type, desc(method))

  expect_gt(dt$interval_score[1] - dt$interval_score[2], 0) # Cases
  expect_gt(dt$interval_score[3] - dt$interval_score[4], 0) # Deaths
})

test_that("sizes of training and validation set are correct", {
  expect_equal(dplyr::n_distinct(training_set$target_end_date), unique_training_dates)
  expect_equal(dplyr::n_distinct(validation_set$target_end_date), unique_validation_dates)
})


df_list <- update_predictions(
  df,
  methods = "cqr", model, location, cv_init_training = 0.5
) |> suppressMessages()

df_combined_new <- collect_predictions(df_list)

training_set <- extract_training_set(df_combined_new)
validation_set <- extract_validation_set(df_combined_new)

training_length <- dplyr::n_distinct(training_set$target_end_date)
validation_length <- dplyr::n_distinct(validation_set$target_end_date)

test_that("cv_init_training = 0.5 creates equally sized data splits", {
  # sizes differ at most by one time point
  expect_lt(abs(training_length - validation_length), 2)
})
