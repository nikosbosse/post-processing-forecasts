df <- read.csv("test-data/full-data-uk-challenge.csv")
model <- "epiforecasts-EpiExpert"


#   ____________________________________________________________________________
#   Tests for update_subset()                                               ####

df_subset <- update_subset(df,
  method = "cqr", model = model,
  target_type = "Cases", horizon = 1, quantile = 0.01
)

test_that("dimensions of updated data frame are correct", {
  expect_equal(dim(df), dim(df_subset))
})

test_that("prediction column is updated", {
  expect_false(all(df$prediction == df_subset$prediction))
})




#   ____________________________________________________________________________
#   Tests for update_predictions()                                          ####

df_updated <- update_predictions(df, method = "cqr", models = model)

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
    nrow(update_predictions(df, method = "cqr", models = c(model, "seb"))),
    nrow(df)
  )
})

test_that("error message for wrong method is triggered as intended", {
  expect_error(
    update_predictions(df, method = "qr", models = model),
    "qr is not an implemented post processing method."
  )
})

test_that("error message for wrong model is triggered as intended", {
  expect_error(
    update_predictions(df, method = "cqr", models = c(model, "s")),
    "At least one of the input models is not contained in the input data frame."
  )
})



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Tests for collect_predictions()                                         ####

df_combined <- collect_predictions(original = df, cqr = df_updated)

test_that("old and new data frames are vertically stacked", {
  expect_equal(nrow(df_combined), nrow(df) + nrow(df_updated))
})

test_that("columns names of combined data frame are correct", {
  expect_equal(colnames(df_combined), c("method", colnames(df)))
})
