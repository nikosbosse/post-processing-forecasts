df <- read.csv("test-data/full-data-uk-challenge.csv")
model <- "epiforecasts-EpiExpert"

df_updated <- update_predictions(df, method = "cqr", models = model)
df_combined <- collect_predictions(original = df, cqr = df_updated)

# use expect_error() with second argument 'NA' to test for simple functionality
# (function produces no error)

#   ____________________________________________________________________________
#   Tests for plot_quantiles()                                              ####

test_that("default arguments work", {
  expect_error(plot_quantiles(df, model), NA)
  expect_error(plot_quantiles(df_updated, model), NA)
})

test_that("custom quantiles input works", {
  expect_error(plot_quantiles(df, model, quantiles = c(0.01, 0.025, 0.25)), NA)
  expect_error(plot_quantiles(df_updated, model, quantiles = 0.1), NA)
})




#   ____________________________________________________________________________
#   Tests for plot_intervals_grid()                                              ####

test_that("default arguments work", {
  expect_error(plot_intervals_grid(df_combined, model), NA)
  expect_error(plot_intervals_grid(df_combined, model, facet_by = "horizon"), NA)
  expect_error(plot_intervals_grid(df_combined, model, facet_by = "quantile"), NA)
})

test_that("inputs for faceting by horizon work", {
  expect_error(
    plot_intervals_grid(df_combined, model, facet_by = "horizon", quantiles = 0.25),
    NA
  )
})

test_that("inputs for faceting by quantile work", {
  expect_error(
    plot_intervals_grid(df_combined, model, facet_by = "quantile", horizon = 3),
    NA
  )
  expect_error(
    plot_intervals_grid(
      df_combined, model,
      facet_by = "quantile", quantiles = c(0.01, 0.1, 0.25), horizon = 4
    ),
    NA
  )
})
