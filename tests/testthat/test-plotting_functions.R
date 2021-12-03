df <- read.csv("test-data/full-data-uk-challenge.csv")
model <- "epiforecasts-EpiExpert"

df_updated <- update_predictions(df, method = "cqr", models = model)
df_combined <- collect_predictions(original = df, cqr = df_updated)


#   ____________________________________________________________________________
#   Tests for plot_intervals()                                              ####

# use expect_error() with second argument 'NA' to test that function produces no error

test_that("default arguments work", {
  expect_error(plot_intervals(df_combined, model), NA)
  expect_error(plot_intervals(df_combined, model, facet_by = "horizon"), NA)
  expect_error(plot_intervals(df_combined, model, facet_by = "quantile"), NA)
})


test_that("inputs for faceting by horizon work", {
  expect_error(
    plot_intervals(df_combined, model, facet_by = "horizon", quantiles = 0.25),
    NA
  )
})

test_that("inputs for faceting by quantile work", {
  expect_error(
    plot_intervals(df_combined, model, facet_by = "quantile", horizon = 3),
    NA
  )
  expect_error(
    plot_intervals(
      df_combined, model,
      facet_by = "quantile", quantiles = c(0.01, 0.1, 0.25), horizon = 4
    ),
    NA
  )
})
