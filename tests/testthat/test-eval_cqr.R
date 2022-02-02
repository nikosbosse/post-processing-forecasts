#   ____________________________________________________________________________
#   Tests for UK Data                                                       ####

df_combined <- readr::read_rds("test-data/uk_cqr.rds")

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Tests for single category                                               ####

eval_models <- eval_methods(df_combined, summarise_by = "model")
eval_target_types <- eval_methods(df_combined, summarise_by = "target_type")
eval_horizons <- eval_methods(df_combined, summarise_by = "horizon")
eval_quantiles <- eval_methods(df_combined, summarise_by = "quantile")

test_that("works for single category", {
  expect_equal(dim(eval_models), c(6, 2))
  expect_equal(colnames(eval_models), c("model", "relative_change"))

  expect_equal(dim(eval_target_types), c(2, 2))
  expect_equal(colnames(eval_target_types), c("target_type", "relative_change"))

  expect_equal(dim(eval_horizons), c(4, 2))
  expect_equal(colnames(eval_horizons), c("horizon", "relative_change"))

  expect_equal(dim(eval_quantiles), c(23, 2))
  expect_equal(colnames(eval_quantiles), c("quantile", "relative_change"))

  expect_equal(attr(eval_models, "summarise_by"), "model")
})

test_that("plot works for single category", {
  expect_error(plot_eval(eval_models), NA)
  expect_error(plot_eval(eval_models, heatmap = FALSE), NA)
})


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Tests for two categories                                                ####

eval_models_horizons <- eval_methods(
  df_combined,
  summarise_by = c("model", "horizon")
)

eval_models_target_types <- eval_methods(
  df_combined,
  summarise_by = c("model", "target_type")
)

eval_horizons_target_types <- eval_methods(
  df_combined,
  summarise_by = c("horizon", "target_type")
)

test_that("works for two categories", {
  expect_equal(dim(eval_models_horizons), c(6, 5))
  expect_equal(dim(eval_models_target_types), c(6, 3))
  expect_equal(dim(eval_horizons_target_types), c(4, 3))
  expect_equal(attr(eval_models_horizons, "summarise_by"), c("model", "horizon"))
})

test_that("plot works for two categories", {
  expect_error(plot_eval(eval_models_horizons), NA)
})

test_that("argument heatmap = FALSE triggers error correctly", {
  expect_error(
    plot_eval(eval_models_horizons, heatmap = FALSE),
    paste(
      "Barplot is only available in one dimension",
      "(1 input to 'summarise_by' in eval_methods())"
    ),
    fixed = TRUE
  )
})


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Tests for input margins                                                 ####

margins_models_horizons <- eval_methods(
  df_combined,
  summarise_by = c("model", "horizon"), margins = TRUE
)

row_margins <- margins_models_horizons$margins
col_margins <- margins_models_horizons[nrow(margins_models_horizons), ]

test_that("margins are added correctly", {
  expect_equal(sum(row_margins, na.rm = TRUE), sum(eval_models$relative_change))
  expect_equal(
    sum(as.numeric(col_margins), na.rm = TRUE),
    sum(eval_horizons$relative_change)
  )
  expect_equal(attr(margins_models_horizons, "summarise_by"), c("model", "horizon"))
})

test_that("plot works for margins = TRUE", {
  expect_error(plot_eval(margins_models_horizons), NA)
})


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Tests for input averages                                                ####

row_horizon_target_type <- eval_methods(
  df_combined,
  summarise_by = c("horizon", "target_type"), row_averages = TRUE
)

col_horizon_target_type <- eval_methods(
  df_combined,
  summarise_by = c("horizon", "target_type"), col_averages = TRUE
)

row_col_horizon_target_type <- eval_methods(
  df_combined,
  summarise_by = c("horizon", "target_type"), row_averages = TRUE,
  col_averages = TRUE
)

test_that("row and column averages work", {
  expect_equal(dim(row_horizon_target_type), c(4, 4))
  expect_equal(
    attr(row_horizon_target_type, "summarise_by"), c("horizon", "target_type")
  )

  expect_equal(dim(col_horizon_target_type), c(5, 3))
  expect_equal(
    attr(col_horizon_target_type, "summarise_by"), c("horizon", "target_type")
  )

  expect_equal(dim(row_col_horizon_target_type), c(5, 4))
  expect_equal(
    attr(row_col_horizon_target_type, "summarise_by"), c("horizon", "target_type")
  )
})

test_that("plot works for averages = TRUE", {
  expect_error(plot_eval(row_horizon_target_type), NA)
  expect_error(plot_eval(col_horizon_target_type), NA)
  expect_error(plot_eval(row_col_horizon_target_type), NA)
})

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Tests for input round_digits                                            ####

test_that("works for different numeric value of round_digits", {
  eval_models_rounded <- eval_methods(df_combined, summarise_by = "model", round_digits = 1)

  expect_equal(
    eval_models_rounded[1, 2], round(eval_models_rounded[1, 2], digits = 1)
  )
})

test_that("NULL value of round_digits does not trigger error", {
  expect_error(
    eval_methods(df_combined, summarise_by = "model", round_digits = NULL), NA
  )
})

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Tests for error messages                                                ####

test_that("error messages are triggered as intended", {
  expect_error(
    eval_methods(df_combined,
      summarise_by = "model", margins = TRUE,
      row_averages = TRUE
    ),
    "Either margins or averages can be specified."
  )
  expect_error(
    eval_methods(df_combined,
      summarise_by = "model", margins = TRUE,
      col_averages = TRUE
    ),
    "Either margins or averages can be specified."
  )
})
