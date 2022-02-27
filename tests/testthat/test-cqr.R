#   ____________________________________________________________________________
#   Tests for compute_scores functions                                              ####

true_values <- c(10, 20)
quantiles_low <- c(3, 5)
quantiles_high <- c(15, 30)
scores_symmetric <- compute_scores_symmetric(true_values, quantiles_low, quantiles_high)
scores_asymmetric <- compute_scores_asymmetric(true_values, quantiles_low, quantiles_high)
scores_multiplicative <- compute_scores_multiplicative(true_values, quantiles_low, quantiles_high)

test_that("produces vector of same length as inputs", {
  expect_equal(length(scores_symmetric), length(true_values))
  expect_equal(length(scores_symmetric), length(true_values))

  expect_equal(length(scores_asymmetric$scores_lower), length(true_values))
  expect_equal(length(scores_asymmetric$scores_upper), length(true_values))

  expect_equal(length(scores_multiplicative$scores_lower), length(true_values))
  expect_equal(length(scores_multiplicative$scores_upper), length(true_values))
})

test_that("results are correct for toy inputs", {
  expect_equal(scores_symmetric, c(-5, -10))

  expect_equal(scores_asymmetric$scores_lower, c(-7, -15))
  expect_equal(scores_asymmetric$scores_upper, c(-5, -10))

  expect_equal(
    scores_multiplicative$scores_lower, c(12.85859, 18.93050),
    tolerance = 1e-3
  )
  expect_equal(
    scores_multiplicative$scores_upper, c(0.6666667, 0.6666667),
    tolerance = 1e-3
  )
})



#   ____________________________________________________________________________
#   Tests for compute_margin()                                              ####

quantile <- 0.05
margin_symmetric <- compute_margin(scores_symmetric, quantile)

margin_asymmetric_lower <- compute_margin(scores_asymmetric$scores_lower, quantile)
margin_asymmetric_upper <- compute_margin(scores_asymmetric$scores_upper, quantile)

scores_multiplicative_lower <- compute_margin(scores_multiplicative$scores_lower, quantile)
scores_multiplicative_upper <- compute_margin(scores_multiplicative$scores_upper, quantile)

test_that("produces scalar", {
  expect_equal(length(margin_symmetric), 1)
})

test_that("results are correct for toy input", {
  expect_equal(margin_symmetric, -5, ignore_attr = TRUE)

  expect_equal(margin_asymmetric_lower, -7, ignore_attr = TRUE)
  expect_equal(margin_asymmetric_upper, -5, ignore_attr = TRUE)

  expect_equal(
    scores_multiplicative_lower, 18.9305,
    ignore_attr = TRUE, tolerance = 1e-5
  )
  expect_equal(
    scores_multiplicative_upper, 0.6666667,
    ignore_attr = TRUE, tolerance = 1e-5
  )
})



#   ____________________________________________________________________________
#   Tests for cqr_symmetric()                                               ####

result_symmetric <- cqr_symmetric(
  quantile, true_values, quantiles_low, quantiles_high
)
result_asymmetric <- cqr_asymmetric(
  quantile, true_values, quantiles_low, quantiles_high
)
result_multiplicative <- cqr_multiplicative(
  quantile, true_values, quantiles_low, quantiles_high
)

test_that("returns correctly named list", {
  expect_type(result_symmetric, "list")
  expect_type(result_asymmetric, "list")
  expect_type(result_multiplicative, "list")

  expect_named(
    result_symmetric,
    c("margin_lower", "margin_upper", "lower_bound", "upper_bound")
  )
  expect_named(
    result_asymmetric,
    c("margin_lower", "margin_upper", "lower_bound", "upper_bound")
  )
  expect_named(
    result_multiplicative,
    c("margin_lower", "margin_upper", "lower_bound", "upper_bound")
  )
})

test_that("result is correct for toy input", {
  expect_equal(result_symmetric$lower_bound, c(8, 10))
  expect_equal(result_symmetric$upper_bound, c(10, 25))

  expect_equal(result_asymmetric$lower_bound, c(10, 12))
  expect_equal(result_asymmetric$upper_bound, c(10, 25))

  expect_equal(
    result_multiplicative$lower_bound, c(15.98630, 26.64383),
    tolerance = 1e-3
  )
  expect_equal(
    result_multiplicative$upper_bound, c(2.814910, 5.629821),
    tolerance = 1e-3
  )
})
