#   ____________________________________________________________________________
#   Tests for compute_scores()                                              ####

true_values <- c(10, 20)
quantiles_low <- c(3, 5)
quantiles_high <- c(15, 30)
scores <- compute_scores(true_values, quantiles_low, quantiles_high)

test_that("produces vector of same length as inputs", {
  expect_equal(length(scores), length(true_values))
})

test_that("results is correct for toy inputs", {
  expect_equal(scores, c(-5, -10))
})



#   ____________________________________________________________________________
#   Tests for compute_margin()                                              ####

quantile <- 0.05
margin <- compute_margin(scores, quantile)

test_that("produces scalar", {
  expect_equal(length(margin), 1)
})

test_that("result is correct for toy input", {
  expect_equal(unname(margin), -5)
})



#   ____________________________________________________________________________
#   Tests for cqr()                                                         ####

result <- cqr(quantile, true_values, quantiles_low, quantiles_high)

test_that("returns correctly named list", {
  expect_type(result, "list")
  expect_named(result, c("lower_bound", "upper_bound", "margin"))
})

test_that("result is correct for toy input", {
  expect_equal(result$lower_bound, c(8, 10))
  expect_equal(result$upper_bound, c(10, 25))
})
