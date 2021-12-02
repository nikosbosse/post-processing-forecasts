#   ____________________________________________________________________________
#   Tests for compute_scores()                                              ####

y <- c(10, 20)
quantiles_low <- c(3, 5)
quantiles_high <- c(15, 30)
scores <- compute_scores(y, quantiles_low, quantiles_high)

test_that("produces vector of same length as inputs", {
  expect_equal(length(scores), length(y))
})

test_that("results is correct for toy inputs", {
  expect_equal(scores, c(-5, -10))
})



#   ____________________________________________________________________________
#   Tests for compute_margin()                                              ####

