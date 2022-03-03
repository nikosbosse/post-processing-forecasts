devtools::load_all(".")
library(foreach)
library(doParallel)
registerDoParallel(cores = 2)
# https://stackoverflow.com/questions/30688307/parallelization-doesnt-work-with-the-foreach-package
# my mac has 2 cores, see this by running the following line in your terminal: system_profiler SPHardwareDataType
# https://techwiser.com/how-many-cores-does-my-cpu-have/
# library(tictoc)


#   ____________________________________________________________________________
#   Test that parallel and sequence processing of qsa give the same result  ####
df <- read.csv(here::here("data_modified", "uk_data_incidences.csv"))
m <- "epiforecasts-EpiExpert"
l <- "GB"
t <- "Cases"
h <- c(1, 2)

df_subset <- dplyr::filter(df, model %in% m & location %in% l &
  target_type %in% t & horizon %in% h)

# Run QSA parallel
# tic()
df_updated_parallel <- update_predictions(df_subset,
  methods = "qsa_uniform",
  # cv_init_training = 11,
  optim_method = "L-BFGS-B",
  lower_bound_optim = 0,
  upper_bound_optim = 5,
  return_list = TRUE,
  parallel = TRUE,
  verbose = TRUE
)
# toc()
# 272.797 sec elapsed
test_that("parallel computation of qsa gives back the right number of rows, e.g. as many as in original data", {
  expect_equal(TRUE, nrow(df_updated_parallel$qsa_uniform) == nrow(df_updated_parallel$original))
})


# Run QSA in sequence
# tic()
df_updated_sequence <- update_predictions(df_subset,
  methods = "qsa_uniform",
  # cv_init_training = 5,
  optim_method = "L-BFGS-B",
  lower_bound_optim = 0,
  upper_bound_optim = 5,
  return_list = TRUE,
  parallel = FALSE,
  verbose = TRUE
)
# toc()
# 465.232 sec elapsed
test_that("sequence computation of qsa gives back the right number of rows, e.g. as many as in original data", {
  expect_equal(TRUE, nrow(df_updated_sequence$qsa_uniform) == nrow(df_updated_sequence$original))
})

# As expected the time spent is about halfed by 2 cores (will converge towards half for more models, here only 2)
# 272.797  / 465.232 # = 0.5863677

# Combine DataFrames so that we can plot results
df_combined_parallel <- df_updated_parallel |> collect_predictions()
df_combined_sequence <- df_updated_sequence |> collect_predictions()

# Plot results for a specific quantile
# plot_intervals(df_combined_parallel, model = m, location = l, target_type = t, quantile = 0.2, horizon = 1)
# plot_intervals(df_combined_sequence, model = m, location = l, target_type = t, quantile = 0.2, horizon = 1)

# plots are identical except the parallel version misses the cv arguement

test_that("sequence and parallel computation give back identical values.", {
  expect_equal(TRUE, any(df_updated_sequence$qsa_uniform[
    order(
      df_updated_sequence$qsa_uniform[, "model"],
      df_updated_sequence$qsa_uniform[, "target_type"],
      df_updated_sequence$qsa_uniform[, "horizon"],
      df_updated_sequence$qsa_uniform[, "horizon"]
    ),
  ] == df_updated_parallel$qsa_uniform[
    order(
      df_updated_parallel$qsa_uniform[, "model"],
      df_updated_parallel$qsa_uniform[, "target_type"],
      df_updated_parallel$qsa_uniform[, "horizon"],
      df_updated_parallel$qsa_uniform[, "horizon"]
    ),
  ]))
})


#   ____________________________________________________________________________
#   Test that parallel processing can handle combinations in df with no obs ####


# Examples for combinations with no observations: | quantile observations = 0 | model = anonymous_Magpie | location = GB | target_type = Deaths | horizon = 1
df <- read.csv(here::here("data_modified", "uk_data_incidences.csv"))
m <- c("epiforecasts-EpiExpert","anonymous_Goldfinch")
l <- "GB"
t <- c("Cases","Deaths")
h <- c(1, 2)

df_subset <- dplyr::filter(df, model %in% m & location %in% l &
                             target_type %in% t & horizon %in% h)

# Tests wether subset contains all combiantions we expect
unique(df_subset$model) == m
unique(df_subset$location) == l
unique(df_subset$target_type) == t
unique(df_subset$horizon) == h
nrow(dplyr::filter(df, model %in% "anonymous_Goldfinch" & location %in% "GB" & target_type %in% "Deaths" & horizon %in% 1)) == 0



# Run QSA parallel
# tic()
df_updated_parallel <- update_predictions(df_subset,
                                          methods = "qsa_uniform",
                                          # cv_init_training = 11,
                                          optim_method = "L-BFGS-B",
                                          lower_bound_optim = 0,
                                          upper_bound_optim = 5,
                                          return_list = TRUE,
                                          parallel = TRUE,
                                          verbose = TRUE
)
# toc()
# 272.797 sec elapsed
test_that("parallel computation of qsa gives back the right number of rows, e.g. as many as in original data even if some combinations of model, location, target_type and horizon dont have observations", {
  expect_equal(TRUE, nrow(df_updated_parallel$qsa_uniform) == nrow(df_updated_parallel$original))
})
