devtools::load_all(".")
library(foreach)
library(doParallel)
registerDoParallel(cores = 2)
# https://stackoverflow.com/questions/30688307/parallelization-doesnt-work-with-the-foreach-package
# my mac has 2 cores, see this by running the following line in your terminal: system_profiler SPHardwareDataType
# https://techwiser.com/how-many-cores-does-my-cpu-have/
# library(tictoc)


df <- read.csv(here::here("data_modified", "uk_data_incidences.csv"))

models <- NULL
locations <- NULL
target_types <- NULL
horizons <- NULL
quantiles <- NULL

preprocessed_list <- preprocess_df(
  df, models, locations, target_types, horizons, quantiles
)

df_preprocessed <- preprocessed_list$df
models <- preprocessed_list$models
locations <- preprocessed_list$locations
target_types <- preprocessed_list$target_types
horizons <- preprocessed_list$horizons
quantiles <- preprocessed_list$quantiles

# get all complete models
complete_models <- df |>
  dplyr::count(model) |>
  dplyr::filter(n == max(n)) |>
  dplyr::pull(model)


time_series_ids <- stats::setNames(data.frame(
  matrix(ncol = 4, nrow = 0)
), c("model", "location", "target_type", "horizon"))
for (m in complete_models) { # models
  for (l in locations) {
    for (t in target_types) {
      for (h in horizons) {
        time_series_ids[nrow(time_series_ids) + 1, ] <- c(m, l, t, h)
      }
    }
  }
}


for (i in seq(1, nrow(time_series_ids))) {
  m <- time_series_ids["model"][[1]][i]
  l <- time_series_ids["location"][[1]][i]
  t <- time_series_ids["target_type"][[1]][i]
  h <- time_series_ids["horizon"][[1]][i]

  subset <- dplyr::filter(df_preprocessed, .data$model == m & .data$location == l & .data$target_type == t & .data$horizon == h)

  if (nrow(subset) == 0) { # !nrow(subset) == 83
    cat(" | quantile observations = ", nrow(subset), " | model = ", m, " | location = ", l, " | target_type = ", t, " | horizon = ", h, "\n",
      sep = ""
    )
  }

  if (!ncol(subset) == 10) { # !nrow(subset) == 83
    print("col num issue")
    cat(" | quantile observations = ", nrow(subset), " | model = ", m, " | location = ", l, " | target_type = ", t, " | horizon = ", h, "\n",
      sep = ""
    )
  }
}

# Examples for combinations with no observations: | quantile observations = 0 | model = anonymous_Magpie | location = GB | target_type = Deaths | horizon = 1
df <- read.csv(here::here("data_modified", "uk_data_incidences.csv"))
m <- c("epiforecasts-EpiExpert", "anonymous_Goldfinch")
l <- "GB"
t <- c("Cases", "Deaths")
h <- c(1, 2)

df_subset <- dplyr::filter(df, model %in% m & location %in% l &
  target_type %in% t & horizon %in% h)

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
test_that("parallel computation of qsa gives back the right number of rows, e.g. as many as in original data", {
  expect_equal(TRUE, nrow(df_updated_parallel$qsa_uniform) == nrow(df_updated_parallel$original))
})


# checking wether somewhere we have cols no equal to 10
for (i in seq(1, nrow(time_series_ids))) {
  m <- time_series_ids["model"][[1]][i]
  l <- time_series_ids["location"][[1]][i]
  t <- time_series_ids["target_type"][[1]][i]
  h <- time_series_ids["horizon"][[1]][i]

  subset <- dplyr::filter(df_preprocessed, .data$model == m & .data$location == l & .data$target_type == t & .data$horizon == h)

  if (!ncol(subset) == 10) { # !nrow(subset) == 83
    cat(" | quantile observations = ", nrow(subset), " | model = ", m, " | location = ", l, " | target_type = ", t, " | horizon = ", h, "\n",
      sep = ""
    )
  }
}


# Testing a rbind and saving function

df_subset1 <- dplyr::filter(df, model %in% m & location %in% l &
  target_type %in% t & horizon %in% h)
df_subset2 <- dplyr::filter(df, model %in% m & location %in% l &
  target_type %in% t & horizon %in% h)
df_subset3 <- dplyr::filter(df, model %in% m & location %in% l &
  target_type %in% t & horizon %in% h)

rbind_and_saving <- function(...) {
  df_combined_1 <- rbind(...)
  readr::write_rds(df_combined_1, file = here::here("data_results", "parallel_qsa_cache.rds"))
}

rbind_and_saving(df_subset1, df_subset2, df_subset3)
