library(qra)
library(dplyr)
library(scoringutils)
library(purrr)

uk_data <- readr::read_csv("data_modified/uk_data_incidences.csv")
models <- "epiforecasts-EpiExpert"

# value variable gives prediction
as_qra_forecasts <- function(df) {
  df |>
    dplyr::select(
      value = prediction,
      forecast_horizon = horizon,
      creation_date = forecast_date,
      value_date = target_end_date,
      model,
      target_type,
      quantile,
      location,
      location_name
    )
}


# value variable gives truth for each prediction in forecasts df
as_qra_data <- function(df) {
  df |>
    dplyr::select(
      value = true_value,
      value_date = target_end_date,
      target_type,
      location,
      location_name
    ) |>
    unique()
}


forecasts <- uk_data |>
  as_qra_forecasts() |>
  dplyr::filter(model == models)

data <- uk_data |> as_qra_data()




#############################

# per_quantile_weights = FALSE: one weight for each combination of model, target_type
# and forecast_horizon => n_models x 2 x 4 different weights
# within each of these subgroups, weights are equal for all quantile levels

res <- qra(
  forecasts, data,
  per_quantile_weights = TRUE, noncross = FALSE,
  intercept = TRUE, enforce_normalisation = FALSE
)

# all vectors have 184 elements (23 quantiles x 2 target_types x 4 horizons) in
# the same order
original_predictions <- forecasts |>
  filter(value_date == "2021-08-21") |>
  pull(value)

qra_predictions <- res$ensemble$value

weights <- res$weights$weight

intercept <- res$intercepts$intercept



(original_predictions * weights + intercept) |> head()
qra_predictions |> head()

##########################




# execute qra() for all dates in data set (except first one which leads to error)
unique_dates <- forecasts$creation_date |> unique()

fit_date <- function(target_date) {
  res <- qra::qra(
    forecasts, data,
    target_date = target_date,
    per_quantile_weights = TRUE, noncross = FALSE,
    intercept = TRUE, enforce_normalisation = FALSE
  )

  res$ensemble
}

dates_df <- purrr::map_dfr(unique_dates[-1], fit_date)
dates_df

# transform output of qra() to valid input format for scoringutils::score()
qra_to_score <- function(result_df, qra_data_input, model) {
  result_df |>
    dplyr::select(-c(model, intercepts, horizon)) |>
    dplyr::rename(prediction = value) |>
    dplyr::full_join(qra_data_input) |>
    # dplyr::select(-c(intercepts, horizon)) |>
    dplyr::rename(
      true_value = value, horizon = forecast_horizon, forecast_date = creation_date,
      target_end_date = value_date
    ) |>
    dplyr::mutate(model = model) |>
    # sort in same order as uk_data
    dplyr::select(
      location, location_name, target_end_date,
      target_type, true_value, forecast_date, quantile,
      prediction, model, horizon
    )
}

# correct shape for collective input in collect_predictions()
# name function qra() and adjust select_method() such that new method column is added with entry "qra"
# remaining issues: first date is left out, scoring and fitting with training set

dates_df |>
  qra_to_score(qra_data_input = data, model = models) |>
  scoringutils::score() |>
  scoringutils::summarise_scores(by = c("model", "target_type", "horizon")) |>
  dplyr::select(model:interval_score)

# compare to original predictions
uk_data |>
  dplyr::filter(model == models) |>
  scoringutils::score() |>
  scoringutils::summarise_scores(by = c("model", "target_type", "horizon")) |>
  dplyr::select(model:interval_score)

# right now result is worse than original predictions => likely mistake
