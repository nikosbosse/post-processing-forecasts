# it seems like the qra package uses the magrittr pipe, but does not reexport it
# thus, library(qra) alone does not work
library(dplyr)

uk_data <- readr::read_csv("data_modified/uk_data_incidences.csv")
mod <- "epiforecasts-EpiExpert"

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

fit_single_date <- function(qra_forecasts, qra_data, target_date) {
  res <- qra::qra(
    qra_forecasts, qra_data,
    target_date = target_date,
    per_quantile_weights = TRUE, noncross = FALSE,
    intercept = TRUE, enforce_normalisation = FALSE
  )

  res$ensemble
}

# same shape as uk_data except first date is missing
fit_all_dates <- function(qra_forecasts, qra_data, all_dates) {
  # qra() cannot predict first date in dataframe as target_date
  purrr::map_dfr(
    .x = all_dates[-1], .f = ~ fit_single_date(qra_forecasts, qra_data, .x)
  )
}

# add first date to input equally shaped dataframes into scoringutils::score()
# here original_df = uk_data
add_first_date <- function(qra_df, original_df, first_date) {
  contained_models <- unique(qra_df$model)

  to_add <- original_df |>
    dplyr::filter(forecast_date == first_date, model %in% contained_models) |>
    tidyr::drop_na()

  dplyr::bind_rows(to_add, qra_df)
}

# transform output of qra() to valid input format for scoringutils::score()
qra_to_score <- function(result_df, qra_data_input, model) {
  result_df |>
    dplyr::select(-c(model, intercepts, horizon)) |>
    dplyr::rename(prediction = value) |>
    dplyr::full_join(qra_data_input) |>
    dplyr::rename(
      true_value = value,
      horizon = forecast_horizon,
      forecast_date = creation_date,
      target_end_date = value_date
    ) |>
    dplyr::mutate(model = model) |>
    # sort in same order as uk_data
    dplyr::select(
      location, location_name, target_end_date, target_type, true_value,
      forecast_date, quantile, prediction, model, horizon
    ) |>
    tidyr::drop_na()
}


qra_forecasts <- uk_data |>
  as_qra_forecasts() |>
  dplyr::filter(model == mod)

qra_data <- uk_data |> as_qra_data()

# execute qra() for all dates in data set (except first one which leads to error)
all_dates <- unique(qra_forecasts$creation_date)

qra_df <- fit_all_dates(qra_forecasts, qra_data, all_dates)

qra_predictions <- qra_df |>
  qra_to_score(qra_data_input = qra_data, model = mod) |>
  add_first_date(original_df = uk_data, first_date = all_dates[1])

original_predictions <- uk_data |>
  dplyr::filter(model == mod)

# original predictions and qra predictions have the same shape and only differ in the updated prediction column
dplyr::all_equal(
  original_predictions |> dplyr::select(-prediction),
  qra_predictions |> dplyr::select(-prediction),
)


# correct shape for collective input in collect_predictions()
# name function qra() and adjust select_method() such that new method column is added with entry "qra"
# remaining issues: first date is left out, scoring and fitting with training set


display_scores <- function(predictions, by) {
  predictions |>
    scoringutils::score() |>
    scoringutils::summarise_scores(by = by) |>
    dplyr::select(model:interval_score)
}

# qra() seems to make predictions worse on the TRAINING set?

# separated by target_type and horizon
display_scores(original_predictions, by = c("model", "target_type", "horizon"))
display_scores(qra_predictions, by = c("model", "target_type", "horizon"))

# aggregated over all target_types and horizons
display_scores(original_predictions, by = c("model"))
display_scores(qra_predictions, by = c("model"))
