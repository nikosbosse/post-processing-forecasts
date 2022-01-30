library(qra)
library(dplyr)
library(scoringutils)

uk_data <- readr::read_csv("data_modified/uk_data_incidences.csv")

as_qra_forecasts <- function(df) {
  df |>
    dplyr::select(
      value = prediction,
      forecast_horizon = horizon,
      creation_date = forecast_date,
      value_date = target_end_date,
      model,
      location,
      target_type,
      quantile
    )
}

as_qra_data <- function(df) {
  df |>
    dplyr::select(
      value = true_value,
      value_date = target_end_date,
      target_type,
      location
    ) |>
    unique()
}


forecasts <- uk_data |>
  as_qra_forecasts() |>
  filter(model == "epiforecasts-EpiExpert")

data <- uk_data |> as_qra_data()

res <- qra(
  forecasts, data,
  per_quantile_weights = TRUE, noncross = FALSE, intercept = TRUE,
  enforce_normalisation = FALSE, sort = FALSE
)

# only consider forecast_horizon = 1, works for "Deaths" instead of "Cases" as well
first_cases_quantile <- forecasts |>
  filter(
    value_date == "2021-08-21", forecast_horizon == 1, target_type == "Cases",
    # choose ONLY 0.01 quantile
    quantile == 0.01,
  ) |>
  arrange(forecast_horizon, target_type) |>
  pull(value)

qra_predictions <- res$ensemble |>
  filter(forecast_horizon == 1, target_type == "Cases") |>
  pull(value)

qra_weights <- res$weights |>
  filter(forecast_horizon == 1, target_type == "Cases") |>
  pull(weight)

qra_intercepts <- res$intercepts |>
  filter(forecast_horizon == 1, target_type == "Cases") |>
  pull(intercept)

# qra predictions for ALL quantiles use only the observed prediction for 0.01 quantile
sum(first_cases_quantile * qra_weights + qra_intercepts != qra_predictions)






qra_predictions <- res$ensemble$value

weights <- res$weights$weight

intercept <- res$intercepts$intercept

original_predictions <- forecasts |>
  filter(value_date == "2021-08-21") |>
  arrange(forecast_horizon, target_type) |>
  select(forecast_horizon, target_type, quantile, original_prediction = value)

res$ensemble |>
  select(forecast_horizon, target_type, quantile, qra_prediction = value) |>
  left_join(res$weights) |>
  left_join(res$intercepts) |>
  left_join(original_predictions) |>
  select(-c(location, horizon, model)) |>
  group_by(forecast_horizon, target_type) |>
  mutate(manual_calculation = original_prediction[1] * weight + intercept) |>
  summarise(sum(manual_calculation == qra_prediction))

(first_row$value * weights + intercept) == qra_predictions
qra_predictions |> head()

head(weights, 4)
head(intercept, 4)
head(original_predictions, 4)

head(qra_predictions, 4)


first_used_value <- (qra_predictions[1] - intercept[1]) / weights[1]

forecasts |>
  filter(value_date == "2021-08-21") |>
  filter(abs(value - first_used_value) < 0.01)

second_used_value <- (qra_predictions[46] - intercept[46]) / weights[46]

forecasts |>
  filter(value_date == "2021-08-21") |>
  filter(abs(value - second_used_value) < 0.001)
