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
  per_quantile_weights = TRUE, noncross = FALSE,
  intercept = TRUE, enforce_normalisation = FALSE
)

res$ensemble$creation_date |>
  unique()

res$ensemble$value_date |>
  unique()

original_predictions <- forecasts |>
  filter(creation_date == "2021-08-16") |>
  arrange(creation_date, forecast_horizon, value_date, model, location, target_type, quantile) |>
  pull(value)

qra_predictions <- res$ensemble |>
  arrange(creation_date, forecast_horizon, value_date, model, location, target_type, quantile) |> 
  pull(value)

weights <- res$weights |>
  arrange(forecast_horizon, model, location, target_type, quantile) |> 
  pull(weight)

intercept <- res$intercepts |>
  arrange(forecast_horizon, location, target_type, quantile) |> # model is not in here...
  pull(intercept)

# these two are not the same
(original_predictions * weights + intercept) |> head()
qra_predictions |> head()
