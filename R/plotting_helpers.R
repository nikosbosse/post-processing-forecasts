#' @importFrom rlang .data

filter_model <- function(df, model) {
  df |> dplyr::filter(.data$model == !!model)
}

filter_alpha_sym <- function(df, alpha) {
  lower_quantile <- alpha
  upper_quantile <- 1 - alpha

  df |>
    dplyr::filter(.data$quantile %in% c(lower_quantile, upper_quantile)) |>
    dplyr::mutate(quantile = factor(.data$quantile) |>
      forcats::fct_recode(
        "lower" = as.character(lower_quantile),
        "upper" = as.character(upper_quantile)
      ))
}

filter_alpha_asym <- function(df, alphas) {
  df |> dplyr::filter(.data$quantile %in% alphas)
}

mutate_horizon <- function(df) {
  df |> 
    dplyr::mutate(horizon = ifelse(
    .data$horizon == 1,
    paste(.data$horizon, "week ahead"),
    paste(.data$horizon, "weeks ahead")
  ))
}

mutate_date <- function(df) {
  df |> dplyr::mutate(forecast_date = as.Date(.data$forecast_date))
}
