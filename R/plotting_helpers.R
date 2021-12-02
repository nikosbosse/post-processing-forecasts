#' @importFrom rlang .data

filter_model <- function(df, model) {
  df |> dplyr::filter(.data$model == !!model)
}

process_model_input <- function(df, model) {
  # if input "model" is specified
  if (!is.null(model)) {
    df <- filter_model(df, model)
  } else {
    # input df is already filtered, take only existing model
    model <- df$model[1]
  }
  
  return(list(df = df, model = model))
}

filter_alpha_sym <- function(df, quantile) {
  lower_quantile <- quantile
  upper_quantile <- 1 - quantile

  df |>
    dplyr::filter(.data$quantile %in% c(lower_quantile, upper_quantile)) |>
    dplyr::mutate(quantile = factor(.data$quantile) |>
      forcats::fct_recode(
        "lower" = as.character(lower_quantile),
        "upper" = as.character(upper_quantile)
      ))
}

filter_alpha_asym <- function(df, quantiles) {
  df |> dplyr::filter(.data$quantile %in% quantiles)
}

paste_horizon <- function(df) {
  df |> 
    dplyr::mutate(horizon = ifelse(
    .data$horizon == 1,
    paste(.data$horizon, "week ahead"),
    paste(.data$horizon, "weeks ahead")
  ))
}

change_to_date <- function(df) {
  df |> dplyr::mutate(forecast_date = as.Date(.data$forecast_date))
}
