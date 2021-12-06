#' @importFrom rlang .data


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### model column                                                            ####

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



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### quantile column                                                         ####

filter_quantiles <- function(df, quantiles) {
  df |> dplyr::filter(.yata$quantile %in% quantiles)
}

filter_quantile_pairs <- function(df, quantiles) {
  df |> dplyr::filter(.data$quantile %in% c(quantiles, 1 - quantiles))
}

process_quantile_pair <- function(df, quantile) {
  lower_quantile <- quantile
  upper_quantile <- 1 - quantile

  df |>
    filter_quantile_pairs(quantile) |>
    dplyr::mutate(quantile = factor(.data$quantile) |>
      forcats::fct_recode(
        "lower" = as.character(lower_quantile),
        "upper" = as.character(upper_quantile)
      ))
}

add_quantile_group <- function(df, quantiles) {
  l <- list()
  
  for (quantile in quantiles) {
    l[[as.character(quantile)]] <- df |> 
      process_quantile_pair(quantile) |> 
      dplyr::mutate(quantile_group = paste("quantile =", as.character(!!quantile)))
  } 
  
  dplyr::bind_rows(l)
}



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### horizon column                                                          ####


filter_horizon <- function(df, horizon) {
  df |> dplyr::filter(.data$horizon == !!horizon)
}

paste_horizon <- function(horizon) {
  ifelse(
    horizon == 1,
    paste(horizon, "week ahead"),
    paste(horizon, "weeks ahead")
  )
}

mutate_horizon <- function(df) {
  df |>
    dplyr::mutate(horizon = paste_horizon(horizon))
}



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### date column                                                             ####

change_to_date <- function(df) {
  df |> dplyr::mutate(forecast_date = as.Date(.data$forecast_date))
}



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### facet helpers                                                           ####

facet_horizon <- function(df, quantile, horizon) {
  df |>
    process_quantile_pair(quantile) |> 
    mutate_horizon() 
}

facet_quantile <- function(df, quantiles, horizon) {
  df |> 
    filter_horizon(horizon) |> 
    filter_quantile_pairs(quantiles) |> 
    add_quantile_group(quantiles)
}



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### further filter helpers                                                  ####

filter_methods <- function(df, methods) {
  df |> dplyr::filter(.data$method %in% methods)
}

filter_target_types <- function(df, target_types) {
  df |> dplyr::filter(.data$target_type %in% target_types)
}

filter_locations <- function(df, locations) {
  df |> dplyr::filter(.data$location %in% locations)
}
