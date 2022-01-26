#' @importFrom rlang .data

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### location column                                                         ####

filter_locations <- function(df, locations) {
  locations <- c(locations)
  df |> dplyr::filter(.data$location %in% locations)
}

process_location_input <- function(df, location) {
  if (!is.null(location)) {
    df <- filter_locations(df, location)
  }

  # return location_name instead of location!
  location_name <- df$location_name[1]
  return(list(df = df, location_name = location_name))
}


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### model column                                                            ####

filter_models <- function(df, models) {
  models <- c(models)
  df |> dplyr::filter(.data$model %in% models)
}

process_model_input <- function(df, model) {
  # if input "model" is specified
  if (!is.null(model)) {
    df <- filter_models(df, model)
  } else {
    # input df is already filtered, take only existing model
    model <- df$model[1]
  }

  return(list(df = df, model = model))
}



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### quantile column                                                         ####

filter_quantiles <- function(df, quantiles) {
  df |> dplyr::filter(.data$quantile %in% quantiles)
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

filter_horizons <- function(df, horizons) {
  horizons <- c(horizons)
  df |> dplyr::filter(.data$horizon %in% horizons)
}

paste_horizon <- function(horizon) {
  ifelse(
    horizon == 1,
    paste(horizon, "week ahead"),
    paste(horizon, "weeks ahead")
  )
}

mutate_horizon <- function(df) {
  df |> dplyr::mutate(horizon = paste_horizon(.data$horizon))
}



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### target_type column                                                      ####

filter_target_types <- function(df, target_types) {
  target_types <- c(target_types)
  df |> dplyr::filter(.data$target_type %in% target_types)
}


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### date column                                                             ####

# two functions to separate transformation from printing message to console
# change_to_date() might be used in other contexts
change_to_date <- function(df, forecast = FALSE, target_end = FALSE) {
  if (forecast) {
    df <- df |> dplyr::mutate(
      forecast_date = lubridate::ymd(.data$forecast_date)
    )
  }
  if (target_end) {
    df <- df |> dplyr::mutate(
      target_end_date = lubridate::ymd(.data$target_end_date)
    )
  }
  return(df)
}

validate_dates <- function(df) {
  forecast_class <- class(df$forecast_date)
  target_end_class <- class(df$target_end_date)

  if (forecast_class != "Date") {
    df <- df |> change_to_date(forecast = TRUE)
    message(stringr::str_glue(
      "Changed column forecast_date from class {forecast_class} to Date"
    ))
  }

  if (target_end_class != "Date") {
    df <- df |> change_to_date(target_end = TRUE)
    message(stringr::str_glue(
      "Changed column target_end_date from class {target_end_class} to Date"
    ))
  }

  return(df)
}



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### new method column                                                       ####

filter_methods <- function(df, methods) {
  df |> dplyr::filter(.data$method %in% methods)
}


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### validate inputs                                                         ####

validate_input <- function(df, input, string) {
  if (!all(input %in% unique(df[[string]]))) {
    stop(stringr::str_glue(
      "At least one of the input {string}s is not contained in the input data frame."
    ))
  }
}


validate_inputs <- function(df, models, locations, target_types, horizons, quantiles) {
  input_mapping <- list(
    model = models, location = locations, target_type = target_types,
    horizon = horizons, quantile = quantiles
  )

  for (i in seq_along(input_mapping)) {
    validate_input(df, input = input_mapping[[i]], string = names(input_mapping[i]))
  }
}

validate_cv_init <- function(df, cv_init_training) {
  num_dates <- dplyr::n_distinct(df$target_end_date)

  # if input cv_init_training is NULL, function returns NULL again
  if (!is.null(cv_init_training)) {
    if (cv_init_training < 0 || cv_init_training > num_dates) {
      stop(
        paste(
          "'cv_init_training' must be positive and not greater than",
          "the number of unique dates in the data set."
        )
      )
    }

    if (cv_init_training < 1) {
      # input is fraction
      return(as.integer(cv_init_training * num_dates))
    } else {
      # input is absolute number of dates
      return(as.integer(cv_init_training))
    }
  }
}

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### preprocess inputs                                                       ####

preprocess_input <- function(df, input, string) {
  if (is.null(input)) {
    # Default is no filtering, so each variable is equal to its unique values
    input <- stats::na.omit(unique(df[[string]]))
  } else {
    # make function work for single model and single locations
    input <- c(input)
  }
  return(input)
}

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### preprocessing df based on inputs                                        ####

preprocess_df <- function(df, models = NULL, locations = NULL, target_types = NULL,
                          horizons = NULL, quantiles = NULL) {

  # Preprocessing the inputs
  models <- preprocess_input(df, models, "model")
  locations <- preprocess_input(df, locations, "location")
  target_types <- preprocess_input(df, target_types, "target_type")
  horizons <- preprocess_input(df, horizons, "horizon")
  quantiles <- preprocess_input(df, quantiles, "quantile")

  # Filtering out all combinations that are not updated
  df <- df |>
    filter_models(models) |>
    filter_locations(locations) |>
    filter_target_types(target_types) |>
    filter_horizons(horizons) |>
    filter_quantile_pairs(quantiles)

  return(list(
    df = df, models = models, locations = locations,
    target_types = target_types, horizons = horizons,
    quantiles = quantiles
  ))
}


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### combination of columns                                                  ####

filter_combination <- function(df, model, location, target_type, horizon, quantile) {
  # for nicer function input names,
  # input names equal to column names are a little painful with tidyverse functions
  # => temporary variables with different names
  mod <- model
  t <- target_type
  h <- horizon
  q <- quantile
  l <- location

  # To make sure that the predictions and true_values are in the correct order we also arrange by the target_end_date
  quantiles_low_df <- df |>
    dplyr::filter(
      .data$model == mod & .data$location == l & .data$target_type == t & .data$horizon == h & .data$quantile == q
    ) |>
    dplyr::arrange(.data$target_end_date)

  quantiles_low <- quantiles_low_df$prediction
  true_values <- quantiles_low_df$true_value

  quantiles_high <- df |>
    dplyr::filter(
      .data$model == mod & .data$location == l & .data$target_type == t & .data$horizon == h & .data$quantile == 1 - q
    ) |>
    dplyr::arrange(.data$target_end_date) |>
    dplyr::pull(.data$prediction)

  return(list(
    true_values = true_values, quantiles_low = quantiles_low,
    quantiles_high = quantiles_high
  ))
}


replace_combination <- function(df, model, location, target_type, horizon, quantile,
                                quantiles_low_updated, quantiles_high_updated) {
  mod <- model
  t <- target_type
  h <- horizon
  q <- quantile
  l <- location

  df |>
    dplyr::mutate(prediction = replace(
      .data$prediction,
      .data$model == mod & .data$location == l & .data$target_type == t & .data$horizon == h & .data$quantile == q,
      values = quantiles_low_updated
    )) |>
    dplyr::mutate(prediction = replace(
      .data$prediction,
      .data$model == mod & .data$location == l & .data$target_type == t & .data$horizon == h & .data$quantile == 1 - q,
      values = quantiles_high_updated
    ))
}
