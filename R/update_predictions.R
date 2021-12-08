select_method <- function(method) {
  # add all methods as named vector
  implemented_methods <- c(cqr = cqr)

  if (!(method %in% names(implemented_methods))) {
    stop(stringr::str_glue("{method} is not an implemented post processing method."))
  }

  implemented_methods[[method]]
}


collect_predictions <- function(...) {
  dplyr::bind_rows(..., .id = "method")
}





# TODO: fails unit test when data is imported with tidyverse readr::read_csv()
update_subset <- function(df, method, model, location, target_type, horizon, quantile, cv_init_training) {
  method <- select_method(method = method)

  quantiles_list <- filter_combination(df, model, location, target_type, horizon, quantile)
  
  true_values <- quantiles_list$true_values
  quantiles_low <- quantiles_list$quantiles_low
  quantiles_high <- quantiles_list$quantiles_high

  if (is.null(cv_init_training)) {
    # By default cv_init_training is equal to NULL and therefore equal to the complete data.
    # e.g. by default no split in training and validation set
    result <- method(quantile * 2, true_values, quantiles_low, quantiles_high)

    margin <- result$margin
    quantiles_low_updated <- result$lower_bound
    quantiles_high_updated <- result$upper_bound
  } else {
    # This Section runs the Time Series Cross validation.
    # 1. It runs the method on the training set and updates all values of the training set.
    #    Then by using the margin it makes the first prediction in the validation set.
    #    The training values and the first adjusted validation set valued are stored in the lists
    #    quantiles_low_updated and quantiles_high_updated. The following section appends to these vectors.
    results <- method(
      quantile * 2,
      true_values[1:cv_init_training],
      quantiles_low[1:cv_init_training],
      quantiles_high[1:cv_init_training]
    )

    lower_bound_updated <- results$lower_bound
    upper_bound_updated <- results$upper_bound

    margin <- results$margin
    quantiles_low_updated <- c(lower_bound_updated, (quantiles_low[cv_init_training + 1] - margin))
    quantiles_high_updated <- c(upper_bound_updated, (quantiles_high[cv_init_training + 1] + margin))

    # 2. The loop goes increases the training set by one observation each step
    #    and computes the next validation set
    #    prediction. This is done by rerunning cqr with the new observations set and extracting the margin.
    #    Then with the new margin the one horizon step ahead prediction is updated.
    for (training_length in (cv_init_training + 1):(length(true_values) - 1)) {
      results <- method(
        quantile * 2,
        true_values[1:training_length],
        quantiles_low[1:training_length],
        quantiles_high[1:training_length]
      )

      margin <- results$margin
      quantiles_low_updated <- c(quantiles_low_updated, (quantiles_low[training_length + 1] - margin))
      quantiles_high_updated <- c(quantiles_high_updated, (quantiles_high[training_length + 1] + margin))
    }
  }

  df_updated <- replace_combination(
    df, model, location, target_type, horizon, quantile,
    quantiles_low_updated, quantiles_high_updated
  )

  return(df_updated)
}






update_predictions <- function(df, method,
                               models = NULL, locations = NULL, target_types = NULL, horizons = NULL, quantiles_below_median = NULL,
                               cv_init_training = NULL) {
  # TODO: Write general error message function validate_inputs()
  # for inputs models, locations, target_types, horizons, quantiles_below_median
  if (!all(models %in% unique(df$model))) {
    stop("At least one of the input models is not contained in the input data frame.")
  }
  
  if (!all(locations %in% unique(df$location))) {
    stop("At least one of the input locations is not contained in the input data frame.")
  }

  # Preprocessing the df and inputs
  preprocessed_list <- preprocess_df(
    df = df, models = models, locations = locations, target_types = target_types,
    horizons = horizons, quantiles_below_median = quantiles_below_median
  )

  df <- preprocessed_list$df
  models <- preprocessed_list$models
  locations <- preprocessed_list$locations
  target_types <- preprocessed_list$target_types
  horizons <- preprocessed_list$horizons
  quantiles_below_median <- preprocessed_list$quantiles_below_median

  df_updated <- df

  for (model in models) {
    for (location in locations) {
      for (target_type in target_types) {
        for (horizon in horizons) {
          for (quantile in quantiles_below_median) {
            df_updated <- update_subset(df_updated, method, model, location, target_type, horizon, quantile, cv_init_training)
          }
        }
      }
    }
  }

  return(df_updated)
}
