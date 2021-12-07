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
update_subset <- function(df, method, model, target_type, horizon, quantile, cv_init_training) {
  # for nicer function input names,
  # input names equal to column names are a little painful with tidyverse functions
  # => temporary variables with different names
  mod <- model
  t <- target_type
  h <- horizon
  q <- quantile

  # To make sure that the predictions and true_values are in the correct order we also arrange by the target_end_date
  # TODO: quantiles_low and true_values into one function as we basically do the same just pic a different variable
  quantiles_low_df <- dplyr::filter(
    df, model == mod & target_type == t & horizon == h & quantile == q
  ) |>
    dplyr::arrange(target_end_date) |>
    dplyr::pull(prediction)

  quantiles_high_df <- dplyr::filter(
    df, model == mod & target_type == t & horizon == h & quantile == 1 - q
  ) |>
    dplyr::arrange(target_end_date) |>
    dplyr::pull(prediction)

  true_values <- dplyr::filter(
    df, model == mod & target_type == t & horizon == h & quantile == q
  ) |>
    dplyr::arrange(target_end_date) |>
    dplyr::pull(true_value)

  if (is.null(cv_init_training)) {
    # By default the training length is equal to NULL and therefore equal to the complete data.
    # e.g. by default not training and validation set
    result <- method(
      quantile * 2, # method was cqr in prior version
      true_values,
      quantiles_low,
      quantiles_high
    )

    margin <- result$margin
    quantiles_low_updated <- result$lower_bound
    quantiles_high_updated <- result$upper_bound
  } else {
    # This Section runs the Time Series Cross validation.
    # 1. It runs the method on the training set and updates all values of the training set. 
    #    Then by using the margin it makes the first prediction in the validation set.
    #    The training values and the first adjusted validation set valued are stored in the lists
    #    quantiles_low_updated and quantiles_high_updated. The following section appends to these vectors.
    results <- method(q * 2,
                      true_values[0:cv_init_training], 
                      quantiles_low_df[0:cv_init_training], 
                      quantiles_high_df[0:cv_init_training])
    
    lower_bound_updated <- results$lower_bound
    upper_bound_updated <- results$upper_bound
    
    margin <- results$margin
    quantiles_low_updated <- c(lower_bound_updated,(quantiles_low_df[cv_init_training+1]-margin))
    quantiles_high_updated <- c(upper_bound_updated,(quantiles_high_df[cv_init_training+1]+margin))
    
    # 2. The loop goes increases the training set by one observation each step and computes the next validation set
    #    prediction. This is done by rerunning cqr with the new observations set and extracting the margin.
    #    Then with the new margin the one horizon step ahead prediction is updated.
    for (training_length in seq((cv_init_training+1),(length(true_values)-1),1)){
      
      results <- method(q * 2,
                        true_values[1:training_length], 
                        quantiles_low_df[1:training_length], 
                        quantiles_high_df[1:training_length])
      
      margin <- results$margin
      
      quantiles_low_updated <- c(quantiles_low_updated,(quantiles_low_df[training_length+1]-margin))
      quantiles_high_updated <- c(quantiles_high_updated,(quantiles_high_df[training_length+1]+margin))
      
    }}


  # TODO: add .data$ in each select of tidyverse stuff. its from rlang. Check how joel did it
  df_updated <- df |>
    dplyr::mutate(prediction = replace(
      prediction,
      model == mod & target_type == t & horizon == h & quantile == q,
      values = quantiles_low_updated
    )) |>
    dplyr::mutate(prediction = replace(
      prediction,
      model == mod & target_type == t & horizon == h & quantile == 1 - q,
      values = quantiles_high_updated
    ))

  return(df_updated)
}


update_predictions <- function(df, method, models, locations, cv_init_training = NULL) {
  if (!all(models %in% unique(df$model))) {
    stop("At least one of the input models is not contained in the input data frame.")
  }
  
  # make function work for single model and signle locations
  models <- c(models)
  locations <- c(locations)
  
  # Filtering out all models and locations that are not updated
  df <- df |> 
    filter_models(models) |> 
    filter_locations(locations)

  method <- select_method(method = method)

  horizons <- na.omit(unique(df$horizon))
  quantiles_below_median <- na.omit(unique(df$quantile)[unique(df$quantile) < 0.5])
  target_types <- na.omit(unique(df$target_type))

  df_updated <- df

  for (model in models) {
    for (target_type in target_types) {
      for (horizon in horizons) {
        for (quantile in quantiles_below_median) {
          df_updated <- update_subset(df_updated, method, model, target_type, horizon, quantile, cv_init_training)
        }
      }
    }
  }

  return(df_updated)
}
