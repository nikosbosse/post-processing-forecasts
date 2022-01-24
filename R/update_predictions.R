select_method <- function(method) {
  # add all methods as named vector
  implemented_methods <- c(cqr = cqr)

  if (!(method %in% names(implemented_methods))) {
    stop(stringr::str_glue("{method} is not an implemented post processing method."))
  }

  implemented_methods[[method]]
}



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

  # set training length as attribute for plotting vertical line
  attr(df_updated, "cv_init_training") <- cv_init_training

  return(df_updated)
}



# TODO: discuss how to structure package as qsa needs a separate update_subset function. We could for example exchange the update_subset function by a update_subset_qsa and a update_subset_cqr function. Or we could merge them together somehow, but how to then solve the different requirement of a qunatile or not a quantile
# TODO: after discussing package structure put functionality within update_subset_qsa into smaller functions
# TODO: after having multiple funciotns for qsa write unit tests
update_subset_qsa <- function(df, method, model, location, target_type, horizon, cv_init_training) {
  method <- select_method(method = method)
  
  #Moved the extraction of the dataframe into the if else condition as we use the data as it comes out of filter
  
  if (is.null(cv_init_training)) {
    # By default cv_init_training is equal to NULL and therefore equal to the complete data.
    # e.g. by default no split in training and validation set
    
    subset <- dplyr::filter(df, model == m & location == l & target_type == t & horizon == h)
    
    # optim minimizes the wrapper function
    # We can get a hessian if we want but it takes additional compute time
    # starts with spread factor of 1 e.g. no spread necessary
    optim_results <- optim(par=1, fn=wrapper, subset=subset, gr=NULL, method="BFGS")#, hessian=T)
    optimal_spread_factor <- optim_results$par
    #TODO: Decide if it is makes sense to write a gradient function that gives back the gradient dependent on the subset at that time point
    
    #function to apply optimal spread factor to data
    subset_updated <- quantile_spreads_adjustment(subset=subset,spread_factor= optimal_spread_factor)
    
  } else {
    # This Section runs the Time Series Cross validation.
    # 1. It runs the method on the training set and updates all values of the training set.
    
    # get initial training period
    subset <- dplyr::filter(df, model == m & location == l & target_type == t & horizon == h & target_end_date %in% sort(unique(subset$target_end_date))[0:cv_init_training])
    
    optim_results <- optim(par=1, fn=wrapper, subset=subset, gr=NULL, method="BFGS")#, hessian=T)
    optimal_spread_factor <- optim_results$par
    
    subset_updated <- quantile_spreads_adjustment(subset=subset,spread_factor= optimal_spread_factor)
    
    # 2. The loop goes increases the training set by one observation each step
    #    and computes the next validation set
    #    prediction. This is done by rerunning cqr with the new observations set and extracting the margin.
    #    Then with the new margin the one horizon step ahead prediction is updated.
    for (training_length in (cv_init_training + 1):(length(true_values) - 1)) {
      
      # gets subset for training length
      subset <- dplyr::filter(df, model == m & location == l & target_type == t & horizon == h & target_end_date %in% sort(unique(subset$target_end_date))[0:training_length])
      
      # for faster computation it starts optimization at the optimal spread factor of the last iteration
      optim_results <- optim(par=optimal_spread_factor, fn=wrapper, subset=subset, gr=NULL, method="BFGS")#, hessian=T)
      optimal_spread_factor <- optim_results$par
      
      subset_updated <- quantile_spreads_adjustment(subset=subset,spread_factor= optimal_spread_factor)
      
    }
  }
  
  #TODO: adjust the replace_combination function for the subset we get from quantile spreads
  # The below function should work if df and subset_updated are sorted the same way. Check this
  
  #TODO: write unit test for this part of the code
  
  # Make sure the updated subset is arranged by quantiles and then target end dates 
  # That way we can pass its prediction column to the updated df directly
  subset_updated <- subset_updated |> 
    dplyr::arrange(.data$quantile, .data$target_end_date)
  
  # Again we first arrange the data to make we can pass the prediction column to the updated df directly
  df_updated <- df |> 
    dplyr::arrange(.data$quantile, .data$target_end_date) |> 
    dplyr::mutate(prediction = replace(
      .data$prediction,
      .data$model == m & .data$location == l & .data$target_type == t & .data$horizon == h, #no quantile specified in quantile spread post processing
      values = subset_updated$prediction
    ))
  
  # set training length as attribute for plotting vertical line
  attr(df_updated, "cv_init_training") <- cv_init_training
  
  return(df_updated)
}



update_predictions <- function(df, methods,
                               models = NULL, locations = NULL, target_types = NULL,
                               horizons = NULL, quantiles = NULL,
                               cv_init_training = NULL, return_list = TRUE) {
  # stops function for invalid input values
  validate_inputs(df, models, locations, target_types, horizons, quantiles)
  df <- validate_dates(df)
  
  # Preprocessing the df and inputs
  preprocessed_list <- preprocess_df(
    df, models, locations, target_types, horizons, quantiles
  )
  
  df_preprocessed <- preprocessed_list$df
  models <- preprocessed_list$models
  locations <- preprocessed_list$locations
  target_types <- preprocessed_list$target_types
  horizons <- preprocessed_list$horizons
  quantiles <- preprocessed_list$quantiles
  
  # store updated dataframes for all methods in list
  updated_list <- list()
  
  for (method in c(methods)) {
    # start with original data frame for each method
    df_updated <- df_preprocessed
    
    #TODO: Discuss that now the nested loop is within the method condition
    # specifically for cqr select only quantiles below median
    if (method == "cqr") {
      quantiles <- quantiles[quantiles < 0.5]
      
      for (model in models) {
        for (location in locations) {
          for (target_type in target_types) {
            for (horizon in horizons) {
              for (quantile in quantiles) {
                df_updated <- update_subset(df_updated, method, model, location, target_type, horizon, quantile, cv_init_training)
              }
            }
          }
        }
      }
    }
    # specifically for qsa select only quantiles below median
    if (method == "qsa") {
      quantiles <- quantiles[quantiles < 0.5]
      
      for (model in models) {
        for (location in locations) {
          for (target_type in target_types) {
            for (horizon in horizons) {
              update_subset_qsa(df_updated, method, model, location, target_type, horizon, cv_init_training) #no quantile is passed
            }
          }
        }
      }
    }
    # updated data frames are named after corresponding method
    updated_list[[method]] <- df_updated
  }
  
  # return list of original and all updated data frames (one for each method)
  if (return_list) {
    return(c(list(original = df_preprocessed), updated_list))
  }
  
  # return only first updated data frame (old behaviour unchanged)
  return(updated_list[[1]])
}



collect_predictions <- function(...) {
  df_combined <- dplyr::bind_rows(..., .id = "method")

  # always convert to list to safely check for type of input '...'
  df_list <- list(...)

  # if input was already a list of dataframes, loop through input list
  if (class(df_list[[1]]) == "list") {
    df_list <- df_list[[1]]
  }

  # keep cv_init_training attribute from second input argument
  for (df in df_list) {
    cv_init_training <- attr(df, "cv_init_training")
    if (!is.null(cv_init_training)) {
      attr(df_combined, "cv_init_training") <- cv_init_training
    }
  }
  return(df_combined)
}
