fix_quantile_crossing <- function(df_updated, model, location, target_type, horizon) {
  target_end_dates <- unique(df_updated$target_end_date)

  for (date in target_end_dates) {
    # first sort by quantiles, then replace prediction column for each target_end_date with
    # sorted predictions
    df_updated <- df_updated |> dplyr::arrange(.data$target_end_date, .data$quantile)

    df_updated[
      df_updated$model == model &
        df_updated$location == location &
        df_updated$target_type == target_type &
        df_updated$horizon == horizon &
        df_updated$target_end_date == date,
      "prediction"
    ] <- df_updated[
      df_updated$model == model &
        df_updated$location == location &
        df_updated$target_type == target_type &
        df_updated$horizon == horizon &
        df_updated$target_end_date == date,
      "prediction",
      # prevents collapsing to vector, such that arrange() still works
      drop = FALSE
    ] |>
      dplyr::arrange(.data$prediction)
  }

  return(df_updated)
}

wrapper_parallel_update_subset_qsa <- function(model, location, target_type, horizon){
  subset_updated <- parallel_update_subset_qsa(df=df_preprocessed, method=method,
                                               model=model, location=location, target_type=target_type, horizon=horizon, 
                                               cv_init_training=cv_init_training, penalty_weight=penalty_weight, optim_method=optim_method, 
                                               lower_bound_optim=lower_bound_optim, upper_bound_optim=upper_bound_optim, steps_optim=steps_optim)
  return(subset_updated)
}

parallel_update_predictions <- function(df, methods = c(
                                 "cqr", "cqr_asymmetric", "cqr_multiplicative",
                                 "qsa_uniform", "qsa_flexibel", "qsa_flexibel_symmetric"
                               ), models = NULL, locations = NULL,
                               target_types = NULL, horizons = NULL,
                               quantiles = NULL, cv_init_training = NULL,
                               #QSA method arguments
                               penalty_weight = NULL, optim_method = NULL, 
                               lower_bound_optim = 0, upper_bound_optim = 5, steps_optim=0.1,
                               #CQR method arguments
                               regularize_scores = FALSE, constrain_margins = FALSE,
                               return_list = TRUE,
                               verbose = FALSE) {
  # stops function for invalid input values
  validate_inputs(df, methods, models, locations, target_types, horizons, quantiles)
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
  
  #TODO: set standard values to 0 not Null because R cant create a vector only containing the value Null multiple times, see penalty_weight
  #TODO: discuss if parallel processing cqr is sensible

  for (method in c(methods)) {

    df_updated <- df_preprocessed
    if (stringr::str_detect(method, "qsa")) {
      model_vec <- c()
      location_vec <- c()
      target_type_vec <- c()
      horizon_vec <- c()
      method_vec <- c()
      cv_init_training_vec <- c()
      #penalty_weight_vec <- c()
      optim_method_vec <- c()
      lower_bound_optim_vec <- c()
      upper_bound_optim_vec <- c()
      steps_optim_vec <- c()
      for (m in models) {
        for (l in locations) {
          for (t in target_types) {
            for (h in horizons) {
              #subset <- dplyr::filter(df_preprocessed, .data$model == m & .data$location == l & .data$target_type == t & .data$horizon == h)
              #subset_vec <- c(subset_vec,subset)
              model_vec <- c(model_vec,m)
              location_vec <- c(location_vec,l)
              target_type_vec <- c(target_type_vec,t)
              horizon_vec <- c(horizon_vec,h)
              
              method_vec <- c(method_vec, method)
              cv_init_training_vec <- c(cv_init_training_vec,cv_init_training)
              #penalty_weight_vec <- c(penalty_weight_vec,penalty_weight)
              optim_method_vec <- c(optim_method_vec,optim_method)
              lower_bound_optim_vec <- c(lower_bound_optim,lower_bound_optim)
              upper_bound_optim_vec <- c(upper_bound_optim_vec,upper_bound_optim)
              steps_optim_vec <- c(steps_optim_vec,steps_optim)
  
            }}}} 
      
      df_updated <- foreach(m=model_vec,
                            l=location_vec,
                            t=target_type_vec,
                            h=horizon_vec,
                            #subset=subset_vec, 
                            method=method_vec, 
                            cv_init_training=cv_init_training_vec, 
                            #penalty_weight=penalty_weight_vec, 
                            optim_method=optim_method_vec, 
                            lower_bound_optim=lower_bound_optim_vec, 
                            upper_bound_optim=upper_bound_optim_vec, 
                            steps_optim=steps_optim_vec,
                            .combine='rbind') %dopar% {
        subset <- dplyr::filter(df_preprocessed, .data$model == m & .data$location == l & .data$target_type == t & .data$horizon == h)
        penalty_weight <- NULL
        updated_subset <- parallel_update_subset_qsa(subset, method, cv_init_training, penalty_weight, optim_method, lower_bound_optim, upper_bound_optim, steps_optim)
        updated_subset <- fix_quantile_crossing(updated_subset, model, location, target_type, horizon)
        return(updated_subset)}
      
    } else {
      for (m in models) {
        for (l in locations) {
          for (t in target_types) {
            for (h in horizons) {
              # cqr methods use pair of quantiles => only needs lower quantiles
              quantiles <- quantiles[quantiles < 0.5]
              for (quantile in quantiles) {
                df_updated <- update_subset_cqr(
                  df_updated, method, model, location, target_type, horizon,
                  quantile, cv_init_training)
              
              df_updated <- fix_quantile_crossing(df_updated, model, location, target_type, horizon)
              
              }}}}}
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
