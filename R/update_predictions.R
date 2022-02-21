fix_quantile_crossing <- function(df_updated, model, location, target_type, horizon) {
  target_end_dates <- unique(df_updated$target_end_date)

  for (target_end_date in target_end_dates) {
    # first sort by quantiles, then replace prediction column for each target_end_date with
    # sorted predictions
    df_updated <- df_updated |> dplyr::arrange(.data$target_end_date, .data$quantile)

    df_updated[
      df_updated$model == model &
        df_updated$location == location &
        df_updated$target_type == target_type &
        df_updated$horizon == horizon &
        df_updated$target_end_date == target_end_date,
      "prediction"
    ] <- df_updated[
      df_updated$model == model &
        df_updated$location == location &
        df_updated$target_type == target_type &
        df_updated$horizon == horizon &
        df_updated$target_end_date == target_end_date,
      "prediction"
    ] |>
      dplyr::arrange(.data$prediction)
  }

  return(df_updated)
}


update_predictions <- function(df, methods = c(
                                 "cqr", "cqr_asymmetric", "cqr_multiplicative",
                                 "qsa_uniform", "qsa_flexibel", "qsa_flexibel_symmetric"
                               ), models = NULL, locations = NULL,
                               target_types = NULL, horizons = NULL,
                               quantiles = NULL, cv_init_training = NULL,
                               penalty_weight = NULL, return_list = TRUE,
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

  for (method in c(methods)) {
    # start with original data frame for each method
    df_updated <- df_preprocessed

    for (model in models) {
      for (location in locations) {
        if (verbose) {
          cat(
            "method = ", method, " | model = ", model, " | location = ", location, "\n",
            sep = ""
          )
        }
        for (target_type in target_types) {
          for (horizon in horizons) {
            # qsa methods use all quantiles in one run
            if (stringr::str_detect(method, "qsa")) {
              df_updated <- update_subset_qsa(
                df_updated, method, model, location, target_type, horizon,
                cv_init_training, penalty_weight
              )
            } else {
              # cqr methods use pair of quantiles => only needs lower quantiles
              quantiles <- quantiles[quantiles < 0.5]
              for (quantile in quantiles) {
                df_updated <- update_subset_cqr(
                  df_updated, method, model, location, target_type, horizon, 
                  quantile, cv_init_training
                )
              }
            }
            # df_updated <- fix_quantile_crossing(
            #   df_updated, model, location, target_type, horizon
            # )
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