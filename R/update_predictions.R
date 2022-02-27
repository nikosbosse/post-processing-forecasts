update_predictions <- function(df, methods = c(
                                 "cqr", "qsa_uniform",
                                 "qsa_flexibel", "qsa_flexibel_symmetric"
                               ), models = NULL, locations = NULL,
                               target_types = NULL, horizons = NULL,
                               quantiles = NULL, cv_init_training = NULL,
                               penalty_weight = NULL, optim_method = "BFGS", 
                               lower_bound_optim = -Inf, upper_bound_optim = Inf,
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

  for (method in c(methods)) {
    # start with original data frame for each method
    df_updated <- df_preprocessed

    for (model in models) {
      for (location in locations) {
        if (verbose) {
          cat(
            "method = ", method, " | model = ", model, " | location = ",
            location, "\n",
            sep = ""
          )
        }
        for (target_type in target_types) {
          for (horizon in horizons) {

            # distinction of which method we use at this step in the nested loop
            # as cqr loops over qunatiles but qsa uses all quantiles in one run
            if (method == "cqr") {
              quantiles <- quantiles[quantiles < 0.5]

              for (quantile in quantiles) {
                # only one method => does not require 'method' argument
                df_updated <- update_subset_cqr(
                  df_updated, model, location, target_type, horizon,
                  quantile, cv_init_training
                )
              }
            }

            if (stringr::str_detect(method, "qsa")) {
              df_updated <- update_subset_qsa(
                df_updated, method, model, location, target_type, horizon,
                cv_init_training, penalty_weight, optim_method, lower_bound_optim, upper_bound_optim
              ) # no quantile is passed
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
