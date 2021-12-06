
cross_validation_forecasting <- function(df, models, locations, method, training_lengths){
  
  # Filtering out all models and locations that are not updated
  df <- df |> 
    filter_models(models) |> 
    filter_locations(locations)
  
  # We make sure the target_end_date used to sort the df is a date.
  # This is required so that the observations are in the correct order so that training and validation set
  # can be correctly identified in the update_subset
  df <- df |> dplyr::mutate(target_end_date = lubridate::ymd(target_end_date)) 
  
  method <- select_method(method = method)
  # make function work for single model
  models <- c(models)
  
  horizons <- na.omit(unique(df$horizon))
  quantiles_below_median <- na.omit(unique(df$quantile)[unique(df$quantile) < 0.5])
  target_types <- na.omit(unique(df$target_type))
  
  df_updated <- df
  
  for (loc in locations) {
    for (model in models) {
      for (target_type in target_types) {
        for (horizon in horizons) {
          for (quantile in quantiles_below_median) {
            for (training_length in training_lengths){
              
              # added location as a condition
              quantiles_low_df <- dplyr::filter(
                df, location == loc & model == mod & target_type == t & horizon == h & quantile == q 
              ) |> dplyr::arrange(target_end_date) |> dplyr::pull(prediction)
              
              quantiles_high_df <- dplyr::filter(
                df, location == loc & model == mod & target_type == t & horizon == h & quantile == 1 - q
              ) |> dplyr::arrange(target_end_date) |> dplyr::pull(prediction)
              
              
              true_values <- dplyr::filter(
                df, location == loc & model == mod & target_type == t & horizon == h & quantile == q
              ) |> dplyr::arrange(target_end_date) |> dplyr::pull(true_value)
              
              
              quantiles_low_df_updated <- dplyr::filter(
                df, location == loc & model == mod & target_type == t & horizon == h & quantile == q 
              ) |> dplyr::arrange(target_end_date) |> dplyr::pull(prediction)
              
              quantiles_high_df_updated <- dplyr::filter(
                df, location == loc & model == mod & target_type == t & horizon == h & quantile == 1 - q
              ) |> dplyr::arrange(target_end_date) |> dplyr::pull(prediction)
              
              
              if (training_length == min(training_lengths)){
                
                result <- method(quantile * 2,
                                true_values[1:training_length], 
                                quantiles_low_df[1:training_length], 
                                quantiles_high_df[1:training_length])
                
                margin <- result$margin
                
                # In the first run we update the full training set quantiles by the respective margin
                # Further we also update the first observation of the validation set by the same margin
                quantiles_low_updated <- c((quantiles_low_df[1:(training_length+1)] - margin),
                                           quantiles_high_df[(training_length+2):length(quantiles_high)] )
                
                quantiles_high_updated <- c((quantiles_high_df[1:(training_length+1)] + margin),
                                            quantiles_high_df[(training_length+2):length(quantiles_high)] )
                
                
              }
              else{
                
                result <- method(quantile * 2,
                                 true_values[1:training_length], 
                                 quantiles_low_df[1:training_length], 
                                 quantiles_high_df[1:training_length])
                
                margin <- result$margin
                
                # The quantiles of all values up till the current training length, including the training set and all 
                # already updated values of the validation set are left the same.
                # The value one after the length of the training set is the currently first value in the validation set and
                # updated with the current margin.
                # all following values are left the same. here it doesn't matter whether we use df or df_updated as they have the same values for now.
                quantiles_low_updated <- c(quantiles_low_df_updated[1:training_length], 
                                           (quantiles_low_df_updated[(training_length+1)] - margin),
                                           quantiles_high_df_updated[(training_length+2):length(quantiles_high)] )
                
                quantiles_high_updated <- c(quantiles_high_df_updated[1:training_length], 
                                            (quantiles_high_df_updated[(training_length+1)] + margin),
                                            quantiles_high_df_updated[(training_length+2):length(quantiles_high)] )
              }
              #TODO: change logic of full loop and only always change one value except in the first step with training set  
              df_updated <- df_updated |> #here it doesnt matter whether we update df or df_updated as we incldue a full set of new predictions
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
              
              }
          }
        }
      }
    }
  }
    
  return(df_updated)
}

