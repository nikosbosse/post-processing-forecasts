
cross_validation_forecasting <- function(df, models, locations, method, training_length_min){ #TODO: training_length_min rename. das wird zum crossvalidation parameter und bei Null macht es cqr auf die ganzen daten
  
  # make function work for single model and signle locations
  models <- c(models)
  locations <- c(locations)
  
  # Filtering out all models and locations that are not updated
  df <- df |> 
    filter_models(models) |> 
    filter_locations(locations)
  
  # We make sure the target_end_date used to sort the df is a date.
  # This is required so that the observations are in the correct order so that training and validation set
  # can be correctly identified in the update_subset
  df <- df |> dplyr::mutate(target_end_date = lubridate::ymd(target_end_date)) #TODO: change to date helper function nutzen
  
  method <- select_method(method = method)
  
  horizons <- na.omit(unique(df$horizon))
  quantiles_below_median <- na.omit(unique(df$quantile)[unique(df$quantile) < 0.5])
  target_types <- na.omit(unique(df$target_type))
  
  df_updated <- df
  
  for (loc in locations) {
    for (mod in models) {
      for (t in target_types) {
        for (h in horizons) {
          for (q in quantiles_below_median) {
            #for (training_length in training_lengths){
            
            #print("Running for training length: ", training_length)
            
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
            
            
            #quantiles_low_df_updated <- dplyr::filter(
            #  df, location == loc & model == mod & target_type == t & horizon == h & quantile == q 
            #) |> dplyr::arrange(target_end_date) |> dplyr::pull(prediction)
            #
            #quantiles_high_df_updated <- dplyr::filter(
            #  df, location == loc & model == mod & target_type == t & horizon == h & quantile == 1 - q
            #) |> dplyr::arrange(target_end_date) |> dplyr::pull(prediction)
            
            #print("Relevant Data is extracted.")
            
            
            results <- method(q * 2,
                             true_values[0:training_length_min], 
                             quantiles_low_df[0:training_length_min], 
                             quantiles_high_df[0:training_length_min])
            
            lower_bound_updated <- results$lower_bound
            upper_bound_updated <- results$upper_bound
            
            margin <- results$margin
            quantiles_low_updated <- c(lower_bound_updated,(quantiles_low_df[training_length_min+1]-margin))
            quantiles_high_updated <- c(upper_bound_updated,(quantiles_high_df[training_length_min+1]+margin))
            
            
            for (training_length in seq((training_length_min+1),(length(true_values)-1),1)){
              
              results <- method(q * 2,
                               true_values[1:training_length], 
                               quantiles_low_df[1:training_length], 
                               quantiles_high_df[1:training_length])
              
              margin <- results$margin
              
              quantiles_low_updated <- c(quantiles_low_updated,(quantiles_low_df[training_length+1]-margin))
              quantiles_high_updated <- c(quantiles_high_updated,(quantiles_high_df[training_length+1]+margin))
              
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
            
            #}
          }
        }
      }
    }
  }
    
  return(df_updated)
}

