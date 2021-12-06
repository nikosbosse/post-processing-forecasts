# Need to write a testing function that checks if for a training_length set the update_predictions behaves differently

#TODO: Decide how to name the method and where to store the info about the length of the training set: additional column for the training length
#TODO: Decide how to handle location variable. we will handle location like we handle models, it is passed as list from crossval to update_predictions where we loop over locations
#TODO: Decide in which format to store the information as the file gets very large very fast: we keep this format but we filter the df passed so that we only consider models and locations that are looped over. this makes each of the df smaller. the filtering in a small helper function 


cross_validation <- function(df, models, method, training_lengths){
  
  # We make sure the target_end_date used to sort the df is a date.
  # This is required so that the observations are in the correct order so that training and validation set
  # can be correctly identified in the update_subset
  df <- df |> dplyr::mutate(target_end_date = lubridate::ymd(target_end_date)) 
  
  # Defining a new dataframe holding the original vlaues which is marked by the method column.
  # The following loop appends to this dataframe.
  df_save <- df
  df_save["method"] <- "original"
  
  for (training_length in training_lengths){
    
    cat("Running Model with training length:", training_length)
    
    # Updating the predictions and storing them in a new updated dataframe.
    df_updated <- update_predictions(df, method=method, models=models, training_length=training_length)
    
    # Renaming the method of the updated dataframe according to the method and traning set length
    df_updated["method"] <- paste0(method,"_updated_training_length_",training_length)
    
    # Binding the updated dataframe to the bottom of the full dataframe
    df_save <- dplyr::bind_rows(df_save,df_updated)
    
  }
  return(df_save)
}

