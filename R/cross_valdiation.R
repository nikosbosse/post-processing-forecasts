#cross_validation <- function(df, initial_training_length=40){
#  
#  # defining an intervall score table with values no postprocessing, then starting from 40 one step increases
#  # in the size of the training set till there is no more validation set
#  # intervall score represents the score over the entire data e.g. it should fall consitently the larger the trainingset becomes
#  intervall_scores_table <- data.frame(training_length=c("no postprocessing",seq(40,73,1),"full training"),interval_score=c("no postprocessing",seq(40,73,1),"full training"))
#  
#  # Determining the intervall score for the basic df without postprocessing
#  score <- df |>
#    filter(model == !!"epiforecasts-EpiExpert", target_type == "Cases") |>
#    eval_forecasts(summarise_by = c("model", "target_type"))
#  
#  # Including the interval score for the basic df without post processing into the results table
#  intervall_scores_table |> dplyr::mutate(interval_score = replace(
#    interval_score,
#    training_length == "no postprocessing", 
#    values = score$interval_score))
#  
#  for (training_length in seq(initial_training_length,73,1)){
#    
#    print(cat("Running Model with training length:", training_length))
#    print(training_length)
#    
#    # Updating the predictions
#    df_updated <- update_predictions(df, method=c("cqr"), models=c("epiforecasts-EpiExpert"), training_length=training_length)
#    
#    # Calculating the interval score
#    score <- df_updated |>
#      filter(model == !!"epiforecasts-EpiExpert", target_type == "Cases") |>
#      eval_forecasts(summarise_by = c("model", "target_type"))
#    
#    print(cat("Appending intervall score:", score$interval_score))
#    print(score$interval_score)
#    
#    # appending the intercal score to the df
#    intervall_scores_table |> dplyr::mutate(interval_score = replace(
#      interval_score,
#      training_length == "no postprocessing", 
#      values = score$interval_score
#    ))
#    
#  }
#  return(intervall_scores_table)
#}
#
#
#cross_validation(df=df_eu, initial_training_length=40)
  
  



# Possible issue: if we pass a training length than we assume that the data is ordered regarding the dates
# We should not do this. That why we should rather pass a date or sort by the date at least


# Need to write a testing function that checks if for a training_length set the update_predictions behaves differently

# Running the loop over all training set lengths for one model in all its combinations takes a long time.
# Therefore I 'll create a loop running over all models in all combinations and then store the resulting dataframe with additional prediction
# columns for each training set length. Then we can just call this dataframe in order to interpret results





cross_validation <- function(df, models, method, training_lengths){
  
  # Defining a new dataframe holding the original vlaues which is marked by the method column.
  # The following loop appends to this dataframe.
  df_save <- df
  df_save["method"] <- "original"
  
  for (training_length in training_lengths){
    
    print(cat("Running Model with training length:", training_length))
    
    # Updating the predictions and storing them in a new updated dataframe.
    df_updated <- update_predictions(df, method=method, models=models, training_length=training_length)
    
    # Renaming the method of the updated dataframe according to the method and traning set length
    df_updated["method"] <- paste(method,"_updated_training_length_",training_length,sep="")
    
    # Binding the updated dataframe to the bottom of the full dataframe
    df_save <- rbind(df_save,df_updated)
    
  }
  return(df_save)
}
