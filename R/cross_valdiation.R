cross_validation <- function(df, initial_training_length=40){
  
  #defining an intervall score table with values no postprocessing, then starting from 40 one step increases
  # in the size of the training set till there is no more validation set
  # intervall score represents the score over the entire data e.g. it should fall consitently the larger the trainingset becomes
  intervall_scores_table <- data.frame(training_length=c("no postprocessing",seq(40,73,1),"full training"),interval_score=c("no postprocessing",seq(40,73,1),"full training"))
  
  # Determining the intervall score for the basic df without postprocessing
  scores_combined <- df |>
    filter(model == !!"epiforecasts-EpiExpert", target_type == "Cases") |>
    eval_forecasts(summarise_by = c("model", "target_type"))
  
  # Including the intervall score for the basic df without postprocessing into the results table
  intervall_scores_table |> dplyr::mutate(interval_score = replace(
    interval_score,
    training_length == "no postprocessing", 
    values = scores_combined$interval_score
  ))
  
  for (training_length in seq(initial_training_length,73,1){
    df_updated <- update_predictions(df, method, models, training_length=training_length)
    
    intervall_scores_table |> dplyr::mutate(interval_score = replace(
      interval_score,
      training_length == "no postprocessing", 
      values = scores_combined$interval_score
    ))
    
  }
  return(intervall_scores_table)
}


crossvalidation(df=df_eu, initial_training_length=40)
  
  



# Possible issue: if we pass a training length than we assume that the data is ordered regarding the dates
# We should not do this. That why we should rather pass a date or sort by the date at least


# Need to write a testing function that checks if for a training_length set the update_predictions behaves differently