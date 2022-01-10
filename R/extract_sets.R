#' @importFrom rlang .data


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### extract set function used for the training and validation set

extract_set <- function(df_combined,set) {
  # extract the length of the validation set
  cv_init_training <- attr(df_combined, "cv_init_training")
  
  # checking where target_end_date is a data variable, NA are omitted and all dates are unique
  # if not this is handeld and a warning is returned
  if (all((as.Date(na.omit(unique(df_combined$target_end_date))) == df_combined$target_end_date))) {
    warning("target_end_date may not be specifiyed correctly. 
            Check whether it is a data variable, NA are omitted and all dates are unique.")
    sorted_target_end_dates <- sort(df_combined$target_end_date)
  } else {
    sorted_target_end_dates <- sort(as.Date(na.omit(unique(df_combined$target_end_date))))
  }
  
  
  # based on the length of the validation set extract the dates of the validation set
  sorted_target_end_dates <- sort(df_combined$target_end_date)
  
  # extracting the target end dates of the set in question
  if (set == "validation") {
    set_target_end_date <- sorted_target_end_dates[-(1:cv_init_training)]
  }else if (set == "train") {
    set_target_end_date <- sorted_target_end_dates[(1:cv_init_training)]
  } else {
  stop("set must be validation or train.")
  }
  
  # Restricting the combined df to only the set in question
  df_combined$target_end_date <- as.Date(df_combined$target_end_date)
  df_combined_val <- df_combined |>
    dplyr::filter(target_end_date %in% set_target_end_date)
  
  return(df_combined_val)
}


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### extracting the validation set from the combined dataset

extract_validation_set <- function(df_combined) {
  
  df_combined_val <- extract_set(df_combined,set="validation")
  
  return(df_combined_val)
}


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### extracting the training set from the combined dataset

extract_training_set <- function(df_combined) {
  
  df_combined_train <- extract_set(df_combined,set="train")
  
  return(df_combined_train)
}

