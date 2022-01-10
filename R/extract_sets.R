#' @importFrom rlang .data

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### extract set function used for the training and validation set

extract_set <- function(df_combined, set = c("train", "validation")) {
  rlang::arg_match(arg = set, values = c("train", "validation"))

  # extract the length of the validation set
  cv_init_training <- attr(df_combined, "cv_init_training")

  # checking where target_end_date is a date variable, NA are omitted and all dates are unique
  # if not this is handled and a warning is returned
  if (all((as.Date(stats::na.omit(unique(df_combined$target_end_date))) == df_combined$target_end_date))) {
    warning("target_end_date may not be specifiyed correctly.
            Check whether it is a date variable, NA are omitted and all dates are unique.")
    sorted_target_end_dates <- sort(df_combined$target_end_date)
  } else {
    sorted_target_end_dates <- sort(as.Date(stats::na.omit(unique(df_combined$target_end_date))))
  }

  # based on the length of the validation set extract the dates of the validation set
  sorted_target_end_dates <- sort(df_combined$target_end_date)

  # extracting the target end dates of the set in question
  set_target_end_date <- ifelse(
    set == "train",
    sorted_target_end_dates[(1:cv_init_training)],
    sorted_target_end_dates[-(1:cv_init_training)]
  )

  # Restricting the combined df to only the set in question
  df_combined |>
    change_to_date(target_end = TRUE) |>
    dplyr::filter(.data$target_end_date %in% set_target_end_date)
}


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### extracting the validation set from the combined dataset

extract_validation_set <- function(df_combined) {
  extract_set(df_combined, set = "validation")
}


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### extracting the training set from the combined dataset

extract_training_set <- function(df_combined) {
  extract_set(df_combined, set = "train")
}
