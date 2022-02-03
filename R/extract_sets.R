#' @importFrom rlang .data

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### extract set function used for the training and validation set

extract_set <- function(df_combined, set = c("train", "validation"),
                        cv_init_training) {
  rlang::arg_match(arg = set, values = c("train", "validation"))

  # extract the length of the validation set
  if (is.null(cv_init_training)) {
    cv_init_training <- attr(df_combined, "cv_init_training")
  }

  # checking where target_end_date is a date variable, NA are omitted and all dates are unique
  # if not this is handled and a warning is returned
  actual_dates <- unique(df_combined$target_end_date)
  correct_dates <- lubridate::ymd(stats::na.omit(unique(df_combined$target_end_date)))

  if (all(correct_dates == actual_dates)) {
    sorted_target_end_dates <- sort(actual_dates)
  } else {
    warning(paste0(
      "target_end_date may not be specifiyed correctly.\n",
      "Check whether it is a date variable, NA are omitted and all dates are unique."
    ))
    sorted_target_end_dates <- sort(correct_dates)
  }

  # extracting the target end dates of the set in question
  # using ifelse in one line leads to strange behaviour
  if (set == "train") {
    set_target_end_date <- sorted_target_end_dates[(1:cv_init_training)]
  } else {
    set_target_end_date <- sorted_target_end_dates[-(1:cv_init_training)]
  }

  # Restricting the combined df to only the set in question
  df_combined |>
    change_to_date(target_end = TRUE) |>
    dplyr::filter(.data$target_end_date %in% set_target_end_date)
}


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### extracting the validation set from the combined dataset

extract_validation_set <- function(df_combined, cv_init_training = NULL) {
  extract_set(df_combined, set = "validation", cv_init_training = cv_init_training)
}


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### extracting the training set from the combined dataset

extract_training_set <- function(df_combined, cv_init_training = NULL) {
  extract_set(df_combined, set = "train", cv_init_training = cv_init_training)
}
