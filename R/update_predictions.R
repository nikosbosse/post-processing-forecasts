# df <- readr::read_csv(("data/full-data-uk-challenge.csv")) |>
#   dplyr::filter(stringr::str_detect(model, "epi"))
# cqr <- select_method("cqr")
# cqr(alpha = 0.05, df)

select_method <- function(method) {
  # add all methods as named vector
  implemented_methods <- c(cqr = cqr)
  
  if (!(method %in% names(implemented_methods))) {
    stop(stringr::str_glue("{method} is not an implemented post processing method."))
  }
  
  implemented_methods[[method]]
}



# df <- readr::read_csv(("data/full-data-uk-challenge.csv")) |>
#   dplyr::filter(stringr::str_detect(model, "epi"))
# collect_predictions(original = df, cqr = df)

collect_predictions <- function(...) {
  dplyr::bind_rows(..., .id = "method")
}


# TODO: fails unit test when data is imported with tidyverse readr::read_csv()
update_subset <- function(df, method, model, target_type, horizon, quantile, training_length) {
  # for nicer function input names,
  # input names equal to column names are a little painful with tidyverse functions
  # => temporary variables with different names
  mod <- model
  t <- target_type
  h <- horizon
  q <- quantile
  
  # To make sure that the predictions and true_values are in the correct order we also arrange by the target_end_date
  quantiles_low <- dplyr::filter(
    df, model == mod & target_type == t & horizon == h & quantile == q 
  ) |> dplyr::arrange(target_end_date) |> dplyr::pull(prediction)
  
  quantiles_high <- dplyr::filter(
    df, model == mod & target_type == t & horizon == h & quantile == 1 - q
  ) |> dplyr::arrange(target_end_date) |> dplyr::pull(prediction)
  
  true_values <- dplyr::filter(
    df, model == mod & target_type == t & horizon == h & quantile == q
  ) |> dplyr::arrange(target_end_date) |> dplyr::pull(true_value)
  
  #TODO: discuss if date is not required and thus can be deleted here.
  #date <- dplyr::filter(
  #  df, model == mod & target_type == t & horizon == h & quantile == q
  #)$forecast_date
  
  #date <- as.Date(date)
  
  # By default the training length is equal to this string and therefor equal to the complete data.
  # e.g. by default not training and validation set
  if (training_length =="complete_data"){
    training_length <- length(true_values)
  }
  
  # Splitting into the case of training and validation set or only a training set
  # The reason is that if training_length == lenngth(true_values) then we dont issues in the quantiles_low[training_length+1:len(quantiles_low)]
  # functionality. e.g. c(1,2,3,4)[5:4] returns  NA  4
  if (training_length < length(true_values)){
    result <- cqr(quantile * 2, 
                  true_values[1:training_length], 
                  quantiles_low[1:training_length], 
                  quantiles_high[1:training_length])
    
    margin <- result$margin
    quantiles_low_updated <- c(result$lower_bound, (quantiles_low[(training_length+1):length(quantiles_low)] - margin))
    quantiles_high_updated <- c(result$upper_bound, (quantiles_high[(training_length+1):length(quantiles_high)] + margin))
    
  } else{
    result <- cqr(quantile * 2, 
                  true_values, 
                  quantiles_low, 
                  quantiles_high)
    
    margin <- result$margin
    quantiles_low_updated <- result$lower_bound
    quantiles_high_updated <- result$upper_bound
    
  }
  
  
  # TODO: add .data$ in each select of tidyverse stuff. its from rlang. Check how joel did it
  df_updated <- df |>
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

  return(df_updated)
}


update_predictions <- function(df, method, models, training_length = "complete_data") {
  
  if (!all(models %in% unique(df$model))) {
    stop("At least one of the input models is not contained in the input data frame.")
  }
  
  # We make sure the target_end_date used to sort the df is a date.
  # This is required so that the observations are in the correct order so that training and validation set
  # can be correctly identified in the update_subset
  df <- df |> dplyr::mutate(target_end_date = as.Date(target_end_date, "%d-%m-%Y")) 
  
  
  method <- select_method(method = method)
  # make function work for single model
  models <- c(models)

  horizons <- na.omit(unique(df$horizon))
  quantiles_below_median <- na.omit(unique(df$quantile)[unique(df$quantile) < 0.5])
  target_types <- na.omit(unique(df$target_type))

  df_updated <- df

  for (model in models) {
    for (target_type in target_types) {
      for (horizon in horizons) {
        for (quantile in quantiles_below_median) {
          df_updated <- update_subset(df_updated, method, model, target_type, horizon, quantile, training_length)
        }
      }
    }
  }

  return(df_updated)
}
