# df <- readr::read_csv(("data/full-data-uk-challenge.csv")) |>
#   dplyr::filter(stringr::str_detect(model, "epi"))
# cqr <- select_method("cqr")
# cqr(alpha = 0.05, df)

select_method <- function(method) {
  # add all methods as named vector
  implemented_methods <- c(cqr = cqr)
  implemented_methods[[method]]
}



# df <- readr::read_csv(("data/full-data-uk-challenge.csv")) |>
#   dplyr::filter(stringr::str_detect(model, "epi"))
# collect_predictions(original = df, cqr = df)

collect_predictions <- function(...) {
  dplyr::bind_rows(..., .id = "method")
}



update_subset <- function(df, method, model, target_type, horizon, quantile) {
  # for nicer function input names,
  # input names equal to column names are a little painful with tidyverse functions
  # => temporary variables with different names
  mod <- model
  t <- target_type
  h <- horizon
  q <- quantile

  quantiles_low <- dplyr::filter(
    df, model == mod & target_type == t & horizon == h & quantile == q
  )$prediction
  
  quantiles_high <- dplyr::filter(
    df, model == mod & target_type == t & horizon == h & quantile == 1 - q
  )$prediction
  
  true_values <- dplyr::filter(
    df, model == mod & target_type == t & horizon == h & quantile == q
  )$true_value
  
  date <- dplyr::filter(
    df, model == mod & target_type == t & horizon == h & quantile == q
  )$forecast_date
  
  date <- as.Date(date)
  
  result <- cqr(quantile * 2, true_values, quantiles_low, quantiles_high)
  
  quantiles_low_updated <- result$lower_bound
  quantiles_high_updated <- result$upper_bound
  
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


update_predictions <- function(df, method, models) {
  method <- select_method(method = method)
  # make function work for single model
  models <- c(models)

  horizons <- unique(df$horizon)[-1]
  quantiles_below_median <- unique(df$quantile)[unique(df$quantile) < 0.5][-1]
  target_types <- unique(df$target_type)

  df_updated <- df

  for (model in models) {
    for (target_type in target_types) {
      for (horizon in horizons) {
        for (quantile in quantiles_below_median) {
          df_updated <- update_subset(df_updated, method, model, target_type, horizon, quantile)
        }
      }
    }
  }

  return(df_updated)
}
