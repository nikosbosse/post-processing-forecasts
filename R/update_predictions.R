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
  
  if (!all(models %in% unique(df$model))) {
    stop("At least one of the input models is not contained in the input data frame.")
  }
  
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
          df_updated <- update_subset(df_updated, method, model, target_type, horizon, quantile)
        }
      }
    }
  }

  return(df_updated)
}
