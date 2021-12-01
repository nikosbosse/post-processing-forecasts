# internal function that can be used in other functions to select one of
# multiple implemented methods, takes string of method name as input and returns
# corresponding function

# example:
# df <- readr::read_csv(("data/full-data-uk-challenge.csv")) |>
#   dplyr::filter(stringr::str_detect(model, "epi"))
# cqr <- select_method("cqr")
# cqr(alpha = 0.05, df)

select_method <- function(method) {
  # add all methods as named vector
  implemented_methods <- c(cqr = cqr)
  implemented_methods[[method]]
}


# takes any number of data frames of the same size as input and returns combined
# data frame with added identifier column, each input data frame contains
# prediction from a different post processing method or the original data

# example:
# df <- readr::read_csv(("data/full-data-uk-challenge.csv")) |>
#   dplyr::filter(stringr::str_detect(model, "epi"))
# collect_predictions(original = df, cqr = df)

collect_predictions <- function(...) {
  dplyr::bind_rows(..., .id = "method")
}


# TODO: consistent input name for model
update_subset <- function(df, method, mod, t, h, q) {
  ql <- dplyr::filter(
    df,
    model == mod & target_type == t & horizon == h & quantile == q
  )$prediction
  qh <- dplyr::filter(
    df,
    model == mod & target_type == t & horizon == h & quantile == 1 - q
  )$prediction
  tv <- dplyr::filter(
    df,
    model == mod & target_type == t & horizon == h & quantile == q
  )$true_value

  res <- method(
    alpha = q * 2, true_values = tv, quantiles_low = ql, quantiles_high = qh
  )

  ql_updated <- res$lower_bound
  qh_updated <- res$upper_bound

  df_updated <- df |>
    dplyr::mutate(prediction = replace(
      prediction,
      model == mod & target_type == t & horizon == h & quantile == q,
      ql_updated
    )) |>
    dplyr::mutate(prediction = replace(
      prediction,
      model == mod & target_type == t & horizon == h & quantile == 1 - q,
      qh_updated
    ))

  return(df_updated)
}

# TODO: Joel
# df <- read.csv("data/full-data-uk-challenge.csv")
# method <- "cqr"
# models <- c("epiforecasts-EpiExpert", "ryan")
#
# cqr_df <- update_predictions(df, method = method, models = models)
#
# # WHY DOESN'T THIS WORK??
# collect_predictions(original = df, cqr = cqr_df) |>
#   plot_intervals(model = "epiforecasts-EpiExpert", alpha = 0.05)

# returns updated data frame for vector of input models
update_predictions <- function(df, method, models) {
  method <- select_method(method = method)
  # make function work for single model
  models <- c(models)

  horizons <- unique(df$horizon)[-1]
  quantiles_below_median <- unique(df$quantile)[unique(df$quantile) < 0.5][-1]
  target_types <- unique(df$target_type)

  df_updated <- df

  for (mod in models) {
    for (t in target_types) {
      for (h in horizons) {
        for (q in quantiles_below_median) {
          df_updated <- update_subset(df_updated, method, mod, t, h, q)
        }
      }
    }
  }

  return(df_updated)
}
