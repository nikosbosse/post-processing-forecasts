#   ____________________________________________________________________________
#   Helper Functions                                                        ####


# scores = E_i values
# y is vector of true values
# quantiles_low is vector of predicted lower quantiles
# quantiles_high is vector of predicted upper quantiles
compute_scores <- function(y, quantiles_low, quantiles_high) {
  pmax(quantiles_low - y, y - quantiles_high)
}


# margin = Q_{1-alpha} values
# scores is output vector of compute_scores()
# alpha is quantile value between 0 and 1
compute_margin <- function(scores, alpha) {
  candidate <- (1 - alpha) * (1 + 1 / length(scores))
  prob <- ifelse(candidate <= 0, 0, min(candidate, 1))

  stats::quantile(scores, prob)
}

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



#   ____________________________________________________________________________
#   Main Functions                                                          ####


#' @examples
#' df <- read.csv("data/full-data-uk-challenge.csv")
#' corrected_quantiles <- df |>
#'   dplyr::filter(model == "epiforecasts-EpiExpert") |>
#'   cqr(alpha = 0.05)
#' lower_bound <- corrected_quantiles$lower_bound
#' upper_bound <- corrected_quantiles$upper_bound
#'
#' @importFrom rlang .data


# returns corrected lower quantile and upper quantile predictions for a single
# alpha value,
# initializes inputs with NULL such that only alpha is a mandatory argument,
# alpha value is required for compute_margin() independent of input type
cqr <- function(alpha, df = NULL, true_value = NULL,
                quantiles_low = NULL, quantiles_high = NULL) {

  # case 1: dataframe and alpha value are given
  # requires columns named 'true_value', 'prediction' and 'quantile', long format
  # and restrictions for alpha value, convenient e.g. for given uk_data
  if (!is.null(df)) {
    df <- df |>
      dplyr::filter(.data$quantile %in% c(alpha, 1 - alpha)) |>
      tidyr::pivot_wider(
        names_from = .data$quantile,
        values_from = .data$prediction
      )

    quantiles_low <- df[[stringr::str_glue("{alpha}")]]
    quantiles_high <- df[[stringr::str_glue("{1-alpha}")]]
    y <- df$true_value
  }
  # case 2: true values and predicted quantiles are given directly
  else {
    quantiles_low <- quantiles_low
    quantiles_high <- quantiles_high
    y <- true_value
  }

  # TODO: add informative error messages for invalid input combinations / types
  scores <- compute_scores(y, quantiles_low, quantiles_high)
  margin <- compute_margin(scores, alpha)

  list(
    lower_bound = quantiles_low - margin,
    upper_bound = quantiles_high + margin
  )
}

# TODO: consistent input name for model 
update_predicted_values <- function(df, method, example_model, t, h, q) {
  ql <- dplyr::filter(df, model == example_model & target_type == t & horizon == h & quantile == q)$prediction
  qh <- dplyr::filter(df, model == example_model & target_type == t & horizon == h & quantile == 1 - q)$prediction
  tv <- dplyr::filter(df, model == example_model & target_type == t & horizon == h & quantile == q)$true_value
  
  # TODO: where are the following two lines used?
  date <- dplyr::filter(df, model == example_model & target_type == t & horizon == h & quantile == q)$forecast_date
  date <- as.Date(date)
  
  # TODO: replace cqr with general method
  # res <- method(alpha = q * 2, true_values = tv, quantiles_low = ql, quantiles_high = qh)
  res <- cqr(alpha = q * 2, true_values = tv, quantiles_low = ql, quantiles_high = qh)
  
  ql_updated <- res$lower_bound
  qh_updated <- res$upper_bound
  
  df_updated <- df |>
    dplyr::mutate(prediction = replace(prediction, model == example_model & target_type == t & horizon == h & quantile == q, ql_updated)) |>
    dplyr::mutate(prediction = replace(prediction, model == example_model & target_type == t & horizon == h & quantile == 1 - q, qh_updated))
  
  # print(cat("Values before for ql: ",ql))
  # print(cat("Values before for qh: ",qh))
  #
  # print(cat("Values after for ql_updated: ",ql_updated))
  # print(cat("Values after for qh_updated: ",qh_updated))
  #
  # print(cat("Difference ql: ",ql_updated-ql))
  # print(cat("Difference qh: ",qh_updated-qh))
  
  return(df_updated)
}


# TODO: Joel
# df <- read.csv("data/full-data-uk-challenge.csv")
# method <- "cqr"
# model <- "epiforecasts-EpiExpert"
# 
# cqr_df <- update_predictions(df, method = method, model = model)
# 
# # WHY DOESN'T THIS WORK??
# collect_predictions(original = df, cqr = cqr_df) |> 
#   plot_intervals(model = model, alpha = 0.05)

# returns updated data frame for one specific model
update_predictions <- function(df, method, model) {
  method <- select_method(method = method)
  horizons <- unique(df$horizon)[-1]
  quantiles_below_median <- unique(df$quantile)[unique(df$quantile) < 0.5][-1]
  target_types <- unique(df$target_type)
  
  # necessary?
  df_updated <- df
  
  for (h in horizons) {
    for (q in quantiles_below_median) {
      for (t in target_types) {
        df_updated <- update_predicted_values(df, method, model, t, h, q)
      }
    }
  }
  
  return(df_updated)
}