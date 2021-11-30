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
  implemented_methods[method]
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




# commented out old functions for fresh start with new approach
#'
#' #' @examples
#' #' df <- read.csv("data/full-data-uk-challenge.csv")
#' #' df |>
#' #'   dplyr::filter(model == "epiforecasts-EpiExpert") |>
#' #'   collect_predictions(method = "cqr")
#' #' @export
#' #'
#' #' @importFrom rlang .data
#'
#'
#' # convenience function specific for uk data
#' # joins new prediction intervals for all alpha values with original predictions,
#' # right now only works for cqr(), maybe generalizable for multiple methods
#'
#' # Probably (?) leads to wrong result right now, since the rows are wrongly aligned
#' # after back joining to original data frame!!
#' collect_predictions <- function(df, method = c("cqr")) {
#'   # relevant if more methods are implemented
#'   if (method == "cqr") {
#'     fun <- cqr
#'   }
#'
#'   # step 1: returns data frame with two columns (alpha value and new
#'   # predictions) with same number of rows as the input dataframe
#'   new_predictions <- dplyr::tibble(alpha = unique(df$quantile) |> stats::na.omit()) |>
#'     dplyr::rowwise() |>
#'     # TODO: generalize next two lines for any input method
#'     dplyr::mutate(cqr = list(fun(.data$alpha, df) |> purrr::pluck(1))) |>
#'     tidyr::unnest(cols = .data$cqr) |>
#'     dplyr::rename(quantile = .data$alpha)
#'
#'   # step 2: add new predictions as new column to input dataframe (this is the
#'   # problemtic step), then transform into long format to evaluate with
#'   # eval_forecasts
#'   df |>
#'     dplyr::rename(original = .data$prediction) |>
#'     dplyr::left_join(new_predictions) |>
#'     # TODO: generalize next line for any input method
#'     tidyr::pivot_longer(
#'       cols = c(.data$original, .data$cqr),
#'       names_to = "method", values_to = "prediction"
#'     ) |>
#'     dplyr::relocate(.data$method, .data$prediction, .after = .data$true_value)
#' }
#'
#'
#'
#'
#' # implementation much simpler for asymmetric procedure,
#' # combines cqr() and collect_predictions() functions,
#' # gives the desired output format with maintaining row alignment,
#' # takes data frame as input, returns data frame with doubled number of rows
#'
#' # df <- read.csv("data/full-data-uk-challenge.csv")
#' # df |>
#' #   dplyr::filter(model == "epiforecasts-EpiExpert") |>
#' #   cqr_simplified()
#'
#' cqr_simplified <- function(df) {
#'
#'   # cqr calculates scores by comparing true value with both lower and upper
#'   # quantile,
#'   # simplified version compares with only one of them
#'   # => different score for 0.01 quantile prediction as for 0.99 quantile prediction
#'   scores <- abs(df$true_value - df$prediction)
#'
#'   df |>
#'     dplyr::rowwise() |>
#'     dplyr::mutate(margin = compute_margin(scores, .data$quantile)) |>
#'     dplyr::mutate(cqr = dplyr::case_when(
#'       .data$quantile < 0.5 ~ .data$prediction - .data$margin,
#'       .data$quantile > 0.5 ~ .data$prediction + .data$margin,
#'       # TODO: how to handle the median prediction?
#'       .data$quantile == 0.5 ~ .data$prediction
#'     )) |>
#'     dplyr::ungroup() |>
#'     dplyr::rename(original = .data$prediction) |>
#'     tidyr::pivot_longer(
#'       cols = c(.data$original, .data$cqr),
#'       names_to = "method", values_to = "prediction"
#'     ) |>
#'     dplyr::relocate(.data$method, .data$prediction, .after = .data$true_value)
#' }
#'
