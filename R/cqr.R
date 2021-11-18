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



#   ____________________________________________________________________________
#   Main Functions                                                          ####


#' @examples
#' df <- read.csv("data/full-data-uk-challenge.csv")
#' corrected_quantiles <- df |>
#'   dplyr::filter(model == "epiforecasts-EpiExpert") |>
#'   cqr(alpha = 0.05)
#' lower_bound <- corrected_quantiles$lower_bound
#' upper_bound <- corrected_quantiles$upper_bound
#' @export
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


#' @examples
#' df <- read.csv("data/full-data-uk-challenge.csv")
#' df |>
#'   dplyr::filter(model == "epiforecasts-EpiExpert") |>
#'   collect_predictions(method = "cqr")
#' @export
#'
#' @importFrom rlang .data


# convenience function specific for uk data
# joins new prediction intervals for all alpha values with original predictions,
# right now only works for cqr(), maybe generalizable for multiple methods

# Probably (?) leads to wrong result right now, since the rows are wrongly aligned
# after back joining to original data frame!!
collect_predictions <- function(df, method = c("cqr")) {
  if (method == "cqr") {
    fun <- cqr
  }
  
  new_predictions <- dplyr::tibble(alpha = unique(df$quantile) |> stats::na.omit()) |>
    dplyr::rowwise() |> 
    # TODO: generalize next two lines for any input method
    dplyr::mutate(cqr = list(fun(alpha, df) |> purrr::pluck(1))) |> 
    tidyr::unnest(cols = cqr) |> 
    dplyr::rename(quantile = alpha)

  df |>
    dplyr::rename(original = .data$prediction) |>
    dplyr::left_join(new_predictions) |>
    # TODO: generalize next line for any input method
    tidyr::pivot_longer(
      cols = c(.data$original, .data$cqr),
      names_to = "method", values_to = "prediction"
    ) |>
    dplyr::relocate(.data$method, .data$prediction, .after = .data$true_value)
}
