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
  (1 - alpha) * (1 + 1 / length(scores))
}



#   ____________________________________________________________________________
#   Main Function                                                           ####


#' @examples
#' df <- read.csv("data/full-data-uk-challenge.csv")
#' corrected_quantiles <- df |>
#'   dplyr::filter(model == "epiforecasts-EpiExpert") |>
#'   cqr(alpha = 0.05)
#'
#' lower_bound <- corrected_quantiles$lower_bound
#' upper_bound <- corrected_quantiles$upper_bound
#' 
#' @export
#'
#' @importFrom rlang .data


# initialize inputs with NULL, such that only alpha is mandatory argument
# required for compute_margin() independent of input type
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
        values_from = .data$prediction,
        names_prefix = "q_"
      )

    quantiles_low <- df[stringr::str_glue("q_{alpha}")]
    quantiles_high <- df[stringr::str_glue("q_{1-alpha}")]
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

  return(list(
    lower_bound = quantiles_low - margin,
    upper_bound = quantiles_high + margin
  ))
}
