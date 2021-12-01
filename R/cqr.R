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
}                                                      ####



#' @examples
#' df <- read.csv("data/full-data-uk-challenge.csv")
#' corrected_quantiles <- df |>
#'   dplyr::filter(model == "epiforecasts-EpiExpert") |>
#'   cqr(alpha = 0.05)
#' lower_bound <- corrected_quantiles$lower_bound
#' upper_bound <- corrected_quantiles$upper_bound
#' @importFrom rlang .data


# returns corrected lower quantile and upper quantile predictions for a single
# alpha value,
# initializes inputs with NULL such that only alpha is a mandatory argument,
# alpha value is required for compute_margin() independent of input type
cqr <- function(alpha, true_values = NULL, quantiles_low = NULL,
                quantiles_high = NULL) {
  scores <- compute_scores(true_values, quantiles_low, quantiles_high)
  margin <- compute_margin(scores, alpha)

  list(
    lower_bound = quantiles_low - margin,
    upper_bound = quantiles_high + margin
  )
}


