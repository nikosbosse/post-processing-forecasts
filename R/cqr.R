# scores = E_i values
# true_values is vector of true values
# quantiles_low is vector of predicted lower quantiles
# quantiles_high is vector of predicted upper quantiles
compute_scores <- function(true_values, quantiles_low, quantiles_high) {
  pmax(quantiles_low - true_values, true_values - quantiles_high)
}


# margin = Q_{1-quantile} values
# scores is output vector of compute_scores()
# quantile is quantile value between 0 and 1
compute_margin <- function(scores, quantile) {
  candidate <- (1 - quantile) * (1 + 1 / length(scores))
  prob <- ifelse(candidate <= 0, 0, min(candidate, 1))

  stats::quantile(scores, prob)
}



# df <- read.csv("data/full-data-uk-challenge.csv")
# corrected_quantiles <- df |>
#   dplyr::filter(model == "epiforecasts-EpiExpert") |>
#   cqr(quantile = 0.05)
# lower_bound <- corrected_quantiles$lower_bound
# upper_bound <- corrected_quantiles$upper_bound
#' @importFrom rlang .data


# returns corrected lower quantile and upper quantile predictions for a single
# quantile value
cqr <- function(quantile, true_values, quantiles_low, quantiles_high) {
  scores <- compute_scores(true_values, quantiles_low, quantiles_high)
  margin <- compute_margin(scores, quantile)

  list(
    lower_bound = quantiles_low - margin,
    upper_bound = quantiles_high + margin,
    margin = margin
  )
}
