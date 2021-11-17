#   ____________________________________________________________________________
#   Helper Functions                                                        ####


# scores = E_i values
# y is vector of true values
# quantiles_low is vector of predicted lower quantiles
# quantiles_high is vector of predicted higher quantiles
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

cqr <- function(df, alpha) {
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

  scores <- compute_scores(y, quantiles_low, quantiles_high)
  margin <- compute_margin(scores, alpha)

  return(list(
    lower_bound = quantiles_low - margin,
    upper_bound = quantiles_high + margin
  ))
}
