# scores = E_i values
# true_values is vector of true values
# quantiles_low is vector of predicted lower quantiles
# quantiles_high is vector of predicted upper quantiles
compute_scores_symmetric <- function(true_values, quantiles_low, quantiles_high) {
  pmax(quantiles_low - true_values, true_values - quantiles_high)
}

compute_scores_asymmetric <- function(true_values, quantiles_low, quantiles_high) {
  scores_lower <- quantiles_low - true_values
  scores_upper <- true_values - quantiles_high

  list(scores_lower = scores_lower, scores_upper = scores_upper)
}

compute_scores_multiplicative <- function(true_values, quantiles_low, quantiles_high) {
  # lower quantile might be negative => threshold scores_lower at zero
  scores_lower <- pmax(true_values / quantiles_low, 0)
  scores_upper <- true_values / quantiles_high

  list(scores_lower = scores_lower, scores_upper = scores_upper)
}

# margin = Q_{1-quantile} values
# scores is output vector of compute_scores()
# quantile is quantile value between 0 and 1
# SAME function for all three cqr versions!
compute_margin <- function(scores, quantile) {
  candidate <- (1 - quantile) * (1 + 1 / length(scores))
  prob <- ifelse(candidate <= 0, 0, min(candidate, 1))

  stats::quantile(scores, prob)
}


# returns corrected lower quantile and upper quantile predictions for a single
# quantile value
cqr_symmetric <- function(quantile, true_values, quantiles_low, quantiles_high) {
  scores <- compute_scores_symmetric(true_values, quantiles_low, quantiles_high)
  margin <- compute_margin(scores, quantile)

  # each cqr version is designed to return same output format as input for cross_validate_cqr()
  list(
    # margin(s) required for cross validation in update_subset_cqr,
    # same margin for adjusting lower and upper quantile for symmetric cqr version
    margin_lower = margin,
    margin_upper = margin,
    lower_bound = quantiles_low - margin_lower,
    upper_bound = quantiles_high + margin_upper
  )
}

cqr_asymmetric <- function(quantile, true_values, quantiles_low, quantiles_high) {
  scores_list <- compute_scores_asymmetric(true_values, quantiles_low, quantiles_high)

  list(
    # different margins for adjusting lower and upper quantile for asymmetric cqr version
    margin_lower = compute_margin(scores_list$scores_lower, quantile),
    margin_upper = compute_margin(scores_list$scores_upper, quantile),
    lower_bound = quantiles_low - margin_lower,
    upper_bound = quantiles_high + margin_upper
  )
}

cqr_multiplicative <- function(quantile, true_values, quantiles_low, quantiles_high) {
  scores_list <- compute_scores_multiplicative(true_values, quantiles_low, quantiles_high)

  list(
    # adjust lower and upper bound with multiplicative margin factor
    margin_lower = compute_margin(scores_list$scores_lower, quantile),
    margin_upper = compute_margin(scores_list$scores_upper, quantile),
    lower_bound = quantiles_low * margin_lower,
    upper_bound = quantiles_high * margin_upper
  )
}

update_quantiles_cqr <- function(method, margin_lower, margin_upper, quantiles_low, quantiles_high,
                                 lower_bound_updated, upper_bound_updated, cv_init_training) {
  if (method == "cqr_multiplicative") {
    return(
      list(
        quantiles_low_updated = c(lower_bound_updated, (quantiles_low[cv_init_training + 1] * margin_lower)),
        quantiles_high_updated = c(upper_bound_updated, (quantiles_high[cv_init_training + 1] * margin_upper))
      )
    )
  }

  list(
    quantiles_low_updated = c(lower_bound_updated, (quantiles_low[cv_init_training + 1] - margin_lower)),
    quantiles_high_updated = c(upper_bound_updated, (quantiles_high[cv_init_training + 1] + margin_upper))
  )
}

select_cqr_method <- function(method) {
  if (method == "cqr") {
    return(cqr_symmetric)
  } else if (method == "cqr_asymmetric") {
    return(cqr_asymmetric)
  } else if (method == "cqr_multiplicative") {
    return(cqr_multiplicative)
  }
}

cross_validate_cqr <- function(method, quantile, true_values, quantiles_low,
                               quantiles_high, cv_init_training) {
  cqr_method <- select_cqr_method(method)

  if (is.null(cv_init_training)) {
    # By default cv_init_training is equal to NULL and therefore equal to the complete data.
    # e.g. by default no split in training and validation set
    cqr_results <- cqr_method(quantile * 2, true_values, quantiles_low, quantiles_high)

    return(
      list(
        quantiles_low_updated = cqr_results$lower_bound,
        quantiles_high_updated = cqr_results$upper_bound
      )
    )
  } else {
    # This Section runs the Time Series Cross validation.
    # 1. It runs cqr on the training set and updates all values of the training set.
    #    Then by using the margin it makes the first prediction in the validation set.
    #    The training values and the first adjusted validation set valued are stored in the lists
    #    quantiles_low_updated and quantiles_high_updated. The following section appends to these vectors.
    cqr_results <- cqr_method(
      quantile * 2,
      true_values[1:cv_init_training],
      quantiles_low[1:cv_init_training],
      quantiles_high[1:cv_init_training]
    )

    quantiles_updated <- update_quantiles_cqr(
      method, cqr_results$margin_lower, cqr_results$margin_upper, quantiles_low,
      quantiles_high, cqr_results$lower_bound, cqr_results$upper_bound, cv_init_training
    )

    quantiles_low_updated <- quantiles_updated$quantiles_low_updated
    quantiles_high_updated <- quantiles_updated$quantiles_high_updated

    # 2. The loop goes increases the training set by one observation each step
    #    and computes the next validation set
    #    prediction. This is done by rerunning cqr with the new observations set and extracting the margin.
    #    Then with the new margin the one horizon step ahead prediction is updated.
    for (training_length in (cv_init_training + 1):(length(true_values) - 1)) {
      cqr_results <- cqr_method(
        quantile * 2,
        true_values[1:training_length],
        quantiles_low[1:training_length],
        quantiles_high[1:training_length]
      )

      quantiles_updated <- update_quantiles_cqr(
        method, cqr_results$margin_lower, cqr_results$margin_upper, quantiles_low,
        quantiles_high, quantiles_low_updated, quantiles_high_updated, cv_init_training
      )

      quantiles_low_updated <- quantiles_updated$quantiles_low_updated
      quantiles_high_updated <- quantiles_updated$quantiles_high_updated
    }

    return(
      list(
        quantiles_low_updated = quantiles_low_updated,
        quantiles_high_updated = quantiles_high_updated
      )
    )
  }
}

update_subset_cqr <- function(df, method, model, location, target_type, horizon, quantile, cv_init_training) {
  # 'validate_cv_init' must be placed on filtered data frame (i.e. lowest level,
  # not in update_predictions()) such that fractional inputs can be correctly
  # converted
  cv_init_training <- validate_cv_init(df, cv_init_training)
  quantiles_list <- filter_combination(df, model, location, target_type, horizon, quantile)

  true_values <- quantiles_list$true_values
  quantiles_low <- quantiles_list$quantiles_low
  quantiles_high <- quantiles_list$quantiles_high

  quantiles_updated <- cross_validate_cqr(
    method, quantile, true_values, quantiles_low, quantiles_high, cv_init_training
  )

  df_updated <- replace_combination(
    df, model, location, target_type, horizon, quantile,
    quantiles_updated$quantiles_low_updated, quantiles_updated$quantiles_high_updated
  )

  # set training length as attribute for plotting vertical line
  attr(df_updated, "cv_init_training") <- cv_init_training

  return(df_updated)
}
