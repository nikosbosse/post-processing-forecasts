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

# add regularization by pulling all values in scores vector closer to 1
# diminishes the impact of outliers
regularize_scores <- function(scores) {
  spread <- stats::sd(scores)

  # if score vector only contains one element => no adjustments
  # if score vector is constant there are no outliers => no adjustments
  # interestingly, making the spread larger when sd(scores) < 1 improves weighted
  # interval score!
  if (is.na(spread) || spread == 0) {
    return(scores)
  }
  scores^(1 / spread)
}

compute_scores_multiplicative <- function(true_values, quantiles_low, quantiles_high) {
  # lower quantile might be 0 or negative => threshold scores_lower at zero
  scores_lower <- ifelse(quantiles_low > 0, true_values / quantiles_low, 0) |>
    regularize_scores()
  scores_upper <- (true_values / quantiles_high) |>
    regularize_scores()

  list(scores_lower = scores_lower, scores_upper = scores_upper)
}


# margin = Q_{1-quantile} values
# scores is output vector of compute_scores()
# quantile is quantile value between 0 and 1
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

  # margin(s) required for cross validation in update_subset_cqr,
  # same margin for adjusting lower and upper quantile for symmetric cqr version
  margin_lower <- margin
  margin_upper <- margin

  # each cqr version is designed to return same output format as input for
  # cross_validate_cqr()
  list(
    margin_lower = margin_lower,
    margin_upper = margin_upper,
    lower_bound = quantiles_low - margin_lower,
    upper_bound = quantiles_high + margin_upper
  )
}

cqr_asymmetric <- function(quantile, true_values, quantiles_low, quantiles_high) {
  scores_list <- compute_scores_asymmetric(true_values, quantiles_low, quantiles_high)

  margin_lower <- compute_margin(scores_list$scores_lower, quantile)
  margin_upper <- compute_margin(scores_list$scores_upper, quantile)

  list(
    # different margins for adjusting lower and upper quantile for asymmetric cqr
    # version
    margin_lower = margin_lower,
    margin_upper = margin_upper,
    lower_bound = quantiles_low - margin_lower,
    upper_bound = quantiles_high + margin_upper
  )
}

# add restriction that lower_margin * upper_margin = 1
# if upper bound gets larger, lower bound has to get smaller
# prevents issue that lower bound is always increased for extreme quantiles
constrain_margins <- function(margin_lower, margin_upper) {
  # in EuroCovidhubBaseline Model all score values can be 0
  # => margin also equal to 0
  if (margin_lower == 0 || margin_upper == 0) {
    return(
      list(margin_lower = margin_lower, margin_upper = margin_upper)
    )
  }

  scaled_lower <- margin_lower / sqrt(margin_lower * margin_upper)
  scaled_upper <- margin_upper / sqrt(margin_lower * margin_upper)

  list(margin_lower = scaled_lower, margin_upper = scaled_upper)
}

cqr_multiplicative <- function(quantile, true_values, quantiles_low, quantiles_high) {
  scores_list <- compute_scores_multiplicative(
    true_values, quantiles_low, quantiles_high
  )

  margin_lower <- compute_margin(scores_list$scores_lower, quantile)
  margin_upper <- compute_margin(scores_list$scores_upper, quantile)

  margins_list <- constrain_margins(margin_lower, margin_upper)
  margin_lower <- margins_list$margin_lower
  margin_upper <- margins_list$margin_upper

  list(
    # adjust lower and upper bound with multiplicative margin factor
    margin_lower = margin_lower,
    margin_upper = margin_upper,
    lower_bound = quantiles_low * margin_lower,
    upper_bound = quantiles_high * margin_upper
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
    #    The training values and the first adjusted validation set valued are stored in
    #    the lists quantiles_low_updated and quantiles_high_updated. The following
    #    section appends to these vectors.
    cqr_results <- cqr_method(
      quantile * 2,
      true_values[1:cv_init_training],
      quantiles_low[1:cv_init_training],
      quantiles_high[1:cv_init_training]
    )

    # multiplicative adjustments
    if (method == "cqr_multiplicative") {
      quantiles_low_updated <- c(
        cqr_results$lower_bound, (quantiles_low[cv_init_training + 1] * cqr_results$margin_lower)
      )
      quantiles_high_updated <- c(
        cqr_results$upper_bound, (quantiles_high[cv_init_training + 1] * cqr_results$margin_upper)
      )
    } else {
      # additive adjustments
      quantiles_low_updated <- c(
        cqr_results$lower_bound, (quantiles_low[cv_init_training + 1] - cqr_results$margin_lower)
      )
      quantiles_high_updated <- c(
        cqr_results$upper_bound, (quantiles_high[cv_init_training + 1] + cqr_results$margin_upper)
      )
    }

    # 2. The loop goes increases the training set by one observation each step
    #    and computes the next validation set prediction. This is done by rerunning cqr
    #    with the new observations set and extracting the margin. Then with the new
    #    margin the one horizon step ahead prediction is updated.
    for (training_length in (cv_init_training + 1):(length(true_values) - 1)) {
      cqr_results <- cqr_method(
        quantile * 2,
        true_values[1:training_length],
        quantiles_low[1:training_length],
        quantiles_high[1:training_length]
      )

      if (method == "cqr_multiplicative") {
        quantiles_low_updated <- c(
          quantiles_low_updated, (quantiles_low[training_length + 1] * cqr_results$margin_lower)
        )
        quantiles_high_updated <- c(
          quantiles_high_updated, (quantiles_high[training_length + 1] * cqr_results$margin_upper)
        )
      } else {
        quantiles_low_updated <- c(
          quantiles_low_updated, (quantiles_low[training_length + 1] - cqr_results$margin_lower)
        )
        quantiles_high_updated <- c(
          quantiles_high_updated, (quantiles_high[training_length + 1] + cqr_results$margin_upper)
        )
      }
    }

    return(
      list(
        quantiles_low_updated = quantiles_low_updated,
        quantiles_high_updated = quantiles_high_updated
      )
    )
  }
}

update_subset_cqr <- function(df, method, model, location, target_type,
                              horizon, quantile, cv_init_training) {
  quantiles_list <- filter_combination(df, model, location, target_type, horizon, quantile)

  # 'validate_cv_init' must be placed on filtered data frame (i.e. lowest level,
  # not in update_predictions()) such that fractional inputs can be correctly
  # converted
  cv_init_training <- validate_cv_init(df, cv_init_training)

  true_values <- quantiles_list$true_values
  quantiles_low <- quantiles_list$quantiles_low
  quantiles_high <- quantiles_list$quantiles_high

  quantiles_updated <- cross_validate_cqr(
    method, quantile, true_values, quantiles_low, quantiles_high, cv_init_training
  )

  quantiles_low_updated <- quantiles_updated$quantiles_low_updated
  quantiles_high_updated <- quantiles_updated$quantiles_high_updated

  df_updated <- replace_combination(
    df, model, location, target_type, horizon, quantile,
    quantiles_low_updated, quantiles_high_updated
  )

  # set training length as attribute for plotting vertical line
  attr(df_updated, "cv_init_training") <- cv_init_training

  return(df_updated)
}
