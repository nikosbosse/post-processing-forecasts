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


update_subset_cqr <- function(df, method, model, location, target_type, horizon, quantile, cv_init_training) {
  method <- select_method(method = method)
  
  quantiles_list <- filter_combination(df, model, location, target_type, horizon, quantile)
  
  true_values <- quantiles_list$true_values
  quantiles_low <- quantiles_list$quantiles_low
  quantiles_high <- quantiles_list$quantiles_high
  
  if (is.null(cv_init_training)) {
    # By default cv_init_training is equal to NULL and therefore equal to the complete data.
    # e.g. by default no split in training and validation set
    result <- method(quantile * 2, true_values, quantiles_low, quantiles_high)
    
    margin <- result$margin
    quantiles_low_updated <- result$lower_bound
    quantiles_high_updated <- result$upper_bound
  } else {
    # This Section runs the Time Series Cross validation.
    # 1. It runs the method on the training set and updates all values of the training set.
    #    Then by using the margin it makes the first prediction in the validation set.
    #    The training values and the first adjusted validation set valued are stored in the lists
    #    quantiles_low_updated and quantiles_high_updated. The following section appends to these vectors.
    results <- method(
      quantile * 2,
      true_values[1:cv_init_training],
      quantiles_low[1:cv_init_training],
      quantiles_high[1:cv_init_training]
    )
    
    lower_bound_updated <- results$lower_bound
    upper_bound_updated <- results$upper_bound
    
    margin <- results$margin
    quantiles_low_updated <- c(lower_bound_updated, (quantiles_low[cv_init_training + 1] - margin))
    quantiles_high_updated <- c(upper_bound_updated, (quantiles_high[cv_init_training + 1] + margin))
    
    # 2. The loop goes increases the training set by one observation each step
    #    and computes the next validation set
    #    prediction. This is done by rerunning cqr with the new observations set and extracting the margin.
    #    Then with the new margin the one horizon step ahead prediction is updated.
    for (training_length in (cv_init_training + 1):(length(true_values) - 1)) {
      results <- method(
        quantile * 2,
        true_values[1:training_length],
        quantiles_low[1:training_length],
        quantiles_high[1:training_length]
      )
      
      margin <- results$margin
      quantiles_low_updated <- c(quantiles_low_updated, (quantiles_low[training_length + 1] - margin))
      quantiles_high_updated <- c(quantiles_high_updated, (quantiles_high[training_length + 1] + margin))
    }
  }
  
  df_updated <- replace_combination(
    df, model, location, target_type, horizon, quantile,
    quantiles_low_updated, quantiles_high_updated
  )
  
  # set training length as attribute for plotting vertical line
  attr(df_updated, "cv_init_training") <- cv_init_training
  
  return(df_updated)
}