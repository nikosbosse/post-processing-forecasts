qsa <- function(subset, spread_factor_vec, method) {
  # copy of subset to fill with updates
  # subset_updated <- subset

  # extracting all quantiles that arnt the median
  quantiles_list <- stats::na.omit(unique(subset$quantile))
  quantiles_list_no_median <- quantiles_list[!quantiles_list == 0.50]

  # extracting the median
  median_vals <- subset |>
    dplyr::filter(.data$quantile == 0.50) |>
    dplyr::arrange(.data$target_end_date) |>
    dplyr::pull(.data$prediction)

  # creating a matrix in which to return the updated quantile values
  target_end_date_uniques <- stats::na.omit(unique(subset$target_end_date))
  return_matrix <- matrix(
    nrow = length(target_end_date_uniques),
    ncol = length(quantiles_list_no_median)
  )

  # calculate all quantile spreads as differences between median and the respective quantile q at each target_end_date
  for (q in quantiles_list_no_median) {
    quantile <- subset |>
      dplyr::filter(.data$quantile == q) |>
      dplyr::arrange(.data$target_end_date) |>
      dplyr::pull(.data$prediction)

    # getting quantile spread
    quantile_spread <- quantile - median_vals

    # computing absolute adjustment based on spread factor
    if (method == "qsa_uniform") {
      absolute_quantile_adjustments <- quantile_spread * spread_factor_vec - quantile_spread
      quantile_updated <- quantile + absolute_quantile_adjustments
    } else if (method == "qsa_flexibel") {
      # getting the index of the quantile to then get the corresponding spread factor
      index_of_quantile <- which(quantiles_list_no_median == q)
      absolute_quantile_adjustments <- quantile_spread * spread_factor_vec[index_of_quantile] - quantile_spread
      quantile_updated <- quantile + absolute_quantile_adjustments
    } else if (method == "qsa_flexibel_symmetric") {
      # getting the index of the quantile to then get the corresponding spread factor
      # As we have symmetry we define a vector that holds the indexes to get the right spread factor:
      # example: 22 quantiles without median, e.g. 11 pairs, so indexes_of_spread_factor <- (11,10,...,2,1,1,2,...,10,11)
      num_quantile_pairs <- length(quantiles_list_no_median) / 2
      indexes_of_spread_factor <- c(num_quantile_pairs:1, 1:num_quantile_pairs)
      index_of_quantile <- indexes_of_spread_factor[which(quantiles_list_no_median == q)]

      absolute_quantile_adjustments <- quantile_spread * spread_factor_vec[index_of_quantile] - quantile_spread
      quantile_updated <- quantile + absolute_quantile_adjustments
    }

    # updating the subset
    # subset_updated <- subset_updated |>
    #  dplyr::mutate(prediction = replace(.data$prediction,
    #                                     .data$quantile == q,
    #                                     values = quantile_updated))

    # putting the updated values in a matrix which we return, each col is a quantile
    return_matrix[, which(quantiles_list_no_median == q)] <- quantile_updated
  }

  return(return_matrix)
}


wis <- function(subset, spread_factor_vec, penalty_weight) {

  # Calculating the score for the adjusted series
  res <- subset |>
    scoringutils::score() |>
    scoringutils::summarise_scores(by = c("model"))

  if (is.null(penalty_weight)) {
    wis <- res$interval_score
  } else {
    # We penalize by the sum of squared differences between spread factors to penalize differences between them
    # Note: penalty_weight default is zero and thus there is no penalization
    wis <- res$interval_score + penalty_weight * sum((spread_factor_vec - mean(spread_factor_vec))^2)
  }

  return(wis)
}


wrapper <- function(spread_factor_vec, subset, method_pp, penalty_weight) {
  # Note: called method method_pp as optim has an arguement called method

  # applies the quantile spread adjustment
  updates_matrix <- qsa(subset, spread_factor_vec, method_pp)

  subset_updated <- subset

  # extracting all quantiles that arnt the median
  quantiles_list <- stats::na.omit(unique(subset$quantile))
  quantiles_list_no_median <- quantiles_list[!quantiles_list == 0.50]

  # updating the subset by exchanging the values in the dataframe with the ones from the update matrix
  # Note: which(quantiles_list_no_median == q) tells us the number of the quantile and thus of the update_matrix
  for (q in quantiles_list_no_median) {
    # updating the subset
    subset_updated <- subset_updated |>
      dplyr::mutate(prediction = replace(.data$prediction,
        .data$quantile == q,
        values = updates_matrix[, which(quantiles_list_no_median == q)]
      ))
  }

  # calculates its weighted intervall score
  interval_score <- wis(subset_updated, spread_factor_vec, penalty_weight)

  return(interval_score)
}

optimize_spread_factor <- function(method,subset,penalty_weight,par=NULL){
  # TODO: Decide if it is makes sense to write a gradient function that gives back the gradient dependent on the subset at that time point
  
  # optim minimizes the wrapper function
  # We can get a hessian if we want but it takes additional compute time
  # starts with spread factor of 1 e.g. no spread necessary
  if (method == "qsa_uniform") {
    if (is.null(par)) {par <- c(1)}
    
    optim_results <- optim(
      par = par, fn = wrapper, subset = subset, method_pp = method, penalty_weight = penalty_weight,
      gr = NULL, method = "BFGS"
    ) # , hessian=T)
    optimal_spread_factor <- optim_results$par
    
  } else if (method == "qsa_flexibel") {
    # getting number of spreads as: number of quantiles without the mean
    num_spreads <- length(na.omit(unique(subset$quantile))) - 1 #-1 due to mean
    
    if (is.null(par)) {par <- rep(1, num_spreads)}
    
    optim_results <- optim(
      par = par, fn = wrapper, subset = subset, method_pp = method,
      penalty_weight = penalty_weight, gr = NULL, method = "BFGS"
    ) # , hessian=T)
    optimal_spread_factor <- optim_results$par
    
  } else if (method == "qsa_flexibel_symmetric") {
    # getting number of spreads as: number of quantiles without the mean
    num_spreads <- length(na.omit(unique(subset$quantile))) - 1 #-1 due to mean
    num_of_params <- num_spreads / 2
    
    if (is.null(par)) {par <- rep(1, num_of_params)}
    
    optim_results <- optim(
      par = par, fn = wrapper, subset = subset, method_pp = method,
      penalty_weight = penalty_weight, gr = NULL, method = "BFGS"
    ) # , hessian=T)
    optimal_spread_factor <- optim_results$par
  }
  return(optimal_spread_factor)
}


update_subset_qsa <- function(df, method, model, location, target_type, horizon, cv_init_training, penalty_weight) {
  # must be placed on filtered data frame (i.e. lowest level, not in
  # update_predictions()) such that fractional inputs can be correctly converted
  cv_init_training <- validate_cv_init(df, cv_init_training)
  
  m <- model
  l <- location
  t <- target_type
  h <- horizon

  # Moved the extraction of the dataframe into the if else condition as we use the data as it comes out of filter

  if (is.null(cv_init_training)) {
    # By default cv_init_training is equal to NULL and therefore equal to the complete data.
    # e.g. by default no split in training and validation set

    subset <- dplyr::filter(
      df, .data$model == m & .data$location == l & .data$target_type == t & .data$horizon == h)

    # Run optimization to get the optimal spread factor
    optimal_spread_factor <- optimize_spread_factor(method = method,subset = subset,
                                                    penalty_weight = penalty_weight, par = NULL)

    # function to apply optimal spread factor to data
    # We return a matrix containing the updates of the quantiles
    updates_matrix <- qsa(
      subset = subset, spread_factor_vec = optimal_spread_factor, method = method
    )

    # copy of subset to fill with updates
    subset_updated <- subset

    # extracting all quantiles that arnt the median
    quantiles_list <- stats::na.omit(unique(subset$quantile))
    quantiles_list_no_median <- quantiles_list[!quantiles_list == 0.50]

    # updating the subset by exchanging the values in the dataframe with the ones from the update matrix
    # Note: which(quantiles_list_no_median == q) tells us the number of the quantile and thus of the update_matrix
    for (q in quantiles_list_no_median) {
      # updating the subset
      subset_updated <- subset_updated |>
        dplyr::mutate(prediction = replace(
          .data$prediction, .data$quantile == q,
          values = updates_matrix[, which(quantiles_list_no_median == q)]
        ))
    }
  } else {
    # This Section runs the Time Series Cross validation.
    # 1. It runs the method on the training set and updates all values of the training set.

    # gets subset of the data
    subset <- dplyr::filter(df, model == m & location == l & target_type == t & horizon == h)
    # then extract the sorted list of target end dates and from there the training target end dates
    target_end_date_train <- sort(unique(subset$target_end_date))[0:cv_init_training]
    # reduce subset to the training period
    subset_train <- dplyr::filter(subset, .data$target_end_date %in% target_end_date_train)

    # target end date for val
    target_end_date_val <- sort(unique(subset$target_end_date))[cv_init_training + 1]
    # subset for one step ahead prediction
    subset_val <- dplyr::filter(subset, .data$target_end_date == target_end_date_val)

    # Run optimization to get the optimal spread factor
    optimal_spread_factor <- optimize_spread_factor(method = method, subset = subset,
                                                    penalty_weight = penalty_weight, par = NULL)
    
    # training set
    updates_matrix <- qsa(subset = subset, spread_factor_vec = optimal_spread_factor, method = method)
    # one step ahead prediction adjustment
    val_row <- qsa(subset = subset_val, spread_factor_vec = optimal_spread_factor, method = method)

    # appending the one step ahead prediciton to the update matrix
    updates_matrix <- rbind(updates_matrix, val_row)

    # 2. The loop goes increases the training set by one observation each step
    #    and computes the next validation set
    #    prediction. This is done by rerunning cqr with the new observations set and extracting the margin.
    #    Then with the new margin the one horizon step ahead prediction is updated.
    for (training_length in (cv_init_training + 1):(length(unique(subset$target_end_date)) - 1)) {

      # gets subset of the data
      subset <- dplyr::filter(df, model == m & location == l & target_type == t & horizon == h)
      target_end_date_train <- sort(unique(subset$target_end_date))[0:training_length]
      subset_train <- dplyr::filter(subset, .data$target_end_date %in% target_end_date_train)

      target_end_date_val <- sort(unique(subset$target_end_date))[training_length + 1]
      subset_val <- dplyr::filter(subset, .data$target_end_date == target_end_date_val)

      # for faster computation it starts optimization at the optimal spread factor of the last iteration

      # Run optimization to get the optimal spread factor
      optimal_spread_factor <- optimize_spread_factor(method = method,subset = subset,
                                                      penalty_weight = penalty_weight, par = optimal_spread_factor)
      
      # For the iteration forward we only need the validation set prediction
      val_row <- qsa(subset = subset_val, spread_factor_vec = optimal_spread_factor, method = method)

      updates_matrix <- rbind(updates_matrix, val_row)
    }

    # copy of subset to fill with updates
    subset_updated <- subset

    # extracting all quantiles that arnt the median
    quantiles_list <- stats::na.omit(unique(subset$quantile))
    quantiles_list_no_median <- quantiles_list[!quantiles_list == 0.50]

    # updating the subset by exchanging the values in the dataframe with the ones from the update matrix
    # Note: which(quantiles_list_no_median == q) tells us the number of the quantile and thus of the update_matrix
    for (q in quantiles_list_no_median) {
      # updating the subset
      subset_updated <- subset_updated |>
        dplyr::mutate(prediction = replace(.data$prediction,
          .data$quantile == q,
          values = updates_matrix[, which(quantiles_list_no_median == q)]
        ))
    }

    # Make sure the updated subset is arranged by quantiles and then target end dates
    # That way we can pass its prediction column to the updated df directly
    subset_updated <- subset_updated |>
      dplyr::arrange(.data$quantile, .data$target_end_date)

    quantiles_list <- stats::na.omit(unique(subset$quantile))
    quantiles_list_no_median <- quantiles_list[!quantiles_list == 0.50]

    # Again we first arrange the data to make we can pass the prediction column to the updated df directly
    df_updated <- df |>
      dplyr::arrange(.data$quantile, .data$target_end_date) |>
      dplyr::mutate(prediction = replace(
        .data$prediction,
        .data$model == m & .data$location == l & .data$target_type == t & .data$horizon == h, # no quantile specified in quantile spread post processing
        values = subset_updated$prediction
      ))

    # set training length as attribute for plotting vertical line
    attr(df_updated, "cv_init_training") <- cv_init_training

    return(df_updated)
  }
}
#TODO: write helper function that computes the applied optimal spread parameters of a qsa approach as well as the actual absolute adjustment. this is a nice way to compare the different adjustments different methods do
