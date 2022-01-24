# Computing the quantile spread adjustment
quantile_spreads_adjustment <- function(subset, spread_factor){
  # copy of subset to fill with updates
  subset_updated <- subset
  
  # extracting all quantiles that arnt the median
  quantiles_list <- na.omit(unique(subset$quantile))
  quantiles_list_no_median <- quantiles_list[!quantiles_list == 0.50]
  
  # extracting the median
  median_vals <- subset |>
    dplyr::filter(.data$quantile == 0.50) |>
    dplyr::arrange(.data$target_end_date) |>
    dplyr::pull(.data$prediction)
  
  # calculate all quantile spreads as differences between median and the repsective quantile q at each target_end_date
  for (q in quantiles_list_no_median){
    quantile <- subset |>
      dplyr::filter(.data$quantile == q) |>
      dplyr::arrange(.data$target_end_date) |>
      dplyr::pull(.data$prediction)
    
    # getting quantile spread
    quantile_spread <- quantile - median_vals
    
    # computing absolute adjustment based on spread factor 
    absolute_quantile_adjustments <- quantile_spread * spread_factor - quantile_spread
    
    # updating the subset
    subset_updated <- subset_updated |>
      dplyr::mutate(prediction = replace(.data$prediction, 
                                         .data$quantile == q,
                                         values = absolute_quantile_adjustments))
  }
  
  return(subset_updated)
}


wis <- function(subset){
  
  #Calculating the score for the adjusted series
  res <- subset |> 
    score() |>
    summarise_scores(by = c("model"))
  
  wis <- res$interval_score
  
  return(wis)
}

wrapper <- function(spread_factor, subset){
  # applies the quantile spread adjustment
  subset_updated <- quantile_spreads_adjustment(subset,spread_factor)
  
  # calculates its weighted intervall score
  interval_score <- wis(subset_updated)
  
  return(interval_score)
}
