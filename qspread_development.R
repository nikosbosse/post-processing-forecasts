################ Quantile Spread applied to example Data ################ 

#TODO: write flexibel quantile spread with one factor per quantile spread: symmetric, non symmetric, with peanalisation for divergence
# optim linkl: https://cran.r-project.org/web/packages/fitdistrplus/vignettes/Optimalgo.html

#Neue Syntax:
devtools::install_github("epiforecasts/scoringutils@major-update")
library(scoringutils)

# temporary and not recommended way, library(postforecasts) imports only functions with @export tag
# => requires more complete documentation
library(dplyr)

# Loading data 
df <- read.csv(here::here("data", "full-data-uk-challenge.csv"))

#Defining the specific series to forecast and getting it from the total data
m <- "epiforecasts-EpiExpert"
l <- "GB"
t <- "Cases"
h <- 1
#cv_init_training <- 10

update_predictions(df, methods = "qsa",
                   models = m, locations = l, target_types = t,
                   horizons = h, quantiles = NULL,
                   cv_init_training = NULL, return_list = TRUE)


subset <- dplyr::filter(df, model == m & location == l & target_type == t & horizon == h)


subset$true_value <- 1

dplyr::filter(df, model == m & location == l & target_type == t & horizon == h) <- subset


subset <- dplyr::filter(df, model == m & location == l & target_type == t & horizon == h)



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

# optim minimizes the wrapper function
# We can get a hessian if we want but it takes additional compute time
optim_results <- optim(par=1, fn=wrapper, subset=subset, gr=NULL, method="BFGS")#, hessian=T)
optimal_spread_factor <- optim_results$par
#TODO: Decide if it is makes sense to write a gradient function that gives back the gradient dependent on the subset at that time point

#function to apply optimal spread factor to data
subset_updated <- quantile_spreads_adjustment(subset=subset,spread_factor= optimal_spread_factor)


#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################


# wrote this filter at the beginning. probably wont need it but keept it in case:
filter_combination <- function(df, model, location, target_type, horizon) {
  # for nicer function input names,
  # input names equal to column names are a little painful with tidyverse functions
  # => temporary variables with different names
  mod <- model
  t <- target_type
  h <- horizon
  l <- location
  
  # To make sure that the predictions and true_values are in the correct order we also arrange by the target_end_date
  #true_values <- df |>
  #  dplyr::filter(
  #    .data$model == mod & .data$location == l & .data$target_type == t & .data$horizon == h & .data$quantile == 0.01
  #  ) |>
  #  dplyr::arrange(.data$target_end_date)
  #
  #true_values <- true_values$predictions
  
  return_list <- list()
  
  for (q in na.omit(unique(df$quantile))){
    quantile <- df |>
      dplyr::filter(
        .data$model == mod & .data$location == l & .data$target_type == t & .data$horizon == h & .data$quantile == 0.01
      ) |>
      dplyr::arrange(.data$target_end_date) |>
      dplyr::pull(.data$prediction)
    print(quantile)
    return_list[[paste0("quantile ", q)]] <- quantile
  }
  
  return(return_list)
}
