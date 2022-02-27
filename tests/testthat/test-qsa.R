test <- function(qs = c(0.25, 0.5, 0.75),
                 error_factor = 1,
                 true_values_new = c(rep(50,4), 55, rep(5,5), rep(c(50,5),8),50),
                 methods = "qsa_uniform",
                 cv_init_training = NULL,
                 quantile_plot = 0.25,
                 optim_method = "BFGS",
                 lower_bound_optim = 0, 
                 upper_bound_optim = 5){
  
  # Get a subset of original data as template 
  # We only make it have the length of the true values (max remains 25)
  df <- read.csv(here::here("data_modified", "hub_data_2_incidences.csv"))
  m <- "epiforecasts-EpiExpert"
  l <- "GB"
  t <- "Cases"
  h <- 1
  subset <- dplyr::filter(df, model == m & location == l 
                          & target_type == t & horizon == h 
                          & quantile %in% qs 
                          & target_end_date %in% sort(unique(df$target_end_date))[0:length(true_values_new)])
  
  # Changing the quantile predictions to the value of the quantile * 100 * error_factor
  subset_generated <- subset
  for (qi in qs) {
    subset_generated <- subset_generated |>
      dplyr::mutate(prediction = replace(
        .data$prediction, .data$quantile == qi,
        values = rep(qi * 100 * error_factor,length(unique(subset$target_end_date)))
      ))
  }
  
  # Changing the true values at each time point (for each quantile the same)
  for (i in seq(0,length(unique(subset_generated$target_end_date))) ) {
    date <- unique(subset_generated$target_end_date)[i]
    value <- true_values_new[i]
    
    subset_generated <- subset_generated |>
      dplyr::mutate(true_value = replace(
        .data$true_value, .data$target_end_date == date,
        values = rep(value,length(unique(subset_generated$quantile)))
      ))
  }
  
  # Run QSA
  df_updated <- update_predictions(subset_generated, methods = methods,
                                   models = m, locations = l, target_types = t,
                                   horizons = h, quantiles = qs,
                                   cv_init_training = cv_init_training, 
                                   penalty_weight=NULL, 
                                   optim_method = optim_method, 
                                   lower_bound_optim = lower_bound_optim, 
                                   upper_bound_optim = upper_bound_optim,
                                   return_list = TRUE)
  
  # Combine DataFrames so that we can plot results
  df_combined <- df_updated |> collect_predictions()
  
  return(df_combined)
}

#   ____________________________________________________________________________
#   Test that no adjustment of intervals is done if in optimum              ####

#qsa_uniform
qs = c(0.25, 0.5, 0.75)
true_values_new = c(rep(50,4), 60, rep(20,5))

df_combined <- test(qs=qs, true_values_new=true_values_new, methods = "qsa_uniform")
original_prediction_sorted <- df_combined[0:30,]$prediction 
updated_prediction_sorted <- df_combined[31:60,][order(df_combined[31:60,]$forecast_date),]$prediction 

test_that("original and updated predictions are identical for qsa_uniform with optim method BFGS", {
  expect_equal(original_prediction_sorted, updated_prediction_sorted)
})

#qsa_flexible_symmetric
qs = c(0.25, 0.5, 0.75)
true_values_new = c(rep(50,4), 60, rep(20,5))

df_combined <- test(qs=qs, true_values_new=true_values_new, methods = "qsa_flexible_symmetric")
original_prediction_sorted <- df_combined[0:30,]$prediction 
updated_prediction_sorted <- df_combined[31:60,][order(df_combined[31:60,]$forecast_date),]$prediction 

test_that("original and updated predictions are identical for qsa_flexible_symmetric with optim method BFGS", {
  expect_equal(original_prediction_sorted, updated_prediction_sorted)
})

#qsa_flexible
qs = c(0.25, 0.5, 0.75)
true_values_new = c(rep(50,4), 0,0, 100,100)

df_combined <- test(qs=qs, true_values_new=true_values_new, methods = "qsa_flexible")
original_prediction_sorted <- df_combined[0:24,]$prediction 
updated_prediction_sorted <- df_combined[25:48,][order(df_combined[25:48,]$forecast_date),]$prediction 

test_that("original and updated predictions are identical for qsa_flexible with optim method BFGS", {
  expect_equal(original_prediction_sorted, updated_prediction_sorted)
})


#   ____________________________________________________________________________
#   Test that intervals are decreased till optimum                          ####

#qsa_uniform
qs = c(0.3, 0.5, 0.7)
true_values_new = c(rep(50,4), 60, rep(90,5))

df_combined <- test(qs=qs, true_values_new=true_values_new, methods = "qsa_uniform")

updated_prediction_sorted <- df_combined[31:60,][order(df_combined[31:60,]$forecast_date),]$prediction 
optimum <- rep(c(40, 50, 60),10)

test_that("updated predictions are rounded equal to expected optimum for qsa_uniform with optim method BFGS", {
  expect_equal(TRUE, any(abs(optimum - updated_prediction_sorted) < 1))
})

#qsa_flexible_symmetric
qs = c(0.3, 0.5, 0.7)
true_values_new = c(rep(50,4), 60, rep(90,5))

df_combined <- test(qs=qs, true_values_new=true_values_new, methods = "qsa_flexible_symmetric")

updated_prediction_sorted <- df_combined[31:60,][order(df_combined[31:60,]$forecast_date),]$prediction 
optimum <- rep(c(40, 50, 60),10)

test_that("original and updated predictions are identical for qsa_flexible_symmetric with optim method BFGS", {
  expect_equal(TRUE, any(abs(optimum - updated_prediction_sorted) < 1))
})

#qsa_flexible
qs = c(0.3, 0.5, 0.7)
true_values_new = c(rep(50,4), 60, rep(90,5))

df_combined <- test(qs=qs, true_values_new=true_values_new, methods = "qsa_flexible")

updated_prediction_sorted <- df_combined[31:60,][order(df_combined[31:60,]$forecast_date),]$prediction 
optimum <- rep(c(50, 50, 90),10)

test_that("original and updated predictions are identical for qsa_flexible with optim method BFGS", {
  expect_equal(TRUE, any(abs(optimum - updated_prediction_sorted) < 1))
})


#   ____________________________________________________________________________
#   Test that intervals are increased till optimum                          ####

#qsa_uniform
qs = c(0.2, 0.5, 0.8)
true_values_new = c(rep(50,4), 60, rep(90,5))

df_combined <- test(qs=qs, true_values_new=true_values_new, methods = "qsa_uniform")

updated_prediction_sorted <- df_combined[31:60,][order(df_combined[31:60,]$forecast_date),]$prediction 
optimum <- rep(c(10, 50, 90),10)

test_that("updated predictions are rounded equal to expected optimum for qsa_uniform with optim method BFGS", {
  expect_equal(TRUE, any(abs(optimum - updated_prediction_sorted) < 1))
})

#qsa_flexible_symmetric
qs = c(0.2, 0.5, 0.8)
true_values_new = c(rep(50,4), 60, rep(90,5))

df_combined <- test(qs=qs, true_values_new=true_values_new, methods = "qsa_flexible_symmetric")

updated_prediction_sorted <- df_combined[31:60,][order(df_combined[31:60,]$forecast_date),]$prediction 
optimum <- rep(c(10, 50, 90),10)

test_that("original and updated predictions are identical for qsa_flexible_symmetric with optim method BFGS", {
  expect_equal(TRUE, any(abs(optimum - updated_prediction_sorted) < 1))
})

#qsa_flexible
qs = c(0.2, 0.5, 0.8)
true_values_new = c(rep(50,4), 60, rep(90,5))

df_combined <- test(qs=qs, true_values_new=true_values_new, methods = "qsa_flexible")

updated_prediction_sorted <- df_combined[31:60,][order(df_combined[31:60,]$forecast_date),]$prediction 
optimum <- rep(c(50, 50, 90),10)

test_that("original and updated predictions are identical for qsa_flexible with optim method BFGS", {
  expect_equal(TRUE, any(abs(optimum - updated_prediction_sorted) < 1))
})

