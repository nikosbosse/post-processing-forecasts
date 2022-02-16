devtools::load_all(".")
library(scoringutils)
library(dplyr)


df <- read.csv(here::here("data_modified", "hub_data_2_incidences.csv"))
m <- "epiforecasts-EpiExpert"
l <- "GB"
t <- "Cases"
h <- 1


test <- function(qs = c(0.25, 0.5, 0.75),
                 error_factor = 1,
                 true_values_new = c(rep(50,4), 55, rep(5,5), rep(c(50,5),8),50),
                 methods = "qsa_uniform",
                 cv_init_training = NULL,
                 quantile_plot = 0.25,
                 optim_method = "BFGS"){
  
  # Get a subset of original data as template 
  # We only make it have the length of the true values (max remains 25)
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
                                   cv_init_training = cv_init_training, penalty_weight=NULL, optim_method = optim_method,
                                   return_list = TRUE)
  
  # Combine DataFrames so that we can plot results
  df_combined <- df_updated |> collect_predictions()
  
  # Plot results for a specific quantile
  plot_intervals(df_combined, model = m, location = l, target_type = t, quantile = quantile_plot, horizon = h)
  
}

# Test1
# Expectation: QSA should make intervals smaller
# Observation: it doesnt?
test(qs = c(0.25, 0.5, 0.75),
     error_factor = 1,
     true_values_new = c(rep(50,4), 60, rep(20,5)),
     methods = "qsa_uniform",
     cv_init_training = NULL,
     quantile_plot = 0.25)

# Test1.1
# Expectation: QSA should make intervals larger because other values are far away
# Observation: intervals stay the same
test(qs = c(0.25, 0.5, 0.75),
     error_factor = 1,
     true_values_new = c(rep(50,4), 60, rep(200,5)),
     methods = "qsa_uniform",
     cv_init_training = NULL,
     quantile_plot = 0.25)

# Test2
# Expectation: QSA should make intervals smaller
# Observation: makes intervals zero
test(qs = c(0.25, 0.5, 0.75),
     error_factor = 1,
     true_values_new = c(rep(50,4), 60, rep(50,5)),
     methods = "qsa_uniform",
     cv_init_training = NULL,
     quantile_plot = 0.25)

# Test3
# Expectation: QSA should make intervals smaller
# Observation: makes intervals smaller so that then encompase at least 5 values
test(qs = c(0.25, 0.5, 0.75),
     error_factor = 1,
     true_values_new = c(rep(50,4), 60, rep(40,5)),
     methods = "qsa_uniform",
     cv_init_training = NULL,
     quantile_plot = 0.25)


# Test4
# Expectation: QSA should make intervals larger
# Observation: makes intervals larger to encompase 5 values
test(qs = c(0.25, 0.5, 0.75),
     error_factor = 1,
     true_values_new = c(rep(50,4), 80, rep(5,5)),
     methods = "qsa_uniform",
     cv_init_training = NULL,
     quantile_plot = 0.25)

############################################################################################################

# Test5
# Expectation: QSA should make intervals larger
# Observation: makes intervals larger to encompase 5 values
test(qs = c(0.4, 0.5, 0.6),
     error_factor = 1,
     true_values_new = c(rep(50,2), 80, rep(5,5)),
     methods = "qsa_uniform",
     cv_init_training = NULL,
     quantile_plot = 0.40)


#--> seems like QSA makes intervals larger if it doesnt cover enough values
#    It isnt affected by the distance of the values it doesnt need to cover?
#    It also doesnt make intervall smaller allthough it can, except if it can make intervalls go to zero

test(qs = c(0.25, 0.5, 0.75),
     error_factor = 1,
     true_values_new = c(rep(50,4), 51, rep(100,5)),
     methods = "qsa_uniform",
     cv_init_training = NULL,
     quantile_plot = 0.25)


# Trying different optimization methods

# apparently unreliable for one parameter
test(qs = c(0.25, 0.5, 0.75),
     error_factor = 1,
     true_values_new = c(rep(50,4), 60, rep(20,5)),
     methods = "qsa_uniform",
     cv_init_training = NULL,
     quantile_plot = 0.25,
     optim_method = "Nelder-Mead")

# Cant handle lower and upper bounds
test(qs = c(0.25, 0.5, 0.75),
     error_factor = 1,
     true_values_new = c(rep(50,4), 60, rep(20,5)),
     methods = "qsa_uniform",
     cv_init_training = NULL,
     quantile_plot = 0.25,
     optim_method = "BFGS")

test(qs = c(0.25, 0.5, 0.75),
     error_factor = 1,
     true_values_new = c(rep(50,4), 60, rep(20,5)),
     methods = "qsa_uniform",
     cv_init_training = NULL,
     quantile_plot = 0.25,
     optim_method = "CG")

#can handle lower and upper bounds
test(qs = c(0.25, 0.5, 0.75),
     error_factor = 1,
     true_values_new = c(rep(50,4), 60, rep(20,5)),
     methods = "qsa_uniform",
     cv_init_training = NULL,
     quantile_plot = 0.25,
     optim_method = "L-BFGS-B")

#Destroys R session
test(qs = c(0.25, 0.5, 0.75),
     error_factor = 1,
     true_values_new = c(rep(50,4), 60, rep(20,5)),
     methods = "qsa_uniform",
     cv_init_training = NULL,
     quantile_plot = 0.25,
     optim_method = "SANN")

# Added lower an upper bounds of (-100,100) because this optimization requires bounds
# Moved in the interval most 
test(qs = c(0.25, 0.5, 0.75),
     error_factor = 1,
     true_values_new = c(rep(50,4), 60, rep(20,5)),
     methods = "qsa_uniform",
     cv_init_training = NULL,
     quantile_plot = 0.25,
     optim_method = "Brent")


# Larger distance of outliers
test(qs = c(0.25, 0.5, 0.75),
     error_factor = 1,
     true_values_new = c(rep(50,4), 60, rep(5,5)),
     methods = "qsa_uniform",
     cv_init_training = NULL,
     quantile_plot = 0.25,
     optim_method = "Brent")

# Larger distance of outliers
test(qs = c(0.25, 0.5, 0.75),
     error_factor = 1,
     true_values_new = c(rep(50,4), 60, rep(5,5)),
     methods = "qsa_uniform",
     cv_init_training = NULL,
     quantile_plot = 0.25,
     optim_method = "L-BFGS-B")

#check if optim has verbose param to get internal information
# warum gibt uns BFGS genau 1




