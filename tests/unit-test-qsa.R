devtools::load_all(".")
library(scoringutils)
library(dplyr)

df <- read.csv(here::here("data_modified", "hub_data_2_incidences.csv"))

m <- "epiforecasts-EpiExpert"
l <- "GB"
t <- "Cases"
h <- 1
qs <- c(0.2, 0.4, 0.5, 0.6, 0.8) #seq(0.100,0.900,0.100) doesnt work?
#cv_init_training <- 10

subset <- dplyr::filter(df, model == m & location == l & target_type == t & horizon == h & quantile %in% qs)

length(unique(subset$target_end_date))
length(unique(subset$quantile)) == length(qs)


subset_generated <- subset
for (qi in qs) {
  subset_generated <- subset_generated |>
    dplyr::mutate(prediction = replace(
      .data$prediction, .data$quantile == qi,
      values = rep(qi * 100,length(unique(subset$target_end_date)))
    ))
}

true_values = c(rep(10,5),
                rep(30,5),
                rep(45,2),
                rep(50,1),
                rep(55,2),
                rep(70,5),
                rep(90,5))

for (i in length(unique(subset_generated$target_end_date)) ) {
  date <- unique(subset_generated$target_end_date)[i]
  value <- true_values[i]
  
  subset_generated <- subset_generated |>
    dplyr::mutate(prediction = replace(
      .data$true_value, .data$target_end_date == date,
      values = rep(value,length(unique(subset$quantile)))
    ))
}

df_combined_1 <- update_predictions(subset_generated, methods = "qsa_uniform",
                   models = m, locations = l, target_types = t,
                   horizons = h, quantiles = qs,
                   cv_init_training = 10, penalty_weight=NULL, return_list = TRUE)

plot_intervals(df_combined_1$qsa_uniform, model = m, location = l, target_type = "Cases", quantile = 0.2, horizon = 1)

