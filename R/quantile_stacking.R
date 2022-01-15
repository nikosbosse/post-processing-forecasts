# Link tp package: https://ryantibs.github.io/quantgen/stacking_example.html

library(dplyr)
library(tidyr)

df <- read.csv("data/full-data-uk-challenge.csv")

models <- unique(df$model)
quantiles <- unique(df$quantile) |> na.omit()
target_types <- "Cases"
horizons <- 1

# create list of matrices, each entry contains values for one specific quantile
matrix_list <- list()

# list of models that are complete for all quantiles
final_models <- NULL

# save target_end_date column of reduced data frame for later rejoining
target_end_dates <- NULL

for (quantile in quantiles) {
  q <- quantile
  # filter dataframe for specific combination of quantile, target_type and horizon
  df_filtered <- df |>
    # expert column creates NA entries after pivot_wider()
    select(-expert) |>
    drop_na() |>
    filter(prediction != 0) |>
    filter(quantile == q, target_type %in% target_types, horizon %in% horizons)

  # stores only values from first iteration
  if (is.null(target_end_dates)) {
    target_end_dates <- unique(df_filtered$target_end_date)
    true_values <- unique(df_filtered$true_value)
  }

  # find models with complete prediction column for this variable combination
  quantile_models <- df_filtered |>
    group_by(model) |>
    summarise(n = n()) |>
    filter(n == max(n)) |>
    pull(model)

  # update list of complete models with information from current quantile in loop
  if (is.null(final_models)) {
    final_models <- quantile_models
  } else {
    final_models <- intersect(final_models, complete_models)
  }

  prediction_matrix <- df_filtered |>
    # consider only models with complete prediction column for this variable combination
    filter(model %in% final_models) |>
    pivot_wider(names_from = model, values_from = prediction) |>
    select(all_of(final_models), quantile) |>
    as.matrix()

  matrix_list[[as.character(q)]] <- prediction_matrix
}

drop_models <- function(array, final_models) {
  array[, colnames(array) %in% c(final_models, "quantile")]
}

qarr <- matrix_list |>
  # make sure all quantile matrices have the same column names, otherwise take intersection
  purrr::map(.f = ~ drop_models(.x, final_models)) |>
  # transform two dimensional matrix into three-dimensional array
  # last dimension is given by unique quantile values
  abind::abind(along = 3)

y <- df |>
  filter(target_type %in% target_types, horizon %in% horizons) |>
  distinct(true_value) |>
  pull(true_value)

ensemble_model <- quantgen::quantile_ensemble(qarr, y, tau = quantiles)

# contributing models
final_models

# corresponding weights
coef(ensemble_model)

# predictions based on learned weights
predictions <- predict(ensemble_model, newq = qarr)
colnames(predictions) <- quantiles
predictions

# next task: bring these updated predictions into prediction column of uk_data frame
df_updated <- predictions |>
  as.data.frame() |>
  mutate(target_type = target_types, horizon = horizons, target_end_date = target_end_dates, true_value = true_values) |>
  pivot_longer(cols = `0.01`:`0.99`, names_to = "quantile", values_to = "prediction") |>
  mutate(quantile = as.numeric(quantile))

# evaluation of updated predictions for this combination of horizon and target_type
df_updated |>
  scoringutils::eval_forecasts(summarise_by = c("target_type", "horizon")) |>
  arrange(horizon)

# evaluation of original predictions for this combination of horizon and target_type
df |>
  filter(model == models[2], target_type == target_types, horizon == horizons) |>
  scoringutils::eval_forecasts(summarise_by = c("target_type", "horizon")) |>
  arrange(horizon)
