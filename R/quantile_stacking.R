# Link tp package: https://ryantibs.github.io/quantgen/stacking_example.html

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Toy Example                                                             ####


pred1 <- matrix(rep(20:29, 3), nrow = 10, ncol = 3, byrow = FALSE)
pred2 <- matrix(rep(11:20, 3), nrow = 10, ncol = 3, byrow = FALSE)
pred3 <- matrix(rep(21:30, 3), nrow = 10, ncol = 3, byrow = FALSE)

q_arr <- quantgen::combine_into_array(pred1, pred2, pred3)

true_y <- 1:10

quantiles <- c(0.05, 0.2, 0.3)


res <- quantgen::quantile_ensemble(
  qarr = q_arr, y = true_y, tau = quantiles, verbose = TRUE
)

coef(res)


res2 <- quantgen::quantile_ensemble(
  qarr = q_arr, y = true_y, tau = quantiles,
  tau_groups = c(1, 2, 2), verbose = TRUE
)

coef(res2)

predict(res2, newq = q_arr)


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Example with UK Data                                                    ####


library(dplyr)
df <- read.csv("data/full-data-uk-challenge.csv")

model1 <- "epiforecasts-EpiExpert"
model2 <- "EuroCOVIDhub-ensemble"
model3 <- "EuroCOVIDhub-baseline"

y <- df$true_value

quantiles <- c(0.05)

# (104 x 1)
pred_model1 <- df |>
  filter(model == model1, quantile == quantiles) |>
  pull(prediction) |>
  as.matrix()

# (104 x 1)
pred_model2 <- df |>
  filter(model == model2, quantile == quantiles) |>
  pull(prediction) |>
  as.matrix()

# (104 x 1)
pred_model3 <- df |>
  filter(model == model3, quantile == quantiles) |>
  pull(prediction) |>
  as.matrix()

# Step 1: Combine into Array with dimension (104 x 3 x 1)
qarr <- quantgen::combine_into_array(pred_model1, pred_model2, pred_model3)

# Step 2: Find Optimal Weights of linear combination of the models
model <- quantgen::quantile_ensemble(qarr = qarr, y = y, tau = quantiles, verbose = TRUE)

# Weights
weights <- coef(model)
weights

# Updated Predictions
ensemble_predictions <- predict(model, newq = qarr)

# here identical with predictions from model 3
all(ensemble_predictions == pred_model3)
