devtools::load_all()

uk_data <- readr::read_csv(
  here::here("data_modified", "uk_data_incidences.csv")
)

complete_models <- uk_data |>
  dplyr::count(model) |>
  dplyr::filter(n == max(n)) |>
  dplyr::pull(model)

fit_cqr <- function(cv_init_training) {
  df_updated <- update_predictions(
    df = uk_data, methods = c("cqr", "cqr_asymmetric", "cqr_multiplicative"),
    models = complete_models, cv_init_training = cv_init_training, verbose = TRUE
  )
  
  df_combined <- df_updated |> 
    collect_predictions() |> 
    dplyr::mutate(cv_init_training = cv_init_training)
}

cv_init_training <- seq(0, 0.8, 0.1)

full_results <- purrr::map_dfr(.x = cv_init_training, .f = fit_cqr)

readr::write_rds(full_results, here::here("data_results", "uk_cqr3_cv.rds"))