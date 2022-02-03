# run script to generate cqr and/or qsa_uniform updates for all complete models
# in uk data including all models requires individual treatment of models

CQR <- FALSE
QSA_UNIFORM <- FALSE
CQR_QSA_UNIFORM <- TRUE

devtools::load_all()
cv_init_training <- 0.5

uk_data <- readr::read_csv(
  here::here("data_modified", "uk_data_incidences.csv")
)

complete_models <- uk_data |>
  dplyr::count(model) |>
  dplyr::filter(n == max(n)) |>
  dplyr::pull(model)

if (CQR) {
  df_updated <- update_predictions(
    df = uk_data, methods = "cqr", models = complete_models,
    cv_init_training = cv_init_training, verbose = TRUE
  )

  df_combined <- df_updated |> collect_predictions()

  readr::write_rds(df_combined, file = here::here("data_results", "uk_cqr.rds"))
}

if (QSA_UNIFORM) {
  df_updated <- update_predictions(
    df = uk_data, methods = "qsa_uniform", models = complete_models,
    cv_init_training = cv_init_training, verbose = TRUE
  )

  df_combined <- df_updated |> collect_predictions()

  readr::write_rds(
    df_combined,
    file = here::here("data_results", "uk_qsa_uniform.rds")
  )
}

if (CQR_QSA_UNIFORM) {
  df_updated <- update_predictions(
    df = uk_data, methods = c("cqr", "qsa_uniform"), models = complete_models,
    cv_init_training = cv_init_training, verbose = TRUE
  )

  df_combined <- df_updated |> collect_predictions()

  readr::write_rds(
    df_combined,
    file = here::here("data_results", "uk_cqr_qsa_uniform.rds")
  )
}
