# run script to generate cqr, qsa_uniform and ensemble updates for all complete
# models in uk data
# including incomplete models as well requires individual treatment of models

CQR <- FALSE
<<<<<<< HEAD
QSA_UNIFORM <- TRUE
QSA_FLEXIBEL <- FALSE
QSA_FLEXIBEL_SYMMETRIC <- FALSE
=======
CQR3 <- FALSE
CQR3_ENSEMBLE <- TRUE
QSA_UNIFORM <- FALSE
>>>>>>> master
CQR_QSA_UNIFORM <- FALSE


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
    df = uk_data, methods = "cqr",
    models = complete_models, cv_init_training = cv_init_training, verbose = TRUE
  )

  df_combined <- df_updated |> collect_predictions()

  readr::write_rds(df_combined, file = here::here("data_results", "uk_cqr.rds"))
}

if (CQR3) {
  df_updated <- update_predictions(
    df = uk_data, methods = c("cqr", "cqr_asymmetric", "cqr_multiplicative"),
    models = complete_models, cv_init_training = cv_init_training, verbose = TRUE
  )

  df_combined <- df_updated |> collect_predictions()

  readr::write_rds(df_combined, file = here::here("data_results", "uk_cqr3.rds"))
}

if (CQR3_ENSEMBLE) {
  df_combined <- readr::read_rds(here::here("data_results", "uk_cqr3.rds"))
  df_ensemble <- add_ensemble(
    df_combined,
    train_val_split = TRUE, verbose = TRUE, max_iter = 1e5
  )

  readr::write_rds(df_ensemble, here::here("data_results", "uk_cqr3_ensemble.rds"))
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

if (QSA_FLEXIBEL) {
  df_updated <- update_predictions(
    df = uk_data, methods = "qsa_flexibel", models = complete_models,
    cv_init_training = cv_init_training, verbose = TRUE
  )
  
  df_combined <- df_updated |> collect_predictions()
  
  readr::write_rds(
    df_combined,
    file = here::here("data_results", "uk_qsa_flexibel.rds")
  )
}

if (QSA_FLEXIBEL_SYMMETRIC) {
  df_updated <- update_predictions(
    df = uk_data, methods = "qsa_flexibel_symmetric", models = complete_models,
    cv_init_training = cv_init_training, verbose = TRUE
  )
  
  df_combined <- df_updated |> collect_predictions()
  
  readr::write_rds(
    df_combined,
    file = here::here("data_results", "uk_qsa_flexibel_symmetric.rds")
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
