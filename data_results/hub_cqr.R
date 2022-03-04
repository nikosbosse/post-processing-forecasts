# run script to generate cqr, qsa_uniform and ensemble updates for all complete
# models in european forecast hub data
# including incomplete models as well requires individual treatment of models

CQR <- FALSE
CQR2 <- TRUE
CQR_QSA_UNIFORM_ENSEMBLE_SUBSET <- FALSE

devtools::load_all()
cv_init_training <- 0.5

hub_data_1 <- readr::read_csv(
  here::here("data_modified", "hub_data_1_incidences.csv")
)

hub_data_2 <- readr::read_csv(
  here::here("data_modified", "hub_data_2_incidences.csv")
)

hub_data_3 <- readr::read_csv(
  here::here("data_modified", "hub_data_3_incidences.csv")
)

hub_data <- dplyr::bind_rows(hub_data_1, hub_data_2, hub_data_3)


# find all combinations of model and country with complete data
model_country_combinations <- hub_data |>
  dplyr::count(model, location) |>
  dplyr::filter(n == max(n))

# select only models with complete data for at least 10 countries
models <- model_country_combinations |>
  dplyr::count(model) |>
  dplyr::filter(n >= 10) |>
  dplyr::pull(model)

# select only countries which have complete data for selected models
locations <- model_country_combinations |>
  dplyr::filter(model %in% models) |>
  dplyr::count(location) |>
  dplyr::filter(n == max(n)) |>
  dplyr::pull(location)

if (CQR) {
  # fit cqr() for 6 different models with the same 18 locations each
  df_updated <- update_predictions(
    df = hub_data, methods = "cqr", models = models, locations = locations,
    cv_init_training = cv_init_training, verbose = TRUE
  )

  df_combined <- df_updated |> collect_predictions()

  # split in 2 data frames to keep file sizes below github limit of 100MB
  num_rows <- as.integer(nrow(df_combined) / 2)

  df_combined_1 <- df_combined |>
    dplyr::slice(1:num_rows)

  df_combined_2 <- df_combined |>
    dplyr::slice((num_rows + 1):nrow(df_combined))

  readr::write_rds(df_combined_1, file = here::here("data_results", "hub_cqr_1.rds"))
  readr::write_rds(df_combined_2, file = here::here("data_results", "hub_cqr_2.rds"))
}

if (CQR2) {
  df_updated <- update_predictions(
    df = hub_data, methods = c("cqr", "cqr_asymmetric"),
    models = models, locations = locations,
    cv_init_training = cv_init_training, verbose = TRUE
  )

  df_combined <- df_updated |> collect_predictions()

  # split in 2 data frames to keep file sizes below github limit of 100MB
  num_rows <- as.integer(nrow(df_combined) / 2)

  df_combined_1 <- df_combined |>
    dplyr::slice(1:num_rows)

  df_combined_2 <- df_combined |>
    dplyr::slice((num_rows + 1):nrow(df_combined))

  readr::write_rds(df_combined_1, file = here::here("data_results", "hub_cqr2_1.rds"))
  readr::write_rds(df_combined_2, file = here::here("data_results", "hub_cqr2_2.rds"))
}

if (CQR_QSA_UNIFORM_ENSEMBLE_SUBSET) {
  # fit cqr(), qsa_uniform() and add_ensemble() for 1 model, only Cases and the same 18 locations each
  df_updated <- update_predictions(
    df = hub_data,
    methods = c("cqr", "qsa_uniform"),
    models = "epiforecasts-EpiNow2",
    locations = locations,
    target_types = "Cases",
    cv_init_training = cv_init_training,
    verbose = TRUE
  )

  df_combined <- df_updated |> collect_predictions()

  df_extended <- df_combined |>
    add_ensemble(per_quantile_weights = TRUE, verbose = TRUE)

  readr::write_rds(
    df_extended,
    file = here::here("data_results", "hub_cqr_qsa_uniform_ensemble_subset.rds")
  )
}
