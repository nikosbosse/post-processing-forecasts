# run script to generate cqr, qsa_uniform and ensemble updates for all complete
# models in european forecast hub data
# including incomplete models as well requires individual treatment of models


QSA_UNIFORM <- FALSE
QSA_FLEXIBlE_SYMMETRIC <- FALSE
QSA_FLEXIBLE <- FALSE


devtools::load_all()
library(foreach)
library(doParallel)
registerDoParallel(cores=2)
#https://stackoverflow.com/questions/30688307/parallelization-doesnt-work-with-the-foreach-package
#my mac has 2 cores, see this by running the following line in your terminal: system_profiler SPHardwareDataType
#https://techwiser.com/how-many-cores-does-my-cpu-have/


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


if (QSA_UNIFORM) {
  df_updated <- update_predictions(
    df = uk_data, methods = "qsa_uniform", models = complete_models,
    cv_init_training = cv_init_training, verbose = TRUE, parallel = TRUE
  )
  
  df_combined <- df_updated |> collect_predictions()
  
  readr::write_rds(
    df_combined,
    file = here::here("data_results", "hub_qsa_uniform.rds")
  )
}


if (QSA_FLEXIBLE_SYMMETRIC) {
  df_updated <- update_predictions(
    df = uk_data, methods = "qsa_flexible_symmetric", models = complete_models,
    cv_init_training = cv_init_training, verbose = TRUE, parallel = TRUE
  )
  
  df_combined <- df_updated |> collect_predictions()
  
  readr::write_rds(
    df_combined,
    file = here::here("data_results", "hub_qsa_flexible_symmetric.rds")
  )
}


if (QSA_FLEXIBLE_SYMMETRIC) {
  df_updated <- update_predictions(
    df = uk_data, methods = "qsa_flexible", models = complete_models,
    cv_init_training = cv_init_training, verbose = TRUE, parallel = TRUE
  )
  
  df_combined <- df_updated |> collect_predictions()
  
  readr::write_rds(
    df_combined,
    file = here::here("data_results", "hub_qsa_flexible.rds")
  )
}