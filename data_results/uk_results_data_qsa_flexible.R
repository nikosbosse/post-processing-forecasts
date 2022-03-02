QSA_FLEXIBLE <- TRUE
devtools::load_all()
library(foreach)
library(doParallel)

registerDoParallel(cores = 8)
# https://stackoverflow.com/questions/30688307/parallelization-doesnt-work-with-the-foreach-package
# my mac has 2 cores, see this by running the following line in your terminal: system_profiler SPHardwareDataType
# https://techwiser.com/how-many-cores-does-my-cpu-have/

cv_init_training <- 0.5

uk_data <- readr::read_csv(
  here::here("data_modified", "uk_data_incidences.csv")
)

complete_models <- uk_data |>
  dplyr::count(model) |>
  dplyr::filter(n == max(n)) |>
  dplyr::pull(model)


if (QSA_FLEXIBLE) {
  df_updated <- update_predictions(
    df = uk_data, methods = "qsa_flexible", models = complete_models,
    cv_init_training = cv_init_training, parallel = TRUE
  )

  df_combined <- df_updated |> collect_predictions()

  readr::write_rds(
    df_combined,
    file = here::here("data_results", "uk_qsa_flexible.rds")
  )
}
