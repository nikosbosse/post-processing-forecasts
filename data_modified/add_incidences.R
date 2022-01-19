uk_data <- readr::read_csv(here::here("data", "full-data-uk-challenge.csv"))

hub_data_1 <- readr::read_csv(here::here("data", "full-data-european-forecast-hub-1.csv"))
hub_data_2 <- readr::read_csv(here::here("data", "full-data-european-forecast-hub-2.csv"))

hub_data <- dplyr::bind_rows(hub_data_1, hub_data_2)

transform_to_incidences <- function(df) {
  df |>
    dplyr::mutate(
      true_value = (true_value / population) * 100000,
      prediction = (prediction / population) * 100000
    )
}

uk_data_modified <- transform_to_incidences(uk_data)
readr::write_csv(
  uk_data_modified,
  file = here::here("data_modified", "uk_data_incidences.csv")
)

# eu hub data does not contain population column in current version

# hub_data_modified <- transform_to_incidences(hub_data)
# readr::write_csv(
#   hub_data_modified,
#   file = here::here("data_modified", "hub_data_incidences.csv")
# )
