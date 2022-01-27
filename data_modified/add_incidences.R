transform_to_incidences <- function(df) {
  df |>
    dplyr::mutate(
      true_value = (true_value / population) * 100000,
      prediction = (prediction / population) * 100000
    )
}


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### UK Data                                                                 ####

uk_data <- readr::read_csv(here::here("data", "full-data-uk-challenge.csv"))

uk_data_modified <- transform_to_incidences(uk_data) |> 
  dplyr::select(-c(population, target, expert))

readr::write_csv(
  uk_data_modified,
  file = here::here("data_modified", "uk_data_incidences.csv")
)


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Forecast Hub Data                                                       ####

hub_data_1 <- readr::read_csv(
  here::here("data", "full-data-european-forecast-hub-1.csv")
)

hub_data_2 <- readr::read_csv(
  here::here("data", "full-data-european-forecast-hub-2.csv")
)

hub_data <- dplyr::bind_rows(hub_data_1, hub_data_2)

country_info <- covidHubUtils::hub_locations_ecdc

# add population of countries to dataframe
hub_data <- hub_data |>
  dplyr::left_join(country_info) |>
  dplyr::select(
    location, location_name, target_end_date, target_type, true_value,
    forecast_date, quantile, prediction, model, horizon, population
  )

hub_data_modified <- transform_to_incidences(hub_data) |> 
  dplyr::select(-population)

# split in 3 data frames to keep file sizes below github limit of 100MB
num_rows <- 700000

hub_data_modified_1 <- hub_data_modified |>
  dplyr::slice(1:num_rows)

hub_data_modified_2 <- hub_data_modified |>
  dplyr::slice((num_rows + 1):(2 * num_rows))

hub_data_modified_3 <- hub_data_modified |>
  dplyr::slice((2 * num_rows + 1):nrow(hub_data_modified))

readr::write_csv(
  hub_data_modified_1,
  file = here::here("data_modified", "hub_data_1_incidences.csv")
)

readr::write_csv(
  hub_data_modified_2,
  file = here::here("data_modified", "hub_data_2_incidences.csv")
)

readr::write_csv(
  hub_data_modified_3,
  file = here::here("data_modified", "hub_data_3_incidences.csv")
)
