devtools::load_all()

uk_cqr3 <- readr::read_rds(here::here("data_results", "uk_cqr3.rds"))
uk_cqr3 <- uk_cqr3 |> dplyr::filter(method != "cqr_multiplicative")

uk_qsa_uniform <- readr::read_rds(here::here("data_results", "uk_qsa_uniform.rds"))
uk_qsa_uniform <- uk_qsa_uniform |> dplyr::filter(method == "qsa_uniform")

uk_qsa_flexible_symmetric <- readr::read_rds(here::here("data_results", "uk_qsa_flexible_symmetric.rds"))
uk_qsa_flexible_symmetric <- uk_qsa_flexible_symmetric |> dplyr::filter(method == "qsa_flexible_symmetric")

uk_qsa_flexible <- readr::read_rds(here::here("data_results", "uk_qsa_flexible.rds"))
uk_qsa_flexible <- uk_qsa_flexible |> dplyr::filter(method == "qsa_flexible")

uk_cqr_qsa <- dplyr::bind_rows(uk_cqr3, uk_qsa_uniform, uk_qsa_flexible_symmetric, uk_qsa_flexible)

uk_complete <- add_ensemble(uk_cqr_qsa, verbose = TRUE)

readr::write_rds(uk_complete, here::here("data_results", "uk_complete.rds"))