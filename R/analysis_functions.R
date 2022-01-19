cqr_change_by_categories <- function(df_combined, categories) {
  one_category_result <- df_combined |>
    extract_validation_set() |>
    scoringutils::eval_forecasts(summarise_by = c("method", categories)) |>
    dplyr::select(method:interval_score) |>
    tidyr::pivot_wider(names_from = method, values_from = interval_score) |>
    dplyr::mutate(relative_change = (cqr - original) / original) |>
    dplyr::select(-c(cqr, original))

  # if one category is specified
  if (length(c(categories)) == 1) {
    return(one_category_result |> dplyr::arrange(relative_change))
  }

  # display horizon in increasing order for two-dimenensional display
  if ("horizon" %in% categories) {
    one_category_result <- one_category_result |> dplyr::arrange(horizon)
  }

  # if two categories are specified
  one_category_result |>
    tidyr::pivot_wider(
      names_from = .data[[categories[2]]], values_from = relative_change
    )
}
