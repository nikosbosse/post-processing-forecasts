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

geometric_mean <- function(x) {
  prod(x)^(1 / length(x))
}

add_row_average <- function(df) {
  average_change <- df |>
    # values are percentage changes => add one to get multiplicative changes
    dplyr::mutate(dplyr::across(.cols = -1, .fns = ~ .x + 1)) |>
    dplyr::rowwise() |>
    # calculate geometric mean of multiplicative factors in each row and
    # subtract one again to get percentage change
    dplyr::summarise(
      average_change = geometric_mean(dplyr::c_across(cols = -1)) - 1
    ) |>
    dplyr::pull(average_change)

  df |> dplyr::mutate(average_change = average_change)
}

add_col_average <- function(df) {
  average_change <- df |>
    # calculate geometric mean of multiplicative factors in each column
    dplyr::mutate(dplyr::across(.cols = -1, .fns = ~ .x + 1)) |>
    dplyr::summarise(dplyr::across(.cols = -1, .fns = geometric_mean) - 1)

  # add as new row to dataframe, surprisingly difficult to add rows to df
  df[nrow(df) + 1, ] <- c(NA, as.numeric(average_change[1, ])) |> as.list()

  return(df)
}
