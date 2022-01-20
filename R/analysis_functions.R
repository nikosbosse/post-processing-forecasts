cqr_change_by_one <- function(df_combined, category) {
  df_combined |>
    extract_validation_set() |>
    scoringutils::eval_forecasts(summarise_by = c("method", category)) |>
    dplyr::select(method:interval_score) |>
    tidyr::pivot_wider(names_from = method, values_from = interval_score) |>
    dplyr::mutate(relative_change = (cqr - original) / original) |>
    dplyr::select(-c(cqr, original))
}

convert_row_types <- function(df, new_row) {
  for (i in seq_along(df)) {
    dtype <- class(df[[i]])
    if (dtype == "character") {
      new_row[[i]] <- as.character(new_row[[i]])
    } else if (dtype == "numeric") {
      new_row[[i]] <- as.numeric(new_row[[i]])
    }
  }
  return(new_row)
}

add_row <- function(df, row) {
  new_row <- as.list(row)

  # convert to compatible data types for appending
  df[nrow(df) + 1, ] <- convert_row_types(df, new_row)
  return(df)
}

add_marginals <- function(df, row_margins, col_margins) {
  df |>
    # exclude column of first category
    add_row(c(NA, col_margins)) |>
    # exclude new appended row
    dplyr::mutate(marginals = c(row_margins, NA))
}

geometric_mean <- function(x) {
  prod(x)^(1 / length(x))
}

add_row_averages <- function(df) {
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


add_col_averages <- function(df) {
  average_change <- df |>
    # calculate geometric mean of multiplicative factors in each column
    dplyr::mutate(dplyr::across(.cols = -1, .fns = ~ .x + 1)) |>
    dplyr::summarise(dplyr::across(.cols = -1, .fns = geometric_mean) - 1)

  df |> add_row(c(NA, as.numeric(average_change[1, ])))
}

# TODO: Write Unit Tests
cqr_change_by_categories <- function(df_combined, categories, marginals = FALSE,
                                     row_averages = FALSE, col_averages = FALSE) {
  one_category_result <- cqr_change_by_one(df_combined, categories)

  # if only one category is specified
  if (length(c(categories)) == 1) {
    return(one_category_result |> dplyr::arrange(relative_change))
  }

  # display horizon in increasing order for two-dimensional display
  if ("horizon" %in% categories) {
    one_category_result <- one_category_result |> dplyr::arrange(horizon)
  }

  two_categories_result <- one_category_result |>
    tidyr::pivot_wider(
      names_from = .data[[categories[2]]], values_from = relative_change
    )

  # either marginals or table averages can be added
  if (marginals) {
    row_margins <- cqr_change_by_one(df_combined, category = categories[1]) |>
      dplyr::pull(relative_change)
    col_margins <- cqr_change_by_one(df_combined, category = categories[2]) |>
      dplyr::pull(relative_change)

    return(add_marginals(two_categories_result, row_margins, col_margins))
  }

  if (row_averages) {
    two_categories_result <- two_categories_result |> add_row_averages()
  }

  if (col_averages) {
    two_categories_result <- two_categories_result |> add_col_averages()
  }

  return(two_categories_result)
}
