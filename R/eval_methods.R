#' @importFrom rlang .data

eval_one_category <- function(df_combined, summarise_by) {
  if (dplyr::n_distinct(df_combined$method) > 2 && length(summarise_by) == 2) {
    stop("Multiple categories can only be evaluated for a single method.")
  }

  wide_format <- df_combined |>
    extract_validation_set() |>
    scoringutils::score() |>
    scoringutils::summarise_scores(by = c("method", summarise_by)) |>
    dplyr::select(.data$method:.data$interval_score) |>
    tidyr::pivot_wider(
      names_from = .data$method, values_from = .data$interval_score
    )
  
  # move ensemble column to the last position in data frame
  if ("ensemble" %in% colnames(wide_format)) {
    wide_format <- wide_format |> dplyr::relocate(ensemble, .after = dplyr::last_col())
  }

  all_columns <- colnames(wide_format)
  method_names <- all_columns[!all_columns %in% c(summarise_by, "original")]

  if (length(method_names) == 1) {
    # outputs data frame with two columns:
    # first column = selected category (e.g. target_type), second column =
    # relative change compared to original predictions in input data
    output_df <- wide_format |>
      dplyr::mutate(relative_change = (.data[[method_names]] - .data$original) / .data$original) |>
      dplyr::select(-c(.data[[method_names]], .data$original))

    return(output_df)
  }

  # add one column of relative change for each method in method column of df_combined
  # output column names are e.g. 'cqr' and 'qsa_uniform', hence the existing columns are overwritten and only the 'original' column has to be dropped
  for (method_name in method_names) {
    wide_format[method_name] <- (wide_format[method_name] - wide_format["original"]) / wide_format["original"]
  }

  wide_format |> dplyr::select(-.data$original)
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

add_margins <- function(df, row_margins, col_margins) {
  df |>
    # exclude column of first category
    add_row(c(NA, col_margins)) |>
    # exclude new appended row
    dplyr::mutate(margins = c(row_margins, NA))
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
    dplyr::pull(.data$average_change)

  df |> dplyr::mutate(average_change = average_change)
}

add_col_averages <- function(df) {
  average_change <- df |>
    # calculate geometric mean of multiplicative factors in each column
    dplyr::mutate(dplyr::across(.cols = -1, .fns = ~ .x + 1)) |>
    dplyr::summarise(dplyr::across(.cols = -1, .fns = geometric_mean) - 1)

  df |> add_row(c(NA, as.numeric(average_change[1, ])))
}

round_output <- function(df, round_digits) {
  if (!is.null(round_digits)) {
    df <- df |>
      dplyr::mutate(dplyr::across(
        .cols = -1, .fns = ~ round(.x, round_digits)
      ))
  }

  return(df)
}


eval_methods <- function(df_combined, summarise_by, margins = FALSE,
                         row_averages = FALSE, col_averages = FALSE,
                         round_digits = 4) {
  if ((margins && row_averages) || (margins && col_averages)) {
    stop("Either margins or averages can be specified.")
  }

  result_long_format <- eval_one_category(df_combined, summarise_by)

  # sort first column in increasing order, surprisingly this works
  result_long_format <- result_long_format |>
    dplyr::arrange(result_long_format[[1]])

  # if only one category is specified
  if (length(c(summarise_by)) == 1) {
    return(
      result_long_format |>
        round_output(round_digits) |>
        # plot_eval() needs to know original colnames after pivoting
        `attr<-`("summarise_by", summarise_by)
    )
  }

  result_wide_format <- result_long_format |>
    tidyr::pivot_wider(
      names_from = .data[[summarise_by[2]]], values_from = .data$relative_change
    )

  # either margins or table averages can be added
  if (margins) {
    row_margins <- eval_one_category(df_combined, summarise_by = summarise_by[1]) |>
      dplyr::pull(.data$relative_change)
    col_margins <- eval_one_category(df_combined, summarise_by = summarise_by[2]) |>
      dplyr::pull(.data$relative_change)

    return(
      add_margins(result_wide_format, row_margins, col_margins) |>
        round_output(round_digits) |>
        `attr<-`("summarise_by", summarise_by)
    )
  }

  if (row_averages) {
    result_wide_format <- result_wide_format |> add_row_averages()
  }

  if (col_averages) {
    result_wide_format <- result_wide_format |> add_col_averages()
  }

  return(
    result_wide_format |>
      round_output(round_digits) |>
      `attr<-`("summarise_by", summarise_by)
  )
}
