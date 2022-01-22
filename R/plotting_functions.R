#' @importFrom rlang .data

plot_quantiles <- function(df, model = NULL, location = NULL, quantiles = c(0.05, 0.5, 0.95)) {
  l <- process_model_input(df, model)
  df <- l$df
  model <- l$model

  l <- process_location_input(df, location)
  df <- l$df
  location_name <- l$location_name

  df |>
    filter_quantiles(quantiles) |>
    mutate_horizon() |>
    change_to_date(forecast = TRUE, target_end = TRUE) |>
    ggplot2::ggplot(mapping = ggplot2::aes(x = .data$target_end_date)) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(y = .data$prediction, color = factor(.data$quantile))
    ) +
    ggplot2::geom_line(mapping = ggplot2::aes(y = .data$true_value)) +
    ggplot2::scale_y_log10(labels = scales::label_number()) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data$target_type),
      cols = ggplot2::vars(.data$horizon),
      scales = "free_y"
    ) +
    ggplot2::labs(
      title = stringr::str_glue("Predicted Quantiles in {location_name}"),
      subtitle = stringr::str_glue("model: {model}")
    ) +
    set_labels() +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 1)) +
    ggplot2::theme_light() +
    modify_theme()
}


#' @importFrom rlang .data

plot_intervals <- function(df, model = NULL, location = NULL,
                           target_type = c("Cases", "Deaths"),
                           quantile = 0.05, horizon = 1, highlight_cv = TRUE) {
  target <- rlang::arg_match(arg = target_type, values = c("Cases", "Deaths"))
  h <- paste_horizon(horizon)

  l <- process_model_input(df, model)
  df <- l$df
  model <- l$model

  l <- process_location_input(df, location)
  df <- l$df
  location_name <- l$location_name

  p <- df |>
    process_quantile_pair(quantile) |>
    filter_target_types(target) |>
    filter_horizons(horizon) |>
    setup_intervals_plot() +
    ggplot2::labs(
      title = stringr::str_glue("Predicted {target} in {location_name} {h}"),
      subtitle = stringr::str_glue("model: {model}   |   quantile: {quantile}")
    ) +
    set_labels() +
    # making theme specifications before setting general theme does not work!
    ggplot2::theme_minimal() +
    modify_theme()

  if (highlight_cv) {
    p <- plot_training_end(p, df, type = "segment")
  }

  return(p)
}


#' @importFrom rlang .data

plot_intervals_grid <- function(df, model = NULL, location = NULL,
                                facet_by = c("horizon", "quantile"),
                                quantiles = NULL, horizon = NULL, highlight_cv = FALSE) {
  facet_by <- rlang::arg_match(arg = facet_by, values = c("horizon", "quantile"))

  l <- process_model_input(df, model)
  df <- l$df
  model <- l$model

  l <- process_location_input(df, location)
  df <- l$df
  location_name <- l$location_name

  if (facet_by == "horizon") {
    if (is.null(quantiles)) {
      quantiles <- 0.05
    }

    q <- quantiles
    df <- facet_horizon(df, quantiles, horizon)
  } else if (facet_by == "quantile") {
    if (is.null(horizon)) {
      horizon <- 1
    }

    if (is.null(quantiles)) {
      quantiles <- c(0.01, 0.05, 0.1, 0.25)
    }

    h <- paste_horizon(horizon)
    df <- facet_quantile(df, quantiles, horizon)
  }

  p <- setup_intervals_plot(df)

  if (highlight_cv) {
    p <- plot_training_end(p, df, type = "vline")
  }

  if (facet_by == "horizon") {
    p <- p +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data$target_type),
        cols = ggplot2::vars(.data$horizon),
        scales = "free_y"
      ) +
      ggplot2::labs(
        title = stringr::str_glue("Prediction Intervals in {location_name}"),
        subtitle = stringr::str_glue("model: {model}   |   quantile: {q}")
      ) +
      set_labels() +
      ggplot2::theme_light() +
      modify_theme()
  } else if (facet_by == "quantile") {
    p <- p +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data$target_type),
        cols = ggplot2::vars(.data$quantile_group),
        scales = "free_y"
      ) +
      ggplot2::labs(
        title = stringr::str_glue("Prediction Intervals in {location_name} {h}"),
        subtitle = stringr::str_glue("model: {model}")
      ) +
      set_labels() +
      ggplot2::theme_light() +
      modify_theme()
  }

  return(p)
}

plot_eval <- function(df_eval) {
  # keep value order of the first column the same in plot as in the
  # input dataframe
  df_eval[[1]] <- factor(df_eval[[1]], levels = df_eval[[1]])

  # use attributes for axis labels
  orig_columns <- attr(df_eval, which = "summarise_by")
  first_colname <- orig_columns[1]
  ylabels <- first_colname

  if (length(orig_columns) == 2) {
    xlabels <- orig_columns[2]
  } else {
    xlabels <- NULL
  }

  # for limits of colour pallette
  max_value <- df_eval |>
    dplyr::select(where(is.numeric)) |>
    abs() |>
    max(na.rm = TRUE)

  df_plot <- df_eval |>
    tidyr::pivot_longer(
      cols = -1, names_to = "colnames", values_to = "values"
    )

  # when there are no categories on x-axis, do not display any name
  if (dplyr::n_distinct(df_plot[[2]]) == 1) {
    df_plot[[2]] <- ""
  }

  df_plot |>
    ggplot2::ggplot(mapping = ggplot2::aes(
      y = !!dplyr::ensym(first_colname), x = colnames, fill = values
    )) +
    ggplot2::geom_tile() +
    ggplot2::scale_y_discrete(limits = rev, expand = c(0, 0)) +
    ggplot2::scale_fill_distiller(
      palette = "RdBu", limits = c(-max_value, max_value)
    ) +
    ggplot2::labs(
      x = xlabels, y = ylabels, fill = NULL,
      title = "Relative Changes in Weighted Interval Score after CQR Adjustments",
      subtitle = "- Negative Values indicate a lower (better) Score, positive Values a higher (worse) Score -"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5))
}
