# example:
#
# df <- readr::read_csv(file = "data/full-data-uk-challenge.csv")
# df_new <- df |> dplyr::mutate(prediction = prediction * 2)
# df_combined <- collect_predictions(original = df, new = df_new)
# plot_intervals(df = df_combined, model = "epiforecasts-EpiExpert", alpha = 0.05)
#' @importFrom rlang .data

plot_intervals <- function(df, model = NULL, alpha = 0.05) {
  if (!is.null(model)) {
    df <- filter_model(df, model)
  }

  df |>
    filter_alpha_sym(alpha = alpha) |>
    paste_horizon() |>
    change_to_date() |> 
    tidyr::pivot_wider(names_from = .data$quantile, values_from = .data$prediction) |>
    ggplot2::ggplot(mapping = ggplot2::aes(x = .data$forecast_date)) +
    ggplot2::geom_point(ggplot2::aes(y = .data$true_value), size = 1) +
    ggplot2::geom_line(ggplot2::aes(y = .data$true_value)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$lower, ymax = .data$upper, color = .data$method),
      position = ggplot2::position_dodge2(padding = 0.01)
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data$target_type),
      cols = ggplot2::vars(.data$horizon),
      scales = "free_y"
    ) +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::labs(
      x = NULL, y = NULL, color = NULL,
      title = stringr::str_glue("Prediction Intervals for {model} model at level {alpha}"),
      subtitle = "Prediction methods separated by color"
    ) +
    ggplot2::theme_light() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      legend.position = "top"
    )
}
