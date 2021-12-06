#' @importFrom rlang .data

facet_horizon <- function(df, quantile, horizon) {
  df |>
    process_quantile_pair(quantile) |> 
    mutate_horizon() 
}

facet_quantile <- function(df, quantiles, horizon) {
  df |> 
    filter_horizon(horizon) |> 
    filter_quantile_pairs(quantiles) |> 
    add_quantile_group(quantiles)
}

setup_intervals_plot <- function(df) {
  df |> 
    tidyr::pivot_wider(names_from = .data$quantile, values_from = .data$prediction) |>
    ggplot2::ggplot(mapping = ggplot2::aes(x = .data$target_end_date)) +
    ggplot2::geom_point(ggplot2::aes(y = .data$true_value), size = 1) +
    ggplot2::geom_line(ggplot2::aes(y = .data$true_value)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$lower, ymax = .data$upper, color = .data$method),
      position = ggplot2::position_dodge2(padding = 0.01)
    ) +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::labs(
      x = NULL, y = NULL, color = NULL,
      subtitle = "Prediction methods separated by color"
    ) +
    ggplot2::theme_light() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      legend.position = "top"
    )
}