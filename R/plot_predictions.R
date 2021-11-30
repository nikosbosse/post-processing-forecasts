# example:
#
# df <- readr::read_csv(file = "data/full-data-uk-challenge.csv")
# plot_predictions(
#   df = df, model = "epiforecasts-EpiExpert",
#   alphas = c(0.01, seq(0.1, 0.9, 0.1), 0.99)
# )

plot_predictions <- function(df, model = NULL, alphas = c(0.05, 0.5, 0.95)) {
  if (!is.null(model)) {
    df <- filter_model(df, model)
  }


  df |>
    filter_alpha_asym(alphas) |>
    mutate_horizon() |>
    ggplot2::ggplot(mapping = ggplot2::aes(x = .data$forecast_date)) +
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
      title = stringr::str_glue("Quantile Predictions for {model} model"),
      subtitle = "True Values indicated by black line",
      color = NULL, x = NULL, y = NULL,
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 1)) +
    ggplot2::theme_light() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      legend.position = "top"
    )
}
