#' @importFrom rlang .data

facet_horizon <- function(df, quantile, horizon) {
  df |>
    process_quantile_pair(quantile) |>
    mutate_horizon()
}

facet_quantile <- function(df, quantiles, horizon) {
  df |>
    filter_horizons(horizon) |>
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
    ggplot2::scale_color_brewer(palette = "Set1")
}

set_labels <- function() {
  ggplot2::labs(x = NULL, y = NULL, color = NULL)
}

modify_theme <- function() {
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5),
    legend.position = "top"
  )
}

plot_training_end <- function(p, df, type = c("segment", "vline")) {
  cv_init_training <- attr(df, "cv_init_training")

  if (!is.null(cv_init_training)) {
    training_end_date <- lubridate::ymd(unique(df$target_end_date)[cv_init_training])
    if (type == "segment") {
      p <- p + ggplot2::geom_segment(ggplot2::aes(
        x = training_end_date, xend = training_end_date,
        y = min(.data$lower), yend = max(.data$upper)
      ),
      linetype = "dashed", color = "grey60"
      )
    } else if (type == "vline") {
      p <- p + ggplot2::geom_vline(
        xintercept = training_end_date, linetype = "dashed", color = "grey60"
      )
    }
  }
  return(p)
}

plot_heatmap <- function(df_plot, first_colname, max_value, xlabel, ylabel) {
  # to use columns as strings in aes(), encode string first to symbol with sym()
  # and then decode / unquote it with !! inside of aes()
  first_colname <- rlang::sym(first_colname)

  df_plot |>
    ggplot2::ggplot(mapping = ggplot2::aes(
      y = !!first_colname, x = colnames, fill = values
    )) +
    ggplot2::geom_tile() +
    ggplot2::scale_y_discrete(limits = rev, expand = c(0, 0)) +
    ggplot2::scale_fill_distiller(
      palette = "RdBu", limits = c(-max_value, max_value)
    ) +
    ggplot2::labs(
      x = xlabel, y = ylabel, fill = NULL,
      title = "Relative Changes in Weighted Interval Score after CQR Adjustments",
      subtitle = paste(
        "- Negative values indicate a lower (better) Score,",
        "positive values a higher (worse) Score -"
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5))
}
