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
    dplyr::mutate(method = factor(.data$method) |> forcats::fct_inorder()) |>
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


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Helpers for plot_eval()                                                 ####

get_plot_title <- function(df_eval) {
  methods <- attr(df_eval, which = "methods")

  if (length(methods) > 1) {
    title_end <- "Post Processing"
  } else {
    method <- stringr::str_replace(methods, pattern = "_", replacement = " ") |>
      stringr::str_to_upper()
    title_end <- paste(method, "Adjustment")
  }

  paste("Relative Changes in Weighted Interval Score after", title_end)
}

get_xlabel <- function(summarise_by) {
  # multiple categories
  if (length(summarise_by) == 2) {
    xlabel <- summarise_by[2]
  } else {
    xlabel <- NULL
  }

  return(xlabel)
}

get_max_value <- function(df_eval) {
  df_eval |>
    # exclude numeric quantile column when taking max of dataframe
    # where() leads to warning in RCMDCHECK, prefixing with tidyselect:::where()
    # does not solve it, discussed here:
    # https://github.com/r-lib/tidyselect/issues/201
    dplyr::select(-1 & tidyselect:::where(is.numeric)) |>
    abs() |>
    max(na.rm = TRUE)
}

plot_bars <- function(df_eval, title, first_colname, max_value, base_size) {
  first_colname <- rlang::sym(first_colname)

  df_eval |>
    dplyr::mutate(positive_effect = .data$relative_change < 0) |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = .data$relative_change, y = !!first_colname,
        col = .data$positive_effect
      )
    ) +
    ggplot2::geom_vline(xintercept = 0, color = "grey40", linetype = "dashed") +
    ggplot2::geom_segment(
      mapping = ggplot2::aes(xend = 0, yend = !!first_colname)
    ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::scale_color_manual(values = c("TRUE" = "#67a9cf", "FALSE" = "#ef8a62")) +
    ggplot2::labs(x = NULL, y = first_colname, color = NULL, title = title) +
    ggplot2::guides(color = "none") +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
}


plot_heatmap <- function(df_plot, title, first_colname,
                         max_value, xlabel, base_size) {
  # to use columns as strings in aes(), encode string first to symbol with sym()
  # and then decode / unquote it with !! inside of aes()
  first_colname <- rlang::sym(first_colname)

  df_plot |>
    ggplot2::ggplot(mapping = ggplot2::aes(
      y = !!first_colname, x = .data$colnames, fill = .data$values
    )) +
    ggplot2::geom_tile() +
    ggplot2::scale_y_discrete(limits = rev, expand = c(0, 0)) +
    # ggplot2::scale_y_reverse(expand = c(0, 0)) +
    ggplot2::scale_fill_distiller(
      palette = "RdBu", limits = c(-max_value, max_value)
    ) +
    ggplot2::labs(
      x = xlabel, y = first_colname, fill = NULL, title = title,
      subtitle = paste(
        "- Negative values indicate a lower (better) Score,",
        "positive values a higher (worse) Score -"
      )
    ) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5))
}
