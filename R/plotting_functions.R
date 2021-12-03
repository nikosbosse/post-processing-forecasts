# df <- readr::read_csv(file = "data/full-data-uk-challenge.csv")
# plot_quantiles(
#   df = df, model = "epiforecasts-EpiExpert",
#   quantiles = c(0.01, seq(0.1, 0.9, 0.1), 0.99)
# )
#' @importFrom rlang .data

plot_quantiles <- function(df, model = NULL, quantiles = c(0.05, 0.5, 0.95)) {
  l <- process_model_input(df, model)
  df <- l$df
  model <- l$model

  df |>
    filter_quantiles(quantiles) |>
    mutate_horizon() |>
    change_to_date() |>
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





# TODO: make functions pass RCMDCHECK, i.e. namespacing or import from

# TODO: rewrite the function so that inputs are: df, df_updated, model, target_type, horizon, quantile. Then add a helper fct for filtering out quantiles_low, quantiles_high, true_values
# This helper function can then also be used in update_subset and thus needs to go in a seperate helper file

# TODO: change the theme to theme_light() or theme_minimal(), orient on Joels functions
plot_cqr_results <- function(df, model, target_type, horizon, quantile) {
  # same reason as in update_subset() function
  mod <- model
  t <- target_type
  h <- horizon
  q <- quantile

  quantiles_low <- dplyr::filter(
    df, model == mod & target_type == t & horizon == h & quantile == q
  )$prediction

  quantiles_high <- dplyr::filter(
    df, model == mod & target_type == t & horizon == h & quantile == 1 - q
  )$prediction

  true_values <- dplyr::filter(
    df, model == mod & target_type == t & horizon == h & quantile == q
  )$true_value

  date <- dplyr::filter(
    df, model == mod & target_type == t & horizon == h & quantile == q
  )$forecast_date

  date <- as.Date(date)

  result <- cqr(quantile * 2, true_values, quantiles_low, quantiles_high)

  quantiles_low_updated <- result$lower_bound
  quantiles_high_updated <- result$upper_bound

  # TODO: code duplication with update_subset(), extract to own helper
  # function and call this function in plot_cqr_results() and update_subset()

  ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(x = date, y = quantiles_high, colour = "quantiles_high")
    ) +
    ggplot2::geom_line(
      ggplot2::aes(x = date, y = quantiles_high_updated, colour = "quantiles_high_updated"),
      linetype = "dashed"
    ) +
    ggplot2::geom_line(ggplot2::aes(x = date, y = true_values, colour = "true_values")) +
    ggplot2::geom_line(ggplot2::aes(x = date, y = quantiles_low, colour = "quantiles_low")) +
    ggplot2::geom_line(
      ggplot2::aes(x = date, y = quantiles_low_updated, colour = "quantiles_low_updated"),
      linetype = "dashed"
    ) +
    ggplot2::ylab("Values") +
    ggplot2::xlab("date") +
    ggplot2::scale_colour_manual("",
      breaks = c(
        "quantiles_high", "quantiles_high_updated", "true_values", "quantiles_low",
        "quantiles_low_updated"
      ),
      values = c("red", "red", "green", "blue", "blue")
    )
}






# df <- readr::read_csv(file = "data/full-data-uk-challenge.csv")
# df_new <- df |> dplyr::mutate(prediction = prediction * 2)
# df_combined <- collect_predictions(original = df, new = df_new)
# plot_intervals(df = df_combined, model = "epiforecasts-EpiExpert", quantile = 0.05)
#' @importFrom rlang .data

# TODO Joel: add option to display horizons or quantiles in columns
plot_intervals <- function(df, model = NULL, facet_by = c("horizon", "quantile"), quantiles = NULL, horizon = NULL) {
  facet_by <- rlang::arg_match(arg = facet_by, values = c("horizon", "quantile"))

  l <- process_model_input(df, model)
  df <- l$df
  model <- l$model

  if (facet_by == "horizon") {
    default_quantiles <- 0.05
    
    if (is.null(quantiles)) {
      quantiles <- default_quantiles
    }
    
    q <- quantiles
    df <- facet_horizon(df, quantiles, horizon)
  } else if (facet_by == "quantile") {
    default_horizon = 1
    default_quantiles <- c(0.01, 0.05, 0.1, 0.25)
    
    if (is.null(horizon)) {
      horizon <- default_horizon
    }
    
    if (is.null(quantiles)) {
      quantiles <- default_quantiles
    }
    
    h <- paste_horizon(horizon)
    df <- facet_quantile(df, quantiles, horizon)
  } 

  p <- df |>
    change_to_date() |>
    tidyr::pivot_wider(names_from = .data$quantile, values_from = .data$prediction) |>
    ggplot2::ggplot(mapping = ggplot2::aes(x = .data$forecast_date)) +
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

  if (facet_by == "horizon") {
    p <- p +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data$target_type),
        cols = ggplot2::vars(.data$horizon),
        scales = "free_y"
      ) +
      ggplot2::ggtitle(
        stringr::str_glue("Prediction Intervals for {model} model at level {q}")
      )
  } else if (facet_by == "quantile") {
    p <- p +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data$target_type),
        cols = ggplot2::vars(.data$quantile_group),
        scales = "free_y"
      ) +
      ggplot2::ggtitle(
        stringr::str_glue("Prediction Intervals for {model} model {h}")
      )
  }
  return(p)
}
