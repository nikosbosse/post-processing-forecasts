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
    change_to_date() |>
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
      x = NULL, y = NULL, color = NULL, 
      title = stringr::str_glue("Predicted Quantiles in {location_name}"),
      subtitle = stringr::str_glue("model: {model}")
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
  )$target_end_date

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


#' @importFrom rlang .data
<<<<<<< HEAD
#TODO: add a vertical line at the end of the training set
plot_intervals <- function(df, model = NULL, target_type = c("Cases", "Deaths"),
=======

# TODO: title: target, location, horizon, subtitle: model, quantile
plot_intervals <- function(df, model = NULL, location = NULL,
                           target_type = c("Cases", "Deaths"),
>>>>>>> 411ecdc11181a69cd33175ced28e1c06ce23a6b0
                           quantile = 0.05, horizon = 1) {
  target <- rlang::arg_match(arg = target_type, values = c("Cases", "Deaths"))
  h <- paste_horizon(horizon)

  l <- process_model_input(df, model)
  df <- l$df
  model <- l$model
  
  l <- process_location_input(df, location)
  df <- l$df
  location_name <- l$location_name

  df |>
    process_quantile_pair(quantile) |> #dplyr::count(quantile)
    filter_target_types(target) |>
    filter_horizons(horizon) |>
    change_to_date() |>
    setup_intervals_plot() +
    ggplot2::labs(
      x = NULL, y = NULL, color = NULL,
      title = stringr::str_glue("Predicted {target} in {location_name} {h}"),
      subtitle = stringr::str_glue("model: {model}   |   quantile: {quantile}")
    )
}


#' @importFrom rlang .data

plot_intervals_grid <- function(df, model = NULL, location = NULL,
                                facet_by = c("horizon", "quantile"),
                                quantiles = NULL, horizon = NULL) {
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

  p <- df |>
    change_to_date() |>
    setup_intervals_plot()

  if (facet_by == "horizon") {
    p <- p +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data$target_type),
        cols = ggplot2::vars(.data$horizon),
        scales = "free_y"
      ) +
      ggplot2::labs(
        x = NULL, y = NULL, color = NULL,
        title = stringr::str_glue("Prediction Intervals in {location_name}"),
        subtitle = stringr::str_glue("model: {model}   |   quantile: {q}")
      )
  } else if (facet_by == "quantile") {
    p <- p +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data$target_type),
        cols = ggplot2::vars(.data$quantile_group),
        scales = "free_y"
      ) +
      ggplot2::labs(
        x = NULL, y = NULL, color = NULL,
        title = stringr::str_glue("Prediction Intervals in {location_name} {h}"),
        subtitle = stringr::str_glue("model: {model}")
      )
  }
  return(p)
}
