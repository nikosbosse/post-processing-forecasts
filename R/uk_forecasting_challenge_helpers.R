# TODO: restructure files, make functions pass RCMDCHECK, i.e. namespacing or import from

# TODO: consistent theme for all plotting functions
# TODO: rewrite the function so that inputs are: df, df_updated, example_model, t, h, q. Then add a helper fct for filter in out ql,qh,tv
# This helper function can then also be used in update_subset and thus needs to go in a seperate helper file
# TODO: change the theme to theme light or minimal, orient on Joels functions
plot_cqr_results <- function(df, example_model, t, h, q) {
  ql <- dplyr::filter(df, model == example_model & target_type == t & horizon == h & quantile == q)$prediction
  qh <- dplyr::filter(df, model == example_model & target_type == t & horizon == h & quantile == 1 - q)$prediction
  tv <- dplyr::filter(df, model == example_model & target_type == t & horizon == h & quantile == q)$true_value

  date <- dplyr::filter(df, model == example_model & target_type == t & horizon == h & quantile == q)$forecast_date
  date <- as.Date(date)

  res <- cqr(alpha = q * 2, true_values = tv, quantiles_low = ql, quantiles_high = qh)

  ql_updated <- res$lower_bound
  qh_updated <- res$upper_bound
  
  # TODO: code duplication with update_predicted_values(), extract to own helper
  # function and call this function in plot_cqr_results() and
  # update_predicted_values() if plotting function is intended to stay

  ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = date, y = qh, colour = "qh")) +
    ggplot2::geom_line(ggplot2::aes(x = date, y = qh_updated, colour = "qh_updated"), linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(x = date, y = tv, colour = "tv")) +
    ggplot2::geom_line(ggplot2::aes(x = date, y = ql, colour = "ql")) +
    ggplot2::geom_line(ggplot2::aes(x = date, y = ql_updated, colour = "ql_updated"), linetype = "dashed") +
    ggplot2::ylab("Values") +
    ggplot2::xlab("date") +
    ggplot2::scale_colour_manual("",
      breaks = c("qh", "qh_updated", "tv", "ql", "ql_updated"),
      values = c("red", "red", "green", "blue", "blue")
    )
}


test_df_updated <- function(df, df_updated, example_model, t, h, q) {
  ql <- dplyr::filter(df, model == example_model & target_type == t & horizon == h & quantile == q)$prediction
  qh <- dplyr::filter(df, model == example_model & target_type == t & horizon == h & quantile == 1 - q)$prediction

  ql_updated <- dplyr::filter(df_updated, model == example_model & target_type == t & horizon == h & quantile == q)$prediction
  qh_updated <- dplyr::filter(df_updated, model == example_model & target_type == t & horizon == h & quantile == 1 - q)$prediction

  print(cat("Values before for ql: ", ql))
  print(cat("Values before for qh: ", qh))

  print(cat("Values after for ql_updated: ", ql_updated))
  print(cat("Values after for qh_updated: ", qh_updated))

  print(cat("Difference ql: ", ql_updated - ql))
  print(cat("Difference qh: ", qh_updated - qh))
}


# TODO: code durch kommentieren
