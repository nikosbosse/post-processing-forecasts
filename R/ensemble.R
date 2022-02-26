#' @importFrom rlang .data

# quantile is a scalar
# true_values, quantiles_low, quantiles_high are vectors
# returns a scalar
compute_wis <- function(quantile, true_values, quantiles_low, quantiles_high) {
  # q = 0.1 => 2 * q - 1 = -0.8 => range = 0.8
  # q = 0.9 => 2 * q - 1 = 0.8 => range = 0.8
  # special case q = 0.5
  interval_range <- abs(2 * quantile - 1)
  alpha <- (100 - interval_range) / 100
  dispersion <- (quantiles_high - quantiles_low)
  overprediction <- 2 / alpha * (quantiles_low - true_values) * (true_values < quantiles_low)
  underprediction <- 2 / alpha * (true_values - quantiles_high) * (true_values > quantiles_high)
  scores <- dispersion + underprediction + overprediction

  # consider interval score for each quantile separately => weighted sum is just the simple sum
  sum(scores)
}

# w is a vector
# quantile is a scalar
# true_values is a vector
# quantiles_low_matrix, quantiles_high_matrix are matrices
# returns a scalar
compute_wis_matrix <- function(w, quantile, true_values,
                               quantiles_low_matrix, quantiles_high_matrix) {
  # convex combination of lower quantile predictions
  quantiles_low <- as.numeric(quantiles_low_matrix %*% w)

  # convex combination of upper quantile predictions
  quantiles_high <- as.numeric(quantiles_high_matrix %*% w)

  # find weights w that minimize interval score of updated quantile predictions
  # note that the SAME weights w are used for updating lower and upper quantile predictions
  compute_wis(quantile, true_values, quantiles_low, quantiles_high)
}

get_true_values <- function(df_subset, methods) {
  df_subset |>
    dplyr::filter(method == methods[1]) |>
    dplyr::pull(true_value)
}

get_quantiles_low_matrix <- function(df_subset, q, methods) {
  df_subset |>
    dplyr::filter(quantile == q) |>
    tidyr::pivot_wider(
      names_from = "method", values_from = "prediction"
    ) |>
    dplyr::select(dplyr::all_of(methods)) |>
    as.matrix()
}

get_quantiles_high_matrix <- function(df_subset, q, methods) {
  df_subset |>
    dplyr::filter(quantile == 1 - q) |>
    tidyr::pivot_wider(
      names_from = "method", values_from = "prediction"
    ) |>
    dplyr::select(dplyr::all_of(methods)) |>
    as.matrix()
}

compute_weights <- function(quantile, true_values, quantiles_low_matrix,
                            quantiles_high_matrix, max_iter, print_level) {
  eval_f <- function(w) {
    compute_wis_matrix(
      w, quantile, true_values, quantiles_low_matrix, quantiles_high_matrix
    )
  }

  eval_g_eq <- function(w) {
    1 - sum(w)
  }

  opts <- list(
    "algorithm" = "NLOPT_GN_ISRES",
    "xtol_rel" = 1.0e-8,
    "maxeval" = max_iter,
    "print_level" = print_level
  )

  num_weights <- ncol(quantiles_high_matrix)
  x0 <- runif(num_weights)

  res <- nloptr::nloptr(
    x0 = x0,
    eval_f = eval_f,
    lb = rep(0, num_weights),
    ub = rep(1, num_weights),
    eval_g_eq = eval_g_eq,
    opts = opts
  )

  res$solution
}

assign_quantiles <- function(ensemble_df, quantiles_ensemble, l, m, t, h, q) {
  ensemble_df[
    ensemble_df$location == l &
      ensemble_df$model == m &
      ensemble_df$target_type == t &
      ensemble_df$horizon == h &
      ensemble_df$quantile == q,
    "prediction"
  ] <- quantiles_ensemble

  ensemble_df
}

assign_quantiles_low <- function(ensemble_df, quantiles_low_ensemble, l, m, t, h, q) {
  assign_quantiles(ensemble_df, quantiles_low_ensemble, l, m, t, h, q)
}

assign_quantiles_high <- function(ensemble_df, quantiles_high_ensemble, l, m, t, h, q) {
  assign_quantiles(ensemble_df, quantiles_high_ensemble, l, m, t, h, 1 - q)
}

get_pairs_subset <- function(df_combined, l, m, t, h, q) {
  df_combined |>
    dplyr::filter(
      location == l, model == m, target_type == t, horizon == h, quantile %in% c(q, 1 - q)
    )
}


update_subset_ensemble <- function(df_combined, ensemble_df, methods,
                                   l, m, t, h, q, max_iter, print_level) {
  df_subset <- get_pairs_subset(df_combined, l, m, t, h, q)

  true_values <- get_true_values(df_subset, methods)
  quantiles_low_matrix <- get_quantiles_low_matrix(df_subset, q, methods)
  quantiles_high_matrix <- get_quantiles_high_matrix(df_subset, q, methods)

  weights <- compute_weights(
    q, true_values, quantiles_low_matrix, quantiles_high_matrix,
    max_iter, print_level
  )

  quantiles_low_ensemble <- as.numeric(quantiles_low_matrix %*% weights)
  quantiles_high_ensemble <- as.numeric(quantiles_high_matrix %*% weights)

  ensemble_df <- assign_quantiles_low(ensemble_df, quantiles_low_ensemble, l, m, t, h, q)
  ensemble_df <- assign_quantiles_high(ensemble_df, quantiles_high_ensemble, l, m, t, h, q)

  return(ensemble_df)
}

add_ensemble <- function(df_combined, cv_init_training = NULL, verbose = FALSE,
                         max_iter = 1e5, print_level = 0) {

  # overwrite subset of prediction column in each iteration
  # same dimensions as data for single method => can be appended at the end
  ensemble_df <- df_combined |> dplyr::filter(.data$method == "original")
  ensemble_df["method"] <- "ensemble"

  methods <- unique(df_combined$method)
  methods <- methods[methods != "original"]

  # like cqr considers pairs of quantiles except median
  quantiles <- unique(df_combined$quantile)
  quantiles <- quantiles[quantiles < 0.5]

  for (l in unique(df_combined$location)) {
    for (m in unique(df_combined$model)) {
      for (t in unique(df_combined$target_type)) {
        for (h in unique(df_combined$horizon)) {
          if (verbose) {
            cat(
              "location = ", l, " | model = ", m, " | target_type = ", t, " | horizon = ", h, "\n",
              sep = ""
            )
          }
          for (q in quantiles) {
            ensemble_df <- update_subset_ensemble(
              df_combined, ensemble_df, methods, l, m, t, h, q,
              max_iter, print_level
            )
          }
        }
      }
    }
  }

  # return larger data frame with model = "ensemble" appended to input data frame
  dplyr::bind_rows(df_combined, ensemble_df)
}
