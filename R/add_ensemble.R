#' @importFrom rlang .data

get_pairs_subset <- function(df_combined, l, m, t, h, q) {
  df_combined |>
    dplyr::filter(
      .data$location == l, .data$model == m, .data$target_type == t,
      .data$horizon == h, .data$quantile %in% c(q, 1 - q)
    )
}

score_pairs_subset <- function(df_subset, train_val_split) {
  if (train_val_split) {
    # compute weights on training set only, apply weights for all observations
    df_subset <- extract_training_set(df_subset)
  }

  df_subset |>
    scoringutils::score() |>
    dplyr::select(.data$method:.data$horizon, .data$quantile, .data$interval_score)
}

pivot_score_subset <- function(score_subset) {
  score_subset |>
    tidyr::pivot_wider(
      names_from = .data$method, values_from = .data$interval_score
    )
}

get_score_matrix <- function(wide_score_subset, methods, q) {
  wide_score_subset |>
    # score matrix for lower and upper quantile is identical
    # only need to compute once
    dplyr::filter(.data$quantile == q) |>
    dplyr::select(dplyr::all_of(methods)) |>
    as.matrix()
}

eval_score_matrix <- function(w, score_matrix) {
  sum(as.numeric(score_matrix %*% w))
}

compute_weights <- function(score_matrix, max_iter, print_level) {
  eval_f <- function(w) {
    eval_score_matrix(w, score_matrix)
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

  num_weights <- ncol(score_matrix)
  # initialize with random values out of [0, 1]
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

store_weights <- function(weights_df, weights, l, m, t, h, q) {
  weights_df[
    weights_df$location == l &
      weights_df$model == m &
      weights_df$target_type == t &
      weights_df$horizon == h &
      weights_df$quantile == q,
    "weights"
  ] <- list(list(weights))

  weights_df
}

get_quantiles_low_matrix <- function(df_subset, q, methods) {
  df_subset |>
    dplyr::filter(.data$quantile == q) |>
    tidyr::pivot_wider(
      names_from = .data$method, values_from = .data$prediction
    ) |>
    dplyr::select(dplyr::all_of(methods)) |>
    as.matrix()
}

get_quantiles_high_matrix <- function(df_subset, q, methods) {
  df_subset |>
    dplyr::filter(quantile == 1 - q) |>
    tidyr::pivot_wider(
      names_from = .data$method, values_from = .data$prediction
    ) |>
    dplyr::select(dplyr::all_of(methods)) |>
    as.matrix()
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

update_subset_ensemble <- function(df_combined, ensemble_df, weights_df, methods,
                                   l, m, t, h, q, train_val_split, max_iter, print_level) {
  df_subset <- get_pairs_subset(df_combined, l, m, t, h, q)
  score_subset <- score_pairs_subset(df_subset, train_val_split)
  wide_score_subset <- pivot_score_subset(score_subset)

  score_matrix <- get_score_matrix(wide_score_subset, methods, q)
  weights <- compute_weights(score_matrix, max_iter, print_level)
  weights_df <- store_weights(weights_df, weights, l, m, t, h, q)

  quantiles_low_matrix <- get_quantiles_low_matrix(df_subset, q, methods)
  quantiles_high_matrix <- get_quantiles_high_matrix(df_subset, q, methods)

  quantiles_low_ensemble <- as.numeric(quantiles_low_matrix %*% weights)
  quantiles_high_ensemble <- as.numeric(quantiles_high_matrix %*% weights)

  ensemble_df <- assign_quantiles_low(ensemble_df, quantiles_low_ensemble, l, m, t, h, q)
  ensemble_df <- assign_quantiles_high(ensemble_df, quantiles_high_ensemble, l, m, t, h, q)

  list(ensemble_df = ensemble_df, weights_df = weights_df)
}

add_ensemble <- function(df_combined, train_val_split = TRUE, verbose = FALSE,
                         max_iter = 1e5, print_level = 0) {

  # overwrite subset of prediction column in each iteration
  # same dimensions as data for single method => can be appended at the end
  ensemble_df <- df_combined |> dplyr::filter(.data$method == "original")
  ensemble_df["method"] <- "ensemble"

  # store weights of ensemble as attribute of resulting dataframe
  # initialize list column with zeros and overwrite entry in each iteration of loop
  weights_df <- ensemble_df |>
    dplyr::distinct(
      .data$location, .data$model, .data$target_type, .data$horizon, .data$quantile
    ) |>
    dplyr::mutate(weights = list(0))

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
            df_list <- update_subset_ensemble(
              df_combined, ensemble_df, weights_df, methods, l, m, t, h, q,
              train_val_split, max_iter, print_level
            )

            ensemble_df <- df_list$ensemble_df
            weights_df <- df_list$weights_df
          }
        }
      }
    }
  }

  # return larger data frame with model = "ensemble" appended to input data frame
  output_df <- dplyr::bind_rows(df_combined, ensemble_df)
  attr(output_df, which = "weights") <- weights_df

  return(output_df)
}
