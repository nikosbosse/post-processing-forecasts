#' @importFrom rlang .data

get_X_y <- function(df_combined, methods, m, t, h, q) {
  df_combined |>
    dplyr::filter(
      .data$model == m, .data$target_type == t, .data$horizon == h, .data$quantile %in% q
    ) |>
    tidyr::pivot_wider(
      names_from = "method", values_from = "prediction"
    ) |>
    dplyr::select(dplyr::all_of(methods), .data$true_value)
}

compute_weights <- function(X_training, y_training) {
  # divide X matrix and y vector by L2-Norm to avoid numerical overflow
  scaling_factor <- sqrt(norm(X_training, "2"))

  # (q x n) matrix with equality constraints
  # want a1 + a2 = 1
  Aeq <- matrix(rep(1, times = ncol(X_training)), nrow = 1)

  # (q x 1) vector, right hand side of equality constraint
  beq <- c(1)

  # lower bound of coefficients a1 and a2
  lb <- 0
  # upper bound of coefficients a1 and a2
  ub <- 1

  if (qr(X_training)$rank < ncol(X_training)) {
    # if matrix is singular choose equal weight for each method
    weights <- rep(1 / ncol(X_training), times = ncol(X_training))
  } else {
    weights <- pracma::lsqlincon(
      X_training / scaling_factor, y_training / scaling_factor,
      Aeq = Aeq, beq = beq, lb = lb, ub = ub
    )
  }
}

assign_method <- function(ensemble_df, m, t, h, q) {
  ensemble_df[
    ensemble_df$model == m &
      ensemble_df$target_type == t &
      ensemble_df$horizon == h &
      ensemble_df$quantile %in% q,
    "method"
  ] <- "ensemble"
}

assign_predictions <- function(ensemble_df, ensemble_predictions, m, t, h, q) {
  ensemble_df[
    ensemble_df$model == m &
      ensemble_df$target_type == t &
      ensemble_df$horizon == h &
      ensemble_df$quantile %in% q,
    "prediction"
  ] <- ensemble_predictions
}

update_subset_ensemble <- function(df_combined, ensemble_df, cv_init_training,
                                   methods, m, t, h, q) {
  X_y_training <- df_combined |>
    # compute weights only with training set
    # TODO: fraction input of cv_init_training is not possible here
    extract_training_set(cv_init_training = cv_init_training) |>
    get_X_y(methods, m, t, h, q)

  # (m x n) matrix where each column is part of the convex combination
  X_training <- X_y_training |>
    dplyr::select(-.data$true_value) |>
    as.matrix()

  # (m x 1) matrix with true values
  y_training <- as.matrix(X_y_training$true_value)

  weights <- compute_weights(X_training, y_training)

  X_inference <- df_combined |>
    # compute predictions on training and validation set
    get_X_y(methods, m, t, h, q) |>
    dplyr::select(-.data$true_value) |>
    as.matrix()

  ensemble_predictions <- X_inference %*% weights

  ensemble_df <- assign_method(ensemble_df, m, t, h, q)
  ensemble_df <- assign_predictions(ensemble_df, ensemble_predictions, m, t, h, q)

  return(ensemble_df)
}

add_ensemble <- function(df_combined, per_quantile_weights = TRUE,
                         cv_init_training = NULL, verbose = FALSE) {

  # all quantile filter operations have no effect when weights are not computed
  # for each quantile separately
  if (!per_quantile_weights) {
    q <- unique(df_combined$quantile)
  }

  # overwrite subset of prediction column in each iteration
  # same dimensions as data for single method => can be appended at the end
  ensemble_df <- df_combined |> dplyr::filter(.data$method == "original")

  methods <- unique(df_combined$method)
  methods <- methods[methods != "original"]

  for (m in unique(df_combined$model)) {
    for (t in unique(df_combined$target_type)) {
      for (h in unique(df_combined$horizon)) {
        if (verbose) {
          cat("model = ", m, " | target_type = ", t, " | horizon = ", h, "\n", sep = "")
        }
        if (!per_quantile_weights) {
          # all updates of ensemble_df for all quantiles together
          ensemble_df <- update_subset_ensemble(
            df_combined, ensemble_df, cv_init_training, methods, m, t, h, q
          )
        } else {
          # updates of ensemble_df for each quantile separately
          for (q in unique(df_combined$quantile)) {
            ensemble_df <- update_subset_ensemble(
              df_combined, ensemble_df, cv_init_training, methods, m, t, h, q
            )
          }
        }
      }
    }
  }

  # return larger data frame with model = "ensemble" appended to input data frame
  dplyr::bind_rows(df_combined, ensemble_df)
}
