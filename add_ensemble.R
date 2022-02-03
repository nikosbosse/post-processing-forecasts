uk_cqr_qsa <- readr::read_rds(
  here::here("data_results", "uk_cqr_qsa_uniform.rds")
)

models <- "epiforecasts-EpiExpert"
uk_cqr_qsa <- uk_cqr_qsa |> dplyr::filter(model == models)

add_ensemble <- function(df_combined, cv_init_training = NULL, verbose = FALSE) {
  # overwrite subset of prediction column in each iteration
  # same dimensions as data for single method => can be appended at the end
  ensemble_df <- df_combined |> dplyr::filter(method == "original")

  methods <- unique(uk_cqr_qsa$method)
  methods <- methods[methods != "original"]

  for (m in unique(df_combined$model)) {
    for (t in unique(df_combined$target_type)) {
      for (h in unique(df_combined$horizon)) {
        if (verbose) {
          cat("model = ", m, " | target_type = ", t, " | horizon = ", h, "\n", sep = "")
        }
        for (q in unique(df_combined$quantile)) {
          X_y_training <- df_combined |>
            # TODO: fraction input of cv_init_training is not possible here
            extract_training_set(cv_init_training = cv_init_training) |>
            dplyr::filter(
              model == m, target_type == t, horizon == h, quantile == q
            ) |>
            tidyr::pivot_wider(
              names_from = "method", values_from = "prediction"
            ) |>
            dplyr::select(dplyr::all_of(methods), true_value)

          # (m x 1) matrix with true values
          y_training <- X_y_training$true_value |> as.matrix()

          # (m x n) matrix where each column is part of the convex combination
          X_training <- X_y_training |>
            dplyr::select(-true_value) |>
            as.matrix()

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

          X_inference <- df_combined |>
            dplyr::filter(
              model == m, target_type == t, horizon == h, quantile == q
            ) |>
            tidyr::pivot_wider(
              names_from = "method", values_from = "prediction"
            ) |>
            dplyr::select(dplyr::all_of(methods)) |>
            as.matrix()

          ensemble_pred <- X_inference %*% weights

          ensemble_df[
            ensemble_df$model == m &
              ensemble_df$target_type == t &
              ensemble_df$horizon == h &
              ensemble_df$quantile == q,
            "method"
          ] <- "ensemble"

          ensemble_df[
            ensemble_df$model == m &
              ensemble_df$target_type == t &
              ensemble_df$horizon == h &
              ensemble_df$quantile == q,
            "prediction"
          ] <- ensemble_pred
        }
      }
    }
  }

  # return larger data frame with model = "ensemble" appended to input data frame
  dplyr::bind_rows(df_combined, ensemble_df)
}
