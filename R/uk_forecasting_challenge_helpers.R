test_df_updated <- function(df, df_updated, example_model, t, h, q) {
  ql <- dplyr::filter(
    df, model == example_model & target_type == t & horizon == h & quantile == q
  )$prediction
  
  qh <- dplyr::filter(
    df, model == example_model & target_type == t & horizon == h & quantile == 1 - q
  )$prediction

  ql_updated <- dplyr::filter(
    df_updated, model == example_model & target_type == t & horizon == h & quantile == q
  )$prediction
  
  qh_updated <- dplyr::filter(
    df_updated, model == example_model & target_type == t & horizon == h & quantile == 1 - q
  )$prediction

  print(cat("Values before for ql: ", ql))
  print(cat("Values before for qh: ", qh))

  print(cat("Values after for ql_updated: ", ql_updated))
  print(cat("Values after for qh_updated: ", qh_updated))

  print(cat("Difference ql: ", ql_updated - ql))
  print(cat("Difference qh: ", qh_updated - qh))
}
