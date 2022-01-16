#############
# Notes
#
# CQR gets quantiles_low and quantiles_high which are vectors containing one quantile each over time
# Quantile spreads however has two versions that require different inputs:
#   1. constant factor: it requires all quantiles as the optimizer finds one constant factor for all spreads
#   2. varying factor: it requires two symmetrics quantiles and two other quantiles from which to compute the spread as the optimizer finds one constant factor for each quantile spread



compute_quantile_spreads <- function(quantiles_low, quantiles_high){
  
}


# returns corrected lower quantile and upper quantile predictions for a single
# quantile value
qvs <- function(quantile, true_values, quantiles_low, quantiles_high) {
  
  #1. compute current quantile spreads
  quantile_spreads <- compute_quantile_spreads(quantiles_low, quantiles_high)
  
  #2. optimize a constant factor multiplied to all the quantile spreads. It minimizes the weighted intervall scrore.
  
  
}




