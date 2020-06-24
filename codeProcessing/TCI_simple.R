TCI_simple <- function(NDVI, DOI, bin_width = 0.01, bin_range = c(-1,1)){
  
  # Function to get the Temporal Consistency Index (TCI) based on the entropy of 
  # the residues, themselves obtained be comparing each observation with the one 
  # interpolated from its two neighbours in the time series. To get the scaled 
  # index, the entropy is divided by the maximum entropy and all is substracted 
  # from 1. The value can depend on the bin_width used and the bin_range used to
  # calculate the entropy.
  #
  # G.Duveiller - April 2020
  
  require(entropy)
  
  d_doi_lag1 <- diff(DOI, lag = 1)
  d_doi_lag2 <- diff(DOI, lag = 2)
  
  d_ndvi_lag1 <- diff(NDVI, lag = 1)
  d_ndvi_lag2 <- diff(NDVI, lag = 2)
  
  est_d_ndvi <- d_ndvi_lag2/d_doi_lag2 * d_doi_lag1[1:(length(d_doi_lag1) - 1)]
  res_d_ndvi <- est_d_ndvi - d_ndvi_lag1[1:(length(d_doi_lag1) - 1)]
  
  bins <- seq(bin_range[1], bin_range[2], bin_width)
  p.res <- cut(res_d_ndvi, breaks = bins, include.lowest = T)
  
  ts.entropy <- entropy.MillerMadow(table(p.res))
  mx.entropy <- entropy.MillerMadow(rep(1, length(bins) - 1))
  
  TCI <- 1 - (ts.entropy/mx.entropy)
  
  return(TCI)
}

