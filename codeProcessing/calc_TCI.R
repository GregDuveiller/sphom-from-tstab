calc_TCI <- function(NDVI, DOI, bin_width = 0.01, bin_range = c(-1,1)){
  
  # Function to get the Temporal Consistency Index (TCI) based on the entropy of 
  # the residues assuming a normal distribution, themselves obtained be 
  # comparing each observation with the one 
  # interpolated from its two neighbours in the time series. 
  #
  # The index is further scaled between 0 and 1 where TCI = 0 represents a 
  # distribution of residues with a sd = 1, and TCI = 1 represents a 
  # distribution of residues with a sd = 0.01, which represents the minimum  
  # precision of the MODIS observations
  #
  # G.Duveiller - June 2020
  
  
  
  d_doi_lag1 <- diff(DOI, lag = 1)
  d_doi_lag2 <- diff(DOI, lag = 2)
  
  d_ndvi_lag1 <- diff(NDVI, lag = 1)
  d_ndvi_lag2 <- diff(NDVI, lag = 2)
  
  est_d_ndvi <- d_ndvi_lag2/d_doi_lag2 * d_doi_lag1[1:(length(d_doi_lag1) - 1)]
  res_d_ndvi <- est_d_ndvi - d_ndvi_lag1[1:(length(d_doi_lag1) - 1)]
  
  # entropy assuming residues follow a Gaussian distribution
  H <- 0.5 * log(2 * pi * exp(1) * sd(res_d_ndvi)^2)
  # Can be simplified to:
  # H <- log(sd(res_d_ndvi)) + 1.418939
  
  # scaling it
  TCI <- log(sd(res_d_ndvi))/log(0.001)
  
  
  
  # # THE OLD WAY... 
  # 
  # require(entropy)
  # 
  # bins <- seq(bin_range[1], bin_range[2], bin_width)
  # p.res <- cut(res_d_ndvi, breaks = bins, include.lowest = T)
  # 
  # ts.entropy <- entropy.MillerMadow(table(p.res))
  # mx.entropy <- entropy.MillerMadow(rep(1, length(bins) - 1))
  # 
  # TCI <- 1 - (ts.entropy/mx.entropy)
  
  return(TCI)
}

