calc_TCI <- function(VI, DOI, minEntropy = 0.01, filterOutliers = T){
  
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
  # G.Duveiller - July 2020
  
  
  if(filterOutliers){
    d_vi_lag1 <- diff(VI, lag = 1)
    Q <- quantile(d_vi_lag1, probs = c(0.25, 0.75), na.rm = T)
    I2k <- (d_vi_lag1 >= Q[1] - diff(Q)*1.5) & (d_vi_lag1 <= Q[2] + diff(Q)*1.5) # <--- careful with <=... diff from text
  }
  
  VI <- VI[I2k]   # <--- NOT EXACTLY BEST WAY TO GO ... as we remove too much!
  DOI <- DOI[I2k]
  
  #df$X1 -> VI; df$X2 -> DOI
  
  d_doi_lag1 <- diff(DOI, lag = 1)
  d_doi_lag2 <- diff(DOI, lag = 2)
  
  d_vi_lag1 <- diff(VI, lag = 1)
  d_vi_lag2 <- diff(VI, lag = 2)
  
  est_d_vi <- d_vi_lag2/d_doi_lag2 * d_doi_lag1[1:(length(d_doi_lag1) - 1)]
  res_d_vi <- est_d_vi - d_vi_lag1[1:(length(d_doi_lag1) - 1)]
  
  # entropy assuming residues follow a Gaussian distribution
  H <- 0.5 * log(2 * pi * exp(1) * sd(res_d_vi)^2)
  # Can be simplified to:
  # H <- log(sd(res_d_ndvi)) + 1.418939
  
  # scaling it
  TCI <- (log(sd(res_d_vi)) + 1.418939)/(log(minEntropy) + 1.418939)

  return(TCI)
}

