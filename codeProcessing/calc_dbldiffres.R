calc_dbldiffres <- function(NDVI, DOI){
  
  # Function to get residues of a time series by comparing each observation with 
  # the one interpolated from its two neighbors in the time series.
  
  d_doi_lag1 <- diff(DOI, lag = 1)
  d_doi_lag2 <- diff(DOI, lag = 2)
  
  d_ndvi_lag1 <- diff(NDVI, lag = 1)
  d_ndvi_lag2 <- diff(NDVI, lag = 2)
  
  est_d_ndvi <- d_ndvi_lag2/d_doi_lag2 * d_doi_lag1[1:(length(d_doi_lag1) - 1)]
  res_d_ndvi <- est_d_ndvi - d_ndvi_lag1[1:(length(d_doi_lag1) - 1)]
  
  # res_d_ndvi <- c(NA, res_d_ndvi, NA)
  
  return(res_d_ndvi)
}

