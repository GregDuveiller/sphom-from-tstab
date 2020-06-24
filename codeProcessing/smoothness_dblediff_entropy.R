smooth_dbldif_entropy <- function(NDVI, DOI, mode = 'entropy', bin_width = 0.01, bin_range = c(-1,1)){
  
  require(entropy)
  
  d_doi_lag1 <- diff(DOI, lag = 1)
  d_doi_lag2 <- diff(DOI, lag = 2)
  
  d_ndvi_lag1 <- diff(NDVI, lag = 1)
  d_ndvi_lag2 <- diff(NDVI, lag = 2)
  
  est_d_ndvi <- d_ndvi_lag2/d_doi_lag2 * d_doi_lag1[1:(length(d_doi_lag1) - 1)]
  res_d_ndvi <- est_d_ndvi - d_ndvi_lag1[1:(length(d_doi_lag1) - 1)]
  
  if(mode == 'residues'){return(res_d_ndvi)}
  
  bins <- seq(bin_range[1], bin_range[2], bin_width)
  p.res <- cut(res_d_ndvi, breaks = bins, include.lowest = T)
  ts.entropy <- entropy.MillerMadow(table(p.res))
  
  if(mode == 'entropy'){return(ts.entropy)}
  
  mx.entropy <- entropy.MillerMadow(rep(1, length(bins) - 1))
  TSI <- 1 - (ts.entropy/mx.entropy)
  
  if(mode == 'scaled'){return(TSI)} 
  else {print('... mode not valid'); return(NULL)}
}

