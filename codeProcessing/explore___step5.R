
require(dplyr)
require(tidyr)
require(purrr)
require(entropy)


batch_name <- 'batch_002'
psf_fname <- 'PSF-AQUA-48-20'
TS2LC <- c('LC1'='TS1', 'LC2'='TS8', 'LC3'='TS3', 'LC4'='TS9')
temp_subsample <- 4

# load target dataset
load(paste0('dataProcessing/', batch_name, 
            '/datablock-', paste(TS2LC, collapse = '-'),
            '___', psf_fname, '.Rda'))  # 'df.all' & 'df.grd'



load(paste0('dataProcessing/', batch_name, '/df-ideal-ts.Rda'))  # df.ideal.ts
doi_vctr <- df.ideal.ts$DOI





if(!is.null(temp_subsample)){
  set.seed(42)
  doi_vctr_sub <- sort(sample(doi_vctr, length(doi_vctr)/temp_subsample, replace = F))
  temp_sub_lbl <- paste0('tsf', temp_subsample)
} else {doi_vctr_sub <- doi_vctr; temp_sub_lbl <- NULL}


# This, based on double diff, is good... but would be good to scale it
smoothness_dblediff <- function(NDVI, DOI){
  
  d_doi_lag1 <- diff(DOI, lag = 1)
  d_doi_lag2 <- diff(DOI, lag = 2)
  
  d_ndvi_lag1 <- diff(NDVI, lag = 1)
  d_ndvi_lag2 <- diff(NDVI, lag = 2)
  
  est_d_ndvi <- d_ndvi_lag2/d_doi_lag2 * d_doi_lag1[1:(length(d_doi_lag1) - 1)]
  res_d_ndvi <- est_d_ndvi - d_ndvi_lag1[1:(length(d_doi_lag1) - 1)]
  
  #TSI <- mean(res_d_ndvi^2)
  
  return(res_d_ndvi)
}

# This, based on double diff, with entropy in the end... 
smoothness_dblediff_entropy <- function(NDVI, DOI){
  
  d_doi_lag1 <- diff(DOI, lag = 1)
  d_doi_lag2 <- diff(DOI, lag = 2)
  
  d_ndvi_lag1 <- diff(NDVI, lag = 1)
  d_ndvi_lag2 <- diff(NDVI, lag = 2)
  
  est_d_ndvi <- d_ndvi_lag2/d_doi_lag2 * d_doi_lag1[1:(length(d_doi_lag1) - 1)]
  res_d_ndvi <- est_d_ndvi - d_ndvi_lag1[1:(length(d_doi_lag1) - 1)]
  
  
  p.res <- cut(res_d_ndvi, breaks = seq(-2,2,0.01))

  TSI <- entropy.MillerMadow(table(p.res))
  
  return(TSI)
}

# This, based on double diff and MAD, is scaled... but we lack an absolute reference
smoothness_dblediff_scaled <- function(NDVI, DOI, rdm_dev = 0.2, rdm_n = NULL){
  
  d_doi_lag1 <- diff(DOI, lag = 1)
  d_doi_lag2 <- diff(DOI, lag = 2)
  
  d_ndvi_lag1 <- diff(NDVI, lag = 1)
  d_ndvi_lag2 <- diff(NDVI, lag = 2)
  
  est_d_ndvi <- d_ndvi_lag2/d_doi_lag2 * d_doi_lag1[1:(length(d_doi_lag1) - 1)]
  res_d_ndvi <- est_d_ndvi - d_ndvi_lag1[1:(length(d_doi_lag1) - 1)]
  
  set.seed(48)
  rdm_d_ndvi <- runif(n = ifelse(is.null(rdm_n), length(res_d_ndvi), rdm_n), 
                      min = -rdm_dev, max = rdm_dev) # <== this should ideally mean something more tangible
  
  TSI <- 1 - (mean(abs(res_d_ndvi)) / mean(abs(rdm_d_ndvi)))
  
  return(TSI)
}





pb <- txtProgressBar(min = 0, max = max(df.all$grd_id), initial = 0)
df.sum <- data.frame(NULL)
for(iGrd in unique(df.all$grd_id)){
  
  df.1 <- df.all %>% 
    filter(grd_id %in% iGrd, DOI %in% doi_vctr_sub) %>%
    filter(!is.na(NDVI)) %>%    #  for some reason, sometimes we have some
    mutate(NDVI.smo = smooth.spline(x = DOI, y = NDVI, df = 8)$y,
           NDVI.res = NDVI - NDVI.smo,
           p.res.01 = cut(NDVI.res, breaks = seq(-2,2,0.005)))
  
  df.p.res.01 <- df.1 %>% count(p.res.01)
  
  df.2 <- df.1 %>% 
    dplyr::select(paste('purity', TS2LC, sep = '_')) %>% 
    map_dfr(function(x) mean(x)) 
  colnames(df.2) <- sub("purity", "pur_avg", colnames(df.2))
  
  df.3 <- df.1 %>% 
    dplyr::select(paste('purity', TS2LC, sep = '_')) %>% 
    map_dfr(function(x) sd(x)) 
  colnames(df.3) <- sub("purity", "pur_std", colnames(df.3))
  
  # NEED TO GENERALISE THIS STEP SOMEHOW <<<
  df.4 <- df.1 %>%
    summarise(mad_TS1 = mean(abs(NDVI - df.ideal.ts$TS1)),
              mad_TS8 = mean(abs(NDVI - df.ideal.ts$TS8)),
              mad_TS3 = mean(abs(NDVI - df.ideal.ts$TS3)),
              mad_TS9 = mean(abs(NDVI - df.ideal.ts$TS9)))

  # df.5 <- df.1 %>%
  #   dplyr::select('NDVI') %>% 
  #   map_dfr(function(x) sd(x)) 
  
  # >>>
    
  df.sum <- df.1 %>%
    summarise(grd_id = iGrd,
              var_res = var(NDVI.smo - NDVI),
              var_sig = var(NDVI.smo),
              var_tot = var(NDVI),
              s_ddvar = var(smoothness_dblediff(NDVI, DOI)),
              s_ddrms = mean(smoothness_dblediff(NDVI, DOI)^2),
              s_ddmad = mean(abs(smoothness_dblediff(NDVI, DOI))),
              s_ddent = smoothness_dblediff_entropy(NDVI, DOI),
              s_dd_p1 = smoothness_dblediff_scaled(NDVI, DOI, rdm_dev = 0.1, rdm_n = 365),
              s_dd_p2 = smoothness_dblediff_scaled(NDVI, DOI, rdm_dev = 0.2, rdm_n = 365),
              s_dd_p3 = smoothness_dblediff_scaled(NDVI, DOI, rdm_dev = 0.3, rdm_n = 365)) %>% 
    bind_cols(entropy = entropy.MillerMadow(df.p.res.01$n),
              df.2, df.3, df.4) %>%
    bind_rows(df.sum)
  
  setTxtProgressBar(pb, iGrd)
}


df.sum <- df.sum %>% 
  left_join(df.grd, by = 'grd_id') # add grid info

close(pb)



# save outputs
save('df.sum', 
     file = paste0('dataProcessing/', batch_name,
                   '/metrics-', paste(TS2LC, collapse = '-'),
                   '___', psf_fname, '-', temp_sub_lbl,  '.Rda'))
