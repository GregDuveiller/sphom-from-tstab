require(ggplot2)
require(dplyr)



# load target dataset
load(paste0('dataProcessing/', batch_name, 
            '/datablock-', paste(TS2LC, collapse = '-'),
            '___', psf_fname, '.Rda'))  # 'df.all' & 'df.grd'



load(paste0('dataProcessing/', batch_name, '/df-ideal-ts.Rda'))  # df.ideal.ts

df.ts <- df.ideal.ts %>%
  dplyr::select(DOI, all_of(TS2LC)) %>%
  pivot_longer(cols = names(TS2LC), names_to = 'LC', values_to = 'NDVI')




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
  
  return(TSI )
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
  
  # TSI <- sd(res_d_ndvi) / mean(abs(res_d_ndvi))
  
  # TSI <- 1 - sum(abs(res_d_ndvi))/length(res_d_ndvi)  # <== not sure what to scale it with?
  
  
  return(TSI)
}

# This one based on autocorrelation is dependent of amplitude, so sub-optimal
smoothness_autocorr <- function(NDVI, DOI, lag = 1){
  
  # ==> this would need to take into account uneven irregular sampling
  
  # d_doi_lag1 <- diff(DOI, lag = 1)
  # d_doi_lag2 <- diff(DOI, lag = 2)
  
  TSI <- cor(NDVI[1:(length(NDVI)-lag)], NDVI[(1 + lag):length(NDVI)])
  
  return(TSI)
}





doi_vctr <- df.ideal.ts$DOI

plot_ts <- function(iGrd, temp_subsample = NULL){
  
  if(!is.null(temp_subsample)){
    set.seed(42)
    doi_vctr_sub <- sort(sample(doi_vctr, length(doi_vctr)/temp_subsample, replace = F))
  } else {doi_vctr_sub <- doi_vctr}
  
  
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
  
  # # NEED TO GENERALISE THIS STEP SOMEHOW <<<
  # df.4 <- df.1 %>%
  #   summarise(mad_TS1 = mean(abs(NDVI - df.ideal.ts$TS1)),
  #             mad_TS8 = mean(abs(NDVI - df.ideal.ts$TS8)),
  #             mad_TS3 = mean(abs(NDVI - df.ideal.ts$TS3)),
  #             mad_TS9 = mean(abs(NDVI - df.ideal.ts$TS9)))
  # 
  # # >>>
  
  df.sum.pt <- df.1 %>%
    summarise(grd_id = iGrd,
              var_res = var(NDVI.smo - NDVI),
              var_sig = var(NDVI.smo),
              var_tot = var(NDVI),
              s_ddrms = mean((smoothness_dblediff(NDVI, DOI))^2),
              s_ddmad = mean(abs(smoothness_dblediff(NDVI, DOI))),
              s_ddent = smoothness_dblediff_entropy(NDVI, DOI),
              s_dd_p2 = smoothness_dblediff_scaled(NDVI, DOI, rdm_dev = 0.2, rdm_n = 365),
              s_autoc = smoothness_autocorr(NDVI, DOI)) %>% 
    bind_cols(entropy = entropy.MillerMadow(df.p.res.01$n),
              df.2, df.3, df.4) 
  
  
  
  g <- ggplot(df.1, aes(x = DOI)) + 
    geom_line(data = df.ts, aes(y = NDVI, group = LC), colour = 'grey80') +
    geom_point(aes(y = NDVI, alpha = maxPur), colour = 'steelblue') + 
    geom_line(aes(y = NDVI.smo), colour = 'darkblue', size = 1, linetype = 1) +
    scale_alpha_continuous(guide = F) +
    labs(title = paste('Point', df.sum.pt$grd_id, '|', batch_name, '|', 
                       'temp subsample by', temp_subsample),
                       # 'TS1 = ', round(df.sum.pt$pur_avg_TS1, digits = 2), '|',
                       # 'TS3 = ', round(df.sum.pt$pur_avg_TS3, digits = 2), '|',
                       # 'TS8 = ', round(df.sum.pt$pur_avg_TS8, digits = 2), '|',
                       # 'TS9 = ', round(df.sum.pt$pur_avg_TS9, digits = 2)),
         subtitle = paste('s_ddrms = ', round(df.sum.pt$s_ddrms, digits = 4), '|',
                          's_ddmad = ', round(df.sum.pt$s_ddmad, digits = 4), '|',
                          's_ddent = ', round(df.sum.pt$s_ddent, digits = 4), '|',
                          's_dd_p2 = ', round(df.sum.pt$s_dd_p2, digits = 4), '|',
                          's_autoc = ', round(df.sum.pt$s_autoc, digits = 4)))
  
  
  return(g)
  
}


plot_ts(iGrd)

 
# # EXTRA >>>
# 
# df.sum.d <- df.sum %>%
#   mutate(pur_avg_max = max(pur_avg_TS1, pur_avg_TS3, pur_avg_TS8, pur_avg_TS9),
#          mad_min = min(mad_TS1, mad_TS3, mad_TS8, mad_TS9))
# 
# mk.plot(df = df.sum.d, y = 'entropy', 
#         x = 'mad_min', xlim = c(0,0.3),
#         z = 'pur_avg_max', zlim = c(0,1))
# 
# 
# mk.plot(df = df.sum.d, z = 'entropy', 
#         y = 'mad_min', ylim = c(0,0.3),
#         x = 'pur_avg_max', xlim = c(0,1))

