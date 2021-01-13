calc_temp_metrics = function(batch_name, psf_fname, TS2LC){
# step5___calculate-temporal-metrics.R
#
# Step 5 in spHomogeneity simulation: calculating temporal metrics 


require(dplyr)
require(tidyr)
require(purrr)
require(entropy)
  
# psf_fname <- 'PSF-AQUA-48-10'
# LC1 <- 'TS1'
# LC0 <- 'TS4'

# load target dataset
load(paste0('dataProcessing/', batch_name, 
            '/datablock-', paste(TS2LC, collapse = '-'),
            '___', psf_fname, '.Rda'))  # 'df.all' & 'df.grd'


# load('dataProcessing/landscape-2LC-id42___datablock-TS1-TS2-0.01___PSF-AQUA-48-10.Rda')

load(paste0('dataProcessing/', batch_name, '/df-ideal-ts.Rda'))  # df.ideal.ts
doi_vctr <- df.ideal.ts$DOI
temp_subsample <- NULL
# should set.seed to simulate frequency of image acquisition
if(!is.null(temp_subsample)){
  set.seed(42)
  doi_vctr_sub <- sort(sample(doi_vctr, length(doi_vctr)/4, replace = F))
} else {doi_vctr_sub <- doi_vctr}




source('codeProcessing/calc_TCI.R')


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
  
  df.sum <- df.1 %>%
    summarise(grd_id = iGrd,
              # pur_avg = mean(Purity),
              # pur_std = sd(Purity),
              var_res = var(NDVI.smo - NDVI),
              var_sig = var(NDVI.smo),
              var_tot = var(NDVI),
              TCI = calc_TCI(NDVI, DOI)) %>% 
    bind_cols(entropy = entropy.MillerMadow(df.p.res.01$n),
              df.2, df.3) %>%
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
                   '___', psf_fname, '.Rda'))
}
