# step5___calculate-temporal-metrics.R
#
# Step 5 in spHomogeneity simulation: calculating temporal metrics 


require(dplyr)
require(tidyr)
require(purrr)

landscape_ID <- 'landscape-2LC-id42'
psf_fname <- 'PSF-AQUA-48-20'
ndvi_noise <- 0.01
LC1 <- 'TS1'
LC0 <- 'TS2'

# load target dataset
load(paste0('dataProcessing/', landscape_ID,
       '___datablock-', LC1, '-', LC0, '-', ndvi_noise,
       '___', psf_fname, '.Rda'))

# load('dataProcessing/landscape-2LC-id42___datablock-TS1-TS2-0.01___PSF-AQUA-48-10.Rda')


# should set.seed to simulate frequency of image acquisition
set.seed(42)
doi_vctr_sub <- sort(sample(doi_vctr, length(doi_vctr)/4, replace = F))

df.sum <- data.frame(NULL)
for(iGrd in unique(df.all$grd_id)){
  
  df.sum <- df.all %>% 
    filter(grd_id %in% iGrd, DOI %in% doi_vctr_sub) %>%
    mutate(NDVI.sm = smooth.spline(x = DOI, y = NDVI, df = 8)$y) %>%
    summarise(grd_id = iGrd,
              pur_avg = mean(Purity),
              pur_med = median(Purity),
              pur_std = sd(Purity),
              var_res = var(NDVI.sm - NDVI),
              var_sig = var(NDVI.sm),
              var_tot = var(NDVI)) %>% 
    bind_rows(df.sum)
  
}

df.sum <- df.sum %>% 
  bind_cols(grd) %>%
  mutate(original_id = raster::extract(x = r, y = grd))

# save outputs
save('df.sum', file = paste0('dataProcessing/', landscape_ID, 
                             '___metrics-', LC1, '-', LC0, '-', ndvi_noise,
                             '___', psf_fname, '.Rda'))
