calc_temp_metrics = function(landscape_ID, psf_fname, LC1, LC0, ndvi_noise){
# step5___calculate-temporal-metrics.R
#
# Step 5 in spHomogeneity simulation: calculating temporal metrics 


require(dplyr)
require(tidyr)
require(purrr)

# landscape_ID <- 'landscape-2LC-id42'
# psf_fname <- 'PSF-AQUA-48-20'
# ndvi_noise <- 0.01
# LC1 <- 'TS1'
# LC0 <- 'TS4'

# load target dataset
load(paste0('dataProcessing/', landscape_ID,
       '___datablock-', LC1, '-', LC0, '-', ndvi_noise,
       '___', psf_fname, '.Rda'))  # 'df.all' & 'grd'


# load('dataProcessing/landscape-2LC-id42___datablock-TS1-TS2-0.01___PSF-AQUA-48-10.Rda')

load('dataProcessing/df-ideal-ts.Rda')  # df.ideal.ts
doi_vctr <- df.ideal.ts$DOI
  
# should set.seed to simulate frequency of image acquisition
set.seed(42)
doi_vctr_sub <- sort(sample(doi_vctr, length(doi_vctr)/4, replace = F))

df.sum <- data.frame(NULL)
for(iGrd in unique(df.all$grd_id)){
  
  df.sum <- df.all %>% 
    filter(grd_id %in% iGrd, DOI %in% doi_vctr_sub) %>%
    filter(!is.na(NDVI)) %>%    #  for some reason, sometimes we have some
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

##### THIS SHOULD BE DONE IN STEP 4 to BE MORE CLEAN #####

# load original raster
r <- raster::raster(x = paste0('dataProcessing/', landscape_ID, '___map'))
o <- raster::extract(x = r, y = grd)

df.grd <- data.frame(grd_id = as.numeric(rownames(grd)),
                     y = grd$x, x = grd$y, original_id = o) # !!! NOTE FLIPPED x & y
  
##########################################################


df.sum <- df.sum %>% 
  left_join(df.grd, by = 'grd_id') # add grid info




# save outputs
save('df.sum', file = paste0('dataProcessing/', landscape_ID, 
                             '___metrics-', LC1, '-', LC0, '-', ndvi_noise,
                             '___', psf_fname, '.Rda'))
}