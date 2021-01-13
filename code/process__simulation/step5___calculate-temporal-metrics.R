#!/usr/local/bin/Rscript
################################################################################
# Project:  spHomogeneity
# Purpose:  Function to calculate the TCI index on the simulated time series
#           (STEP 5 in simulation exercise)
# License:  GPL v3
# Authors:  Gregory Duveiller - Jul 2020
################################################################################


calc_temp_metrics = function(batch_name, TS2LC, temp_subsample = NULL){
# step5___calculate-temporal-metrics.R
#
# Step 5 in spHomogeneity simulation: calculating temporal metrics 


require(dplyr)
require(tidyr)


# load target datasets
load(paste0('data/inter_data/', batch_name, 
            '/datablock-', paste(TS2LC, collapse = '-'),
            '___', 'PSF-AQUA-48-20', '.Rda'))  # 'df.all' & 'df.grd'
df.all.AQUA <- df.all
df.grd.AQUA <- df.grd

load(paste0('data/inter_data/', batch_name, 
            '/datablock-', paste(TS2LC, collapse = '-'),
            '___', 'PSF-TERRA-48-20', '.Rda'))  # 'df.all' & 'df.grd'
df.all.TERRA <- df.all
df.grd.TERRA <- df.grd

# combine them
df.all <- bind_rows(
  df.all.TERRA %>% mutate(DOI.time = DOI + 10.5/24, platform = 'TERRA'),
  df.all.AQUA %>% mutate(DOI.time = DOI + 13.5/24, platform = 'AQUA')
  )
  


# load('data/inter_data/landscape-2LC-id42___datablock-TS1-TS2-0.01___PSF-AQUA-48-10.Rda')

doi_vctr <- unique(df.all$DOI.time)

# should set.seed to simulate frequency of image acquisition
if(!is.null(temp_subsample)){
  set.seed(42)
  doi_vctr_sub <- sort(sample(doi_vctr, length(doi_vctr)/temp_subsample, replace = F))
} else {doi_vctr_sub <- doi_vctr}

df.all <- df.all %>%
  filter(DOI.time %in% doi_vctr_sub)

save(list = c('df.all','df.grd'), 
     file = paste0('data/inter_data/', batch_name,
                   '/datablock-combined-', paste(TS2LC, collapse = '-'),
                   '___tsub-', temp_subsample, '.Rda'))




source('code/general/calc_TCI.R')

df.sum <- df.all %>%
  group_by(grd_id) %>%
  summarize(pur_avg = mean(maxPur),
            pur_std = sd(maxPur),
            TCI = calc_TCI(NDVI, DOI.time)) %>%
  left_join(df.grd, by = 'grd_id')



# save outputs
save('df.sum', 
     file = paste0('data/inter_data/', batch_name,
                   '/metrics-', paste(TS2LC, collapse = '-'), '.Rda'))
}
