#!/usr/local/bin/Rscript
################################################################################
# Project:  spHomogeneity
# Purpose:  Harvest data for local TCI demo figure 
# License:  GPL v3
# Authors:  Gregory Duveiller - Jan 2021
################################################################################


require(raster)
require(readr)
require(dplyr)
require(dismo)


zone_name <- 'vercelli_2018'
# zone_name <- 'beauce_2019'
# zone_name <- 'hyytiala_2018'
# zone_name <- 'fresno_2019'

dpath <- paste0('data/inter_data/from_GEE')

dat1 <- read_csv(file = paste0(dpath,'/MODIS_AQUA_NDVI_datablock_', zone_name, '.csv'), col_names = T, skip = 1)
dat2 <- read_csv(file = paste0(dpath,'/MODIS_TERRA_NDVI_datablock_', zone_name, '.csv'), col_names = T, skip = 1)

source('code/general/calc_TCI.R')

dir.create(path = paste0('data/inter_data/modis_test_zones/', zone_name), 
           recursive = T, showWarnings = F)

# read the data
dat <- bind_rows(
  dat1 %>% 
    dplyr::select(-X7, -time) %>% 
    rename(ID = `0`, date = 'id') %>% 
    mutate(platform = 'AQUA'),
  dat2 %>% 
    dplyr::select(-X7, -time) %>% 
    rename(ID = `0`, date = 'id') %>% 
    mutate(platform = 'TERRA')) %>%
  rename(lon = 'longitude', lat = 'latitude')  %>% 
  mutate(date = as.Date(date, '%Y_%m_%d')) %>%
  group_by(date, platform) %>%
  mutate(pixID = row_number())


# calc the metrics per time series
dat.sum <- dat %>% 
  filter(!is.na(NDVI)) %>% 
  group_by(pixID) %>%
  mutate(DOI = as.numeric(strftime(date, format = "%j")),
         DOI.hour = DOI + ifelse(platform == 'TERRA', 10.5/24, 13.5/24)) %>%
  group_by(pixID, lat, lon) %>%
  arrange(DOI.hour) %>%
  summarise(TCI = calc_TCI(NDVI, DOI.hour))

save('dat.sum', file = paste0('data/final_data/data4figures/df_MODIS_', 
                              zone_name, '.RData'))


### THERE IS SOME ISSUE HERE>>> with the projections... 
### The modis data, exported as CSV, seems to be in "Sinusidal" from the export
### but it is displayed in lat/lon
### and the S2 is in '+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs'
### need to bring these together... 

# get S2 ref image
S2_img <- brick(paste0(dpath,'/S2_L2A_image_', zone_name, '.tif'))

# get voronoi polygons
MODIS_gridcells <- voronoi(xy = data.frame(x = dat.sum$lon, y = dat.sum$lat))

# reproject MODIS TCI data onto S2_img grid
r <- rasterize(x = MODIS_gridcells, y = S2_img[[1]], field = dat.sum$TCI)



