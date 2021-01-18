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
require(sf)

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

# read the time series data
dat.ts <- bind_rows(
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


# calculate the TCI metric per time series
dat.TCI <- dat.ts %>% 
  filter(!is.na(NDVI)) %>% 
  group_by(pixID) %>%
  mutate(DOI = as.numeric(strftime(date, format = "%j")),
         DOI.hour = DOI + ifelse(platform == 'TERRA', 10.5/24, 13.5/24)) %>%
  group_by(pixID, lat, lon) %>%
  arrange(DOI.hour) %>%
  summarise(TCI = calc_TCI(NDVI, DOI.hour))

# transform to sf object with projection (assuming GEE exports csv in lat/lon) 
pts.TCI <- st_as_sf(x = dat.TCI, coords = c('lon','lat'), crs = 4326)

# function to draw a box around the points
bbox_polygon <- function(x) {
  bb <- sf::st_bbox(x)
  
  p <- matrix(
    c(bb["xmin"], bb["ymin"], 
      bb["xmin"], bb["ymax"],
      bb["xmax"], bb["ymax"], 
      bb["xmax"], bb["ymin"], 
      bb["xmin"], bb["ymin"]),
    ncol = 2, byrow = T
  )
  
  sf::st_polygon(list(p))
}

# get bounding box for our points
bb <- st_sfc(bbox_polygon(pts.TCI)) %>%
  st_set_crs(st_crs(pts.TCI))

# calculate Voronoi polygons which will serve as proxy for MODIS gridcells
point_tiles <- pts.TCI %>%
  st_union() %>%
  st_voronoi(., envelope = bb) %>%
  st_cast()
  
# add the attributes of the original and clip of unnecessarily parts
point_tiles <- point_tiles %>%
  data.frame(geometry = .) %>%
  st_sf(.) %>%
  st_join(., pts.TCI) %>%
  st_intersection(., bb) 

# export it all
fname <- paste0('data/final_data/data4figures/df_MODIS_', zone_name, '.RData')
save('pts.TCI', 'point_tiles', 'dat.ts', file = fname)

