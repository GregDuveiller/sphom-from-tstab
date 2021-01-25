#!/usr/local/bin/Rscript
################################################################################
# Project:  spHomogeneity
# Purpose:  Harvest data for California time series figure in the article 
# License:  GPL v3
# Authors:  Gregory Duveiller - Jan 2021
################################################################################


require(raster)
require(rgdal)
require(dplyr)


# set paths
CDL_path <- 'data/input_data/CDL'
TCI_path <- 'data/inter_data/from_GEE'
out_path <- 'data/final_data/data4figures'

# define the subsample of years to select
years2select <- 2015:2019


# Prepare CDL data ----
# get data from CDL
CDL_flist <- list.files(CDL_path, pattern = '.tif$', full.names = T)
r_CDL_conus <- stack(CDL_flist[2:13])  # Note: we avoid 2007 which has a diff res
names(r_CDL_conus) <- substr(x = names(r_CDL_conus), 1, 8)

# tabulation of class counts
clsfrq <- as.data.frame(table(getValues(r_CDL_conus))) # Tabulation of class counts
colnames(clsfrq) <- c('classID', 'freq')

# get colours and names 
colors <- r_CDL_conus[[12]]@legend@colortable  # Class colors
txt <- read.delim(paste0(CDL_path, '/CDL_names_colors_codes/cdl_names.txt'),
                  header = F, blank.lines.skip = F)
names(txt) <- 'className'
col <- read.table(paste0(CDL_path, '/CDL_names_colors_codes/cdl_colors.txt'), 
                  blank.lines.skip = F)
names(col) <- 'colorCode'

# make legend
lgd <- bind_cols(txt, col)
lgd$colors <- colors[1:255]
lgd$classID <- factor(as.numeric(rownames(lgd))-1)
lgd_sub <- clsfrq %>% left_join(lgd, by = 'classID')

# Get rasters into dataframes
# (there are probably better ways to do this...i.e. avoiding dataframes)
df_CDL <- as.data.frame(r_CDL_conus, xy = T, long = T)
names(df_CDL) <- c('x', 'y', 'year', 'CDL')
df_CDL$year <- as.numeric(substr(df_CDL$year, 5, 8))
df_CDL <- df_CDL %>% filter(year %in% years2select)
save('df_CDL', 'lgd_sub', file = paste0(out_path, '/df_CDL.RData'))



# Prepare TCI data ----
# get TCI data
r_TCI_wgs84 <- stack(paste0(TCI_path, '/TCI_ts_NDVI_California_WGS84.tif'))

# reproject
r_TCI_conus <- projectRaster(r_TCI_wgs84/10000, r_CDL_conus)

# Get rasters into dataframes
# (there are probably better ways to do this...i.e. avoiding dataframes)
df_TCI <- as.data.frame(r_TCI_conus, xy = T, long = T)
names(df_TCI) <- c('x', 'y', 'year', 'TCI')
df_TCI$year <- as.numeric(substr(df_TCI$year, 5, 8))
df_TCI$TCI <- round(df_TCI$TCI, digits = 4)
df_TCI <- df_TCI %>% filter(year %in% years2select)
save('df_TCI', file = paste0(out_path, '/df_TCI.RData'))
