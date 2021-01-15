#!/usr/local/bin/Rscript
################################################################################
# Project:  spHomogeneity
# Purpose:  Gather data exported from GEE 
# License:  GPL v3
# Authors:  Gregory Duveiller - Jan 2021
################################################################################


gee_export_path <- '../../../google_drive/GEE_exported_data'


file.copy(from = paste0(gee_export_path, '/TCI_ts_NDVI_California_WGS84.tif'),
          to = 'data/inter_data/from_GEE/', 
          recursive = T, copy.date = T, overwrite = T)

file.copy(from = paste0(gee_export_path, '/TCI_NDVI_2019_BoliviaMatoGrosso_WGS84.tif'),
          to = 'data/final_data/data4figures', 
          recursive = T, copy.date = T, overwrite = T)

file.copy(from = paste0(gee_export_path, '/TCI_NDVI_2019_Global_WGS84.tif'),
          to = 'data/final_data/data4figures', 
          recursive = T, copy.date = T, overwrite = T)
