#!/usr/local/bin/Rscript
################################################################################
# Project:  spHomogeneity
# Purpose:  Gather data exported from GEE 
# License:  GPL v3
# Authors:  Gregory Duveiller - Jan 2021
################################################################################


gee_export_path <- '../../../google_drive/GEE_exported_data'


zone_name <- 'sahara_2019'

file.copy(from = paste0(gee_export_path, '/MODIS_AQUA_NDVI_datablock_', zone_name,'.csv'),
          to = 'data/inter_data/from_GEE', 
          recursive = T, copy.date = T, overwrite = T)

file.copy(from = paste0(gee_export_path, '/MODIS_TERRA_NDVI_datablock_', zone_name,'.csv'),
          to = 'data/inter_data/from_GEE', 
          recursive = T, copy.date = T, overwrite = T)

file.copy(from = paste0(gee_export_path, '/S2_L2A_image_', zone_name,'.tif'),
          to = 'data/final_data/data4figures', 
          recursive = T, copy.date = T, overwrite = T)




file.copy(from = paste0(gee_export_path, '/MODIS_AQUA_NDVI_datablock_vercelli_2018.csv'),
          to = 'data/inter_data/from_GEE', 
          recursive = T, copy.date = T, overwrite = T)

file.copy(from = paste0(gee_export_path, '/MODIS_TERRA_NDVI_datablock_vercelli_2018.csv'),
          to = 'data/inter_data/from_GEE', 
          recursive = T, copy.date = T, overwrite = T)

file.copy(from = paste0(gee_export_path, '/S2_L2A_image_vercelli_2018.tif'),
          to = 'data/final_data/data4figures', 
          recursive = T, copy.date = T, overwrite = T)


file.copy(from = paste0(gee_export_path, '/MODIS_AQUA_NDVI_datablock_rondonia_2019.csv'),
          to = 'data/inter_data/from_GEE', 
          recursive = T, copy.date = T, overwrite = T)

file.copy(from = paste0(gee_export_path, '/MODIS_TERRA_NDVI_datablock_rondonia_2019.csv'),
          to = 'data/inter_data/from_GEE', 
          recursive = T, copy.date = T, overwrite = T)

file.copy(from = paste0(gee_export_path, '/S2_L2A_image_rondonia_2019.tif'),
          to = 'data/final_data/data4figures', 
          recursive = T, copy.date = T, overwrite = T)



file.copy(from = paste0(gee_export_path, '/MODIS_AQUA_NDVI_datablock_rovieng_2019.csv'),
          to = 'data/inter_data/from_GEE', 
          recursive = T, copy.date = T, overwrite = T)

file.copy(from = paste0(gee_export_path, '/MODIS_TERRA_NDVI_datablock_rovieng_2019.csv'),
          to = 'data/inter_data/from_GEE', 
          recursive = T, copy.date = T, overwrite = T)

file.copy(from = paste0(gee_export_path, '/S2_L2A_image_rovieng_2019.tif'),
          to = 'data/final_data/data4figures', 
          recursive = T, copy.date = T, overwrite = T)


file.copy(from = paste0(gee_export_path, '/TCI_ts_NDVI_California_WGS84.tif'),
          to = 'data/inter_data/from_GEE/', 
          recursive = T, copy.date = T, overwrite = T)

file.copy(from = paste0(gee_export_path, '/TCI_NDVI_2019_BoliviaMatoGrosso_WGS84.tif'),
          to = 'data/final_data/data4figures', 
          recursive = T, copy.date = T, overwrite = T)

file.copy(from = paste0(gee_export_path, '/TCI_NDVI_2019_Global_WGS84.tif'),
          to = 'data/final_data/data4figures', 
          recursive = T, copy.date = T, overwrite = T)


