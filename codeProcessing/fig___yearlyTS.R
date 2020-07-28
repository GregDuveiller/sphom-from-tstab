library(raster)
library(rgdal)

library(ggplot2)
library(scales)

CDL_path <- '../../../Downloads/polygonclip_20200727105509_1043149173'
TCI_path <- '../../../Google Drive/spHomogeneity'

CDL_flist <- list.files(CDL_path, pattern = '.tif$', full.names = T)

r_CDL_conus <- stack(CDL_flist[2:13])  # Note: we avoid 2007 which has a diff res
names(r_CDL_conus) <- substr(x = names(r_CDL_conus), 1, 8)

# classes <- table(getValues(r)) # Tabulation of class counts
# colors <- r@legend@colortable  # Class colors
# plot(r, col=colors[1:length(classes)+1], colNA=colors[1], axes=FALSE, box=FALSE)

#classes <- table(getValues(r_CDL_conus)) # Tabulation of class counts
colors <- r_CDL_conus[[1]]@legend@colortable  # Class colors



r_TCI_wgs84 <- stack(paste0(TCI_path, '/TCI_ts_NDVI_California_WGS84.tif'))
r_TCI_wgs84 <- r_TCI_wgs84[[2:13]]


r_TCI_conus <- projectRaster(r_TCI_wgs84/10000, r_CDL_conus)


df_CDL <- as.data.frame(r_CDL_conus, xy = T, long = T)
names(df_CDL) <- c('x', 'y', 'year', 'CDL')
df_CDL$year <- as.numeric(substr(df_CDL$year, 5, 8))

df_TCI <- as.data.frame(r_TCI_conus, xy = T, long = T)
names(df_TCI) <- c('x', 'y', 'year', 'TCI')
df_TCI$year <- as.numeric(substr(df_TCI$year, 5, 8))
  


years2plot <- 2015:2019


ggplot(df_TCI %>% filter(year %in% years2plot),  
       aes(x = x, y = y)) +
  geom_raster(aes(fill = TCI)) +
  facet_grid(.~year) + 
  scale_fill_viridis_c(option = 'A', limits = c(0,1), oob = squish) +
  coord_equal(expand = F) +
  theme(legend.position = 'top',
        legend.key.width = unit(2.4, "cm")) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))




ggplot(df_CDL %>% filter(year %in% years2plot),  
       aes(x = x, y = y)) +
  geom_raster(aes(fill = factor(CDL))) +
  facet_grid(.~year) + 
  scale_fill_manual(values = colors) +
  coord_equal(expand = F) +
  theme(legend.position = 'none')

