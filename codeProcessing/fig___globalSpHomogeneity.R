require(readr)
require(ggplot2)
require(dplyr)
require(raster)
require(sf)
require(scales)

dpath <- paste0('../../../Google Drive/spHomogeneity')


vpath <- '../../AncillaryDatasets/WorldVector'

# world <- sf::st_read(paste0(vpath,'ne_50m_land.shp'), quiet = TRUE)
# laes_prj <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
# europe_laea <- sf::st_intersection(world, st_set_crs(st_as_sf(as(raster::extent(-10, 55, 26, 72), "SpatialPolygons")), st_crs(world)))%>%
#   st_transform(laes_prj)
# xLims <- c(2.5e6,6e6)
# yLims <- c(1.5e6,4.5e6)

ocean <- sf::st_read(paste0(vpath,'/ne_50m_ocean.shp'), quiet = TRUE)
provs <- sf::st_read(paste0(vpath,'/ne_50m_admin_1_states_provinces_lines.shp'), quiet = TRUE)
countries <- sf::st_read(paste0(vpath,'/ne_50m_admin_0_countries.shp'), quiet = TRUE)
lakes <- sf::st_read(paste0(vpath,'/ne_50m_lakes.shp'), quiet = TRUE)



# Global map
fname.glo <- paste0(dpath, '/global_TCI_NDVI_2019_WGS84.tif')
r.glo <- raster(fname.glo)
r.glo_df <- as.data.frame(r.glo/10000, xy = T)
colnames(r.glo_df) <- c('lon', 'lat', 'TCI')



# regio map
fname.reg <- paste0(dpath, '/TCI_NDVI_2019_BoliviaMatoGrosso_WGS84.tif')
r.reg <- raster(fname.reg)
r.reg.agr <- raster::aggregate(r.reg, fact = 4)
r.reg_df <- as.data.frame(r.reg.agr/10000, xy = T)
colnames(r.reg_df) <- c('lon', 'lat', 'TCI')


# local map
ext.loc <- c(-55, -53, -13.5, -11.5)
r.loc <- crop(r.reg, ext.loc)
r.loc_df <- as.data.frame(r.loc/10000, xy = T)
colnames(r.loc_df) <- c('lon', 'lat', 'TCI')


g.glo.map <- ggplot(r.glo_df) +
  geom_sf(data = ocean, fill = 'black', size = 0) +
  geom_raster(aes(x = lon, y = lat, fill = TCI)) +
  scale_fill_viridis_c('Temporal Coherence Index (TCI)',
                       option = 'magma', oob = squish, 
                       limits = c(0,1)) +
  coord_sf(expand = c(0,0)) + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = 'top',
        legend.key.width = unit(2.4, "cm")) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))




g.reg.map <- ggplot(r.reg_df) +
  geom_sf(data = ocean, fill = 'black', size = 0) +
  geom_sf(data = provs, colour = 'grey20', size = 0.3, linetype = 'dotdash') +
  geom_sf(data = lakes, colour = 'blue', fill = 'lightblue', size = 0.4) +
  geom_sf(data = countries, colour = 'grey10', fill = NA, size = 0.4) +
  geom_raster(aes(x = lon, y = lat, fill = TCI)) +
  scale_fill_viridis_c('Temporal Coherence Index (TCI)',
                       option = 'magma', oob = squish, 
                       limits = c(0,1)) +
  coord_sf(expand = c(0,0)) + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = 'none') +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))


g.loc.map <- ggplot(r.loc_df) +
  #geom_sf(data = provs, colour = 'grey20', size = 0.3, linetype = 'dotdash') +
  geom_raster(aes(x = lon, y = lat, fill = TCI)) +
  scale_fill_viridis_c('Temporal Coherence Index (TCI)',
                       option = 'magma', oob = squish, 
                       limits = c(0,1)) +
  coord_sf(expand = c(0,0)) + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = 'top',
        legend.key.width = unit(2.4, "cm")) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))




## Export figure...

fig.name <- 'fig___globalTCI'

fig.fmt <- 'png'
fig.path <- 'figures/final_figures/'
dir.create(fig.path, recursive = T, showWarnings = F)

fig.fullname <- paste0(fig.path, '/', fig.name, '.', fig.fmt)

ggsave(filename = fig.fullname, plot = gmap, width = 8, height = 5)  

