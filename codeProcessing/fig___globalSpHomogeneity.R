require(readr)
require(ggplot2)
require(dplyr)
require(raster)
require(sf)
require(scales)

dpath <- paste0('../../../Google Drive/spHomogeneity')
vpath <- '../../AncillaryDatasets/WorldVector'
fname <- paste0(dpath, '/global_TCI_NDVI_2019.tif')


world <- sf::st_read(paste0(vpath,'/ne_50m_ocean.shp'), quiet = TRUE)


r <- raster(fname)


r_df <- as.data.frame(r, xy = T, long = T)
colnames(r_df) <- c('lon', 'lat', 'layer', 'TCI')
r_df <- r_df %>% mutate(TCI = TCI/10000) %>% select(-layer) 

gmap <- ggplot(r_df) +
  # geom_sf(data = world, fill = landColor, size = 0) +
  geom_raster(aes(x = lon, y = lat, fill = TCI)) +
  scale_fill_viridis_c('Temporal Coherence Index (TCI)',
                       option = 'magma', oob = squish, 
                       limits = c(0,1)) +
  coord_sf(expand = c(0,0)) + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = 'bottom',
        legend.key.width = unit(2.4, "cm")) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))


fig.name <- 'fig___globalTCI'

fig.fmt <- 'png'
fig.path <- 'figures/final_figures/'
dir.create(fig.path, recursive = T, showWarnings = F)

fig.fullname <- paste0(fig.path, '/', fig.name, '.', fig.fmt)

ggsave(filename = fig.fullname, plot = gmap, width = 8, height = 5)  

