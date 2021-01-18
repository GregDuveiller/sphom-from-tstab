#!/usr/local/bin/Rscript
################################################################################
# Project:  spHomogeneity
# Purpose:  make figure showing the local example (near Vercelli in 2018)
# License:  GPL v3
# Authors:  Gregory Duveiller - Jan 2021
################################################################################


require(ggplot2)
require(dplyr)
require(raster)
require(ggrepel)
require(RStoolbox)
require(patchwork)
require(sf)

## PREPARATION ----

# setup
dpath <- 'data/final_data/data4figures'
zone_name <- 'vercelli_2018'

# get S2 image
S2_img <- brick(paste0(dpath,'/S2_L2A_image_', zone_name, '.tif'))

# load data to make figure 
load(paste0(dpath, '/df_MODIS_', zone_name, '.RData')) # <---'pts.TCI', 'dat.ts', 'point_tiles'

# set colours of the MODIS points
col.1 <- '#1770DC'
col.2 <- '#DCA416'

# select pixels to illustrate in this figure
iPixies <- data.frame(pixLbl = c('A','B','C','D'),
                      pixID = c(
                        which.max(pts.TCI$TCI), 
                        1000, 1200,  # <---- these 2 were selected manually
                        which.min(pts.TCI$TCI))) 


# prepare time series of selected points
df.ts <- dat.ts %>% 
  filter(!is.na(NDVI)) %>%
  inner_join(iPixies, by = 'pixID') %>% 
  left_join(pts.TCI, by = c('pixID')) %>%
  mutate(pixLbl_long = paste('Time series', pixLbl, '|', 
                             'TCI =', round(TCI, digits = 3)))

# prepare dataframe of labels for selected points
df.pixList <- iPixies %>% 
  left_join(pts.TCI, by = c('pixID')) %>%
  mutate(lon = st_coordinates(geometry)[,1],
         lat = st_coordinates(geometry)[,2])



## Plotting ----

# get image in a ggplot layer to be able to add to a ggplot
gIMG <- ggRGB(S2_img, r = 1, g = 2, b = 3, scale = 10000, stretch = "lin", 
              coord_equal = T, ggLayer = T)

# make image panel
g.map.img <-  ggplot(point_tiles) +
  geom_sf(aes(colour = TCI), fill = NA) +
  gIMG +
  geom_label_repel(data = df.pixList, 
                   aes(x = lon, y = lat, label = pixLbl),
                   size = 4, fontface = 'bold', colour = 'white', fill = 'grey20') + 
  geom_point(data = df.pixList, 
             aes(x = lon, y = lat),
             colour = 'white', shape = 3, size = 4) +
  coord_sf(expand = F) +
  scale_colour_viridis_c('Temporal Coherence Index (TCI)', option = 'magma', limits = c(0,1)) +
  theme(legend.position = 'none',
        legend.key.width = unit(2, "cm"),
        panel.background = element_rect(fill = 'white'),
        # axis.ticks = element_blank(),
        # axis.text = element_blank(),
        axis.title = element_blank()) +
  guides(colour = guide_colorbar(title.position = 'top', title.hjust = 0.5))



# make TCI panel
g.map.TCI <- ggplot(point_tiles) +
  geom_sf(aes(fill = TCI, colour = TCI)) +
  geom_label_repel(data = df.pixList, 
                   aes(x = lon, y = lat, label = pixLbl),
                   size = 4, fontface = 'bold', colour = 'white', fill = 'grey20') + 
  geom_point(data = df.pixList, 
             aes(x = lon, y = lat),
             colour = 'white', shape = 3, size = 4) +
  coord_sf(expand = F) +
  scale_fill_viridis_c('Temporal Coherence Index (TCI)', option = 'magma', limits = c(0,1)) +
  scale_colour_viridis_c('Temporal Coherence Index (TCI)', option = 'magma', limits = c(0,1)) +
  theme(legend.position = 'bottom',
        legend.key.width = unit(2, "cm"),
        panel.background = element_rect(fill = 'white'),
        # axis.ticks = element_blank(),
        # axis.text = element_blank(),
        axis.title = element_blank()) +
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = 0.5))


# make time series panel
g.ts <- ggplot(df.ts) +
  geom_point(aes(x = date, y = NDVI, colour = platform, shape = platform),
             size = 2 ) +
  facet_wrap(~pixLbl_long, nc = 1) +
  scale_colour_manual('Satellite platform:',
                        values = c('AQUA' = col.1, 'TERRA' = col.2)) +
  scale_shape_discrete('Satellite platform:') +
  scale_x_date('') +
  scale_y_continuous(position = 'right', limits = c(0, 1)) +
  theme(legend.position = c(0.1, 0.9),
        strip.background = element_blank(),
        strip.text = element_text(size = rel(1.2)),
        panel.grid = element_line(linetype = 'dotted', colour = 'grey50'),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'grey10')) +
  guides(colour = guide_legend(title.position = 'top', title.hjust = 0.5))


# combine them all
gCombined <- (g.map.img / g.map.TCI) | g.ts +
  plot_layout(guides = "collect") & theme(legend.position = "bottom") 

# export
if(!exists('fig.fmt')){fig.fmt = 'png'}
if(!exists('fig.path')){fig.path = 'docs/figures'}
dir.create(path = fig.path, recursive = T, showWarnings = F)
fig.name <- paste0('fig___', 'local-TCI-ex1', '.', fig.fmt)
ggsave(fig.name, plot = gCombined, path = fig.path, device = fig.fmt, 
       width = 10, height = 10)




