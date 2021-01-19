#!/usr/local/bin/Rscript
################################################################################
# Project:  spHomogeneity
# Purpose:  make figure showing the local examples 
# License:  GPL v3
# Authors:  Gregory Duveiller - Jan 2021
################################################################################


require(ggplot2)
require(dplyr)
require(patchwork)
require(ggrepel)
require(scales)
require(raster)
require(RStoolbox)
require(sf)


## PREPARATION ----

# setup
dpath <- 'data/final_data/data4figures'
# set colours of the MODIS points
col.1 <- '#1770DC'
col.2 <- '#DCA416'






# function to get a gg plot object
get_ggTCI_fig <- function(zone_name, TCI.range, iPixies, NDVI.range = c(0, 1)){

# get S2 image
S2_img <- brick(paste0(dpath,'/S2_L2A_image_', zone_name, '.tif'))

# load data to make figure 
load(paste0(dpath, '/df_MODIS_', zone_name, '.RData')) # <---'pts.TCI', 'dat.ts', 'point_tiles'


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
  scale_colour_viridis_c('Temporal Coherence Index (TCI)', option = 'magma', limits = TCI.range) +
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
  scale_fill_viridis_c('Temporal Coherence Index (TCI)', option = 'magma', 
                       limits = TCI.range, oob = squish) +
  scale_colour_viridis_c('Temporal Coherence Index (TCI)', option = 'magma',
                         limits = TCI.range, oob = squish) +
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
  scale_y_continuous(position = 'right', limits = NDVI.range) +
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

return(gCombined)
}

# export
if(!exists('fig.fmt')){fig.fmt = 'png'}
if(!exists('fig.path')){fig.path = 'docs/figures'}
dir.create(path = fig.path, recursive = T, showWarnings = F)

# name
zone_name <- 'sahara_2019'
# colourbar range
TCI.range <- c(0,1)
# select pixels to illustrate in this figure
iPixies <- data.frame(pixLbl = c('A','B','C','D'),
                      pixID = c(24, 1220, 377, 590)) 

iPix <- data.frame(lon = 4.54, lat = 29.04) %>%
  bind_rows(c(lon = 4.54, lat = 29.04))


iPix2Sel <- c(lon = 4.54, lat = 29.04) %>%
  bind_rows(c(lon = 4.54, lat = 29.04)) %>%
  bind_rows(c(lon = 4.54, lat = 29.04)) %>%
  bind_rows(c(lon = 4.54, lat = 29.04))
  




gEx4 <- get_ggTCI_fig('sahara_2019', c(0.8,1.2), iPixies, c(0.075, 0.175))

fig.name <- paste0('fig___', 'local-TCI-ex4', '.', fig.fmt)
ggsave(fig.name, plot = gEx4, path = fig.path, device = fig.fmt, 
       width = 10, height = 10)




