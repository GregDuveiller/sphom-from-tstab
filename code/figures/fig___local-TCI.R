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
require(stringr)

## PREPARATION ----

# setup
dpath <- 'data/final_data/data4figures'
# set colours of the MODIS points
col.1 <- '#1770DC'
col.2 <- '#DCA416'



# function to get a gg plot object
get_ggTCI_fig <- function(zone_name, TCI.range, iPixies, NDVI.range = c(0, 1)){
  
  # get S2 image
  img_file <- list.files(path = dpath, full.names = T, 
                         pattern = paste0('S2_L2A_image_', zone_name))
  S2_img <- brick(img_file)
  
  # get S2 image date
  img_date <- as.Date(str_sub(basename(img_file),-12,-5), format = '%Y%m%d')
  
  # load data to make figure 
  load(paste0(dpath, '/df_MODIS_', zone_name, '.RData')) # <---'pts.TCI', 'dat.ts', 'point_tiles'
  
  # get pixIDs for selected points
  sel.pts.prj <- st_as_sf(x = iPix2Sel, coords = c('lon','lat'), crs = 4326)
  iPixies <- data.frame(pixLbl = LETTERS[1:dim(sel.pts.prj)[1]],
                        pixID = st_nearest_feature(x = sel.pts.prj, y = pts.TCI))
  
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
    geom_vline(xintercept = img_date, colour = 'grey60', size = 1) +
    geom_point(aes(x = date, y = NDVI, colour = platform, shape = platform),
               size = 2 ) +
    geom_point(data = df.ts %>% filter(I2k == F), 
               aes(x = date, y = NDVI, shape = platform),
               colour = 'grey80', size = 2) +
    facet_wrap(~pixLbl_long, nc = 1) +
    scale_colour_manual('Satellite platform:',
                        values = c('AQUA' = col.1, 'TERRA' = col.2)) +
    scale_shape_discrete('Satellite platform:', guide = 'none') +
    scale_x_date('') +
    scale_y_continuous(position = 'right', limits = NDVI.range) +
    theme(legend.position = 'bottom',
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


## Make figures for 'main' ----

# select pixels to illustrate in this figure
iPix2Sel <- c(lon = 8.231, lat = 45.195) %>%
  bind_rows(c(lon = 8.25,  lat = 45.218)) %>%
  bind_rows(c(lon = 8.261, lat = 45.23)) %>%
  bind_rows(c(lon = 8.27,  lat = 45.18))

# get GGPLOT object
g <- get_ggTCI_fig('vercelli_2018', c(0.2, 0.7), iPix2Sel, c(0, 1))

# export figure
fig.name <- paste0('fig___', 'local-TCI-main', '.', fig.fmt)
ggsave(fig.name, plot = g, path = fig.path, device = fig.fmt, 
       width = 10, height = 10)


## Make figures for 'defor1' ----

# select pixels to illustrate in this figure
iPix2Sel <- c(lon = 105.09,  lat = 13.23) %>%
  bind_rows(c(lon = 105.13,  lat = 13.235)) %>%
  bind_rows(c(lon = 105.102, lat = 13.26)) %>%
  bind_rows(c(lon = 105.07,  lat = 13.212))

# get GGPLOT object
g <- get_ggTCI_fig('rovieng_2019', c(0.2, 0.7), iPix2Sel, c(0, 1))

# export figure
fig.name <- paste0('fig___', 'local-TCI-defor1', '.', fig.fmt)
ggsave(fig.name, plot = g, path = fig.path, device = fig.fmt, 
       width = 10, height = 10)


## Make figures for 'dunes' ----

# select pixels to illustrate in this figure
iPix2Sel <- c(lon = 4.53,  lat = 29.04) %>%
  bind_rows(c(lon = 4.518, lat = 29.065)) %>%
  bind_rows(c(lon = 4.593, lat = 29.1)) %>%
  bind_rows(c(lon = 4.56,  lat = 29.053))

# get GGPLOT object
g <- get_ggTCI_fig('sahara_2019', c(0.8, 1.2), iPix2Sel, c(0.075, 0.175))

# export figure
fig.name <- paste0('fig___', 'local-TCI-dunes', '.', fig.fmt)
ggsave(fig.name, plot = g, path = fig.path, device = fig.fmt, 
       width = 10, height = 10)


## Make figures for 'defor2' ----

# select pixels to illustrate in this figure
iPix2Sel <- c(lon = -60.635, lat = -11.52) %>%
  bind_rows(c(lon = -60.635, lat = -11.546)) %>%
  bind_rows(c(lon = -60.610, lat = -11.51)) %>%
  bind_rows(c(lon = -60.595, lat = -11.48))

# get GGPLOT object
g <- get_ggTCI_fig('rondonia_2019', c(0.2, 0.7), iPix2Sel, c(0, 1))

# export figure
fig.name <- paste0('fig___', 'local-TCI-defor2', '.', fig.fmt)
ggsave(fig.name, plot = g, path = fig.path, device = fig.fmt, 
       width = 10, height = 10)



