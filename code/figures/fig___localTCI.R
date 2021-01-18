#!/usr/local/bin/Rscript
################################################################################
# Project:  spHomogeneity
# Purpose:  make figure showing the local example
# License:  GPL v3
# Authors:  Gregory Duveiller - Jan 2021
################################################################################


require(ggplot2)
require(dplyr)
require(raster)
require(ggplotify)
require(grid)
require(ggrepel)


zone_name <- 'vercelli_2018'
# zone_name <- 'beauce_2019'
# zone_name <- 'hyytiala_2018'
# zone_name <- 'fresno_2019'


# load dat.TCI... 
load(paste0('data/final_data/data4figures/df_MODIS_', zone_name, '.RData')) # <---'dat.TCI', 'dat.ts'



col.1 <- '#1770DC'
col.2 <- '#DCA416'

S2_img <- brick(paste0(dpath,'/S2_L2A_image_', zone_name, '.tif'))
g.rgb <- as.grob(~plotRGB(S2_img, r = 3, g = 2, b = 1, scale = 10000,
                          stretch = "lin", margins = F))

raster::image(x, rgb = c(3,2,1))


library(stars)

tif <- paste0(dpath,'/S2_L2A_image_', zone_name, '.tif')
# read_stars(tif) -> x
rasterio = list(nXOff = 6, nYOff = 6, nXSize = 100, nYSize = 100, bands = c(3, 2, 1))
(x = read_stars(tif, RasterIO = rasterio))

x %>% slice(band, 3) -> x3


ndvi = function(x) (x[4] - x[1])/(x[4] + x[1])
(s2.ndvi = st_apply(x, c("x", "y"), ndvi))
x2 <- x %>% slice(rgb = S2_L2A_image_vercelli_2018.tif/1000) -> x2 


library(ggplot2)
ggplot() + geom_stars(data = x, aes(x=x, y=y, col = rgb(3,2,1))) +
  coord_equal() +
  facet_wrap(~band) +
  scale_x_discrete(expand=c(0,0))+
  scale_y_discrete(expand=c(0,0))

x %>% slice(band, 6) -> x6

g = ggplot() + 
  coord_sf() + 
  scale_fill_viridis_c() 

g + geom_stars(data = x, downsample = c(10,10,1)) + 
  facet_wrap(~band)



df.grd <- dat.ts %>%  
  filter(!is.na(NDVI)) %>%
  group_by(pixID) %>% 
  summarize(lat = mean(lat), lon = mean(lon))


iPixies <- data.frame(pixLbl = c('A','B','C','D'),
                      pixID = c(
                        which.max(dat.TCI$TCI), 
                        1000, 1200,
                        # which.min(abs(dat.TCI$TCI-quantile(dat.TCI$TCI, 0.7))),
                        # which.min(abs(dat.TCI$TCI-quantile(dat.TCI$TCI, 0.3))),
                        which.min(dat.TCI$TCI))) 



df.ts <- dat.ts %>% 
  filter(!is.na(NDVI)) %>%
  inner_join(iPixies, by = 'pixID') %>% 
  left_join(dat.TCI, by = c('lat', 'lon', 'pixID')) %>%
  mutate(pixLbl_long = paste('Time series', pixLbl, '|', 
                             'TCI =', round(TCI, digits = 3)))

df.pixList <- iPixies %>% 
  left_join(df.grd, by = 'pixID')




g.map.TCI <- ggplot(point_tiles) +
  geom_sf(aes(fill = TCI, colour = TCI)) +
  geom_label_repel(data = df.pixList, 
                   aes(x = lon, y = lat, label = pixLbl),
                   size = 4, fontface = 'bold', colour = 'black') + 
  geom_point(data = df.pixList, 
             aes(x = lon, y = lat),
             colour = 'black', shape = 3, size = 4) +
  # geom_point(aes(x = lon, y = lat, fill = pixID), 
  #            shape = 22, size = 3, colour = 'grey80') + 
  # geom_point(data = df.ts, aes(x = lon, y = lat), 
  #            shape = 3, size = 2, colour = 'black') +
  # geom_label(data = df.ts, aes(x = lon, y = lat, label = pixLbl),
  #            size = 4, fontface = 'bold', colour = 'black', label.r = unit(0, "lines")) +
  coord_sf(expand = F) +
  scale_fill_viridis_c('Temporal Coherence Index (TCI)', option = 'magma', limits = c(0,1)) +
  scale_colour_viridis_c('Temporal Coherence Index (TCI)', option = 'magma', limits = c(0,1)) +
  theme(legend.position = 'top',
        legend.key.width = unit(2, "cm"),
        panel.background = element_rect(fill = 'white'),
        # axis.ticks = element_blank(),
        # axis.text = element_blank(),
        axis.title = element_blank()) +
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = 0.5))


# 
# 
# g.map.TCI <- ggplot(dat.TCI) +
#   geom_point(aes(x = lon, y = lat, fill = TCI),
#              shape = 22, size = 3, colour = 'grey80') +
#   geom_label_repel(data = df.pixList, 
#                    aes(x = lon, y = lat, label = pixLbl),
#                    size = 4, fontface = 'bold', colour = 'black') + 
#   geom_point(data = df.pixList, 
#              aes(x = lon, y = lat),
#              colour = 'black', shape = 3, size = 4) +
#   # geom_point(aes(x = lon, y = lat, fill = pixID), 
#   #            shape = 22, size = 3, colour = 'grey80') + 
#   # geom_point(data = df.ts, aes(x = lon, y = lat), 
#   #            shape = 3, size = 2, colour = 'black') +
#   # geom_label(data = df.ts, aes(x = lon, y = lat, label = pixLbl),
#   #            size = 4, fontface = 'bold', colour = 'black', label.r = unit(0, "lines")) +
#   coord_cartesian() +
#   scale_fill_viridis_c('Temporal Coherence Index (TCI)', option = 'magma', limits = c(0,1)) +
#   theme(legend.position = 'top',
#         legend.key.width = unit(2, "cm"),
#         panel.background = element_rect(fill = 'white'),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         axis.title = element_blank()) +
#   guides(fill = guide_colorbar(title.position = 'top', title.hjust = 0.5))
# 

g.ts <- ggplot(df.ts) +
  geom_point(aes(x = date, y = NDVI, colour = platform, shape = platform),
             size = 2 ) +
  facet_wrap(~pixLbl_long, nc = 1) +
  scale_colour_manual('Satellite platform:',
                        values = c('AQUA' = col.1, 'TERRA' = col.2)) +
  scale_shape_discrete('Satellite platform:') +
  scale_x_date('') +
  scale_y_continuous(position = 'right', limits = c(0, 1)) +
  theme(legend.position = 'bottom',
        strip.background = element_blank(),
        strip.text = element_text(size = rel(1.2)),
        panel.grid = element_line(linetype = 'dotted', colour = 'grey50'),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'grey10'))

fig.name <- paste0('fig___', 'local', 'TCI')
fig.path <- paste0('figures/final_figures/')
dir.create(fig.path, showWarnings = F, recursive = T)
fig.width <- 12; fig.height <- 9;  fig.fmt <- 'png'
fig.fullfname <- paste0(fig.path, '/', fig.name, '.', fig.fmt)
if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}

w <- 0.4; h <- 0.5


print(g.map.TCI, vp = viewport(width = w, height = 0.55, x = 0,  y = 0.45, just = c(0,0)))

# grid.newpage()
vp = viewport(width = 0.36, height = 0.45, x = 0, y = 0, just = c(0,0))
pushViewport(vp)
grid.draw(g.rgb)
upViewport()


print(g.ts, vp = viewport(width = 1 - w, height = 2*h, x = w, y = 0, just = c(0,0)))


dev.off()



