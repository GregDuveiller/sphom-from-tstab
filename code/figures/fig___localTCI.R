require(readr)
require(ggplot2)
require(dplyr)

zone_name <- 'vercelli_2018'
# zone_name <- 'beauce_2019'
# zone_name <- 'hyytiala_2018'
# zone_name <- 'fresno_2019'

dpath <- paste0('data/inter_data/from_GEE/testzones_', zone_name)


dat1 <- read_csv(file = paste0(dpath,'/MODIS_AQUA_NDVI_datablock.csv'), col_names = T, skip = 1)
dat2 <- read_csv(file = paste0(dpath,'/MODIS_TERRA_NDVI_datablock.csv'), col_names = T, skip = 1)


source('code/general/calc_TCI.R')

dir.create(path = paste0('data/inter_data/modis_test_zones/', zone_name), 
           recursive = T, showWarnings = F)

# read the data
dat <- bind_rows(
  dat1 %>% 
    dplyr::select(-X7, -time) %>% 
    rename(ID = `0`, date = 'id') %>% 
    mutate(platform = 'AQUA'),
  dat2 %>% 
    dplyr::select(-X7, -time) %>% 
    rename(ID = `0`, date = 'id') %>% 
    mutate(platform = 'TERRA')) %>%
  rename(lon = 'longitude', lat = 'latitude')  %>% 
  mutate(date = as.Date(date, '%Y_%m_%d')) %>%
  group_by(date, platform) %>%
  mutate(pixID = row_number())


# Quickplot to check if the data comes out right
ggplot(dat %>% filter(platform == 'TERRA', date == '2018-05-05')) +
  geom_point(aes(x = lon, y = lat, fill = NDVI),
             shape = 22, size = 4) +
  scale_fill_viridis_c()


# calc the metrics per time series
dat.sum <- dat %>% 
  filter(!is.na(NDVI)) %>% 
  group_by(pixID) %>%
  mutate(DOI = as.numeric(strftime(date, format = "%j")),
         DOI.hour = DOI + ifelse(platform == 'TERRA', 10.5/24, 13.5/24)) %>%
  group_by(pixID, lat, lon) %>%
  arrange(DOI.hour) %>%
  summarise(TCI = calc_TCI(NDVI, DOI.hour))



## FIGURE

require(raster)
require(ggplotify)
require(grid)
require(ggrepel)

col.1 <- '#1770DC'
col.2 <- '#DCA416'

S2_img <- brick(paste0(dpath,'/S2_L2A_image_', zone_name, '.tif'))
g.rgb <- as.grob(~plotRGB(S2_img, r = 3, g = 2, b = 1, scale = 10000,
                          stretch = "lin", margins = F))



df.grd <- dat %>% 
  filter(!is.na(NDVI)) %>%
  group_by(pixID) %>% 
  summarize(lat = mean(lat), lon = mean(lon))


iPixies <- data.frame(pixLbl = c('A','B','C','D'),
                      pixID = c(
                        which.max(dat.sum$TCI), 
                        1000, 1200,
                        # which.min(abs(dat.sum$TCI-quantile(dat.sum$TCI, 0.7))),
                        # which.min(abs(dat.sum$TCI-quantile(dat.sum$TCI, 0.3))),
                        which.min(dat.sum$TCI))) 

df.ts <- dat %>% 
  filter(!is.na(NDVI)) %>%
  inner_join(iPixies, by = 'pixID') %>% 
  left_join(dat.sum, by = c('lat', 'lon', 'pixID')) %>%
  mutate(pixLbl_long = paste('Time series', pixLbl, '|', 
                             'TCI =', round(TCI, digits = 3)))

df.pixList <- iPixies %>% 
  left_join(df.grd, by = 'pixID')

g.map.TCI <- ggplot(dat.sum) +
  geom_point(aes(x = lon, y = lat, fill = TCI),
             shape = 22, size = 3, colour = 'grey80') +
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
  coord_cartesian() +
  scale_fill_viridis_c('Temporal Coherence Index (TCI)', option = 'magma', limits = c(0,1)) +
  theme(legend.position = 'top',
        legend.key.width = unit(2, "cm"),
        panel.background = element_rect(fill = 'white'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = 0.5))


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



