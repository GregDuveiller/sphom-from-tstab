require(readr)
require(dplyr)

dat1 <- read_csv(file = '../../../Google Drive/spHomogeneity/NDVI_AQUA.csv', col_names = T, skip = 1)
dat2 <- read_csv(file = '../../../Google Drive/spHomogeneity/NDVI_TERRA.csv', col_names = T, skip = 1)

source('codeProcessing/smoothness_dblediff_entropy.R')

zone_name <- 'testzone_001'

dir.create(path = paste0('dataProcessing/', zone_name), recursive = T)

dat <- bind_rows(
  dat1 %>% select(-X7, -time) %>% rename(ID = `0`, date = 'id') %>% mutate(platform = 'AQUA'),
  dat2 %>% select(-X7, -time) %>% rename(ID = `0`, date = 'id') %>% mutate(platform = 'TERRA'),
) %>%
  rename(lon = 'longitude', lat = 'latitude')  %>% 
  mutate(date = as.Date(date, '%Y_%m_%d')) %>%
  group_by(date, platform) %>%
  mutate(pixID = row_number())


dum <- dat %>% filter(date == '2015-07-05')

ggplot(dum %>% filter(platform == 'AQUA')) +
  geom_point(aes(x = lon, y = lat, fill = NDVI), 
             shape = 22, size = 4) + 
  scale_fill_viridis_c() 

dum <- dat %>% filter(pixID == 175)

ggplot(dum) +
  geom_point(aes(x = date, y = NDVI, color = platform, shape = platform), 
             size = 2) + 
  scale_fill_viridis_c()


dat.sum <- dat %>% 
  filter(!is.na(NDVI)) %>% 
  group_by(pixID) %>%
  mutate(DOI = as.numeric(strftime(date, format = "%j")),
         DOI.hour = DOI + ifelse(platform == 'TERRA', 10.5/24, 13.5/24)) %>%
  group_by(pixID, lat, lon) %>%
  arrange(DOI.hour) %>%
  summarise(TCE = smoothness_dblediff_entropy(NDVI, DOI.hour,      
                                              mode = 'entropy', bin_width = 0.02, 
                                              bin_range = c(-1,1)),
            TCI = smoothness_dblediff_entropy(NDVI, DOI.hour,      
                                              mode = 'scaled', bin_width = 0.02, 
                                              bin_range = c(-1,1)))


iPix <- 340

df.ts <- dat %>% 
  filter(!is.na(NDVI)) %>%    #  for some reason, sometimes we have some
  filter(pixID == iPix) 

g.map.TCI <- ggplot(dat.sum) +
  geom_point(aes(x = lon, y = lat, fill = TCI), 
             shape = 22, size = 4) + 
  geom_point(data = df.ts, aes(x = lon, y = lat), 
             shape = 21, fill = 'cyan', colour = 'white') +
  scale_fill_viridis_c(option = 'B')

g.ts <- ggplot(df.ts) +
  geom_point(aes(x = date, y = NDVI, colour = platform, shape = platform)) +
  scale_colour_discrete(guide = F) +
  scale_shape_discrete(guide = F) +
  scale_x_date('') +
  ggtitle(paste('Point', iPix, '|', zone_name, '|', 'TCI =', 
                round((dat.sum %>% filter(pixID == iPix))$TCI, digits = 4)))


w <- 1; h <- 0.7
print(g.map.TCI, vp = viewport(width = w, height = h, x = 0*w, y = 1-h, just = c(0,0)))
print(g.ts,      vp = viewport(width = w, height = 1-h, x = 0*w, y = 0, just = c(0,0)))





### extract some sample pixels... (used for other purposes)

iPix <- 967

ts <- dat %>% 
  filter(!is.na(NDVI)) %>%    #  for some reason, sometimes we have some
  filter(pixID == iPix) %>%
  mutate(DOI = as.numeric(strftime(date, format = "%j")),
         DOI.hour = DOI + ifelse(platform == 'TERRA', 10.5/24, 13.5/24)) %>%
  arrange(DOI.hour) %>% ungroup() 

save(ts, file = paste0('dataProcessing/', zone_name, '/ts_', iPix, '.Rda'))


iPix <- 954

ts <- dat %>% 
  filter(!is.na(NDVI)) %>%    #  for some reason, sometimes we have some
  filter(pixID == iPix) %>%
  mutate(DOI = as.numeric(strftime(date, format = "%j")),
         DOI.hour = DOI + ifelse(platform == 'TERRA', 10.5/24, 13.5/24)) %>%
  arrange(DOI.hour) %>% ungroup() 

save(ts, file = paste0('dataProcessing/', zone_name, '/ts_', iPix, '.Rda'))



# iPix <- 175
# iPix <- 587
# iPix <- 1287





