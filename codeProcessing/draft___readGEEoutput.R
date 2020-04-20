require(readr)
require(dplyr)

dat1 <- read_csv(file = '../../../Google Drive/spHomogeneity/NDVI_AQUA.csv', col_names = T, skip = 1)
dat2 <- read_csv(file = '../../../Google Drive/spHomogeneity/NDVI_TERRA.csv', col_names = T, skip = 1)


dat <- bind_rows(
  dat1 %>% select(-X7, -time) %>% rename(ID = `0`, date = 'id') %>% mutate(platform = 'AQUA'),
  dat2 %>% select(-X7, -time) %>% rename(ID = `0`, date = 'id') %>% mutate(platform = 'TERRA'),
  ) %>%
  rename(lon = 'longitude', lat = 'latitude')  %>% 
  mutate(date = as.Date(date, '%Y_%m_%d')) %>%
  group_by(date, platform) %>%
  mutate(pixID = row_number())

dum <- dat %>% filter(date == '2015-07-04')

ggplot(dum) +
  geom_point(aes(x = lon, y = lat, fill = NDVI), 
             shape = 22, size = 3) + 
  scale_fill_viridis_c() + 
  facet_wrap(~platform)

dum <- dat %>% filter(pixID == 175)

ggplot(dum) +
  geom_point(aes(x = date, y = NDVI, color = platform), 
             size = 3) + 
  scale_fill_viridis_c()



dat.smooth <- dat %>% 
  filter(!is.na(NDVI)) %>%    #  for some reason, sometimes we have some
  # group_by(pixID) %>%
  filter(pixID == 1) %>%
  mutate(DOI = as.numeric(strftime(date, format = "%j")),
         NDVI.smo = smooth.spline(x = DOI, y = NDVI, df = 8)$y,
         NDVI.res = NDVI - NDVI.smo,
         p.res.01 = cut(NDVI.res, breaks = seq(-2,2,0.005)))
