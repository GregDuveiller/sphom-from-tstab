library(raster)
library(rgdal)

library(ggplot2)
library(scales)
library(grid)


CDL_path <- 'data/input_data/CDL'
TCI_path <- 'data/final_data/support4paper'

CDL_flist <- list.files(CDL_path, pattern = '.tif$', full.names = T)

r_CDL_conus <- stack(CDL_flist[2:13])  # Note: we avoid 2007 which has a diff res
names(r_CDL_conus) <- substr(x = names(r_CDL_conus), 1, 8)

# classes <- table(getValues(r)) # Tabulation of class counts
# colors <- r@legend@colortable  # Class colors
# plot(r, col=colors[1:length(classes)+1], colNA=colors[1], axes=FALSE, box=FALSE)

clsfrq <- as.data.frame(table(getValues(r_CDL_conus))) # Tabulation of class counts
colnames(clsfrq) <- c('classID', 'freq')

colors <- r_CDL_conus[[12]]@legend@colortable  # Class colors

txt <- read.delim(paste0(CDL_path, '/CDL_names_colors_codes/cdl_names.txt'),
                  header = F, blank.lines.skip = F)
names(txt) <- 'className'

col <- read.table(paste0(CDL_path, '/CDL_names_colors_codes/cdl_colors.txt'), 
                  blank.lines.skip = F)
names(col) <- 'colorCode'

lgd <- bind_cols(txt, col)
lgd$colors <- colors[1:255]
lgd$classID <- factor(as.numeric(rownames(lgd))-1)

lgd_sub <- clsfrq %>% left_join(lgd, by = 'classID')

r_TCI_wgs84 <- stack(paste0(TCI_path, '/TCI_NDVI_ts_California_WGS84.tif'))
r_TCI_wgs84 <- r_TCI_wgs84[[2:13]]


r_TCI_conus <- projectRaster(r_TCI_wgs84/10000, r_CDL_conus)


df_CDL <- as.data.frame(r_CDL_conus, xy = T, long = T)
names(df_CDL) <- c('x', 'y', 'year', 'CDL')
df_CDL$year <- as.numeric(substr(df_CDL$year, 5, 8))

df_TCI <- as.data.frame(r_TCI_conus, xy = T, long = T)
names(df_TCI) <- c('x', 'y', 'year', 'TCI')
df_TCI$year <- as.numeric(substr(df_TCI$year, 5, 8))
  


years2plot <- 2015:2019


g.TCI <- ggplot(df_TCI %>% filter(year %in% years2plot),  
       aes(x = x, y = y)) +
  geom_raster(aes(fill = TCI)) +
  facet_grid(.~year) + 
  scale_fill_viridis_c('Temporal Coherence Index (TCI)', 
                       option = 'magma', limits = c(0,1), oob = squish) +
  coord_equal(expand = F) +
  theme(legend.position = 'top',
        axis.title = element_blank(),
        axis.text = element_text(size = 6), 
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.key.width = unit(2.4, "cm")) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))


cols <- lgd_sub$colors
names(cols) <- lgd_sub$classID

g.CDL <- ggplot(df_CDL %>% filter(year %in% years2plot),  
       aes(x = x, y = y)) +
  geom_raster(aes(fill = factor(CDL))) +
  facet_grid(.~year) + 
  scale_fill_manual(values = cols) +
  coord_equal(expand = F) +
  theme(legend.position = 'none',
        axis.title = element_blank(),
        axis.text = element_text(size = 6), 
        axis.text.y = element_text(angle = 90, hjust = 0.5))





# printing the final plot -----
fig.name <- 'fig___yearlyTS'
fig.path <- paste0('figures/final_figures/')
dir.create(fig.path, showWarnings = F, recursive = T)
fig.width <- 12; fig.height <- 7;  fig.fmt <- 'png'
fig.fullfname <- paste0(fig.path, '/', fig.name, '.', fig.fmt)
if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}

w <- 1; h <- 0.55


print(g.TCI, vp = viewport(width = w, height = h, x = 0, y = 1-h, just = c(0,0)))
print(g.CDL, vp = viewport(width = w, height = 1-h, x = 0, y = 0, just = c(0,0)))

dev.off()