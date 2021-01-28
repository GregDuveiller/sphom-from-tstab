#!/usr/local/bin/Rscript
################################################################################
# Project:  spHomogeneity
# Purpose:  make figure of the time series of changes in California ag. zone
# License:  GPL v3
# Authors:  Gregory Duveiller - Jan 2021
################################################################################


require(dplyr)
require(ggplot2)
require(scales)
require(grid)


# default settings for printing
if(!exists('fig.fmt')){fig.fmt = 'png'}
if(!exists('fig.path')){fig.path = 'docs/figures'}
dir.create(path = fig.path, recursive = T, showWarnings = F)


# load data
load('data/final_data/data4figures/df_TCI.RData')
load('data/final_data/data4figures/df_CDL.RData')


# set-up colorbar if needed
TCI.range <- c(0.2, 0.8)

# ggplot pf the TCI for the time series
g.TCI <- ggplot(df_TCI,  aes(x = x, y = y)) +
  geom_raster(aes(fill = TCI)) +
  facet_grid(.~year) + 
  scale_fill_viridis_c('Temporal Coherence Index (TCI)', 
                       option = 'magma', limits = TCI.range, oob = squish) +
  coord_equal(expand = F) +
  theme(legend.position = 'top',
        axis.title = element_blank(),
        axis.text = element_text(size = 6), 
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.key.width = unit(2.4, "cm")) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))

# get colour names
cols <- lgd_sub$colors
names(cols) <- lgd_sub$className

# get top 15 most frequent classes
freq <- lgd_sub$freq
names(freq) <- lgd_sub$className
most_freq <- names(tail(sort(freq), 15))

# add this info to the CDL dataframe
df_CDL <- df_CDL %>%
  mutate(classID = factor(CDL)) %>% 
  left_join(lgd_sub %>% dplyr::select(classID, className), by = 'classID')

# ggplot plot of CDL 
g.CDL <- ggplot(df_CDL, aes(x = x, y = y)) +
  geom_raster(aes(fill = className)) +
  facet_grid(.~year) + 
  scale_fill_manual('CDL class: ', breaks = most_freq, values = cols) +
  coord_equal(expand = F) +
  theme(legend.position = 'bottom',
        axis.title = element_blank(),
        axis.text = element_text(size = 6), 
        axis.text.y = element_text(angle = 90, hjust = 0.5))



# printing the final plot -----
fig.name <- 'fig___yearlyTS'
fig.width <- 12; fig.height <- 8
fig.fullfname <- paste0(fig.path, '/', fig.name, '.', fig.fmt)
if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}

w <- 1; h <- 0.45

print(g.TCI, vp = viewport(width = w, height = h, x = 0, y = 1-h, just = c(0,0)))
print(g.CDL, vp = viewport(width = w, height = 1-h, x = 0, y = 0, just = c(0,0)))

dev.off()