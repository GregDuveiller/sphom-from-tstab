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


g.TCI <- ggplot(df_TCI,  aes(x = x, y = y)) +
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
fig.width <- 12; fig.height <- 7
fig.fullfname <- paste0(fig.path, '/', fig.name, '.', fig.fmt)
if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}

w <- 1; h <- 0.55

print(g.TCI, vp = viewport(width = w, height = h, x = 0, y = 1-h, just = c(0,0)))
print(g.CDL, vp = viewport(width = w, height = 1-h, x = 0, y = 0, just = c(0,0)))

dev.off()