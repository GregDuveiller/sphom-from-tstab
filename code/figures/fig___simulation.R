#!/usr/local/bin/Rscript
################################################################################
# Project:  spHomogeneity
# Purpose:  make figure resuming the simulation exercise
# License:  GPL v3
# Authors:  Gregory Duveiller - Jan 2021
################################################################################


require(ggplot2)
require(ggrepel)
require(dplyr)
require(tidyr)
require(scales)
require(grid)
require(here)


# default settings for printing
if(!exists('fig.fmt')){fig.fmt = 'png'}
if(!exists('fig.path')){fig.path = 'docs/figures'}
dir.create(path = fig.path, recursive = T, showWarnings = F)


# set-up for choice of the dataset to print
batch_name <- 'simulation_final'
TS2LC <- c('LC1'='TS1', 'LC2'='TS8', 'LC3'='TS3', 'LC4'='TS6')
temp_subsample <- 4

# load target dataset
load(paste0('data/inter_data/', batch_name, 
            '/datablock-combined-', paste(TS2LC, collapse = '-'),
            '___tsub-', temp_subsample, '.Rda'))  # 'df.all' & 'df.grd'


load(paste0('data/inter_data/', batch_name, 
            '/metrics-', paste(TS2LC, collapse = '-'),
            '.Rda')) # df.sum

load(paste0('data/inter_data/', batch_name, '/df-ideal-ts.Rda'))  # df.ideal.ts

# rename columns and calc purity indices
df.sum <- df.sum %>% 
  mutate(PI1 = pur_avg, PI2 = pur_std, PI3 = log(pur_avg/pur_std)) %>%
  dplyr::select(-pur_avg, -pur_std)

# setting up some colours
col.0 <- 'grey20'
require(wesanderson)
cols <- wes_palette(name = 'Darjeeling1', n = 4)
cols <- wes_palette(name = 'Chevalier1', n = 4)

# require(RColorBrewer)
# cols <- rev(brewer.pal(n = 4, name = 'Paired'))
col.vals <- c('1' = cols[1], '2' = cols[2],
              '3' = cols[3], '4' = cols[4])
col.1 <- '#1770DC'
col.2 <- '#DCA416'

# defining which time series to show...

qtls <- df.sum %>% 
  group_by(original_id) %>% 
  summarize(q90 = quantile(TCI, 0.95, na.rm = T),
            q50 = quantile(TCI, 0.50 , na.rm = T),
            q10 = quantile(TCI, 0.05 , na.rm = T),
            c90 = grd_id[order(abs(TCI - q90))[1]],
            c50 = grd_id[order(abs(TCI - q50))[1]],
            c10 = grd_id[order(abs(TCI - q10))[1]])

main_ts_id <- 1

pix2plot <- as.numeric(filter(qtls, original_id == main_ts_id)[c('c90', 'c50', 'c10')])

df.pixList <- data.frame(pixLbl = c('A','B','C'), grd_id = pix2plot) %>% 
  left_join(df.sum, by = 'grd_id')





# Landscape 'phenology'
df.ts <- df.ideal.ts %>%
  dplyr::select(DOI, all_of(TS2LC)) %>%
  pivot_longer(cols = names(TS2LC), names_to = 'LC', values_to = 'NDVI') %>%
  mutate(original_id = substr(LC, start = 3, stop = 4))

g.pheno <- ggplot(df.ts, aes(x = DOI, y = NDVI)) + 
  geom_line(aes(colour = original_id), size = 1.2) +
  scale_colour_manual(values = col.vals, guide = 'none') + 
  scale_y_continuous('NDVI') +
  scale_x_continuous('Day of the year (DOY)') +
  ggtitle('Land cover phenology') +
  theme(          panel.grid = element_line(linetype = 'dotted', colour = 'grey50'),
                  panel.grid.minor = element_blank(),
                  panel.background = element_rect(fill = 'white', colour = 'grey10'))


# Landscape spatial patterns
map.class <- ggplot(df.grd) +
  geom_raster(aes(y = y, x = x, fill = factor(original_id))) + 
  scale_fill_manual(values = col.vals, guide = 'none') +
  coord_equal(expand = F) +
  theme(legend.position = 'none', 
        legend.margin = margin(0, 0, 0, 0),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) + 
  ggtitle('Dominant land cover') +  
  geom_label_repel(data = df.pixList, aes(x = x, y = y, label = pixLbl),
                   size = 4, fontface = 'bold', colour = 'black') + 
  geom_point(data = df.pixList, aes(x = x, y = y),
             colour = 'black', shape = 3, size = 4) 




# function for 'time series'
mk.ts <- function(df, df.pixList, pixLabel, ylim = NULL, xlim = NULL, lgd.pos = 'none'){
  
  df.pix <- df.pixList %>% filter(pixLbl == pixLabel)
  
  g <- ggplot(df %>% filter(grd_id == df.pix$grd_id), 
              aes(x = DOI.time, y = NDVI)) +
    geom_point(aes(x = DOI.time, y = NDVI, colour = platform, shape = platform), size = 3) +
    scale_x_continuous('Day of the year (DOY)') +
    scale_colour_manual('Satellite platform:',
                        values = c('AQUA' = col.1, 'TERRA' = col.2)) +
    scale_shape_discrete('Satellite platform:') +
    theme(legend.position = lgd.pos,
          legend.background = element_rect(fill = 'white', colour = 'grey50'),
          panel.grid = element_line(linetype = 'dotted', colour = 'grey50'),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'white', colour = 'grey10')) +
    ggtitle(paste0('Time series at grid cell ', pixLabel, ' | TCI = ', round(df.pix$TCI, digits = 3)))
  
  if(!is.null(ylim) | !is.null(xlim)){
    g <- g + coord_cartesian(ylim = ylim, xlim = xlim)
  }
  return(g) 
}

g.exptA <- mk.ts(df.all, df.pixList, pixLabel = 'A', ylim = c(0, 0.8), lgd.pos = c(0.85, 0.75))
g.exptB <- mk.ts(df.all, df.pixList, pixLabel = 'B', ylim = c(0, 0.8))
g.exptC <- mk.ts(df.all, df.pixList, pixLabel = 'C', ylim = c(0, 0.8))


# function for 'plots'
mk.plot <- function(df, y, x, ylim = NULL, xlim = NULL, pts2add = NULL){
  
  g <- ggplot(df, aes_string(y = y, x = x)) +
    geom_point(shape = 3, aes(colour = factor(original_id))) +
    #geom_smooth(method = 'loess', formula = 'y ~ x', colour = col.0) + 
    scale_colour_manual(values = col.vals, guide = 'none')+
    ggtitle(paste(x, 'vs.', y)) +
    theme(
      panel.grid = element_line(linetype = 'dotted', colour = 'grey50'),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = 'white', colour = 'grey10'))
  

  if(!is.null(pts2add)){
    g <- g +  
      geom_label_repel(data = pts2add, aes_string(x = x, y = y, label = 'pixLbl'),
                       size = 4, fontface = 'bold', colour = 'black') + 
      geom_point(data = pts2add, aes_string(x = x, y = y),
                 colour = 'black', shape = 3, size = 4) 
  }

  if(!is.null(ylim) | !is.null(xlim)){
    g <- g + coord_cartesian(ylim = ylim, xlim = xlim)
  }
  
  return(g)
}

plt.mxpur <- mk.plot(df.sum, y = 'PI1', x = 'TCI', ylim = c(0.4, 1),  pts2add = df.pixList)
plt.sdpur <- mk.plot(df.sum, y = 'PI2', x = 'TCI', ylim = c(0, 0.25), pts2add = df.pixList)
plt.purLN <- mk.plot(df.sum, y = 'PI3', x = 'TCI', ylim = c(1, 3.55), pts2add = df.pixList)




# function for 'maps'
mk.map <- function(df, fill, vir_opt = "D", zlim = NULL, pts2add = NULL){
  g <- ggplot(df, aes(y = y, x = x)) +
    geom_raster(aes_string(fill = fill)) + 
    scale_fill_viridis_c(option = vir_opt, limits = zlim, oob = squish) +
    coord_equal(expand = F) + 
    ggtitle(paste('Map of', fill)) + 
    theme(legend.position = 'right', 
          legend.margin = margin(0, 0, 0, 0),
          legend.title = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    guides(fill = guide_colourbar(title.position = 'right',
                                  title.hjust = 0.5,
                                  barwidth = 1,
                                  barheight = 10))
  
  if(!is.null(pts2add)){
    g <- g +  
      geom_label_repel(data = pts2add, aes(x = x, y = y, label = pixLbl),
                 size = 4, fontface = 'bold', colour = 'black') + 
      geom_point(data = pts2add, aes(x = x, y = y),
                 colour = 'black', shape = 3, size = 4) 
  }
  
  return(g)
}

map.TCI <- mk.map(df.sum, 'TCI', vir_opt = "A", zlim = c(0, 1), pts2add = df.pixList)
map.mxpur <- mk.map(df.sum, 'PI1', vir_opt = "D", zlim = c(0, 1), pts2add = df.pixList)
map.sdpur <- mk.map(df.sum, 'PI2', vir_opt = "C", zlim = c(0, 0.25), pts2add = df.pixList)
map.purLN <- mk.map(df.sum, 'PI3', vir_opt = "B", zlim = c(1, 3.55), pts2add = df.pixList)








# printing the final plot -----
fig.name <- 'fig___simulation'
fig.width <- 16; fig.height <- 12
fig.fullfname <- paste0(fig.path, '/', fig.name, '.', fig.fmt)
if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}

w <- 0.25; h <- 0.25


print(g.pheno, vp = viewport(width = 2*w, height = h, x = 2*w, y = 3*h, just = c(0,0)))
print(g.exptA, vp = viewport(width = 2*w, height = h, x = 2*w, y = 2*h, just = c(0,0)))
print(g.exptB, vp = viewport(width = 2*w, height = h, x = 2*w, y = 1*h, just = c(0,0)))
print(g.exptC, vp = viewport(width = 2*w, height = h, x = 2*w, y = 0*h, just = c(0,0)))

print(map.TCI,   vp = viewport(width = w, height = h, x = 0*w, y = 3*h, just = c(0,0)))
print(map.mxpur, vp = viewport(width = w, height = h, x = 0*w, y = 2*h, just = c(0,0)))
print(map.sdpur, vp = viewport(width = w, height = h, x = 0*w, y = 1*h, just = c(0,0)))
print(map.purLN, vp = viewport(width = w, height = h, x = 0*w, y = 0*h, just = c(0,0)))

print(map.class, vp = viewport(width = w, height = h, x = 1*w, y = 3*h, just = c(0,0)))
print(plt.mxpur, vp = viewport(width = w, height = h, x = 1*w, y = 2*h, just = c(0,0)))
print(plt.sdpur, vp = viewport(width = w, height = h, x = 1*w, y = 1*h, just = c(0,0)))
print(plt.purLN, vp = viewport(width = w, height = h, x = 1*w, y = 0*h, just = c(0,0)))


grid.text(expression(bold("a")), x = unit(0.01, "npc"), y = unit(0.98, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("d")), x = unit(0.01, "npc"), y = unit(0.73, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("g")), x = unit(0.01, "npc"), y = unit(0.49, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("j")), x = unit(0.01, "npc"), y = unit(0.23, "npc"), gp = gpar(fontsize = 18))


grid.text(expression(bold("b")), x = unit(0.26, "npc"), y = unit(0.98, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("e")), x = unit(0.26, "npc"), y = unit(0.73, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("h")), x = unit(0.26, "npc"), y = unit(0.49, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("k")), x = unit(0.26, "npc"), y = unit(0.23, "npc"), gp = gpar(fontsize = 18))


grid.text(expression(bold("c")), x = unit(0.51, "npc"), y = unit(0.98, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("f")), x = unit(0.51, "npc"), y = unit(0.73, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("i")), x = unit(0.51, "npc"), y = unit(0.49, "npc"), gp = gpar(fontsize = 18))
grid.text(expression(bold("l")), x = unit(0.51, "npc"), y = unit(0.23, "npc"), gp = gpar(fontsize = 18))

dev.off()
  
