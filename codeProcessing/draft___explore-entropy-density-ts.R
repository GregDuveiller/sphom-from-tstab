# script to explore the scaling possibilities of the entropy smoothness metric

library(ggplot2)
library(grid)
library(dplyr)
library(wesanderson)


## prepare various time series

set.seed(48)

# TS1: real ts based on a fairly smooth curve
load('dataProcessing/testzone_001/ts_967.Rda')  # ts
ts1 <- ts %>% select(DOI.hour, NDVI)

# TS2: less obs
ts2 <- ts1[sort(sample.int(dim(ts1)[1], size = dim(ts1)[1]/2)),] 

# TS3: 
ts3 <- ts2[sort(sample.int(dim(ts2)[1], size = dim(ts2)[1]/2)),] 

# TS4: 
ts4 <- ts3[sort(sample.int(dim(ts3)[1], size = dim(ts3)[1]/2)),] 



## calculate metrics 

source('codeProcessing/smoothness_dblediff_entropy.R')

ts_seq <- c('ts1', 'ts2', 'ts3', 'ts4')
bin_range_seq <- c(0.2, 0.5, 1, 2)
bin_width_seq <- seq(0.0001, 0.1, 0.0001)

df.metrics <- data.frame(NULL)

for(i in ts_seq){
  eval(parse(text = paste0('ts <- ',i)))
  for(j in bin_range_seq){
    for(k in bin_width_seq){
      df.row <- data.frame(
        ts = i, bin_range = j, bin_width = k,
        entropy = smooth_dbldif_entropy(ts$NDVI, ts$DOI.hour, 
                                              mode = 'entropy', bin_width = k, 
                                              bin_range = c(-j,j)),
        scl_ent = smooth_dbldif_entropy(ts$NDVI, ts$DOI.hour, 
                                              mode = 'scaled', bin_width = k, 
                                              bin_range = c(-j,j))) 
      df.metrics <- bind_rows(df.metrics, df.row)
    }
  }
}


## make plots

# colours
cols <- as.vector(wes_palette('Darjeeling1', n = length(ts_seq)))
names(cols) <- ts_seq

df.ts <- bind_rows(
  bind_cols(ts1 %>% mutate(ts = 'ts1')),
  bind_cols(ts2 %>% mutate(ts = 'ts2')),
  bind_cols(ts3 %>% mutate(ts = 'ts3')),
  bind_cols(ts4 %>% mutate(ts = 'ts4')))

g.tseries <- ggplot(df.ts) +
  geom_point(aes(x = DOI.hour, y = NDVI, colour = ts)) +
  scale_colour_manual(values = cols, guide = F) +
  facet_grid(factor(ts, levels = ts_seq) ~.)

g.entropy <- ggplot(df.metrics) +
  geom_line(aes(x = bin_width, y = entropy, colour = ts)) +
  facet_grid(bin_range~., labeller = label_both) +
  scale_colour_manual(values = cols, guide = F) +
  scale_y_continuous('Entropy of double diff residues')

g.scl_ent <- ggplot(df.metrics) +
  geom_line(aes(x = bin_width, y = scl_ent, colour = ts)) +
  facet_grid(bin_range~., labeller = label_both) +
  scale_colour_manual(values = cols, guide = F) +
  scale_y_continuous('Scaled index based on entropy', limits = c(0,1))

fig.name <- paste0('xplrfig__', zone_name, '__density-ts')
fig.path <- paste0('dataProcessing/', zone_name, '/figs')
dir.create(fig.path, showWarnings = F, recursive = T)
fig.width <- 12; fig.height <- 9;  fig.fmt <- 'png'
fig.fullfname <- paste0(fig.path, '/', fig.name, '.', fig.fmt)
if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}

w <- 1/3; h <- 1
print(g.tseries, vp = viewport(width = w, height = h, x = 0*w, y = 0, just = c(0,0)))
print(g.entropy, vp = viewport(width = w, height = h, x = 1*w, y = 0, just = c(0,0)))
print(g.scl_ent, vp = viewport(width = w, height = h, x = 2*w, y = 0, just = c(0,0)))

dev.off()





