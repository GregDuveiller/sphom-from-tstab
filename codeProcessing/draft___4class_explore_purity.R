require(ggplot2)
require(grid)
require(dplyr)

load("~/Work/Workspace/spHomogeneity/dataProcessing/batch_002/metrics-TS1-TS8-TS3-TS9___PSF-AQUA-48-20.Rda")

# function for 'plots'
mk.plot <- function(df, y, x, z, ylim = NULL, xlim = NULL){
  g <- ggplot(df, aes_string(y = y, x = x)) +
    geom_point(shape = 3, aes_string(colour = z)) +
    geom_smooth(method = 'loess', formula = 'y ~ x', colour = col.3) +
    scale_colour_viridis_c(option = 'A') +
    theme(legend.position = 'top') +
    guides(colour = guide_colourbar(title.position = 'top',
                                  title.hjust = 0.5,
                                  barwidth = 18,
                                  barheight = 1))
  
  if(!is.null(ylim) | !is.null(xlim)){
    g <- g + coord_cartesian(ylim = ylim, xlim = xlim)
  }
  
  return(g)
}


# function for 'maps'
mk.map <- function(df, fill, vir_opt = "D", zlim = NULL){
  g <- ggplot(df, aes(y = y, x = x)) +
    geom_raster(aes_string(fill = fill)) + 
    scale_fill_viridis_c(option = vir_opt, limits = zlim, oob = squish) +
    coord_equal(expand = F) + 
    theme(legend.position = 'top', 
          legend.margin = margin(0, 0, 0, 0),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    guides(fill = guide_colourbar(title.position = 'top',
                                  title.hjust = 0.5,
                                  barwidth = 10,
                                  barheight = 1))
  return(g)
}


df.sum <- df.sum %>% 
  rowwise() %>% 
  mutate(pur_avg_dom = max(pur_avg_TS1, pur_avg_TS8, pur_avg_TS3, pur_avg_TS9))



g.map.pur_avg_TS1 <- mk.map(df = df.sum, fill = 'pur_avg_TS1', vir_opt = "D", zlim = NULL)
g.map.pur_avg_TS3 <- mk.map(df = df.sum, fill = 'pur_avg_TS3', vir_opt = "D", zlim = NULL)
g.map.pur_avg_TS8 <- mk.map(df = df.sum, fill = 'pur_avg_TS8', vir_opt = "D", zlim = NULL)
g.map.pur_avg_TS9 <- mk.map(df = df.sum, fill = 'pur_avg_TS9', vir_opt = "D", zlim = NULL)

g.map.pur_std_TS1 <- mk.map(df = df.sum, fill = 'pur_std_TS1', vir_opt = "D", zlim = NULL)
g.map.pur_std_TS3 <- mk.map(df = df.sum, fill = 'pur_std_TS3', vir_opt = "D", zlim = NULL)
g.map.pur_std_TS8 <- mk.map(df = df.sum, fill = 'pur_std_TS8', vir_opt = "D", zlim = NULL)
g.map.pur_std_TS9 <- mk.map(df = df.sum, fill = 'pur_std_TS9', vir_opt = "D", zlim = NULL)


g.plot.TS1 <- mk.plot(df = df.sum, x = 'pur_avg_TS1', y = 'pur_std_TS1',   xlim = c(0,1),
        z = 'log(pur_avg_TS1/pur_std_TS1)')
g.plot.TS3 <- mk.plot(df = df.sum, x = 'pur_avg_TS3', y = 'pur_std_TS3',   xlim = c(0,1),
        z = 'log(pur_avg_TS3/pur_std_TS3)')
g.plot.TS8 <- mk.plot(df = df.sum, x = 'pur_avg_TS8', y = 'pur_std_TS8',   xlim = c(0,1),
        z = 'log(pur_avg_TS8/pur_std_TS8)')
g.plot.TS9 <- mk.plot(df = df.sum, x = 'pur_avg_TS9', y = 'pur_std_TS9',   xlim = c(0,1),
        z = 'log(pur_avg_TS9/pur_std_TS9)')



fig.name <- paste0('xplrfig___', '4class_purity_overview')
fig.path <- paste0('dataProcessing/', batch_name, '/figs')
dir.create(fig.path, showWarnings = F, recursive = T)
fig.width <- 16; fig.height <- 12;  fig.fmt <- 'png'
fig.fullfname <- paste0(fig.path, '/', fig.name, '.', fig.fmt)
if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}

w <- 0.25; h <- 0.33

print(g.map.pur_avg_TS1, vp = viewport(width = w, height = h, x = 0*w, y = 2*h, just = c(0,0)))
print(g.map.pur_std_TS1, vp = viewport(width = w, height = h, x = 0*w, y = 1*h, just = c(0,0)))
print(g.plot.TS1,        vp = viewport(width = w, height = h, x = 0*w, y = 0*h, just = c(0,0)))

print(g.map.pur_avg_TS3, vp = viewport(width = w, height = h, x = 1*w, y = 2*h, just = c(0,0)))
print(g.map.pur_std_TS3, vp = viewport(width = w, height = h, x = 1*w, y = 1*h, just = c(0,0)))
print(g.plot.TS3,        vp = viewport(width = w, height = h, x = 1*w, y = 0*h, just = c(0,0)))

print(g.map.pur_avg_TS8, vp = viewport(width = w, height = h, x = 2*w, y = 2*h, just = c(0,0)))
print(g.map.pur_std_TS8, vp = viewport(width = w, height = h, x = 2*w, y = 1*h, just = c(0,0)))
print(g.plot.TS8,        vp = viewport(width = w, height = h, x = 2*w, y = 0*h, just = c(0,0)))

print(g.map.pur_avg_TS9, vp = viewport(width = w, height = h, x = 3*w, y = 2*h, just = c(0,0)))
print(g.map.pur_std_TS9, vp = viewport(width = w, height = h, x = 3*w, y = 1*h, just = c(0,0)))
print(g.plot.TS9,        vp = viewport(width = w, height = h, x = 3*w, y = 0*h, just = c(0,0)))

dev.off()


