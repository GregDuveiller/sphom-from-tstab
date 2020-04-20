# initializing the script -----
require(ggplot2)
require(grid)
require(dplyr)

batch_name <- 'batch_002'
psf_fname <- 'PSF-AQUA-48-20'

refval <- 'mad'

# load the necessary data
load(paste0('dataProcessing/', batch_name, '/metrics-', 
            paste(TS2LC, collapse = '-'), '___', psf_fname, '.Rda'))  # df.sum


# setting up some colours
col.1 <- '#1770DC'
col.2 <- '#DCA416'
col.3 <- 'grey20'
col.4 <- 'grey60'

# function for 'pheno'
mk.pheno <- function(TS_ref, TS2LC, ylim = NULL, xlim = NULL){
  
  df.ts <- df.ideal.ts %>%
    dplyr::select(DOI, all_of(TS2LC)) %>%
    pivot_longer(cols = names(TS2LC), names_to = 'LC', values_to = 'NDVI')
  
  g <- ggplot(df.ts, aes(x = DOI, y = NDVI)) + 
    geom_line(aes(group = LC), colour = col.2) +
    geom_line(data = df.ts %>% dplyr::filter(LC == names(which(TS2LC == TS_ref))), 
              colour = col.1) + 
    scale_y_continuous('NDVI') + scale_x_continuous('Day of the year (DOI)')
  
  if(!is.null(ylim) | !is.null(xlim)){
    g <- g + coord_cartesian(ylim = ylim, xlim = xlim)
  }
  return(g) 
}

# function for 'maps'
mk.map <- function(df, fill, vir_opt = "D", vir_dir = 1, zlim = NULL){
  g <- ggplot(df, aes(y = y, x = x)) +
    geom_raster(aes_string(fill = fill)) + 
    scale_fill_viridis_c(option = vir_opt, limits = zlim, 
                         direction = vir_dir, oob = squish) +
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
# mk.map(fill = '1/var_res', vir_opt = "B")

# function for 'plots'
mk.plot <- function(df, y, x, z = NULL, ylim = NULL, xlim = NULL, zlim = NULL){
  g <- ggplot(df, aes_string(y = y, x = x)) +
    geom_point(shape = 3, aes_string(colour = z)) +
    geom_smooth(method = 'loess', formula = 'y ~ x', colour = col.3) + 
    scale_colour_viridis_c(option = 'D', limits = zlim, oob = squish, 
                           guide = 'none')
  
  if(!is.null(ylim) | !is.null(xlim)){
    g <- g + coord_cartesian(ylim = ylim, xlim = xlim)
  }
  
  return(g)
}
# mk.plot(y = 'pur_avg', x = 'var_sig')




# figure 1: MAD-PURE-PHENO -----


# combine by 3 for ease of use
mk.all3.fig1 <- function(TS_ref){
  
  g.all <- list()
  g.all$phenoT <- mk.pheno(TS_ref = TS_ref, TS2LC = TS2LC, ylim = c(0,1))
  g.all$purMap <- mk.map(df = df.sum, vir_opt = 'D', 
                         fill = paste0('pur_avg_', TS_ref), zlim = c(0,1))
  g.all$madMap <- mk.map(df = df.sum, vir_opt = 'B', vir_dir = -1,
                         fill = paste0(refval, '_', TS_ref), zlim = c(0,0.5))
  
  return(g.all)
}


g.LC1 <- mk.all3.fig1(TS_ref = TS2LC[1])
g.LC2 <- mk.all3.fig1(TS_ref = TS2LC[2])
g.LC3 <- mk.all3.fig1(TS_ref = TS2LC[3])
g.LC4 <- mk.all3.fig1(TS_ref = TS2LC[4])



fig.name <- paste0('xplrfig___4TS_', refval)
fig.path <- paste0('dataProcessing/', batch_name, '/figs')
dir.create(fig.path, showWarnings = F, recursive = T)
fig.width <- 16; fig.height <- 12;  fig.fmt <- 'png'
fig.fullfname <- paste0(fig.path, '/', fig.name, '.', fig.fmt)
if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}

w <- 0.25; h <- 0.33

print(g.LC1$phenoT, vp = viewport(width = w, height = h, x = 0*w, y = 2*h, just = c(0,0)))
print(g.LC1$purMap, vp = viewport(width = w, height = h, x = 0*w, y = 1*h, just = c(0,0)))
print(g.LC1$madMap, vp = viewport(width = w, height = h, x = 0*w, y = 0*h, just = c(0,0)))

print(g.LC2$phenoT, vp = viewport(width = w, height = h, x = 1*w, y = 2*h, just = c(0,0)))
print(g.LC2$purMap, vp = viewport(width = w, height = h, x = 1*w, y = 1*h, just = c(0,0)))
print(g.LC2$madMap, vp = viewport(width = w, height = h, x = 1*w, y = 0*h, just = c(0,0)))

print(g.LC3$phenoT, vp = viewport(width = w, height = h, x = 2*w, y = 2*h, just = c(0,0)))
print(g.LC3$purMap, vp = viewport(width = w, height = h, x = 2*w, y = 1*h, just = c(0,0)))
print(g.LC3$madMap, vp = viewport(width = w, height = h, x = 2*w, y = 0*h, just = c(0,0)))

print(g.LC4$phenoT, vp = viewport(width = w, height = h, x = 3*w, y = 2*h, just = c(0,0)))
print(g.LC4$purMap, vp = viewport(width = w, height = h, x = 3*w, y = 1*h, just = c(0,0)))
print(g.LC4$madMap, vp = viewport(width = w, height = h, x = 3*w, y = 0*h, just = c(0,0)))

dev.off()



# figure 2: MAD-vs-Metrics -----



# combine by 3 for ease of use
mk.all3.fig2 <- function(TS_ref){
  
  g.all <- list()
  g.all$metrc1 <-  mk.plot(df = df.sum, y = 'entropy', 
                           x = paste0('mad_', TS_ref), xlim = c(0,0.5),
                           z = paste0('pur_avg_', TS_ref), zlim = c(0,1))
  g.all$metrc2 <-  mk.plot(df = df.sum, y = '1/var_res', 
                           x = paste0('mad_', TS_ref), xlim = c(0,0.5),
                           z = paste0('pur_avg_', TS_ref), zlim = c(0,1))
  g.all$metrc3 <-  mk.plot(df = df.sum, y = 'var_sig/var_tot', 
                           x = paste0('mad_', TS_ref), xlim = c(0,0.5),
                           z = paste0('pur_avg_', TS_ref), zlim = c(0,1))
  
  return(g.all)
}


g.LC1 <- mk.all3.fig2(TS_ref = TS2LC[1])
g.LC2 <- mk.all3.fig2(TS_ref = TS2LC[2])
g.LC3 <- mk.all3.fig2(TS_ref = TS2LC[3])
g.LC4 <- mk.all3.fig2(TS_ref = TS2LC[4])





fig.name <- paste0('xplrfig___4TS_', refval, 'vs-metrics')
fig.path <- paste0('dataProcessing/', batch_name, '/figs')
dir.create(fig.path, showWarnings = F, recursive = T)
fig.width <- 16; fig.height <- 12;  fig.fmt <- 'png'
fig.fullfname <- paste0(fig.path, '/', fig.name, '.', fig.fmt)
if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}

w <- 0.25; h <- 0.33

print(g.LC1$metrc1, vp = viewport(width = w, height = h, x = 0*w, y = 2*h, just = c(0,0)))
print(g.LC1$metrc2, vp = viewport(width = w, height = h, x = 0*w, y = 1*h, just = c(0,0)))
print(g.LC1$metrc3, vp = viewport(width = w, height = h, x = 0*w, y = 0*h, just = c(0,0)))

print(g.LC2$metrc1, vp = viewport(width = w, height = h, x = 1*w, y = 2*h, just = c(0,0)))
print(g.LC2$metrc2, vp = viewport(width = w, height = h, x = 1*w, y = 1*h, just = c(0,0)))
print(g.LC2$metrc3, vp = viewport(width = w, height = h, x = 1*w, y = 0*h, just = c(0,0)))

print(g.LC3$metrc1, vp = viewport(width = w, height = h, x = 2*w, y = 2*h, just = c(0,0)))
print(g.LC3$metrc2, vp = viewport(width = w, height = h, x = 2*w, y = 1*h, just = c(0,0)))
print(g.LC3$metrc3, vp = viewport(width = w, height = h, x = 2*w, y = 0*h, just = c(0,0)))

print(g.LC4$metrc1, vp = viewport(width = w, height = h, x = 3*w, y = 2*h, just = c(0,0)))
print(g.LC4$metrc2, vp = viewport(width = w, height = h, x = 3*w, y = 1*h, just = c(0,0)))
print(g.LC4$metrc3, vp = viewport(width = w, height = h, x = 3*w, y = 0*h, just = c(0,0)))

dev.off()





