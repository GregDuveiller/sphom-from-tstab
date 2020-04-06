# initializing the script -----
require(ggplot2)
require(grid)
require(dplyr)

LC1 <- 'TS8'
LC0 <- 'TS9'
batch_name <- 'batch_001'
load(paste0('dataProcessing/', batch_name, '/df-ideal-ts.Rda'))

load(paste0('dataProcessing/', batch_name, '/metrics-', LC1,'-', LC0, 
            '___PSF-AQUA-48-10.Rda'))  # df.sum

df.sum <- df.sum %>%
  mutate(pur_avg_dom = ifelse(pur_avg < 0.5, 1 - pur_avg, pur_avg))


# parametrizing the plots -----

# setting up some colours
col.1 <- '#1770DC'
col.2 <- '#DCA416'
col.3 <- 'grey20'
col.4 <- 'grey60'

# function for 'plots'
mk.plot <- function(y, x, ylim = NULL, xlim = NULL){
  g <- ggplot(df.sum, aes_string(y = y, x = x)) +
    geom_point(shape = 3, aes(colour = factor(original_id))) +
    geom_smooth(method = 'loess', formula = 'y ~ x', colour = col.3) + 
    scale_colour_manual(values = c('1' = col.1, '0' = col.2), guide = 'none')
  
  if(!is.null(ylim) | !is.null(xlim)){
    g <- g + coord_cartesian(ylim = ylim, xlim = xlim)
  }
  
  return(g)
}
# mk.plot(y = 'pur_avg', x = 'var_sig')

# function for 'maps'
mk.map <- function(fill, vir_opt = "D"){
  g <- ggplot(df.sum, aes(y = y, x = x)) +
    geom_raster(aes_string(fill = fill)) + 
    scale_fill_viridis_c(option = vir_opt) +
    coord_equal(expand = F) + 
    theme(legend.position = 'top', 
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

# generating the subplots -----

g.pheno <- ggplot(df.ideal.ts, aes(x = DOI)) + 
  geom_line(aes_string(y = LC1), colour = col.1) + 
  geom_line(aes_string(y = LC0), colour = col.2) +
  scale_y_continuous('NDVI') + scale_x_continuous('Day of the year (DOI)')

g.map.pur_avg <- mk.map(fill = 'pur_avg_dom', vir_opt = 'D')
# g.map.pur_avg <- mk.map(fill = 'pur_avg', vir_opt = 'D')
# g.map.pur_std <- mk.map(fill = 'pur_std', vir_opt = 'D')
g.map.pur_std <- mk.map(fill = 'log(pur_avg_dom/pur_std)', vir_opt = 'D')

g.map.var_tot <- mk.map(fill = 'entropy', vir_opt = 'B')
g.map.var_sig <- mk.map(fill = '1/var_res', vir_opt = 'B')
g.map.var_res <- mk.map(fill = '1-(entropy/7.78)', vir_opt = 'B')

g.plot.var_tot.pur_avg <- mk.plot(y = 'pur_avg_dom', x = 'entropy')
g.plot.var_sig.pur_avg <- mk.plot(y = 'pur_avg_dom', x = '1/var_res')
g.plot.var_res.pur_avg <- mk.plot(y = 'pur_avg_dom', x = '1-(entropy/7.78)')

g.plot.var_tot.pur_std <- mk.plot(y = 'log(pur_avg_dom/pur_std)', x = 'entropy')
g.plot.var_sig.pur_std <- mk.plot(y = 'log(pur_avg_dom/pur_std)', x = '1/var_res')
g.plot.var_res.pur_std <- mk.plot(y = 'log(pur_avg_dom/pur_std)', x = '1-(entropy/7.78)')


# printing the final plot -----
fig.name <- paste0('xplrfig___',LC1,'-',LC0,'_testmetrics')
fig.path <- paste0('dataProcessing/', batch_name,'/figs')
dir.create(fig.path, showWarnings = F, recursive = T)
fig.width <- 16; fig.height <- 12;  fig.fmt <- 'png'
fig.fullfname <- paste0(fig.path, '/', fig.name, '.', fig.fmt)
if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}

w <- 0.25; h <- 0.33

print(g.pheno,       vp = viewport(width = w, height = h, x = 0*w, y = 2*h, just = c(0,0)))
print(g.map.pur_avg, vp = viewport(width = w, height = h, x = 0*w, y = 1*h, just = c(0,0)))
print(g.map.pur_std, vp = viewport(width = w, height = h, x = 0*w, y = 0*h, just = c(0,0)))

print(g.map.var_tot, vp = viewport(width = w, height = h, x = 1*w, y = 2*h, just = c(0,0)))
print(g.map.var_sig, vp = viewport(width = w, height = h, x = 2*w, y = 2*h, just = c(0,0)))
print(g.map.var_res, vp = viewport(width = w, height = h, x = 3*w, y = 2*h, just = c(0,0)))

print(g.plot.var_tot.pur_avg, vp = viewport(width = w, height = h, x = 1*w, y = 1*h, just = c(0,0)))
print(g.plot.var_sig.pur_avg, vp = viewport(width = w, height = h, x = 2*w, y = 1*h, just = c(0,0)))
print(g.plot.var_res.pur_avg, vp = viewport(width = w, height = h, x = 3*w, y = 1*h, just = c(0,0)))

print(g.plot.var_tot.pur_std, vp = viewport(width = w, height = h, x = 1*w, y = 0*h, just = c(0,0)))
print(g.plot.var_sig.pur_std, vp = viewport(width = w, height = h, x = 2*w, y = 0*h, just = c(0,0)))
print(g.plot.var_res.pur_std, vp = viewport(width = w, height = h, x = 3*w, y = 0*h, just = c(0,0)))

dev.off()



# df.dum <- df.sum %>% 
#   select(x, y, original_id) %>% 
#   transmute(x = 160 - x, y = 160 - y, original_ID = as.factor(original_id))
# 
# df.sumP <- left_join(df.sum, df.dum)

# 
# ggplot(df.sum, aes(x = x, y = y)) +
#   geom_raster(aes(fill = original_id)) +
#   scale_fill_gradient('Dominant land cover', low = col.2, high = col.1) +
#                      #  labels = c('0'='LC0', '1'='LC1')) +
#   coord_equal(expand = F) +
#   #theme_details +
#   guides(fill = guide_colourbar(title.position = 'top',
#                                 title.hjust = 0.5,
#                                 barwidth = 20))
# 
# ggplot(df.sum, aes(x = x, y = y)) +
#   geom_raster(aes(fill = pur_avg)) +
#   scale_fill_viridis_c(option = "D") +
#   coord_equal(expand = F) +
#   theme_details +
#   guides(fill = guide_colourbar(title.position = 'top',
#                                 title.hjust = 0.5,
#                                 barwidth = 20))
# 
# ggplot(df.sum, aes(x = x, y = y)) + 
#   geom_raster(aes(fill = pur_std)) + 
#   scale_fill_viridis_c(option = "D") +
#   coord_equal(expand = F) + 
#   theme_details +
#   guides(fill = guide_colourbar(title.position = 'top',
#                                 title.hjust = 0.5,
#                                 barwidth = 20))
# 
# 
# ggplot(df.sum, aes(x = x, y = y)) + 
#   geom_raster(aes(fill = pur_std/pur_avg)) + 
#   scale_fill_viridis_c(option = "D") +
#   coord_equal(expand = F) + 
#   theme_details +
#   guides(fill = guide_colourbar(title.position = 'top',
#                                 title.hjust = 0.5,
#                                 barwidth = 20))
# 
# 
# ggplot(df.sum, aes(x = x, y = y)) + 
#   geom_raster(aes(fill = pur_avg/pur_std)) + 
#   scale_fill_viridis_c(option = "D") +
#   coord_equal(expand = F) + 
#   theme_details +
#   guides(fill = guide_colourbar(title.position = 'top',
#                                 title.hjust = 0.5,
#                                 barwidth = 20))
# 
# 
# 
# 
# 
# ggplot(df.sum, aes(x = x, y = y)) + 
#   geom_raster(aes(fill = var_tot)) + 
#   scale_fill_viridis_c(option = "A") +
#   coord_equal(expand = F) + 
#   theme_details +
#   guides(fill = guide_colourbar(title.position = 'top',
#                                 title.hjust = 0.5,
#                                 barwidth = 20))
# 
# ggplot(df.sum, aes(x = x, y = y)) + 
#   geom_raster(aes(fill = var_sig)) + 
#   scale_fill_viridis_c(option = "A") +
#   coord_equal(expand = F) + 
#   theme_details +
#   guides(fill = guide_colourbar(title.position = 'top',
#                                 title.hjust = 0.5,
#                                 barwidth = 20))
# 
# ggplot(df.sum, aes(x = x, y = y)) + 
#   geom_raster(aes(fill = var_res)) + 
#   scale_fill_viridis_c(option = "A") +
#   coord_equal(expand = F) + 
#   theme_details +
#   guides(fill = guide_colourbar(title.position = 'top',
#                                 title.hjust = 0.5,
#                                 barwidth = 20))
# 
# 
# ggplot(df.sum, aes(x = x, y = y)) + 
#   geom_raster(aes(fill = var_tot - var_sig)) + 
#   scale_fill_viridis_c(option = "A") +
#   coord_equal(expand = F) + 
#   theme_details +
#   guides(fill = guide_colourbar(title.position = 'top',
#                                 title.hjust = 0.5,
#                                 barwidth = 20))
# 
# ggplot(df.sum, aes(x = x, y = y)) + 
#   geom_raster(aes(fill = var_sig/var_tot)) + 
#   scale_fill_viridis_c(option = "B") +
#   coord_equal(expand = F) + 
#   theme_details +
#   guides(fill = guide_colourbar(title.position = 'top',
#                                 title.hjust = 0.5,
#                                 barwidth = 20))
# 
# 
# ggplot(df.sum, aes(x = x, y = y)) + 
#   geom_raster(aes(fill = var_res/var_tot)) + 
#   scale_fill_viridis_c(option = "B") +
#   coord_equal(expand = F) + 
#   theme_details +
#   guides(fill = guide_colourbar(title.position = 'top',
#                                 title.hjust = 0.5,
#                                 barwidth = 20))
# 
# ggplot(df.sum, aes(x = x, y = y)) + 
#   geom_raster(aes(fill = var_tot/var_res)) + 
#   scale_fill_viridis_c(option = "B") +
#   coord_equal(expand = F) + 
#   theme_details +
#   guides(fill = guide_colourbar(title.position = 'top',
#                                 title.hjust = 0.5,
#                                 barwidth = 20))
# 
# ggplot(df.sum, aes(x = x, y = y)) + 
#   geom_raster(aes(fill = var_sig/var_res)) + 
#   scale_fill_viridis_c(option = "B") +
#   coord_equal(expand = F) + 
#   theme_details +
#   guides(fill = guide_colourbar(title.position = 'top',
#                                 title.hjust = 0.5,
#                                 barwidth = 20))
# 
# ggplot(df.sum, aes(x = x, y = y)) + 
#   geom_raster(aes(fill = 1/var_res)) + 
#   scale_fill_viridis_c(option = "B") +
#   coord_equal(expand = F) + 
#   theme_details +
#   guides(fill = guide_colourbar(title.position = 'top',
#                                 title.hjust = 0.5,
#                                 barwidth = 20))
# 
#          