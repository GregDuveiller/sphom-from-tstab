#!/usr/local/bin/Rscript
################################################################################
# Project:  spHomogeneity
# Purpose:  make figure showing how residues are calculated
# License:  GPL v3
# Authors:  Gregory Duveiller - Jan 2021
################################################################################


require(ggplot2)

am <- 10.5/24
pm <- 13.5/24

doys <-  c(100+am, 101+pm, 102+am, 104+am, 107+pm, 109+am, 112+am, 113+pm, 115+pm)
set.seed(82)
df <- data.frame(doy = doys,
                 y = rnorm(n = length(doys), 0, 0.05) + 
                   (0.01 * (doys - mean(doys))) + 0.5)

uy <- 0.006
ux <- 0.2

lbl.c.1 <- 'cornflowerblue'
lbl.y.1 <- df$y[5] + 8*uy
lbl.x.1 <- df$doy[4] + 8*ux

lbl.c.2 <- 'blue'
lbl.y.2 <- df$y[5] + 4*uy
lbl.x.2 <- df$doy[4] + 8*ux

lbl.c.3 <- 'orange'
lbl.y.3 <- df$y[4] + 4*uy
lbl.x.3 <- df$doy[6] + 4*ux

lbl.c.4 <- 'red'
lbl.y.4 <- df$y[4] + 4*uy
lbl.x.4 <- df$doy[6] + 8*ux


yr5 <- ((((df$y[6] - df$y[4])/(df$doy[6] - df$doy[4]))*(df$doy[5] - df$doy[4])) - (df$y[5] - df$y[4]))
lbl.c.5 <- 'grey40'
lbl.y.5 <- df$y[5] + yr5 + 5*uy
lbl.x.5 <- df$doy[5] - 2*ux

(g <- ggplot(df) + 
    annotate('segment', x = df$doy[4], xend = df$doy[6], y = df$y[4], yend = df$y[6], 
             colour = 'grey70', linetype = 'dashed') +
    annotate("errorbar", xmin = df$doy[4], xmax = df$doy[5], y = lbl.y.1, colour = lbl.c.1, width = .01) +
    annotate('text',  label = 'Delta[1] (DOY)', parse = TRUE, size = 6,
             x = lbl.x.1, y = lbl.y.1 + 2*uy, colour = lbl.c.1) + 
    annotate('linerange', x = df$doy[4], ymin = lbl.y.1, ymax = df$y[4], linetype = 'dotted', colour = lbl.c.1) +
    annotate('linerange', x = df$doy[5], ymin = lbl.y.1, ymax = df$y[5], linetype = 'dotted', colour = lbl.c.1) +
    annotate("errorbar", xmin = df$doy[4], xmax = df$doy[6], y = lbl.y.2, colour = lbl.c.2, width = .01) +
    annotate('text',  label = 'Delta[2] (DOY)', parse = TRUE, size = 6,
             x = lbl.x.2, y = lbl.y.2 - 2*uy, colour = lbl.c.2) + 
    annotate('linerange', x = df$doy[6], ymin = lbl.y.2, ymax = df$y[6], linetype = 'dotted', colour = lbl.c.1) +
    annotate('linerange', x = df$doy[4], ymin = lbl.y.2, ymax = df$y[4], linetype = 'dotted', colour = lbl.c.1) +
    annotate("errorbar", ymin = df$y[4], ymax = df$y[5], x = lbl.x.3, colour = lbl.c.3, width = 0.3) +
    annotate('text', label = 'Delta[1] (y)', parse = TRUE, size = 6,
             y = lbl.y.3, x = lbl.x.3 - 5*ux, colour = lbl.c.3, angle = 0) + 
    annotate('linerange', y = df$y[5], xmin = lbl.x.3, xmax = df$doy[5], linetype = 'dotted', colour = lbl.c.3) +
    annotate('linerange', y = df$y[4], xmin = lbl.x.3, xmax = df$doy[4], linetype = 'dotted', colour = lbl.c.3) +
    annotate("errorbar", ymin = df$y[4], ymax = df$y[6], x = lbl.x.4, colour = lbl.c.4, width = 0.3) +
    annotate('text', label = 'Delta[2] (y)', parse = TRUE, size = 6,
             y = lbl.y.4, x = lbl.x.4 + 5*ux, colour = lbl.c.4, angle = 0) + 
    annotate('linerange', y = df$y[6], xmin = lbl.x.4, xmax = df$doy[6], linetype = 'dotted', colour = lbl.c.4) +
    annotate('linerange', y = df$y[4], xmin = lbl.x.4, xmax = df$doy[4], linetype = 'dotted', colour = lbl.c.4) +
    annotate('segment', x = df$doy[5], xend = df$doy[5], 
             y = df$y[5] + yr5, yend = df$y[5], 
             colour = lbl.c.5, size = 1) +
    annotate('text', label = 'r', parse = TRUE, size = 6,
             y = lbl.y.5, x = lbl.x.5,  colour = lbl.c.5) +
    geom_point(aes(x = doy, y = y), size = 3) +
    annotate('text', label = 'r == frac(Delta[2] (y), Delta[2] (DOY)) %.% Delta[1] (DOY) - Delta[1] (y)', parse = TRUE, size = 6,
             y = 0.33, x = 111,  colour = lbl.c.5) +
    scale_y_continuous('y') +
    scale_x_continuous('Day of Year (DOY)') +  
    coord_cartesian(ylim = c(0.3,0.6)) + 
    theme_classic())


# export
if(!exists('fig.fmt')){fig.fmt = 'png'}
if(!exists('fig.path')){fig.path = 'docs/figures'}
dir.create(path = fig.path, recursive = T, showWarnings = F)

# export figure
fig.name <- paste0('fig___', 'explain-res-calc', '.', fig.fmt)
ggsave(fig.name, plot = g, path = fig.path, device = fig.fmt, 
       width = 6, height = 5)
 