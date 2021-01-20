require(ggplot2)


doys <-  c(100, 101, 102, 105, 107, 109, 112, 113, 115)
set.seed(82)
df <- data.frame(doy = doys,
                 y = rnorm(n = length(doys), 0, 0.05) + 
                   (0.01 * (doys - mean(doys))) + 0.5)

uy <- 0.006
ux <- 0.2

lbl.c.1 <- 'cornflowerblue'
lbl.y.1 <- df$y[4] - 3*uy
lbl.x.1 <- df$doy[4] + 4*ux

lbl.c.2 <- 'blue'
lbl.y.2 <- df$y[4] - 6*uy
lbl.x.2 <- df$doy[4] + 4*ux

lbl.c.3 <- 'orange'
lbl.y.3 <- mean(c(df$y[4],df$y[6]))
lbl.x.3 <- df$doy[6] + 3*ux

lbl.c.4 <- 'red'
lbl.y.4 <- mean(c(df$y[4],df$y[6])) 
lbl.x.4 <- df$doy[6] + 6*ux


yr5 <- ((((df$y[6] - df$y[4])/(df$doy[6] - df$doy[4]))*(df$doy[5] - df$doy[4])) - (df$y[5] - df$y[4]))
lbl.c.5 <- 'grey10'
lbl.y.5 <- df$y[5] + yr5 + 5*uy
lbl.x.5 <- df$doy[5] - 1*ux

(g <- ggplot(df) + 
    geom_point(aes(x = doy, y = y), size = 3) +
    annotate('segment', x = df$doy[4], xend = df$doy[6], y = df$y[4], yend = df$y[6], 
             colour = 'grey70', linetype = 'dotted') +
    annotate("errorbar", xmin = df$doy[4], xmax = df$doy[5], y = lbl.y.1, colour = lbl.c.1, width = .01) +
    annotate('text',  label = 'Delta[1] (DOY)', parse = TRUE,
             x = lbl.x.1, y = lbl.y.1 - uy, colour = lbl.c.1) + 
    annotate("errorbar", xmin = df$doy[4], xmax = df$doy[6], y = lbl.y.2, colour = lbl.c.2, width = .01) +
    annotate('text',  label = 'Delta[2] (DOY)', parse = TRUE,
             x = lbl.x.2, y = lbl.y.2 - uy, colour = lbl.c.2) + 
    annotate("errorbar", ymin = df$y[4], ymax = df$y[5], x = lbl.x.3, colour = lbl.c.3, width = 0.3) +
    annotate('text', label = 'Delta[1] (y)', parse = TRUE,
             y = lbl.y.3, x = lbl.x.3 - ux, colour = lbl.c.3, angle = -90) + 
    annotate("errorbar", ymin = df$y[4], ymax = df$y[6], x = lbl.x.4, colour = lbl.c.4, width = 0.3) +
    annotate('text', label = 'Delta[2] (y)', parse = TRUE,
             y = lbl.y.4, x = lbl.x.4 - ux, colour = lbl.c.4, angle = -90) + 
    annotate('segment', x = df$doy[5], xend = df$doy[5], 
             y = df$y[5] + yr5, yend = df$y[5], 
             colour = 'grey40', linetype = 'dashed') +
    annotate('text', label = 'y[r]', parse = TRUE,
             y = lbl.y.5, x = lbl.x.5 - ux,  colour = lbl.c.5) +
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
       width = 8, height = 6)
