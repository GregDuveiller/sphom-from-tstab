require(ggplot2)
require(grid)
require(dplyr)
require(wesanderson)

load("~/Work/Workspace/spHomogeneity/dataProcessing/batch_002/metrics-TS1-TS8-TS3-TS9___PSF-AQUA-48-20.Rda")


g <- ggplot(df.sum, aes_string(y =  'pur_avg_dom', x = 'entropy')) +
  geom_point(shape = 3, aes(colour = factor(original_id))) +
  geom_smooth(method = 'loess', formula = 'y ~ x', colour = col.3) +
  facet_wrap(~ factor(original_id)) +
  scale_colour_manual(values = wes_palette("GrandBudapest1"), guide = F)
                    
fig.name <- paste0('xplrfig___', '4class_entropy_overview', '.png')
fig.path <- paste0('dataProcessing/', batch_name, '/figs')
dir.create(fig.path, showWarnings = F, recursive = T)
ggsave(filename = fig.name, path = fig.path, width = 6, height = 6, plot = g)
