# initializing the script -----
require(ggplot2)
require(grid)
require(dplyr)



batch_name <- 'batch_001'
psf_fname <- 'PSF-AQUA-48-10'


refval <- 'logMVpu'  # 'domMUpu', 'stdevpu'
metric <- 'sig2tot'  # 'entropyScaled'
testty <- 'miscela'  # 'shift-y', 'shift-x', 'miscela'


for(refval in c('logMVpu', 'domMUpu')){
  for(metric in c('entropy', 'entrSca', 'invResV', 'sig2tot', 'sig2res')){
    for(testty in c('shift-y', 'shift-x', 'miscela')){
      
      # parametrizing the plots -----
      
      if(refval == 'logMVpu'){y_var <- 'log(pur_avg_dom/pur_std^2)'}
      if(refval == 'domMUpu'){y_var <- 'pur_avg_dom'}
      if(refval == 'stdevpu'){y_var <- 'pur_std'}
      
      if(metric == 'entropy'){x_var <- 'entropy'}
      if(metric == 'entrSca'){x_var <- '1 - entropy/7.78'}
      if(metric == 'invResV'){x_var <- '1/var_res'}
      if(metric == 'varTime'){x_var <- 'var_tot'}
      if(metric == 'varSign'){x_var <- 'var_sig'}
      if(metric == 'varResi'){x_var <- 'var_res'}
      if(metric == 'sig2res'){x_var <- 'var_sig/var_res'}
      if(metric == 'sig2tot'){x_var <- 'var_sig/var_tot'}
      
      # get some data out
      load(paste0('dataProcessing/', batch_name, '/df-ideal-ts.Rda'))
      
      # setting up some colours
      col.1 <- '#1770DC'
      col.2 <- '#DCA416'
      col.3 <- 'grey20'
      col.4 <- 'grey60'
      
      # function for 'pheno'
      mk.pheno <- function(LC_ref, LC_oth, ylim = NULL, xlim = NULL){
        g <- ggplot(df.ideal.ts, aes(x = DOI)) + 
          geom_line(aes_string(y = LC_ref), colour = col.1) + 
          geom_line(aes_string(y = LC_oth), colour = col.2) +
          scale_y_continuous('NDVI') + scale_x_continuous('Day of the year (DOI)')
        
        if(!is.null(ylim) | !is.null(xlim)){
          g <- g + coord_cartesian(ylim = ylim, xlim = xlim)
        }
        return(g) 
      }
      
      # function for 'plots'
      mk.plot <- function(df, y, x, ylim = NULL, xlim = NULL){
        g <- ggplot(df, aes_string(y = y, x = x)) +
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
      # mk.map(fill = '1/var_res', vir_opt = "B")
      
      # combine all 3 for ease of use
      mk.all3 <- function(LC_ref, LC_oth){
        load(paste0('dataProcessing/', batch_name, '/metrics-', LC_ref,'-', LC_oth, 
                    '___', psf_fname, '.Rda'))  # df.sum
        # should be in the previous data preparation step
        df.sum <- df.sum %>%
          mutate(pur_avg_dom = ifelse(pur_avg < 0.5, 1 - pur_avg, pur_avg))
        
        g.all <- list()
        g.all$pheno <- mk.pheno(LC_ref = LC_ref, LC_oth = LC_oth, ylim = c(0,1))
        g.all$plot <- mk.plot(df = df.sum, y = y_var, x = x_var)
        g.all$map <- mk.map(df = df.sum, fill = x_var, vir_opt = 'B')
        return(g.all)
      }
      
      
      # generating the subplots -----
      
      # loading any ... 
      LC_ref <- 'TS1'; LC_oth <- 'TS2'
      load(paste0('dataProcessing/', batch_name, '/metrics-', LC_ref,'-', LC_oth, 
                  '___', psf_fname, '.Rda')) 
      # should be in the previous data preparation step
      df.sum <- df.sum %>%
        mutate(pur_avg_dom = ifelse(pur_avg < 0.5, 1 - pur_avg, pur_avg))
      
      
      g.map.pur_avg <- mk.map(df = df.sum , fill = 'pur_avg', vir_opt = "D", zlim = c(0,1))
      g.map.pur_std <- mk.map(df = df.sum , fill = 'pur_std^2', vir_opt = "D", zlim = c(0,0.05))
      g.map.pur_ref <- mk.map(df = df.sum , fill = y_var, vir_opt = "D")
      
      
      if(testty == 'shift-x'){
        g.1 <- mk.all3(LC_ref = 'TS1', LC_oth = 'TS2')
        g.2 <- mk.all3(LC_ref = 'TS1', LC_oth = 'TS3')
        g.3 <- mk.all3(LC_ref = 'TS1', LC_oth = 'TS4')
      }
      
      if(testty == 'shift-y'){
        g.1 <- mk.all3(LC_ref = 'TS1', LC_oth = 'TS5')
        g.2 <- mk.all3(LC_ref = 'TS6', LC_oth = 'TS9')
        g.3 <- mk.all3(LC_ref = 'TS7', LC_oth = 'TS8')
      }
      
      
      if(testty == 'miscela'){
        g.1 <- mk.all3(LC_ref = 'TS1', LC_oth = 'TS6')
        g.2 <- mk.all3(LC_ref = 'TS1', LC_oth = 'TS9')
        g.3 <- mk.all3(LC_ref = 'TS8', LC_oth = 'TS9')
      }
      
      # printing the final plot -----
      fig.name <- paste0('xplrfig___', refval,'_', testty, '_', metric)
      fig.path <- paste0('dataProcessing/', batch_name, '/figs')
      dir.create(fig.path, showWarnings = F, recursive = T)
      fig.width <- 16; fig.height <- 12;  fig.fmt <- 'png'
      fig.fullfname <- paste0(fig.path, '/', fig.name, '.', fig.fmt)
      if(fig.fmt == 'png'){png(fig.fullfname, width = fig.width, height = fig.height, units = "in", res= 150)}
      if(fig.fmt == 'pdf'){pdf(fig.fullfname, width = fig.width, height = fig.height)}
      
      w <- 0.25; h <- 0.33
      
      print(g.map.pur_avg, vp = viewport(width = w, height = h, x = 0*w, y = 2*h, just = c(0,0)))
      print(g.map.pur_std, vp = viewport(width = w, height = h, x = 0*w, y = 1*h, just = c(0,0)))
      print(g.map.pur_ref, vp = viewport(width = w, height = h, x = 0*w, y = 0*h, just = c(0,0)))
      
      print(g.1$pheno, vp = viewport(width = w, height = h, x = 1*w, y = 2*h, just = c(0,0)))
      print(g.1$map,   vp = viewport(width = w, height = h, x = 1*w, y = 1*h, just = c(0,0)))
      print(g.1$plot,  vp = viewport(width = w, height = h, x = 1*w, y = 0*h, just = c(0,0)))
      
      print(g.2$pheno, vp = viewport(width = w, height = h, x = 2*w, y = 2*h, just = c(0,0)))
      print(g.2$map,   vp = viewport(width = w, height = h, x = 2*w, y = 1*h, just = c(0,0)))
      print(g.2$plot,  vp = viewport(width = w, height = h, x = 2*w, y = 0*h, just = c(0,0)))
      
      print(g.3$pheno, vp = viewport(width = w, height = h, x = 3*w, y = 2*h, just = c(0,0)))
      print(g.3$map,   vp = viewport(width = w, height = h, x = 3*w, y = 1*h, just = c(0,0)))
      print(g.3$plot,  vp = viewport(width = w, height = h, x = 3*w, y = 0*h, just = c(0,0)))
      
      dev.off()
      
      
      
    }}}
