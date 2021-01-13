#!/usr/local/bin/Rscript
################################################################################
# Project:  spHomogeneity
# Purpose:  Function to assign the prescribed NDVI phenology to the land cover
#           classes and then convolve it with the MODIS spatial response
#           (STEP 4 in simulation exercise)
# License:  GPL v3
# Authors:  Gregory Duveiller - Jul 2020
################################################################################

gen_syn_datablock = function(batch_name, psf_fname, TS2LC, spat_perturb = NULL, ndvi_noise = 0){

  require(raster)
  require(dplyr)
  
  # # params that are needed...
  # landscape_ID <- 'landscape-2LC-id42'
  # psf_fname <- 'PSF-AQUA-48-10'
  # LC1 <- 'TS1'
  # LC0 <- 'TS7'
  
  
  # # set some NDVI noise to make things more realistic
  # ndvi_noise <- 0.01 #  Not sure still if we need it
  
  # # for now, we set the spatial perturbation to 0
  # spat_perturb <- NULL
  
  # load PSF
  load(paste0('data/inter_data/', batch_name, '/', psf_fname, '.Rda')) # list_PSF
  
  # load purity raster
  purity_stack <- brick(x = paste0('data/inter_data/', batch_name, '/', 
                                   'LC1-purity-', psf_fname))
  
  # load original raster
  r <- raster(x = paste0('data/inter_data/', batch_name, '/map'))
  
  # get spatial reso of original pixels
  spres <- as.numeric(strsplit(psf_fname, split = '-')[[1]][4])/res(r)[1]
  
  # get spatial extent
  npixi <- dim(purity_stack)[1]*res(purity_stack)[1]
  
  # make modis L2G grid 
  buf <- 500 # buffer to avoid border effects...
  dum <- seq(buf, (npixi * spres) - buf, 231.56)/spres 
  grd <- data.frame(x = rep(dum, times = length(dum)), 
                    y = rep(dum, each = length(dum)))
  
  # load original raster
  o <- raster::extract(x = r, y = grd)
  df.grd <- data.frame(grd_id = as.numeric(rownames(grd)),
                       y = grd$y, x = grd$x, original_id = o) 
  
  
  # load prescribed NDVI curves
  load(paste0('data/inter_data/', batch_name, '/df-ideal-ts.Rda'))  # 'df.ideal.ts'
  doi_vctr <- df.ideal.ts$DOI
  
  # # assign TS to LC
  # TS2LC <- c('LC1'='TS1', 'LC2'='TS8', 'LC3'='TS3', 'LC4'='TS9')
  
  # function to calc and gather data at a given time step
  conv.NDVI <- function(ti, TS2LC, grd){
    
    # set time counter
    tic <- Sys.time()
    
    # assign new NDVI values to the pixels adding some slight NDVI noise
    
    # get values at time = ti
    df.ts <- df.ideal.ts %>% filter(DOI == ti) 
    vals <-  as.numeric(df.ts[TS2LC])
    
    # reclassify r accoriding to those values
    rclmat <- matrix(c(1:length(TS2LC), vals), ncol=2, byrow = F)
    ri <- reclassify(r, rcl = rclmat)
    
    # create with noise to add to NDVI noise 
    rn <- setValues(raster(r), values = rnorm(n = ncell(ri), mean = 0, sd = ndvi_noise))
    
    # get angle for this days orbit
    angi <- ((ti - 1) %% 16) + 1
    
    # apply convolution
    conv <- focal(ri + rn, list_PSF[[angi]], pad = T, padValue = mean(vals))
    
    # perturb grid (to simulate position uncertainty)
    if(!is.null(spat_perturb)){
      rho <- rnorm(1, 0, 50/spres)   # division by 'spres' needed to keep units
      theta <- runif(1, 0, 2*pi)
      grdi <- grd + cbind(rep(rho*cos(theta), dim(grd)[1]),
                          rep(rho*sin(theta), dim(grd)[1]))
    } else {grdi <- grd}
    
    # extract convolved NDVI
    ndvi_conv <- raster::extract(conv, grdi) 
    
    # extract purity per class
    purities <- NULL
    for(iLC in names(TS2LC)){
      
      purity_stack <- brick(x = paste0('data/inter_data/', batch_name, '/', 
                                       iLC,'-purity-', psf_fname))
      
      purity <- raster::extract(purity_stack[[angi]], grdi)
      purities <- cbind(purities, purity)
    }
    colnames(purities) <- paste0('purity_', TS2LC)
    purities <- cbind(purities, maxPur = apply(purities,FUN = max, MARGIN = 1))
    
    # setup df by grid id before perturbation
    df.out <- data.frame(grd_id = as.numeric(rownames(grd)),  
                         grdi,   # perturbed grid coordinates  
                         DOI = ti,     # nominal day in the time series
                         NDVI = ndvi_conv) %>%
      bind_cols(as.data.frame(purities))
    
    
    toc <- Sys.time() - tic
    print(paste('Just finished with t =',ti, 'using this number of seconds', toc))
    return(df.out)
  }
  
  # apply function
  list_temp <- NULL
  for(iT in doi_vctr){
    list_temp[[iT]] <- conv.NDVI(iT, TS2LC, grd)
    df.all <- do.call('rbind', list_temp)
  }
  
  # export
  save(list = c('df.all','df.grd'), 
       file = paste0('data/inter_data/', batch_name, 
                     '/datablock-', paste(TS2LC, collapse = '-'),
                     '___', psf_fname, '.Rda'))
  
  
}
