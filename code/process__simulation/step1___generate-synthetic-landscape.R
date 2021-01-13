#!/usr/local/bin/Rscript
################################################################################
# Project:  spHomogeneity
# Purpose:  Function to generate the spatial patterns of the synthetic landscape
#           (STEP 1 in simulatipn exercise)
# License:  GPL v3
# Authors:  Gregory Duveiller - Jul 2020
################################################################################

gen_syn_landscape = function(batch_name, landscape_seed, TS2LC){
  
  require(raster)
  require(secr)
  require(igraph)
  
  # setup scale of the experiment
  npixi <- 200    # number of original pixels: we suppose they are 'spres' m each
  npixo <- 1000   # numscale factor to make it to rasters of workable size
  
  # prepare input mesh 
  tempmask <- make.mask(nx = npixi, ny = npixi, spacing = 1, buffer = 0)
  
  # generate landscape
  r0 <- rasterFromXYZ(tempmask, res=c(NA,NA), crs="", digits=5)
  nLC <- length(TS2LC)
  r0[] <- nLC # set last number in the list
  
  if(nLC == 5){
    set.seed(landscape_seed + 3)
    ts3.mask <- rasterFromXYZ(randomHabitat(tempmask, p = 0.40, A = 0.4, minpatch = 10))
    r0[which(as.vector(ts3.mask)==1)] <- 4
    nLC <- nLC - 1
  }
  if(nLC == 4){
    set.seed(landscape_seed + 2)
    ts3.mask <- rasterFromXYZ(randomHabitat(tempmask, p = 0.40, A = 0.4, minpatch = 10))
    r0[which(as.vector(ts3.mask)==1)] <- 3
    nLC <- nLC - 1
  }
  if(nLC == 3){
    set.seed(landscape_seed + 1)
    ts2.mask <- rasterFromXYZ(randomHabitat(tempmask, p = 0.45, A = 0.4, minpatch = 15))
    r0[which(as.vector(ts2.mask)==1)] <- 2
    nLC <- nLC - 1
  }
  if(nLC == 2){
    set.seed(landscape_seed)
    ts1.mask <- rasterFromXYZ(randomHabitat(tempmask, p = 0.50, A = 0.4, minpatch = 20))
    r0[which(as.vector(ts1.mask)==1)] <- 1
    nLC <- nLC - 1
  }
  
  ## for the record: old binary landscape.
  # r0 <- raster(randomHabitat(tempmask, p = 0.5, A = 0.5, minpatch = 15))
  
  
  # artificially aggregate resolution
  r <- disaggregate(r0, fact = npixo/npixi, overwrite = TRUE,
                    filename = paste0('data/inter_data/', batch_name, '/map.grd'))
  
  # mk quick plot
  png(filename = paste0('data/inter_data/', batch_name, '/map_quickplot.png'), 
      width = 6, height = 6, units = "in", res = 150)
  plot(r)
  dev.off()
}