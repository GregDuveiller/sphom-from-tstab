# step2___simulate-MODIS-purity.R
#
# Step 2 in spHomogeneity simulation: simulating how the MODIS instruments
# samples the synthetic landscape. This involves modelling the MODIS spatial
# response and using it to generate so-called "pixel purity maps"

require(raster)
require(pracma)
require(spatstat)
require(akima)

landscape_ID <- 'landscape-2LC-id42'

# load raster dataset
r <- raster(paste0('dataProcessing/', landscape_ID, '___map'))

# defining the spatial resolution of the initial pixels in meters
spres <- 100 
# definition of the scale at which the MODIS PSF will be implemented
MODsc <- spres * res(r)[1]


# get idea of how angle are distributed in MODIS
calc.ang = 0 
if(calc.ang){
  ### HAVE NOT CHECKED IF THIS STILL WORKS !!! ###
  require(MODIS)
  runGdal('MYD09GA',begin = '2013100', end = '2013300', 
          extent = extent(c(3,4,47,48)), job = 'sampleMODangles',
          SDSstring = '001')
  s <- stack(preStack(pattern = '*SensorZenith*', 
                      path = '/DATA/MODIS_ARC/PROCESSED//sampleMODangles'))
  
  ts1 <- as.vector(raster::extract(s,1))*0.01
  
  ang = c()
  for(i in 1:16){
    x <- round(ts1[seq(i, length(ts1), 16)])
    ang[i] <- as.numeric(names(table(x))[which.max(table(x))])
  }
} else {
  angle.vctr <- c(31, 46, 13, 55,  6, 50, 25, 38, 40, 23, 51,  4, 59, 16, 45, 33)
}


# get the PSF model needed to generate purity maps
source('codeProcessing/MODIS_PSF_simulator.R')

# configure the PSF
platform <- 'AQUA' # --> for AQUA... N.B. should also do for TERRA
lat <- 48   # Latitude...  we could eventually change this parameter (only use positive numbers)

# make the PSF models for all angles/
list_PSF <- list()
for(i in 1:length(angle.vctr)){
  dum.psf <- MODIS_PSF_simulator(v0 = 231.65,  # spatial resolution of Modis grid in m
                           delta = MODsc, # <- beware of this ... 
                           Lat = lat, ScanAngle = angle.vctr[i], 
                           Platform = platform, 
                           optsigma = 30)
  dum.psf <- round(dum.psf, digits = 6)
  if(dim(dum.psf)[1]%%2 == 0) dum.psf <- rbind(dum.psf,0) # add row of zeros if not odd number of rows
  if(dim(dum.psf)[2]%%2 == 0) dum.psf <- cbind(dum.psf,0) # add col of zeros if not odd number of cols
  # save('dum.psf',file=paste0(wpath,'dataMid/SynTest/',paste('PSF','.AQUA.angleNum.',which(ang==scanangle),'.RData',sep='')))
  list_PSF[[i]] <- dum.psf
}

psf_fname <- paste('PSF', platform, lat, MODsc, sep = '-')
save('list_PSF', file = paste0('dataProcessing/', psf_fname, '.Rda'))

list_purityMaps <- list()
# make the purity maps... 
for(i in 1:length(list_PSF)){
  tic <- Sys.time(); out <- focal(r, list_PSF[[i]], pad = T, padValue = 0); toc <- Sys.time() - tic
  out.int <- round(out*1000)/1000
  list_purityMaps[[i]] <- out.int
}

purity_stack <- brick(list_purityMaps)
names(purity_stack) <- paste('angle', angle.vctr, sep = '_')

brick_fname <- paste0(landscape_ID,'___purity-', psf_fname)
writeRaster(x = purity_stack,  overwrite = TRUE,
            filename = paste0('dataProcessing/', brick_fname, '.grd'))

