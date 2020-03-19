# make_synthetic_landscape.R
# script to generate a random landscape 


require(parallel)
require(raster)
require(secr)


### generate a random spatial (binary) landscape ----

spres <- 100  # spatial resolution of the initial pixels
npixi <- 160  # number of original pixels: we suppose they are 'spres' m each
npixo <- 800  # numscale factor to make it to rasters of workable size
# prepare input mesh 
tempmask <- make.mask(nx = npix, ny = npix, spacing = 1, buffer = 0)
# generate landscape
set.seed(42)
r <- raster(randomHabitat(tempmask, p = 0.5, A = 0.5, minpatch = 15))
# convert from 1-NA to binary 
r <- !is.na(r) 
# artificially aggregate resolution
r <- disaggregate(r, fact = npixo/npixi)


### simulate MODIS PSF and purity ----

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
source('codeProcessing/ModisPsfModel.r')

Pnum <- -1
lat <- 48

# make the PSF models for all angles/
list_PSF <- list()
for(i in 1:length(angle.vctr)){
  dum.psf <- ModisPsfModel(v0 = 231.65,  # spatial resolution of Modis grid in m
                          delta = spres/(npixo/npixi), # <- beware of this ... 
                          Lat = lat, ScanAngle = angle.vctr[i], PlatformPass = Pnum, 
                          optsigma = 30)
  dum.psf <- round(dum.psf, digits = 6)
  if(dim(dum.psf)[1]%%2==0) dum.psf <- rbind(dum.psf,0) #add row of zeros if not odd number of rows
  if(dim(dum.psf)[2]%%2==0) dum.psf <- cbind(dum.psf,0) #add col of zeros if not odd number of cols
  # save('dum.psf',file=paste0(wpath,'dataMid/SynTest/',paste('PSF','.AQUA.angleNum.',which(ang==scanangle),'.RData',sep='')))
  list_PSF[[i]] <- dum.psf
  }


list_purityMaps <- list()
# make the purity maps... 
for(i in 1:length(list_PSF)){
  tic <- Sys.time(); out <- focal(r, list_PSF[[i]], pad = T, padValue = 0); toc <- Sys.time() - tic
  out.int <- round(out*1000)/1000
  list_purityMaps[[i]] <- out.int
}

purity_stack <- brick(list_purityMaps)
names(purity_stack) <- paste('angle', angle.vctr, sep = '_')

### Define the prescribed NDVI curves... ----

doi_vctr <- 1:365
glogf <- function(doi_vctr, A, K, Q, B, M, v){
  z <- A + (K-A)/(1 + Q*exp(-B*(doi_vctr - M)))^(1/v)}

# DBF 
gro <- glogf(doi_vctr, A=0.25, K=0.9, B=0.04, Q=1, v=1, M=40)
sen <- glogf(doi_vctr, A=0, K=0.1, B=0.01, Q=1, v=1, M=180)
DBF <- gro-sen

# WCR 
gro <- glogf(doi_vctr, A=0.20, K=0.9, B=0.04, Q=1, v=1, M=70)
sen <- glogf(doi_vctr, A=0, K=0.6, B=0.07, Q=1, v=1, M=180)
WCR <- gro-sen

# SCR 
gro <- glogf(doi_vctr, A=0.20, K=0.9, B=0.05, Q=1, v=1, M=140)
sen <- glogf(doi_vctr, A=0, K=0.6, B=0.06, Q=1, v=1, M=230)
SCR <- gro-sen

# GRA 
gro <- glogf(doi_vctr, A=0.20, K=0.7, B=0.03, Q=1, v=1, M=100)
sen <- glogf(doi_vctr, A=0, K=0.2, B=0.04, Q=1, v=1, M=230)
GRA <- gro-sen

# # TRO 
# gro <- glogf(doi_vctr, A=0.20, K=0.7, B=0.03, Q=1, v=1, M=100)
# sen <- glogf(doi_vctr, A=0, K=0.2, B=0.04, Q=1, v=1, M=230)
# TRO <- gro-sen

df.ideal.ts <- data.frame(DOI = doi_vctr, DBF, WCR, SCR, GRA)


# quickplot
plot(doi_vctr, DBF, type = 'l', col = 'darkgreen', ylim = c(0,1))
lines(doi_vctr, WCR, col = 'wheat3')
lines(doi_vctr, SCR, col = 'cornflowerblue')
lines(doi_vctr, GRA, col = 'red')


### generate synthetic data block ----

# make modis L2G grid 
buf <- 500 # buffer to avoid border effects...
dum <- seq(buf, (npixi * spres) - buf, 231.56)/spres 
grd <- data.frame(y = rep(dum, times = length(dum)), 
                  x = rep(dum, each = length(dum)))

# sanity check plot
plot(r)
plot(list_purityMaps[[1]])
points(grd, pch = 3)




# set some NDVI noise to make things more realistic
ndvi.noise <- 0.01 #  Not sure still if we need it

# function to calc and gather data at a given time step
conv.NDVI <- function(ti, LC1, LC0, grd){
  # set time counter
  tic <- Sys.time()
  
  # get identifier of which pixels are on LC or another 
  id1 <- as.vector(r) == 1; id0 <- !id1
  
  # assign new NDVI values to the pixels adding some slight NDVI noise
  ri <- r
  ri[which(id1)] <- LC1[ti] + rnorm(n = sum(id1), mean = 0, sd = ndvi.noise)
  ri[which(id0)] <- LC0[ti] + rnorm(n = sum(id0), mean = 0, sd = ndvi.noise)

  # get angle for this days orbit
  angi <- ((ti-1) %% 16)+1
  
  # apply convolution
  conv <- focal(ri, list_PSF[[angi]], pad = T, padValue = LC0[ti])
  
  # pertrub grid (to simulate position uncertainty)
  rho <- rnorm(1, 0, 50/spres)   # division by 'spres' needed to keep units 
  theta <- runif(1, 0, 2*pi)
  grdi <- grd + cbind(rep(rho*cos(theta), dim(grd)[1]), 
                      rep(rho*sin(theta), dim(grd)[1])) 
  
  # setup df by grid id before perturbation
  df.out <- data.frame(grd_id = as.numeric(rownames(grd)),  
                       grdi,   # perturbed grid coordinates  
                       DOI = ti,     # nominal day in the time series
                       NDVI = raster::extract(conv, grdi), # convolved NDVI
                       Purity = raster::extract(list_purityMaps[[angi]], grdi)) 
  toc <- Sys.time() - tic
  print(paste('Just finished with t =',ti, 'using this number of seconds', toc))
  return(df.out)
}

# apply function
list_temp <- NULL
for(iT in doi_vctr){
  list_temp[[iT]] <- conv.NDVI(iT, DBF, GRA, grd)
  df.all <- do.call('rbind', list_temp)
  df.all$LC1 <- 'DBF'
  df.all$LC0 <- 'GRA'
}

# quick and dirty plot
ggplot(df.all %>% filter(grd_id %in% sample(dim(grd)[1], size = 3),
                         DOI %in% doi_vctr_sub)) + 
  geom_point(aes(x = DOI, y = NDVI, size = Purity, colour = factor(grd_id))) +
  geom_line(data = df.ideal.ts, aes(x = DOI, y = GRA), colour = 'black') +
  geom_line(data = df.ideal.ts, aes(x = DOI, y = DBF), colour = 'black')


### Calculate metrics ----
require(dplyr)
require(tidyr)
require(purrr)

# should set.seed to simulate frequency of image acquisition
set.seed(42)
doi_vctr_sub <- sort(sample(doi_vctr, length(doi_vctr)/4, replace = F))

df.sum <- data.frame(NULL)
for(iGrd in unique(df.all$grd_id)){
  
  df.sum <- df.all %>% 
    filter(grd_id %in% iGrd, DOI %in% doi_vctr_sub) %>%
    mutate(NDVI.sm = smooth.spline(x = DOI, y = NDVI, df = 8)$y) %>%
    summarise(grd_id = iGrd,
              pur_avg = mean(Purity),
              pur_med = median(Purity),
              pur_std = sd(Purity),
              var_res = var(NDVI.sm - NDVI),
              var_sig = var(NDVI.sm),
              var_tot = var(NDVI)) %>% 
    bind_rows(df.sum)
  
}

df.sum <- df.sum %>% 
  bind_cols(grd) %>%
  mutate(original_id = raster::extract(x = r, y = grd))


ggplot(df.sum) + 
  geom_raster(aes(x = x, y = y, fill = var_sig/var_tot)) +
  scale_fill_viridis_c() 
  

ggplot(df.sum %>% filter(pur_avg > 0.5)) +
  geom_point(aes(x = pur_avg/pur_std, y = var_sig/var_tot,
                 colour = original_id),
             shape = 3) +
  scale_x_log10() +
  scale_colour_continuous(guide = 'none')

ggplot(df.sum %>% filter(pur_avg > 0.5)) +
  geom_point(aes(x = pur_avg/pur_std, y = var_sig/var_res,
             colour = original_id),
             shape = 3) +
  scale_x_log10()

ggplot(df.sum %>% filter(pur_avg > 0.5)) +
  geom_point(aes(x = pur_avg, y = var_sig,
             colour = original_id),
             shape = 3) +
  scale_x_log10() +
  scale_colour_continuous(guide = 'none')

###### WORK IN PROGRESS !!! ######
###### construction stopped ######

# # functions to calculate the SNR
# calcMetrics <- function(t, z, splinedf = 8){
#   
#   s <- smooth.spline(t, z, df = splinedf)  
#   
#   VOR <- var(s$y-z)
#   SNR <- var(s$y)/VOR
#   return(list(SNR = SNR,VOR = VOR))
#   }


# 
# snr.csp.summary <- function(df.out){
#   df.sum <- data.frame(id=1:dim(grd)[1])
#   for(iPts in 1:dim(grd)[1]){
#     dum=df.out[df.out$id==iPts,]
#     df.sum$snr[iPts] <- funSNR(dum$doi_vctr[doi_vctr_sub],dum$z[doi_vctr_sub])
#     df.sum$med.csp[iPts] <- median(dum$w[doi_vctr_sub])
#     df.sum$avg.csp[iPts] <- mean(dum$w[doi_vctr_sub])
#     df.sum$std.csp[iPts] <- sd(dum$w[doi_vctr_sub])
#     df.sum$min.csp.1[iPts] <- min(dum$w[doi_vctr_sub])
#     df.sum$min.csp.0[iPts] <- min(1-dum$w[doi_vctr_sub])
#     df.sum$vor[iPts] <- funSNR(dum$doi_vctr[doi_vctr_sub],dum$z[doi_vctr_sub])$VOR
#   }
#   return(df.sum)
# }
# 
# 
# 
# 
# df.LC <- data.frame(case=paste0('LC',c(14,13,12,24,23,34,41,31,21,42,32,43)),
#                  lc1=c('NDVI.DBF','NDVI.DBF','NDVI.DBF','NDVI.WCR','NDVI.WCR','NDVI.SCR','NDVI.GRA','NDVI.SCR','NDVI.WCR','NDVI.GRA','NDVI.SCR','NDVI.GRA'),
#                  lc2=c('NDVI.GRA','NDVI.SCR','NDVI.WCR','NDVI.GRA','NDVI.SCR','NDVI.GRA','NDVI.DBF','NDVI.DBF','NDVI.DBF','NDVI.WCR','NDVI.WCR','NDVI.SCR'))








