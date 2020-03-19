# simulate_temporal_behaviour.R
#
# Step 4 in spHomogeneity simulation: assigning phenology and convolving 


# params that are needed...
landscape_ID <- 'landscape-2LC-id42'
psf_fname <- 'PSF-AQUA-48-10'
spres <- 50 

# load purity raster
purity_stack <- brick(x = paste0('dataProcessing/', landscape_ID, 
                                 '___purity-', psf_fname))
npixi <- dim(purity_stack)[1]*res(purity_stack)[1]

# make modis L2G grid 
buf <- 500 # buffer to avoid border effects...
dum <- seq(buf, (npixi * spres) - buf, 231.56)/spres 
grd <- data.frame(y = rep(dum, times = length(dum)), 
                  x = rep(dum, each = length(dum)))

# sanity check plot
plot(raster(x = paste0('dataProcessing/', landscape_ID, '___map')))
plot(list_purityMaps[[1]])
points(grd, pch = 3)


# Define the prescribed NDVI curves... ----

doi_vctr <- 1:365
glogf <- function(doi_vctr, A, K, Q, B, M, v){
  z <- A + (K-A)/(1 + Q*exp(-B*(doi_vctr - M)))^(1/v)}





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

