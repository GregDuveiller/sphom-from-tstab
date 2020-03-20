# step4___generate-synthetic-datablock.R
#
# Step 4 in spHomogeneity simulation: assigning phenology and convolving 

require(raster)
require(dplyr)

# params that are needed...
landscape_ID <- 'landscape-2LC-id42'
psf_fname <- 'PSF-AQUA-48-10'
LC1 <- 'TS1'
LC0 <- 'TS7'

# set some NDVI noise to make things more realistic
ndvi_noise <- 0.01 #  Not sure still if we need it


# load PSF
load(paste0('dataProcessing/', psf_fname, '.Rda')) # list_PSF

# load purity raster
purity_stack <- brick(x = paste0('dataProcessing/', landscape_ID, 
                                 '___purity-', psf_fname))
# load original raster
r <- raster(x = paste0('dataProcessing/', landscape_ID, '___map'))

# get spatial reso of original pixels
spres <- as.numeric(strsplit(psf_fname, split = '-')[[1]][4])/res(r)[1]

# get spatial extent
npixi <- dim(purity_stack)[1]*res(purity_stack)[1]

# make modis L2G grid 
buf <- 500 # buffer to avoid border effects...
dum <- seq(buf, (npixi * spres) - buf, 231.56)/spres 
grd <- data.frame(y = rep(dum, times = length(dum)), 
                  x = rep(dum, each = length(dum)))

# # sanity check plot
# plot(r)
# plot(purity_stack[[1]])
# points(grd, pch = 3)
 

# load prescribed NDVI curves
load('dataProcessing/df-ideal-ts.Rda')  # 'df.ideal.ts'
doi_vctr <- df.ideal.ts$DOI

# function to calc and gather data at a given time step
conv.NDVI <- function(ti, LC1, LC0, grd){
  # set time counter
  tic <- Sys.time()
  
  # get identifier of which pixels are on LC or another 
  id1 <- as.vector(r) == 1; id0 <- !id1
  
  # assign new NDVI values to the pixels adding some slight NDVI noise
  ri <- r
  ri[which(id1)] <- LC1[ti] + rnorm(n = sum(id1), mean = 0, sd = ndvi_noise)
  ri[which(id0)] <- LC0[ti] + rnorm(n = sum(id0), mean = 0, sd = ndvi_noise)
  
  # get angle for this days orbit
  angi <- ((ti - 1) %% 16) + 1
  
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
                       Purity = raster::extract(purity_stack[[angi]], grdi)) 
  toc <- Sys.time() - tic
  print(paste('Just finished with t =',ti, 'using this number of seconds', toc))
  return(df.out)
}

# apply function
list_temp <- NULL
eval(expr = parse(text = paste0('dumTS1 <- df.ideal.ts$', LC1)))
eval(expr = parse(text = paste0('dumTS2 <- df.ideal.ts$', LC0)))
for(iT in doi_vctr){
  list_temp[[iT]] <- conv.NDVI(iT, dumTS1, dumTS2, grd)
  df.all <- do.call('rbind', list_temp)
  df.all$LC1 <- LC1
  df.all$LC0 <- LC0
}


# df.sum <- df.all %>% 
#   bind_cols(grd) %>%
#   mutate(original_id = raster::extract(x = r, y = grd))

save(list = c('df.all','grd'), file = paste0('dataProcessing/', landscape_ID, 
                             '___datablock-', LC1, '-', LC0, '-', ndvi_noise, 
                             '___', psf_fname, '.Rda'))


# quick and dirty plot
require(ggplot2)
doi_vctr_sub <- sort(sample(doi_vctr, length(doi_vctr)/4, replace = F))
ggplot(df.all %>% filter(grd_id %in% sample(dim(grd)[1], size = 5),
                         DOI %in% doi_vctr_sub)) + 
  geom_point(aes(x = DOI, y = NDVI, colour = Purity, shape = factor(grd_id))) +
  geom_line(data = df.ideal.ts, aes(x = DOI, y = TS1), colour = 'cornflowerblue') +
  geom_line(data = df.ideal.ts, aes(x = DOI, y = TS2), colour = 'grey10') +
  scale_colour_continuous('Purity of TS1', limits = c(0,1))

