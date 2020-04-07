gen_syn_datablock = function(batch_name, psf_fname, LC1, LC2, LC3, LC4){
# step4___generate-4class-synthetic-datablock.R
#
# Step 4 in spHomogeneity simulation: assigning phenology and convolving 

require(raster)
require(dplyr)

# # params that are needed...
# landscape_ID <- 'landscape-2LC-id42'
# psf_fname <- 'PSF-AQUA-48-10'
# LC1 <- 'TS1'
# LC0 <- 'TS7'

  
# set some NDVI noise to make things more realistic
ndvi_noise <- 0 #  Not sure still if we need it

# for now, we set the spatial perturbation to 0
spat_perturb <- NULL
  
# load PSF
load(paste0('dataProcessing/', batch_name, '/', psf_fname, '.Rda')) # list_PSF

# load purity raster
purity_stack <- brick(x = paste0('dataProcessing/', batch_name, '/', 
                                 'purity-', psf_fname))
# load original raster
r <- raster(x = paste0('dataProcessing/', batch_name, '/map'))

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
load(paste0('dataProcessing/', batch_name, '/df-ideal-ts.Rda'))  # 'df.ideal.ts'
doi_vctr <- df.ideal.ts$DOI

# function to calc and gather data at a given time step
conv.NDVI <- function(ti, LC1, LC2, LC3, LC4, grd){
  # set time counter
  tic <- Sys.time()
  
  # get identifier of which pixels are on LC or another 
  id <- as.vector(r)
  
  # assign new NDVI values to the pixels adding some slight NDVI noise
  ri <- r
  ri[which(id == 1)] <- LC1[ti] + rnorm(n = length(which(id == 1)), mean = 0, sd = ndvi_noise)
  ri[which(id == 2)] <- LC2[ti] + rnorm(n = length(which(id == 2)), mean = 0, sd = ndvi_noise)
  ri[which(id == 3)] <- LC3[ti] + rnorm(n = length(which(id == 3)), mean = 0, sd = ndvi_noise)
  ri[which(id == 4)] <- LC4[ti] + rnorm(n = length(which(id == 4)), mean = 0, sd = ndvi_noise)
  
  # get angle for this days orbit
  angi <- ((ti - 1) %% 16) + 1
  
  # apply convolution
  conv <- focal(ri, list_PSF[[angi]], pad = F, padValue = LC0[ti])
  
  if(!is.null(spat_perturb)){
  # pertrub grid (to simulate position uncertainty)
  rho <- rnorm(1, 0, 50/spres)   # division by 'spres' needed to keep units
  theta <- runif(1, 0, 2*pi)
  grdi <- grd + cbind(rep(rho*cos(theta), dim(grd)[1]),
                      rep(rho*sin(theta), dim(grd)[1]))
  } else {grdi <- grd}
  
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
eval(expr = parse(text = paste0('dumTS2 <- df.ideal.ts$', LC2)))
eval(expr = parse(text = paste0('dumTS3 <- df.ideal.ts$', LC3)))
eval(expr = parse(text = paste0('dumTS4 <- df.ideal.ts$', LC4)))


for(iT in doi_vctr){
  list_temp[[iT]] <- conv.NDVI(iT, dumTS1, dumTS2, grd)
  df.all <- do.call('rbind', list_temp)
  df.all$LC1 <- LC1
  df.all$LC0 <- LC0
}


# df.sum <- df.all %>% 
#   bind_cols(grd) %>%
#   mutate(original_id = raster::extract(x = r, y = grd))

save(list = c('df.all','df.grd'), 
     file = paste0('dataProcessing/', batch_name, 
                   '/datablock-', LC1, '-', LC0, '___', psf_fname, '.Rda'))


}

# # quick and dirty plot
# require(ggplot2)
# doi_vctr_sub <- sort(sample(doi_vctr, length(doi_vctr)/4, replace = F))
# ggplot(df.all %>% filter(grd_id %in% sample(dim(grd)[1], size = 5),
#                          DOI %in% doi_vctr_sub)) + 
#   geom_point(aes(x = DOI, y = NDVI, colour = Purity, shape = factor(grd_id))) +
#   geom_line(data = df.ideal.ts, aes(x = DOI, y = TS1), colour = 'cornflowerblue') +
#   geom_line(data = df.ideal.ts, aes(x = DOI, y = TS2), colour = 'grey10') +
#   scale_colour_continuous('Purity of TS1', limits = c(0,1))

