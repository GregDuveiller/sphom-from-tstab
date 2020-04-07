gen_4LC_landscape = function(batch_name){
# step1___generate-binary-landscape.R
#
# Step 1 in spHomogeneity simulation: generating a random binary landscape that 
# will serve as a basis for the analysis

require(raster)
require(secr)


# set-up landscape identification
landscape_seed <- 24
landscape_ID <- paste0('landscape-4LC-id', landscape_seed)


# setup scale of the experiment
npixi <- 160  # number of original pixels: we suppose they are 'spres' m each
npixo <- 800  # numscale factor to make it to rasters of workable size

# prepare input mesh 
tempmask <- make.mask(nx = npixi, ny = npixi, spacing = 1, buffer = 0)

# generate landscape
set.seed(landscape_seed + 1)
ts1.mask <- raster(randomHabitat(tempmask, p = 0.50, A = 0.3, minpatch = 20))
set.seed(landscape_seed + 2)
ts2.mask <- raster(randomHabitat(tempmask, p = 0.45, A = 0.4, minpatch = 15))
set.seed(landscape_seed + 2)
ts3.mask <- raster(randomHabitat(tempmask, p = 0.40, A = 0.5, minpatch = 10))

r0 <- raster(tempmask)

r0[] <- 4
r0[which(as.vector(ts3.mask)==1)]<-3
r0[which(as.vector(ts2.mask)==1)]<-2
r0[which(as.vector(ts1.mask)==1)]<-1

# artificially aggregate resolution
r <- disaggregate(r0, fact = npixo/npixi, overwrite = TRUE,
                  filename = paste0('dataProcessing/', batch_name, '/map.grd'))

# mk quick plot
png(filename = paste0('dataProcessing/', batch_name, '/map_quickplot.png'), 
    width = 6, height = 6, units = "in", res = 150)
plot(r)
dev.off()
}