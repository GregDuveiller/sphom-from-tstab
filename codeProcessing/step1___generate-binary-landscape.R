# step1___generate-binary-landscape.R
#
# Step 1 in spHomogeneity simulation: generating a random binary landscape that 
# will serve as a basis for the analysis

require(raster)
require(secr)

# set-up landscape identification
landscape_seed <- 42
landscape_ID <- paste0('landscape-2LC-id', landscape_seed)


# setup scale of the experiment
npixi <- 160  # number of original pixels: we suppose they are 'spres' m each
npixo <- 800  # numscale factor to make it to rasters of workable size

# prepare input mesh 
tempmask <- make.mask(nx = npixi, ny = npixi, spacing = 1, buffer = 0)

# generate landscape
set.seed(landscape_seed)
r0 <- raster(randomHabitat(tempmask, p = 0.5, A = 0.5, minpatch = 15))

# convert from 1-NA to binary 
r0 <- !is.na(r0) 

# artificially aggregate resolution
r <- disaggregate(r0, fact = npixo/npixi, overwrite = TRUE,
                  filename = paste0('dataProcessing/', batch_name, '/map.grd'))

# mk quick plot
col.1 <- '#1770DC'; col.2 <- '#DCA416'
png(filename = paste0('dataProcessing/', batch_name, '/map_quickplot.png'), 
    width = 6, height = 6, units = "in", res = 150)
plot(r, legend = FALSE, col = c(col.2, col.1))
dev.off()
