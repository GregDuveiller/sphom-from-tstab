# make_synthetic_landscape.R
# script to generate a random landscape

require(parallel)
require(raster)
require(secr)

ncores = 4


# prepare input mesh 
tempmask <- make.mask(nx = 100, ny = 100, spacing = 1, buffer = 0)
# generate landscape
set.seed(42)
r <- raster(randomHabitat(tempmask, p = 0.5, A = 0.5, minpatch = 15))
# convert from 1-NA to binary 
r <- !is.na(r) 
# artificially aggregate resolution
r <- disaggregate(r, fact = 10)


# get idea of how angle are distributed in MODIS
calc.ang = 0 
if(calc.ang){
  require(MODIS)
  runGdal('MYD09GA',begin = '2013100', end = '2013300', extent=extent(c(3,4,47,48)), job='sampleMODangles',SDSstring = '001')
  s<-stack(preStack(pattern='*SensorZenith*',path='/DATA/MODIS_ARC/PROCESSED//sampleMODangles'))
  
  ts1=as.vector(extract(s,1))*0.01
  
  ang=c()
  for(i in 1:16){
    x<-round(ts1[seq(i,length(ts1),16)])
    ang[i]<-as.numeric(names(table(x))[which.max(table(x))])
  }
} else {
  angle.vctr <- c(31, 46, 13, 55,  6, 50, 25, 38, 40, 23, 51,  4, 59, 16, 45, 33)
}


# script to make purity maps

source('codeProcessing/ModisPsfModel.r')

Pnum <- -1
lat <- 48

# make the PSF models for all angles/
list_PSF <- list()
for(i in 1:length(angle.vctr)){
  dum.psf <- ModisPsfModel(v0 = 231.65, 
                          delta = 10, # <- beware of this one... 
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
  # load(paste0(wpath,'dataMid/SynTest/',paste('PSF','.AQUA.angleNum.',which(ang==scanangle),'.RData',sep='')))
  tic <- Sys.time(); out <- focal(r, list_PSF[[i]], pad = T, padValue = 0); toc <- Sys.time() - tic
  out.int <- round(out*1000)/1000
  list_purityMaps[[i]] <- out.int
#  writeRaster(x = out.int,filename = paste0(wpath,'dataMid/SynTest/',paste('CSP.bin.AQUA.',which(ang==scanangle),'.tif',sep='')),
#              format='GTiff',overwrite=TRUE)
}



# The prescribed NDVI curves...
t <- 1:250
glogf <- function(t,A, K, Q, B, M, v){z=A+(K-A)/(1+Q*exp(-B*(t-M)))^(1/v)}

# DBF 
gro <- glogf(t, A=0.25, K=0.9, B=0.04, Q=1, v=1, M=40)
sen <- glogf(t, A=0, K=0.1, B=0.01, Q=1, v=1, M=180)
NDVI.DBF <- gro-sen

# WCR 
gro <- glogf(t, A=0.20, K=0.9, B=0.04, Q=1, v=1, M=70)
sen <- glogf(t, A=0, K=0.6, B=0.07, Q=1, v=1, M=180)
NDVI.WCR <- gro-sen

# SCR 
gro <- glogf(t, A=0.20, K=0.9, B=0.05, Q=1, v=1, M=140)
sen <- glogf(t, A=0, K=0.6, B=0.06, Q=1, v=1, M=230)
NDVI.SCR <- gro-sen

# GRA 
gro <- glogf(t, A=0.20, K=0.7, B=0.03, Q=1, v=1, M=100)
sen <- glogf(t, A=0, K=0.2, B=0.04, Q=1, v=1, M=230)
NDVI.GRA <- gro-sen


plot(t, NDVI.DBF, type='l', col='darkgreen', ylim=c(0,1))
lines(t, NDVI.WCR, col='wheat3')
lines(t, NDVI.SCR, col='cornflowerblue')
lines(t, NDVI.GRA, col='red')


### generate block of time series...
fname <- "SynTest"

# make grid of modis L2G
tnx <- dim(r)[1] * 10 # need to check these scales... 
dum <- seq(500, tnx-500, 231.56)/100 # we avoid border effects...
grd <- data.frame(y=rep(dum, times=length(dum)), x=rep(dum, each=length(dum)))
# 


fpath=paste0(wpath, 'dataMid/SynTest/')
ndvi.noise=0.025

# should set.seed
set.seed(1)
ok = sort(sample(t, length(t)/3, replace=F))

require(parallel)

# functions to calculate the SNR
funSNR=function(t,z,splinedf=8){
  s=smooth.spline(t, z, df=splinedf)  
  VOR=var(s$y-z)
  SNR=var(s$y)/VOR
  return(list(SNR=SNR,VOR=VOR))}

conv.NDVI <- function(ti,LC1,LC0){
  #pertrub grid
  rho=rnorm(1,0,50); theta=runif(1,0,2*pi)
  grdi <- grd+cbind(rep(rho*cos(theta),dim(grd)[1]),rep(rho*sin(theta),dim(grd)[1])) 
  
  # get angle for this days orbit
  angi <- ((ti-1) %% 16)+1
  load(paste0(wpath,'dataMid/SynTest/',paste('PSF','.AQUA.angleNum.',angi,'.RData',sep='')))
  
  ri=r
  ri[which(as.vector(r)==1)]<-LC1[ti]#+rnorm(n=sum(as.vector(r)==1),mean = 0, sd = 0.02)
  ri[which(as.vector(r)==0)]<-LC0[ti]#+rnorm(n=sum(as.vector(r)==0),mean = 0, sd = 0.02)
  
  
  conv=focal(ri,dum.psf,pad=T,padValue=LC0[ti])
  
  df.out=data.frame(id=rownames(grd),grd)
  
  df.out$t=ti
  df.out$z=extract(conv,grdi)+rnorm(n=dim(grdi)[1],mean = 0, sd = ndvi.noise) # noise added to be more realistic
  
  # get exact (perturbed purity)
  df.out$w<-extract(raster(paste0(fpath,'CSP.bin.AQUA.',angi,'.tif')),grdi)  
  
  
  df.out$rho=rho
  df.out$theta=theta
  
  #   #make a coarse raster
  #   dum=raster(points2grid(SpatialPoints(grd)))
  #   dum[cellFromXY(dum,SpatialPoints(grd))]<-df.out$z
  
  print(paste0('Done t=',ti))
  return(df.out)
}

snr.csp.summary<-function(df.out){
  df.sum<-data.frame(id=1:dim(grd)[1])
  for(iPts in 1:dim(grd)[1]){
    dum=df.out[df.out$id==iPts,]
    df.sum$snr[iPts] <- funSNR(dum$t[ok],dum$z[ok])
    df.sum$med.csp[iPts] <- median(dum$w[ok])
    df.sum$avg.csp[iPts] <- mean(dum$w[ok])
    df.sum$std.csp[iPts] <- sd(dum$w[ok])
    df.sum$min.csp.1[iPts] <- min(dum$w[ok])
    df.sum$min.csp.0[iPts] <- min(1-dum$w[ok])
    df.sum$vor[iPts] <- funSNR(dum$t[ok],dum$z[ok])$VOR
  }
  return(df.sum)
}








