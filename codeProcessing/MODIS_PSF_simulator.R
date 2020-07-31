MODIS_PSF_simulator = function(v0, delta, Lat, ScanAngle, Platform, optsigma){
# % Function to create a model of the MODIS spatial response for a given
# % latitude and view zenith angle
# % 
# % Inputs: 
# %   * v0 = nominal spatial resolution of MODIS pixels in meters
# %   * delta = discretization of the resulting PSF model 
# %             i.e. size (in meters of the pixels of the PSF)
# %   * Lat = latitude in decimal degrees 
# %   * ScanAngle = Scan Angle of the satellite in decimal degrees... 
# %             ! it is different from View zenith angle which can be larger due 
# %             to curvature of the Earth
# %   * Platform = either 'TERRA' for the descending pass or 'AQUA' for the 
# %             ascending pass
# %   * optsigma = sigma of the gaussian effect of the PSF in meters
# %
# % 
# % Theory and application of this function can be found in the following
# % paper:
# %
# % G. Duveiller, F. Baret & P. Defourny (2011). Crop specific green area
# % index retrieval from MODIS data at regional scale by controlling
# % pixel-target adequacy. Remote Sensing of Environment. Volume 115, Issue
# % 10, pp 2686-2701 
# %
# % You are free to use and modify these functions in any way you like, as
# % long as you give proper reference to the original source and cite the
# % paper.
# % Gregory Duveiller
# %
# % Last changes: G.Duveiller 25/07/2012
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 
  
require(pracma)
require(spatstat)
require(akima)
  
#   v0=231.65
#   ScanAngle=45
#   delta=10
#   Lat=40.8
#   Platform='TERRA'
#   optsigma=10


## Set default parameters
Incl = 98.21; # inclination of the orbit
Freq = 16; # satellite revisit frequency
hSat = 705000; # height of the satelite above Earth's surface
R = 6378100; # Radius of the Earth (assumed to be spherical)

## define function for internal calc
calcCE <- function(ScanAngle, dScanAngle, v0, hsat, R){
  
  # % dScanAngle = -0.5.*FOVscan for beginning of obs
  # % dScanAngle = 0 for centre of obs
  # % dScanAngle = +0.5.*FOVscan for beginning of obs
  
  FOVtrack=2*atan(v0/2/hsat)
  
  # distance of sat to centre of the Earth
  AB=hsat+R
  
  SArad=ScanAngle*pi/180
  
  angleBAC=SArad+dScanAngle
  angleACB=pi-asin(AB*sin(angleBAC)/R)
  angleABC=pi-angleACB-angleBAC
  
  CD=R*sin(angleABC)
  if(angleBAC==0){AC=hsat} else {AC=CD/sin(angleBAC)}
  CE=AC*tan(0.5*FOVtrack)
  
  AE=sqrt(AC^2  + CE^2)
  
  return(list(CE=CE,AE=AE,angleABC=angleABC))
}



## Calculate geometric boundaries of footprint due to ScanAngle
FOVscan <- 2*2*atan(v0/2/hSat); 

list1 <- calcCE(ScanAngle,-0.5*FOVscan,v0,hSat,R)
list2 <- calcCE(ScanAngle,0,v0,hSat,R)
list3 <- calcCE(ScanAngle,+0.5*FOVscan,v0,hSat,R)

CE <- list1$CE; AE <- list1$AE; alphaC <- list1$angleABC
IO <- list2$CE; AI <- list2$AE; alphaO <- list2$angleABC
KH <- list3$CE; AH <- list3$AE; alphaK <- list3$angleABC

EI <- sqrt(AE^2 + AI^2 - (2*AE*AI*cos(0.5*FOVscan)))
IH <- sqrt(AH^2 + AI^2 - (2*AH*AI*cos(0.5*FOVscan)))
EH <- sqrt(AE^2 + AH^2 - (2*AE*AH*cos(FOVscan)))

OC <- sqrt(EI^2-(IO-CE)^2)
OK <- sqrt(IH^2-(KH-IO)^2)


# coordinates of points F, E, J, H, G and I
cScan   <- c(-OC, -OC, 0, OK, OK, 0)
cTrack  <- c(CE, -CE, -IO, -KH, KH, IO)



## Rotate footprint boundaries according to Lat

# set if ascending or descending orbit...
platform.direction <- ifelse(Platform == 'AQUA', -1, 1) 
# note above... we assume that if not specifed, it is descending

# Need to find the trackangle 
A = cos(Incl*pi/180) - (1/Freq)*(cos(Lat*pi/180))^2
B = sqrt((cos(Lat*pi/180))^2 - (cos(Incl*pi/180))^2)
trackangle = platform.direction * atan((A/B))*180/pi

# For AQUA we need to inverse sign because it is ascending instead of
# descening! that is why 'platform.direction' has to be set to -1

# Calculate rotation matrix
RotM <- matrix(c(cos(trackangle*pi/180), -sin(trackangle*pi/180), 
                 sin(trackangle*pi/180), cos(trackangle*pi/180)), 
               nrow=2, byrow = T)
latlonCor <- RotM %*% as.matrix(rbind(cScan, cTrack))

cLon <- latlonCor[1,]
cLat <- latlonCor[2,]


## Make the scan PSF

cSptRs  <- c(0, 0, 1, 0, 0, 1)

xrange <- seq(floor(min(cLon)- delta),ceiling(max(cLon)+delta),delta)/delta
yrange <- seq(floor(min(cLat)- delta),ceiling(max(cLat)+delta),delta)/delta

Z  <-  interp(cLon/delta, cLat/delta, cSptRs, xo = xrange,yo = yrange)
PSFscan <- im(t(Z$z), xcol = xrange, yrow = yrange, unitname='v0')

## Opt PSF
s <- optsigma/delta

# define size of the filter
dum <- round(2*(3*s))
dum <- dum+mod(dum+1,2)
siz  <-  c((dum-1)/2,(dum-1)/2)

# Define cartesian coordinate grid
grd <- meshgrid(-siz[2]:siz[2],-siz[1]:siz[1])
# Convert to polar coordinates
theta <- atan2(grd$Y,grd$X)
rho <- sqrt(abs(grd$Y)^2+abs(grd$X)^2)

# Calculate filter according to gaussian equation
arg <- -rho^2/(2*s*s)
PSFopt <- exp(arg)
PSFopt[PSFopt < eps()*max(PSFopt)] <- 0;

# Normalize so that the sum of the values within the filter are equal to 1.
sumPSFopt <- sum(PSFopt)
if(sumPSFopt != 1) PSFopt <- PSFopt/sumPSFopt

PSFopt <- im(PSFopt, xcol = -siz[2]:siz[2], yrow = -siz[1]:siz[1], unitname='v0')



## Calc PSFnet
PSFnet <- convolve.im(PSFopt,PSFscan)
sumPSFnet <- sum(PSFnet)


PSF <- flipdim(as.matrix(PSFnet)/sumPSFnet,1)

return(PSF)
}
