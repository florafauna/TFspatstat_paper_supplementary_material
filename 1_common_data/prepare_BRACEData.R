## prepare the climate model outputs for the data illustration

rm(list=ls())
library(gapfill)
library(fields)
load("rda/BRACEData.rda") # available from https://github.com/NCAR/LatticeKrig

## N and S America region
ix <- c(11, 138)
iy <- c(35, 162)
indx <- ix[1]:ix[2]
indy <- iy[1]:iy[2]
lon <- Longitude[indx]
lat <- Latitude[indy]

U <- JJASlope[indx, indy,]
mU <- apply( U, c( 1,2), mean)
sdU <- apply( U, c( 1,2), sd)


for(k in 1:30){
  U[,,k] <- (U[,,k] - mU)/ sdU
}

slope_x <- U


save(slope_x, lon, lat, file="rda/slope.rda")


local({
    dim(U) <- c(dim(U)[1:2], 6, 5);
    Image(U, asRaster=FALSE)
})
