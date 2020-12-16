## compute the maximum likelihood estimates for the data
## of the data illustration
##
## the script needs a lot of memory 
rm(list=ls())
library(abind)
library(fields)
library(reticulate); np <- import("numpy")
library(parallel)

source("Rscripts/MLESearchMatern.R", local=TRUE)

load("../1_common_data/rda/slope.rda")
source("../1_common_data/Rscripts/to16x16.R")

## use same par grid as the NN models use for training
logLambdaMat <- np$load("../1_common_data/npy/training_201_200_logLambdaMat.npy")
logThetaGrid <- log(np$load("../1_common_data/npy/training_201_200_thetaGrid.npy"))

cl <- makeForkCluster(dim(slope_x)[3], outfile="")
out <- parLapply(cl=cl, X=1:dim(slope_x)[3], function(i)
                 MLESearchMatern(to16x16(slope_x, i),
                                 logLambdaGrid=logLambdaMat,
                                 logThetaGrid=logThetaGrid,
                                 printIndex=i))

## channel wise MLE
slope_mle_c <- do.call(abind, c(lapply(out, function(x) x$MLE), along=3))
   

## combined MLE
ll <- do.call(abind, c(lapply(out, function(x) x$llike), along=4))
ll <- eval(parse(text=paste0("ll[,,,", 1:dim(slope_x)[3], "]", collapse="+")))
index <- t(apply(ll, 3, which.max.matrix))
slope_mle <- cbind(logLambdaMat[index[,2:1]], logThetaGrid[index[,1]])
  
dir.create("rda", showWarnings=FALSE)
save(slope_mle_c, slope_mle, file="rda/slope_mle.rda")




