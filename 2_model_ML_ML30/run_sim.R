## compute the maximum likelihood estimates for the test data
## of the simulation study
##
## the script needs a lot of memory 
rm(list=ls())
library(abind)
library(fields)
library(reticulate); np <- import("numpy")
library(parallel)

source("Rscripts/MLESearchMatern.R", local=TRUE)

test_x <- np$load("../1_common_data/npy/test_x.npy")


## use same par grid as the NN models use for training
logLambdaMat <- np$load("../1_common_data/npy/training_201_200_logLambdaMat.npy")
logThetaGrid <- log(np$load("../1_common_data/npy/training_201_200_thetaGrid.npy"))

cl <- makeForkCluster(dim(test_x)[4], outfile="")
out <- parLapply(cl=cl, X=1:dim(test_x)[4], function(i)
                 MLESearchMatern(test_x[,,,i],
                                 logLambdaGrid=logLambdaMat,
                                 logThetaGrid=logThetaGrid,
                                 printIndex=i))

## channel wise MLE
test_mle_c <- do.call(abind, c(lapply(out, function(x) x$MLE), along=3))
    
## par(mfrow=c(1,2))
## plot(test_y[,1], test_mle_c[,1,16]); abline(a=0, b=1, col="green", lwd=2)
## plot(log(test_y[,2]), test_mle_c[,2,16]); abline(a=0, b=1, col="green", lwd=2)


## combined MLE
ll <- do.call(abind, c(lapply(out, function(x) x$llike), along=4))
ll <- eval(parse(text=paste0("ll[,,,", 1:dim(test_x)[4], "]", collapse="+")))
index <- t(apply(ll, 3, which.max.matrix))
test_mle <- cbind(logLambdaMat[index[,2:1]], logThetaGrid[index[,1]])
  
## par(mfrow=c(1,2))
## plot(test_y[,1], mleComb[,1]); abline(a=0, b=1, col="green", lwd=2)
## plot(log(test_y[,2]), mleComb[,2]); abline(a=0, b=1, col="green", lwd=2)

dir.create("rda", showWarnings=FALSE)
save(test_mle_c, test_mle, file="rda/test_mle.rda")
