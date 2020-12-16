## make test data for the simulation study

rm(list=ls())
library(fields)
library(gapfill)
library(reticulate); np <- import("numpy")
library(abind)
library(ggplot2); theme_set(theme_bw())
library(dplyr)
library(doParallel); registerDoParallel(4)
source("Rscripts/makeMaternTrainingData1.R", chdir=TRUE)

## 16 x 10k images with 40 different thetas and 50 different lambdas
x <- array(NA, c(10000, 16, 16, 30))

for(i in 1:dim(x)[4]){
    testObj <- makeMaternTrainingData1(nTheta=40, 
                                       rangeTheta=c(2, 25),
                                       dfRange=c(40, 216), rangeLambda = c(1e-6, 2000),
                                       nLambda=50,
                                       m=5,
                                       iSeed=334+i)
    x[,,,i] <- testObj$xAll
    cat(".")
}
names(testObj) <- c("x", "y", names(testObj)[3:length(testObj)])
dimnames(testObj$y)[[2]][2] <- "theta"
testObj$x <- x



## save data --------
dir.create("npy", showWarnings=FALSE)
np$save(paste0("npy/test_x.npy"), r_to_py(testObj$x))
np$save(paste0("npy/test_y.npy"), r_to_py(testObj$y))
