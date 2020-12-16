## use the NV30 model to estimate the log(lambda) and theta
## for the test data of the simulation study

rm(list=ls())
library(abind)
library(fields)
library(tictoc)
library(tensorflow)
library(keras)
library(reticulate); np <- import("numpy")


## load the data ---------------------
test_x <- np$load("../common_data/npy/test_x.npy")
test_y <- np$load("../common_data/npy/test_y.npy") # only needed for figurs


dir.create("npy", showWarnings=FALSE)

## convert images into variograms ----
tic("image to vgrams")
np$save('npy/subs.npy', test_x)
py_run_file("images2multivario.py")
toc()


## use the NV30 model to estimate log(lambda) and theta ----
## for all variograms 
tic("vgrams to estimates")
mvgs <- np$load("npy/mvgs.npy")
model <- load_model_tf("model_NV30")
slope_mvg <- predict(model, mvgs)

py_run_file("scaler_to_npy.py")
scales <- np$load("npy/scaler_y_model_NV30.npy")
slope_mvg[,1] <- slope_mvg[,1]*sqrt(scales[2,1]) + scales[1,1]
slope_mvg[,2] <- slope_mvg[,2]*sqrt(scales[2,2]) + scales[1,2]
slope_mvg[,2][slope_mvg[,2]<.Machine$double.eps] <- .Machine$double.eps
toc()

## plot the estimates and the true parameters
plot(test_y[,1], slope_mvg[,1]); abline(a=0, b=1, col="green")
plot(test_y[,2], slope_mvg[,2]); abline(a=0, b=1, col="green")

