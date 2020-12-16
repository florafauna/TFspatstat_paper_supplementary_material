## use the NV30 model to estimate the log(lambda) and theta
## for all 16 x 16 pixel subsets of the data
## 
## the script needs a lot of memory 


rm(list=ls())
library(abind)
library(fields)
library(tictoc)
library(tensorflow)
library(keras)
library(reticulate); np <- import("numpy")

load("../common_data/rda/slope.rda")
source("../common_data/Rscripts/to16x16.R")

## extract 16 x 16 pixel subsets from the climate data ----
tic("to 16x16 images")
slope_images <- array(NA, c(dim(to16x16(slope_x, 1))[1:3],dim(slope_x)[3]))
for(i in 1:dim(slope_x)[3]){
    slope_images[,,,i]<- to16x16(slope_x, i)
    cat(".")
}
cat(fill=TRUE)
toc()

dir.create("npy", showWarnings=FALSE)
np$save('npy/subs.npy', slope_images)

## convert images into variograms --------------------------
tic("images to vgrams")
py_run_file("images2multivario.py")
toc()

mvgs <- np$load("npy/mvgs.npy")
model <- load_model_tf("model_NV30")


## use the NV30 model to estimate log(lambda) and theta ----
## for all variograms 
tic("vgrams to prediction")
slope_mvg <- predict(model, mvgs)
py_run_file("scaler_to_npy.py")
scales <- np$load("npy/scaler_y_model_NV30.npy")
slope_mvg[,1] <- slope_mvg[,1]*sqrt(scales[2,1]) + scales[1,1]
slope_mvg[,2] <- slope_mvg[,2]*sqrt(scales[2,2]) + scales[1,2]
slope_mvg[,2][slope_mvg[,2]<.Machine$double.eps] <- .Machine$double.eps
toc()

dir.create("rda", showWarnings=FALSE)
save(slope_mvg, file="rda/slope_mvg.rda")
