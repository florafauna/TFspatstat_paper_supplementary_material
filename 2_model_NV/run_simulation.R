## use the NV model to estimate the log(lambda) and theta
## for the test data of the simulation study

rm(list=ls())
library(abind)
library(fields)
library(tictoc)
library(tensorflow)
library(keras)
library(reticulate); np <- import("numpy")

test_x <- np$load("../1_common_data/npy/test_x.npy")
test_y <- np$load("../1_common_data/npy/test_y.npy")

 
tic("image to vgrams")
np$save('npy/subs.npy', test_x)
py_run_file("images2multivario.py")
toc()

tic("vgrams to estimates")
mvgs <- np$load("npy/mvgs.npy")
model <- load_model_tf("model_NV")

res <- array(NA, c(dim(test_x)[1], 2, dim(test_x)[4]))

for(i in 1:30){
    res[,,i] <- predict(model, mvgs[,,,i,drop=FALSE])
}

py_run_file("scaler_to_npy.py")
scales <- np$load("npy/scaler_y_model_NV.npy")
res[,1,] <- res[,1,]*sqrt(scales[2,1]) + scales[1,1]
res[,2,] <- res[,2,]*sqrt(scales[2,2]) + scales[1,2]
res[,2,][res[,2,]<.Machine$double.eps] <- .Machine$double.eps
toc()

plot(test_y[,1], res[,1,1]); abline(a=0, b=1, col="green")
plot(test_y[,2], res[,2,1]); abline(a=0, b=1, col="green")

