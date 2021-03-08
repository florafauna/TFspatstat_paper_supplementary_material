rm(list=ls())
library(fields)
library(CompRandFld)
library(reticulate); np <- import("numpy")
library(doParallel); registerDoParallel(24)
library(sp)
library(gapfill)
library(abind)
library(spam)
test_x <- np$load("../common_data/npy/test_x.npy")
test_y <- np$load("../common_data/npy/test_y.npy")

coords <- cbind(1:16, rep(1:16, each=16))
data <- test_x
dim(data) <- c(dim(data)[1], dim(data)[2]*dim(data)[3], dim(data)[4])
data <- aperm(data, c(1,3,2))

start_values <- test_y[sample.int(nrow(test_y), size=dim(data)[1], replace=TRUE), ]


system.time(
    fit <- foreach(i = 1:dim(data)[1], .combine="rbind") %dopar% {
    if(i %% 1==0)
        cat(".")
  
    fit <- try(FitComposite(data=data[i,,],
                            coordx=coords,
                            corrmodel="matern",
                            maxdist=25,
                            likelihood="Marginal",
                            type="Pairwise", #"Difference",
                            varest=FALSE,
                            start=list(nugget=exp(start_values[i,1]),
                                       scale=start_values[i,2]),
                            fixed=list(mean=0, sill=1, smooth=1),
                            replicates=30))
    if(is(fit, "try-error"))
        o <- c(NA,NA)
    else
        o <- c(lambda=fit$param[1], theta=fit$param[2])
    o
})

par(mfrow=c(1,2))
plot(test_y[,1], log(fit[,1])-test_y[,1], main="log lambda");
abline(a=0, b=0, col="green")
plot(test_y[,2], fit[,2]-test_y[,2], main="theta, sigma^2=1");
abline(a=0, b=0, col="green")


dir.create("npy", showWarnings=FALSE)
np$save("npy/test_pred_model_composite.npy", r_to_py(fit))
