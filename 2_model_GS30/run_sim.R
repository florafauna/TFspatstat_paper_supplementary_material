rm(list=ls())
library(abind)
library(doParallel)
registerDoParallel(12)
library(fields)
library(reticulate); np <- import("numpy")
library(parallel)
library(gstat)
library(sp)
test_x <- np$load("../common_data/npy/test_x.npy")
test_y <- np$load("../common_data/npy/test_y.npy")

im2vg <- function(im   # one image
                  ){
    
    ## convert one image to gstat 'variogram'
    df <- data.frame(z=c(im), x1=1:16, x2=rep(1:16, each=16))
    coordinates(df) <- ~ x1 + x2
    vg <- variogram(z~1,data=df, cutoff=25)
    vg
}
codetools::findGlobals(im2vg, merge=FALSE)$variables

getMeanVg <- function(im # 30 images
                      ){
    ## get the 'mean' variogram of 30 images
    vgs <- lapply(1:30, function(i) im2vg(im[,,i]))
    vg <- vgs[[1]]
    vg$gamma <- apply(do.call("cbind", lapply(vgs, function(x) x$gamma)), 1, mean)
    vg
}
codetools::findGlobals(getMeanVg, merge=FALSE)$variables

start_values <- test_y[sample.int(nrow(test_y), size=nrow(test_y), replace=TRUE), ]

system.time(
fit <- foreach(i = 1:dim(test_x)[1], .combine="rbind") %dopar% {
    if(i %% 100==0)
        cat(".")
    vg <- getMeanVg(test_x[i,,,])
    fit <- try(fit.variogram(vg,
                             vgm(psill=1, model="Mat", range=start_values[i,2],
                                 nugget=exp(start_values[i,1]), kappa=1),
                         fit.sills=c(TRUE, FALSE) # fix psill=1 
                         ), silent=TRUE)
    if(is(fit, "try-error")){
        o <- c(NA,NA)
    } else
        o <- c(fit[1,2], fit[2,3])
    o
})
##     user   system  elapsed 
## 4032.873   16.214 1361.184 
any(!is.finite(fit))

dir.create("npy", showWarnings=FALSE)
np$save("npy/test_pred_model_gstat.npy", r_to_py(fit))


system.time(
fit_one <- foreach(i = 1:dim(test_x)[1], .combine="rbind") %dopar% {
    if(i %% 100==0)
        cat(".")
    vg <- im2vg(test_x[i,,,1])
    fit <- try(fit.variogram(vg,
                             vgm(psill=1, model="Mat", range=start_values[i,2],
                                 nugget=exp(start_values[i,1]), kappa=1),
                         fit.sills=c(TRUE, FALSE) # fix psill=1 
                         ), silent=TRUE)
    if(is(fit, "try-error")){
        o <- c(NA,NA)
    } else
        o <- c(fit[1,2], fit[2,3])
    o
})
##     user   system  elapsed 
## 4032.873   16.214 1361.184 
any(!is.finite(fit))

dir.create("npy", showWarnings=FALSE)
np$save("npy/test_pred_model_gstat_one.npy", r_to_py(fit_one))


