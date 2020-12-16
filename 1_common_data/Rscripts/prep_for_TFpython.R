## pre-compute quantities used to simulate new training data in TensorFlow

source("makeDFGrid.R")
prepare <- function(N = 16,
                    nLambda = 10,
                    rangeLambda = c(1e-5, 500),
                    dfRange = c(4, 100),
                    rangeTheta  = c( 2, 25),
                    nTheta  = 11,
                    path=NULL){
    uvGrid <- make.surface.grid(list(x=1:N, y=1:N))
    thetaGrid <- seq(rangeTheta[1], rangeTheta[2], length.out=nTheta)
  
    ## grid of lambda that vay for each theta
    ## this is a  nLambda by nTheta matrix
    objTemp <- makeDFGrid(nLambda=nLambda, thetaGrid=thetaGrid,
                          dfRange=dfRange, rangeLambda=rangeLambda, N=N)
    logLambdaMat <- objTemp$logLambdaGrid
    dfGrid <- objTemp$dfGrid

    alphaMat <- exp(logLambdaMat)/ (1 + exp(logLambdaMat))
    y <- cbind(logLambda=c(logLambdaMat), theta=rep(thetaGrid, each=nrow(logLambdaMat)))
    
    chols <- lapply(seq_along(thetaGrid), function(i){
        sigma <- stationary.cov(uvGrid, uvGrid,
                                Covariance= "Matern",
                                theta = thetaGrid[i], 
                                smoothness = 1.0)
        t(chol(sigma))
    })
    chols <- do.call("abind", c(chols, along=3))
    dimnames(chols) <- NULL

    if(!is.null(path)){
        dir.create(dirname(path), showWarnings=FALSE)
        np = import("numpy")
        np$save(paste0(path,"_chols", ".npy"), r_to_py(chols))
        np$save(paste0(path,"_logLambdaMat", ".npy"), r_to_py(logLambdaMat))
        np$save(paste0(path,"_alphaMat", ".npy"), r_to_py(alphaMat))
        np$save(paste0(path,"_thetaGrid", ".npy"), r_to_py(thetaGrid))
        np$save(paste0(path,"_y", ".npy"), r_to_py(y))
        np$save(paste0(path,"_df", ".npy"), r_to_py(objTemp$dfGrid))
    }
    
    list(chols = chols,  #one Chol for each theta
         alphaMat  = alphaMat,
         thetaGrid = thetaGrid, 
         y = y)
}
codetools::findGlobals(prepare, merge=FALSE)$variables
