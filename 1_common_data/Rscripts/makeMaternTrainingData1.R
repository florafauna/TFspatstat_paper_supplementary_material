## simulate training images
source("makeDFGrid.R")
makeMaternTrainingData1<- function(      N = 16,
                               nLambda = 10,
                               dfRange=c(4,100),
                               rangeLambda = c(1e-5, 500),
                               rangeTheta  = c( 2, 25),
                               nTheta  = 10,
                                 iSeed = 222,
                                     m = 200,
                           standardize = FALSE){
  uGrid<- 1:N
  nObs<-N^2
  
  vGrid<- uGrid
  uvGrid<- make.surface.grid( list( x=uGrid, y=vGrid) )
  #bigD<- rdist( uvGrid)
  thetaGrid <-  seq( (rangeTheta[1]), (rangeTheta[2]),
                         length.out=nTheta)
  
  # grid of lambda that vay for each theta
  # this is a  nLambda by nTheta matrix
  objTemp <- makeDFGrid( nLambda=nLambda, thetaGrid=thetaGrid, rangeLambda=rangeLambda,
                        dfRange=dfRange, N=16)
  logLambdaGrid <- objTemp$logLambdaGrid
         dfGrid <- objTemp$dfGrid

  nAll<- nLambda* nTheta*m
  xAll<- array( NA, c( nAll, N,N,1))
  yAll<- NULL
  dfAll<- NULL
  
  set.seed(iSeed)
  kk<- 1
 
  for(  j  in 1: nTheta){
      
      sigma<- stationary.cov( uvGrid, uvGrid,
                                Covariance= "Matern",
                                theta = thetaGrid[j], 
                                smoothness = 1.0)
      sigma_Chol<- t(chol(sigma))
      
      nChunk <- m*nLambda
      
      bigG   <- (sigma_Chol%*%
               matrix( rnorm(nChunk*nObs), nObs, nChunk)) 
      bigG<- array( bigG, c(N,N,nChunk,1) )
      bigG<- aperm( bigG, c( 3,1,2,4))
      logLambdaAll<- rep( logLambdaGrid[,j], m)
      alphaAll<- exp( logLambdaAll )/( 1 + exp( logLambdaAll) )
# loop over lambdas/alphas for fixed theta  
      for( k in 1: (nChunk) ){
        alpha<- alphaAll[k]
        nugget <- matrix( rnorm( nObs), N,N)
        tmp<- bigG[k,,,1]*sqrt( 1-alpha ) +
                          sqrt( alpha)*nugget
 #       image( tmp, col=tim.colors())
        xAll[kk,,,1]<- tmp
        kk<- kk + 1
      }
      #print( nChunk)
      yAll<- rbind(   yAll, 
        cbind( logLambdaAll, rep( thetaGrid[j], nChunk ) )
               )
      dfAll<- c( dfAll,  rep( dfGrid, m))
   }
 
  return( 
    list(       xAll = xAll, 
                yAll = yAll,
       logLambdaGrid = logLambdaGrid,
           thetaGrid = thetaGrid,
          rangeTheta = rangeTheta,
                   N = N,
                  uv = uvGrid,
              dfGrid = dfGrid,
               dfAll = dfAll
         )
     )
}
