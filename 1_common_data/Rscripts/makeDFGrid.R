## helper functions to take the degrees of freedom into account
## when creating training parameters
makeDFGrid <- function(nLambda,  thetaGrid, rangeLambda = c(1e-5, 500),
                       dfRange=c(4,100), N=16){
    
    nTheta<- length( thetaGrid)
    nL = 300
    lGrid<- seq( log(rangeLambda[1]), log(rangeLambda[2]),
                length.out=nL)
    
    df<- matrix( NA,  nL, nTheta)
                                        #print( dim( df))
    for( j in 1:nTheta ){
        temp<- findDF( N, theta = thetaGrid[j], lGrid )
                                        #  print( length( temp))
        df[ , j] <- findDF( N, theta = thetaGrid[j], lGrid )
    }
    
    dfGrid<- seq( dfRange[1], dfRange[2], length.out= nLambda)
    
    logLambdaGrid<- matrix( NA, nLambda, nTheta )
    
    y<- lGrid
    
    for ( j in 1:nTheta){
        x<- log(df[,j])
        logLambdaGrid[,j] <- splint(x,y, log(dfGrid) )
    }
    
    return( 
        list( logLambdaGrid= logLambdaGrid,
             thetaGrid = thetaGrid,
             nLambda = nLambda, dfGrid= dfGrid )
    )
}

findDF<- function( N, theta, lGrid){
  uGrid<- 1:N
  vGrid<- uGrid
  uvGrid<- make.surface.grid( list( x=uGrid, y=vGrid) )
  nLambda<- length( lGrid)
  alphaAll  <-  exp( lGrid)/( 1 + exp( lGrid) )
  sigma<- stationary.cov( uvGrid, uvGrid,
                          Covariance= "Matern",
                          theta = theta, 
                          smoothness = 1.0)
  eigenSigma<- eigen( sigma, symmetric = TRUE)
  U<- eigenSigma$vectors
  D<- eigenSigma$values
  out<- rep( NA, nLambda)
  for( k in 1:nLambda){
    alpha<- alphaAll[k]
    out[k]   <-  sum( (1-alpha)*D/( (1-alpha)*D + alpha )  )
  }
  return( out)
}
