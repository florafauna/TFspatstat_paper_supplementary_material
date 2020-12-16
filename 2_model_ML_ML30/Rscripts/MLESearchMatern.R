## compute the likelihood from 16 x 16 pixel images
## assumes a GP with a Matern covariance model


library(fields)
MLESearchMatern<- function(x,
logLambdaGrid = seq( log(1e-6), log(10), length.out=100),
logThetaGrid  = seq( log( 2),   log(25), length.out=40),                    
printIndex=1){
      ## this is a  nLambda by nTheta matrix
    if(!is.matrix(logLambdaGrid))
        logLambdaGrid <- matrix(logLambdaGrid,
                                nrow=length(logLambdaGrid),
                                ncol=length(logThetaGrid), byrow=TRUE)
  xDim<- dim(x)
  nAll<- xDim[1]
  N<- xDim[2]
  nObs<- N^2
  
  thetaGrid<-  exp( logThetaGrid)
  nTheta<-  length( logThetaGrid)
  nLambda<- nrow( logLambdaGrid) 

  stopifnot(ncol(logLambdaGrid) == nTheta)
    
  # matrix of log likelihood values 
  llike<- array(NA, c( nTheta, nLambda, nAll) )
  for( j in 1:nTheta ){
      if(printIndex==1)
          cat("|")  
  uGrid<- 1:N
  vGrid<- uGrid
  uvGrid<- make.surface.grid( list( x=uGrid, y=vGrid) )
  sigma<- stationary.cov( uvGrid, uvGrid,
                          Covariance= "Matern",
                          theta = thetaGrid[j], 
                          smoothness = 1.0)
  eigenSigma<- eigen( sigma, symmetric = TRUE)
  U<- eigenSigma$vectors
  D<- eigenSigma$values
 
  #xBig<- array( x, c(dimX[1], nObs) )
  
  xU<- array(x, c(nAll, nObs) )%*%U
  xSpec<- t(xU^2)
  for( k in 1: nLambda){
    logLambda<- logLambdaGrid[k,j]
    alpha<- exp(logLambda)/( 1 + exp(logLambda))
    DL<- 1/( (1-alpha)*D + alpha ) 
    quadForm<-  colSums( (xSpec*DL) )
#  test<-  -.5* colSums(t(xBig)*solve( object$sigma + diag( exp(logLambda), nObs) ) %*% t(xBig))
    logDet<- sum( log(DL) )
    llike[j,k, ] <-  -.5*quadForm + .5*logDet            
  }
  }
  cat( fill=TRUE)
  index<- t( apply( llike, c(3), which.max.matrix) )
  
  MLE<- cbind(  logLambdaGrid[index[,2:1]],logThetaGrid[index[,1]])
  dimnames(MLE)<- list(NULL, c("logLambda", "logTheta") )
    out<- list( MLE= MLE,
               llike=llike,
              logLambdaGrid = logLambdaGrid,
               logThetaGrid = logThetaGrid
#              ,
#                    logLike = list( x=logThetaGrid,
#                                    y=logLambdaGrid,
#                                    z=llike )
             )

  return(out)
}
codetools::findGlobals(MLESearchMatern, merge=FALSE)$variables
