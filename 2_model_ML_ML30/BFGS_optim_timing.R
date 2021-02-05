## get timings for ML estimates found using the BFGS optimizer

library( fields) # v 12.0
set.seed(111)
s<- matrix( runif( 256*2), 256,2)

Sigma<- stationary.cov( s,s, aRange=.2,Covariance="Matern",
                        smoothness=1.0)
u<- rnorm( 256)
e<- .2*rnorm(256)
g<- t( chol( Sigma))%*%u
y<-  g + e

quilt.plot(s, g )

system.time( obj<- spatialProcess(s,y , gridN=15)
           )

system.time( obj<- spatialProcess(s,y, gridN=8))


system.time( obj<- spatialProcess(s,y,
                      cov.params.start= list( lambda=.5, aRange =.1)
                                  )
)
