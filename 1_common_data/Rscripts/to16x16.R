## function to extract 16 x 16 pixel subsets from data

library(fields)
to16x16 <- function(x, channel=1){
    stopifnot(is.numeric(x), is.array(x), length(dim(x))==3,
              length(channel)==1)
    centers <- make.surface.grid(list(x=1:(dim(x)[1]-15), y=1:(dim(x)[2]-15)))
    out <- array(NA, c(nrow(centers), 16, 16, 1))
    seq15 <- 0:15
    for(k in 1:nrow(centers))
        out[k,,,1]<- x[centers[k,1]+seq15,centers[k,2]+seq15,channel]
    out
}
codetools::findGlobals(to16x16, merge=FALSE)$variables
