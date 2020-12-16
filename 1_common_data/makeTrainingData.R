## define the training parameter grid for NN models and
## pre-compute repeatedly use quantities

rm(list=ls())
library(fields)
library(parallel)
library(abind)
library(testthat)
library(reticulate)
source("Rscripts/prep_for_TFpython.R", chdir=TRUE)

prep <- prepare(N=16, nLambda=201, rangeTheta=c(.2,50), nTheta=200,
                dfRange=c(1, 255), rangeLambda = c(1e-6, 2000),
                path="npy/training_201_200")
