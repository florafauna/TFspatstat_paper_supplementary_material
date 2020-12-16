## print the summary of the model structure

rm(list=ls())
library(abind)
library(fields)
library(tictoc)
library(tensorflow)
library(keras)
library(reticulate); np <- import("numpy")


model <- load_model_tf("model_NV30")

summary(model)
