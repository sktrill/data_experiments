library(tidyr, warn.conflicts=FALSE, quietly=TRUE)
library(readr, warn.conflicts=FALSE, quietly=TRUE)
library(dplyr, warn.conflicts=FALSE, quietly=TRUE)
# neural net packages
library(nnet, warn.conflicts=FALSE, quietly=TRUE)
library(neuralnet, warn.conflicts=FALSE, quietly=TRUE)
library(tensorflow, warn.conflicts=FALSE, quietly=TRUE)
library(keras, warn.conflicts=FALSE, quietly=TRUE)


nnet(State ~ ., data=data[,-1], decay=5e-4, maxit=200)

