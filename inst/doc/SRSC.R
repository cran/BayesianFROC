## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,comment ="#>",eval = F)

## ------------------------------------------------------------------------
#   dat <- list(
#  #Confidence level.
#  c = c(3,2,1),
#  
#  #Number of hits for each confidence level.
#  h = c(97,32,31),
#  
#  #Number of false alarms for each confidence level.
#  f = c(1,14,74),
#  
#  #Number of lesions
#  NL= 259,
#  
#  #Number of images
#  NI= 57,
#  
#  #Number of confidence level
#   C= 3
#  )

## ------------------------------------------------------------------------
#  c(3,2,1)

## ---- eval=F-------------------------------------------------------------
#  BayesianFROC::dataList.Chakra.1

## ----eval=FALSE----------------------------------------------------------
#  # I do not know why, but a computer cannot find Rcpp function. So I have to attach the package Rcpp. This is not desired one for me.
#  library(Rcpp)
#  
#  # Prepare dataset
#  dat <- BayesianFROC::dataList.Chakra.1 # data shown in the above example.
#  
#  #Fitting
#  fit <-BayesianFROC::fit_Bayesian_FROC(dat)

## ----echo=F--------------------------------------------------------------
#  BayesianFROC::summary_EAP_CI_srsc(fit)

