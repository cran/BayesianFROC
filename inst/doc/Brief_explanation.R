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

## ---- eval = FALSE-------------------------------------------------------
#  
#  #0) To avoid the following error in Readme file, I have to attach the Rcpp. I do not know why such error occur withou Rcpp. This error occurs only when I run the following R scripts from readme.
#  
#  #Error
#  #in do.call(rbind,sampler_params) :second argument must be a list Calles:<Anonymous>...get_divergent_iterations ->sampler_param_vector =. do.call Execution halted
#  
#  
#  
#  
#   library(Rcpp)  # This code can remove the above unknown error, if someone know why the error occur, please tell me.
#  
#  
#  
#  
#  #1) Build  data for singler reader and single modality  case.
#  
#  
#  
#  
#    dataList <- list(c=c(3,2,1),     # c is ignored, can omit.
#                h=c(97,32,31),
#                f=c(1,14,74),
#                NL=259,
#                NI=57,
#                C=3)
#  
#  
#  
#  
#  
#  #  where,
#  #        c denotes confidence level, each components indicates that
#  #                3 = Definitely lesion,
#  #                2 = subtle,
#  #                1 = very subtle
#  #        h denotes number of hits
#  #          (True Positives: TP) for each confidence level,
#  #        f denotes number of false alarms
#  #          (False Positives: FP) for each confidence level,
#  #        NL denotes number of lesions (signal),
#  #        NI denotes number of images,
#  
#  
#  #2) Fit the FROC model.
#  
#  
#  
#                    fit <- BayesianFROC::fit_Bayesian_FROC(
#  
#  
#  
#                      dataList,
#  
#              #The number of MCMC chains
#                      cha = 4
#                                                           )
#  
#  
#  
#  
#  #  validation of fit via alculation of p -value of the chi square goodness of fit, which is
#  #  calculated by integrating with  predictive posterior measure.
#  
#  
#                      BayesianFROC::p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit(fit, head.only = TRUE)
#  
#  
#  
#  
#  

## ----echo=F--------------------------------------------------------------
#  BayesianFROC::summary_EAP_CI_srsc(fit)

## ----eval=FALSE----------------------------------------------------------
#  #An example dataset for the case of Multiple readers and Multiple Modalities.
#  dat <- BayesianFROC::dataList.Chakra.Web

## ----eval=FALSE----------------------------------------------------------
#  # Fitting for your data with respect to the hierarchical Bayesian model.
#  fit<-BayesianFROC::fit_Bayesian_FROC(dat)
#  

## ----eval=FALSE----------------------------------------------------------
#   #Draw curves for the 1st modality and 2nd reader
#   DrawCurves(
#  
#     #This is estimates
#          fit,
#  
#     # Modatity ID whose curves are drawn.
#        modalityID =1,
#  
#     # Reader ID whose curves are drawn.
#        readerID   =2)

## ------------------------------------------------------------------------
#  BayesianFROC::dataList.Chakra.Web

## ------------------------------------------------------------------------
#  BayesianFROC:::viewdata(BayesianFROC::dataList.Chakra.Web)

## ----eval=FALSE----------------------------------------------------------
#  # I do not why, but Machine cannot find some function in Rcpp. So I have to load the package Rcpp.
#  library(Rcpp)
#  
#  # Prepare dataset
#  dat <- BayesianFROC::dataList.Chakra.Web
#  
#  #Fitting
#  fit <-BayesianFROC::fit_Bayesian_FROC(dat)

## ----eval=FALSE----------------------------------------------------------
#  library(Rcpp)
#  
#  
#  
#  # Prepare a dataset
#  dat <- BayesianFROC::dataList.Chakra.Web
#  
#  
#  # Fitting
#  fit <- BayesianFROC::fit_Bayesian_FROC(dat)
#  
#  
#  
#  
#  #    Draw curves for the 1st modality and 2nd reader
#   DrawCurves(
#  
#  #    This is the above fitted model object
#          fit,
#  
#  #    Specify Modatity ID by vector whose curve are drawn.
#        modalityID =1,
#  
#  #    Specify readerID ID by vector whose curve are drawn.
#        readerID   =2)

