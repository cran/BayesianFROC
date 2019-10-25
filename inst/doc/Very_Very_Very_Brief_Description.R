## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE----------------------------------------------------------
#  library(BayesianFROC)
#  
#  
#                       fit_GUI_Shiny() #   Enjoy fitting!
#  
#  
#  

## ---- eval=FALSE---------------------------------------------------------
#  
#  dat <- list(c=c(3,2,1),    #Confidence level. Note that c is ignored.
#              h=c(97,32,31), #Number of hits for each confidence level
#              f=c(1,14,74),  #Number of false alarms for each confidence level
#  
#              NL=259,        #Number of lesions
#              NI=57,         #Number of images
#              C=3)           #Number of confidence level
#  
#  
#  
#  
#  #  where,
#  #      c denotes confidence level, i.e., rating of reader.
#  #                3 = Definitely deseased,
#  #                2 = subtle,.. deseased
#  #                1 = very subtle
#  #      h denotes number of hits (True Positives: TP) for each confidence level,
#  #      f denotes number of false alarms (False Positives: FP) for each confidence level,
#  #      NL denotes number of lesions,
#  #      NI denotes number of images,
#  
#  
#  
#  
#  
#  fit <- BayesianFROC::fit_Bayesian_FROC(  dataList = d )
#  
#  
#  # Posterior Predictive P value (PPP) for Chi square goodness of fit
#  
#                   ppp(fit)

## ----eval=FALSE----------------------------------------------------------
#  Simulation_Based_Calibration_single_reader_single_modality_via_rstan_sbc()

## ----eval=FALSE----------------------------------------------------------
#  fit <- fit_Bayesian_FROC( dataList = d )
#  
#  ppp(fit)

