## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,comment ="#>",eval = F)

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

