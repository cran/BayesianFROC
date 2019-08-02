## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE----------------------------------------------------------
#  library(BayesianFROC)
#            BayesianFROC::fit_GUI() #   Enjoy fitting!
#  

## ----eval=FALSE----------------------------------------------------------
#  library(BayesianFROC)
#            fit_GUI_dashboard() #   Enjoy fitting!
#  

## ----eval=FALSE----------------------------------------------------------
#  library(BayesianFROC)
#             fit_GUI_simple() #   Enjoy fitting!
#  

## ---- eval=FALSE---------------------------------------------------------
#  d   <- BayesianFROC::d
#  fit <- fit_Bayesian_FROC(  dataList = d )

## ---- eval=FALSE---------------------------------------------------------
#  d   <- BayesianFROC::d
#  fit <- fit_Bayesian_FROC(  dataList = d ,ModifiedPoisson = TRUE)

## ----eval=FALSE----------------------------------------------------------
#  dd  < -BayesianFROC::dd
#  fit <- fit_Bayesian_FROC(  dataList = dd )
#  

## ----eval=FALSE----------------------------------------------------------
#  fit <- fit_Bayesian_FROC(
#  
#    # Here we select the null model !!
#    Null.Hypothesis = TRUE,
#  
#    # example MRMC dataset
#    dataList = dd
#    )

