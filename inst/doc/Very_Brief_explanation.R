## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

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

