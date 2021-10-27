## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  
#  # Installation
#  install.packages("BayesianFROC", dependencies = TRUE)
#  
#  # Load the package
#  library(BayesianFROC);
#  
#  # Run the function
#  BayesianFROC::fit_GUI_Shiny() # Variables are not required.
#  
#  #The above three are R codes,
#  #so please execute them on R console or R-studio console.
#  

## ----eval=FALSE---------------------------------------------------------------
#  library("BayesianFROC")
#  fit_GUI_Shiny()

## ----eval=FALSE---------------------------------------------------------------
#  library("BayesianFROC")
#  # Make an ROC dataset
#  d<-BayesianFROC::ROC_data_creator2()
#  
#  # Fit the above bad ROC model
#  f<-BayesianFROC::fit_srsc_ROC(d,ite  = 111, summary = FALSE,  cha = 1,)
#  
#  # Check the model. The result indicate MCMC does not converge.
#  rstan::check_hmc_diagnostics(f)
#  
#  # Plot looks bad so BAD MODEL...
#  rstan::traceplot(f)
#  
#  # Plot looks good but BAD MODEL...
#  BayesianFROC::draw_ROC_Curve_from_fitted_model(f)
#  BayesianFROC::fit_GUI_ROC() #FROC user interface is used, so under construction...

## ----eval=FALSE---------------------------------------------------------------
#  
#  
#  
#  fit <- fit_Bayesian_FROC(
#     ite  = 1111,
#     cha = 1,
#     summary = TRUE,
#     Null.Hypothesis = F,
#     dataList = dd # example data to be fitted a model
#     )
#  
#  
#  # Or GUI by Shiny
#  
#  fit_GUI_Shiny_MRMC()
#  

## ----eval=FALSE---------------------------------------------------------------
#  
#  stanModel <- stan_model_of_sbc()
#  
#  Simulation_Based_Calibration_single_reader_single_modality_via_rstan_sbc(
#  stanModel = stanModel,
#  ite     = 233,
#  M       = 111, # or 1111 which tooks a lot of times
#  epsilon = 0.04,BBB = 1.1,AAA =0.0091,sbc_from_rstan = TRUE)
#  
#  # or
#  
#  stanModel <- stan_model_of_sbc()
#  
#  Simulation_Based_Calibration_single_reader_single_modality_via_rstan_sbc(
#  stanModel = stanModel,
#  ite     = 233,
#  M       = 1111,
#  epsilon = 0.04,BBB = 1.1,AAA =0.0091,sbc_from_rstan = TRUE)
#  

