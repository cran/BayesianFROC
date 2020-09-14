## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  # > viewdata(d)
#  # * Number of Lesions: 259
#  # * Number of Images : 57
#  # .                     Confidence.Level   False.Positives   True.Positives
#  # -------------------  -----------------  ----------------  ---------------
#  # Obviouly present                     3                 1               97
#  # Relatively obvious                   2                14               32
#  # Subtle                               1                74               31
#  
#  
#  
#       fit_a_model_to(d) # Here, a model is fitted to the data "d"
#  
#  
#  
#  
#  # Or GUI by Shiny
#  
#  fit_GUI_Shiny()

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

