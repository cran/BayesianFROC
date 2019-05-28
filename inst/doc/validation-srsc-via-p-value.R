## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE----------------------------------------------------------
#  # 1) First, attach a package stringr to use pipe operator %>%
#  library(stringr)
#  
#  # 2) Prepare data
#  data <- dataList.Chakra.1
#  
#  # 3) Using the pipe operator we can say the work follow code by only 1 row.
#  data %>%  fit_Bayesian_FROC() %>% p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit()
#  

## ----eval=FALSE----------------------------------------------------------
#  
#  # 1) Prepare a dataset
#  dat <- BayesianFROC::dataList.Chakra.1
#  
#  
#  # 2) Fitting
#  fit <- BayesianFROC::fit_Bayesian_FROC(dat)
#  
#  
#  # 3) Calculation of the P value
#  p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit(fit)
#  

## ----eval=FALSE----------------------------------------------------------
#  
#  |the 7984-th |                          6.440|                            12.00|TRUE                    |
#  |the 7985-th |                          0.857|                            13.00|TRUE                    |
#  |the 7986-th |                          1.670|                            13.50|TRUE                    |
#  |the 7987-th |                          3.880|                            12.20|TRUE                    |
#  |the 7988-th |                          1.300|                            16.20|TRUE                    |
#  |the 7989-th |                          6.190|                            11.40|TRUE                    |
#  |the 7990-th |                         10.900|                            12.60|TRUE                    |
#  |the 7991-th |                          3.360|                            12.80|TRUE                    |
#  |the 7992-th |                          5.110|                            14.50|TRUE                    |
#  |the 7993-th |                          5.530|                            12.90|TRUE                    |
#  |the 7994-th |                          4.430|                            13.00|TRUE                    |
#  |the 7995-th |                          3.570|                            14.70|TRUE                    |
#  |the 7996-th |                          9.890|                            17.70|TRUE                    |
#  |the 7997-th |                         72.200|                             9.40|FALSE                   |
#  |the 7998-th |                          2.140|                            10.40|TRUE                    |
#  |the 7999-th |                          7.750|                             9.95|TRUE                    |
#  |the 8000-th |                          3.310|                            15.20|TRUE                    |
#  
#  *  Note that the posterior predictive p value is a rate of TRUE in the right column in the above table.
#  
#  
#   The p value of the posterior predictive measure for the chi square discrepancy.
#                                                                          0.916375

## ----eval=FALSE----------------------------------------------------------
#  
#  
#  fit <- fit_Bayesian_FROC( ite  = 31, summary = FALSE,  cha = 1, dataList = dataList.Chakra.1 )
#  p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit(fit)
#  
#  
#  
#  fit <- fit_Bayesian_FROC( ite  = 3111, summary = FALSE,  cha = 1, dataList = dataList.Chakra.1 )
#  p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit(fit)
#  

