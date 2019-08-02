## ----setup, include = FALSE----------------------------------------------
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

## ------------------------------------------------------------------------
#1) Build  data for singler reader and single modality  case.





  dataList <- list(
               c=c(3,2,1),     # c is ignored, can omit.
               h=c(97,32,31),
               f=c(1,14,74),
               NL=259,
               NI=57,
               C=3
               )





#  where,
#        c denotes confidence level, each components indicates that 
#                3 = Definitely lesion,
#                2 = subtle,  
#                1 = very subtle
#        h denotes number of hits 
#          (True Positives: TP) for each confidence level,
#        f denotes number of false alarms
#          (False Positives: FP) for each confidence level,
#        NL denotes number of lesions (signal),
#        NI denotes number of images,






BayesianFROC::viewdata(dataList)

## ---- eval = FALSE-------------------------------------------------------
#  #1) Build  Data
#  
#  #   For singler reader and single modality  case.
#  
#  
#  
#  
#  
#    dataList <- list(
#                 c=c(3,2,1),     # c is ignored, can omit.
#                 h=c(97,32,31),  # Changing these number, please enjoy fitting
#                 f=c(1,14,74),
#                 NL=259,
#                 NI=57,
#                 C=3
#                 )
#  
#  
#  
#  
#  #  where,
#  #          c  denotes confidence level, each components indicates that
#  #                3 = Definitely lesion,
#  #                2 = subtle,
#  #                1 = very subtle
#  #          h  denotes number of hits
#  #              (True Positives: TP) for each confidence level,
#  #          f  denotes number of false alarms
#  #              (False Positives: FP) for each confidence level,
#  #          NL  denotes number of lesions (signal),
#  #          NI  denotes number of images,
#  
#  
#  
#  
#  
#  
#  
#  
#  
#  #2)   Fit the FROC model.
#  
#  
#  
#                    fit <- BayesianFROC::fit_Bayesian_FROC(dataList)
#  
#  
#  
#  
#  
#  
#  #3)   Validation of Fit
#  
#  
#  #     via alculation of p -value of the chi square goodness of fit, which is
#  #     calculated by integrating with  predictive posterior measure.
#  
#  
#      p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit(fit)
#  
#  
#  
#  
#  

## ----eval=FALSE----------------------------------------------------------
#  BayesianFROC::dataList.Chakra.Web

## ------------------------------------------------------------------------
BayesianFROC::viewdata(BayesianFROC::dd,head.only = TRUE)

## ----eval=FALSE----------------------------------------------------------
#  #An example dataset for the case of Multiple readers and Multiple Modalities.
#  dat  <- BayesianFROC::dataList.Chakra.Web

## ----eval=FALSE----------------------------------------------------------
#  # Fitting  the hierarchical Bayesian model.
#  fit  <-  BayesianFROC::fit_Bayesian_FROC(dat)
#  

## ----eval=FALSE----------------------------------------------------------
#   #Draw curves for the 1st modality and 2nd reader
#   DrawCurves(
#  
#     # fitted model object
#          fit,
#  
#     # Modatity ID whose curves are drawn.
#        modalityID =1,
#  
#     # Reader ID whose curves are drawn.
#        readerID   =2)

## ----eval=FALSE----------------------------------------------------------
#  fit <- fit_Bayesian_FROC( d, ModifiedPoisson = TRUE)

## ----eval=FALSE----------------------------------------------------------
#  fit <- fit_Bayesian_FROC(
#  
#    #MCMC iteration
#    ite  = 1111,
#  
#    #verbose summary or not
#    summary = FALSE,
#  
#    # number of MCMC chains
#    cha = 1,
#  
#    # Here we select the null model !!
#    Null.Hypothesis = TRUE,
#  
#    # example MRMC dataset
#    dataList = dd
#    )

## ----eval=FALSE----------------------------------------------------------
#  fit <- fit_Bayesian_FROC(
#  
#    #MCMC iteration
#    ite  = 1111,
#  
#    #verbose summary or not
#    summary = FALSE,
#  
#    # number of MCMC chains
#    cha = 1,
#  
#    # Here we select the aleternative model !!
#    Null.Hypothesis = FALSE,
#  
#    # example MRMC dataset
#    dataList = dd
#    )

## ----eval=FALSE----------------------------------------------------------
#  #An example dataset for the case of Multiple readers and Multiple Modalities.
#  data  <- BayesianFROC::dataList.Chakra.Web
#  
#  
#  
#  
#  # Test the null hypothesis
#  
#  Test_Null_Hypothesis_that_all_modalities_are_same(data,ite = 1111)
#  
#  # where ite is the iteration of MCMC to create two stanfit object, one is the null model and the another is the alternative model.
#  

## ----eval=FALSE----------------------------------------------------------
#  
#  
#  # method='WARP' if another algorithm (WARP-III) you want.
#  # silent is whether calculation are shown
#  
#  
#  # Null model
#  H0 <- bridgesampling::bridge_sampler(fitH0, method = "normal", silent = TRUE)
#  print(H0)
#  
#  # Alternative model
#  H1 <- bridgesampling::bridge_sampler(fitH1, method = "normal", silent = TRUE)
#  print(H1)
#  
#  # Test the Null hypothesis that all modalities are same.
#  BF10 <- bridgesampling::bf(H1, H0)
#  print(BF10)
#  
#  # If the number is greater, then we reject H0 with more confidence.

## ----eval=FALSE----------------------------------------------------------
#  fit <- Simulation_Based_Calibration_single_reader_single_modality_via_rstan_sbc()
#  
#  
#  
#  plot(fit, bins = 10) # it is best to specify the bins argument yourself
#  plot(fit, bins = 100) # it is best to specify the bins argument yourself
#  plot(fit, bins = 200) # it is best to specify the bins argument yourself
#  
#  
#  

## ----eval=FALSE----------------------------------------------------------
#  
#  #     1) Build the data for singler reader and single modality  case.
#  
#     dat <- list(
#              c=c(3,2,1),    #  Confidence level, which is ignored.
#              h=c(97,32,31), #  Number of hits for each confidence level
#              f=c(1,14,74),  #  Number of false alarms for each confidence level
#  
#              NL=259,       #   Number of lesions
#              NI=57,        #   Number of images
#              C=3)          #   Number of confidence level
#  
#  
#  
#  
#  #  where,
#  #        c denotes confidence level, , each components indicates that
#  #                3 = Definitely lesion,
#  #                2 = subtle,
#  #                1 = very subtle
#  #          That is the high number indicates the high confidence level.
#  #        h denotes number of hits
#  #          (True Positives: TP) for each confidence level,
#  #        f denotes number of false alarms
#  #          (False Positives: FP) for each confidence level,
#  #        NL denotes number of lesions,
#  #        NI denotes number of images,
#  
#  
#  #     2) Fit  and draw FROC and AFROC curves.
#  
#  
#  
#  
#             fit <-   fit_Bayesian_FROC(dat, DrawCurve = TRUE)
#  
#  
#  #     3) Extract a chi square statistics (not p value)
#  
#            chi_square_goodness_of_fit  <-    fit@chisquare
#  
#  
#  #     4) print a chi square statistics (not p value)
#  
#            print(chi_square_goodness_of_fit)
#  
#  
#  

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
#  #1) Build  data for singler reader and single modality  case.
#  
#  
#  
#  
#  
#   dataList <- list(
#                c=c(3,2,1),     # c is ignored, can omit.
#                h=c(97,32,31),
#                f=c(1,14,74),
#                NL=259,
#                NI=57,
#                C=3
#                )
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
#  
#  
#  
#  
#  
#  # Make a Fitted Model Object of class stanfitExtended inherited from stanfit
#  
#  
#  fit <-    fit_Bayesian_FROC(  dataList )
#  
#  
#  
#  # Calculate the p value, obtained as integral using posterior predictive measure.
#  
#  p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit(fit)
#  
#  

