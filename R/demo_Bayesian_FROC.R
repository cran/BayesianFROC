#' @title demonstration
#' @description demonstration
#' @details The author often forget the R script for execute the demos or bother to write the code to execute demo, thus I made this.
#' @return none
#' @export
#'
#' @examples
#'  \donttest{
#'
#'
#'  demo_Bayesian_FROC()
#'
#' # 2019.05.21 Revised.

#'}# dottest

demo_Bayesian_FROC <- function(){



  # 1) First, we prepare the dataset for a single reader and a single modality.




  viewdata(BayesianFROC::dataList.Chakra.1.with.explantation)

  pause()#1 /10  ----  we fit our model to the above data


  fit <- fit_Bayesian_FROC( ite  = 111, summary = TRUE,  cha=3,  dataList = BayesianFROC::dataList.Chakra.1,new.imaging.device = TRUE,DrawAFROCcurve = TRUE )




  pause()#2 /10  ---- Examine the bias

  a <-error_srsc(
    NLvector = c(100,10000000,10000000),
    ite = 555
  )
  pause()#3 /10  ----


  BayesianFROC::error_srsc_error_visualization(a)

  pause()#4 /10  ----

  BayesianFROC::error_srsc_variance_visualization(a)

  pause()#4 /10  ----  p-value  -------
  ppp(fit)

  pause()#4 /10  ---- Bi normal assumption ----- High AUC--------



  viewdata(BayesianFROC::dataList.High)


  pause()#5 /10  ----- Fitting (in the high ability case).

  fit.High <- fit_Bayesian_FROC(BayesianFROC::dataList.High,ite  = 1111)


  pause()#6 /10  ----- draw a bi normal assumption----- High AUC--------

  draw_latent_signal_distribution(fit.High)

  pause()#7 /10  ---- Bi normal assumption ----- Low AUC--------


  viewdata(BayesianFROC::dataList.Low)
  pause()#8 /10  ----- Fitting (in the low ability case).

  fit.Low <- fit_Bayesian_FROC(BayesianFROC::dataList.Low,ite  = 1111)
  pause()#9 /10  ----- draw a bi normal assumption----- Low AUC--------

  draw_latent_signal_distribution(fit.Low)

  # Demo for srsc finished !!




  pause()
  viewdata(BayesianFROC::dataList.Chakra.Web)
  pause()
  fit <- fit_Bayesian_FROC(BayesianFROC::dataList.Chakra.Web,ite = 1000,summary = TRUE,PreciseLogLikelihood = TRUE)
  pause()
  DrawCurves_MRMC_pairwise(fit)
  pause()
  DrawCurves_MRMC_pairwise(fit,modalityID = c(1,2),readerID = c(1))
  pause()
  DrawCurves_MRMC_pairwise(fit,modalityID = c(3,4),readerID = c(1))
  pause()
  DrawCurves_MRMC_pairwise(fit,modalityID = c(2,4),readerID = c(3))
  pause()
  DrawCurves_MRMC_pairwise(fit,modalityID = c(3,1),readerID = c(1,2,3,4))


message(  "\n\n\n\n* See also the following demos;

    +--- R scripts -----------------------------------------------------+
    | * demo(demo_MRMC,package=\"BayesianFROC\")()                      |
    | * demo(demo_srsc,package=\"BayesianFROC\")()                      |
    | * demo(demo_drawcurves_srsc,package=\"BayesianFROC\")()           |
    | * demo(demo_stan,package=\"BayesianFROC\")                        |
    L-------------------------------------------------------------------+


    * From the above,

      ** Demo for Multiple reader and multiple case (MRMC) data
      ** Demo for a single reader and a single modality
      ** demo to show how to draw curves
      ** demo to show how to use function of package rstan to fitted model object obtained in this package.
" )


message(  "See also the following demos;

    +-------------------------------------------------------------------+
    | *  demo  finished !!                                              |
    L-------------------------------------------------------------------+

       2019.05.21 Revised.

" )


}# function
