#' @title demonstration without pause
#' @description demonstration without pause
#' @return none
#' @export
#'
#' @examples
#'  \donttest{
#'
#'
#'  demo_Bayesian_FROC_without_pause()
#'
#'
#'}# dottest

demo_Bayesian_FROC_without_pause <- function(){



  # 1) First, we prepare the dataset for a single reader and a single modality.




  viewdata(BayesianFROC::dataList.Chakra.1.with.explantation)

  # pause()#1 /10  ----  we fit our model to the above data


  fit <- fit_Bayesian_FROC( ite  = 1111, summary = TRUE,  cha=3,  dataList = BayesianFROC::dataList.Chakra.1,new.imaging.device = TRUE,DrawAFROCcurve = TRUE )

  DrawCurves(fit,Colour = FALSE,DrawFROCcurve = FALSE ,DrawAFROCcurve = FALSE,DrawCFPCTP = TRUE)
  DrawCurves(fit,Colour = TRUE,DrawFROCcurve = FALSE ,DrawAFROCcurve = FALSE,DrawCFPCTP = TRUE)
  DrawCurves(fit,Colour = TRUE,DrawFROCcurve = FALSE ,DrawAFROCcurve = FALSE,DrawCFPCTP = TRUE, upper_y = 0.7)
  DrawCurves(fit,Colour = FALSE)
  DrawCurves(fit,Colour = FALSE,DrawAFROCcurve = TRUE)
  DrawCurves(fit,Colour = TRUE,DrawFROCcurve = TRUE ,DrawAFROCcurve = FALSE,DrawCFPCTP = TRUE, upper_y = 0.6)

  # pause()#2 /10  ---- Examine the bias

  datasets <-validation.dataset_srsc_for_different_NI_NL(
    NLvector = c(100,10000000,1000000000),
    ite = 2222
  )
  # pause()#3 /10  ---- p-value





  p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit(fit,plot.replicated.points = FALSE)

  # pause()#4 /10  ---- Bi normal assumption ----- High AUC--------



  viewdata(BayesianFROC::dataList.High)


  # pause()#5 /10  ----- Fitting (in the high ability case).

  fit.High <- fit_Bayesian_FROC(BayesianFROC::dataList.High,ite  = 1111)


  # pause()#6 /10  ----- draw a bi normal assumption----- High AUC--------

  draw_bi_normal(fit.High)

  # pause()#7 /10  ---- Bi normal assumption ----- Low AUC--------


  viewdata(BayesianFROC::dataList.Low)
  # pause()#8 /10  ----- Fitting (in the low ability case).

  fit.Low <- fit_Bayesian_FROC(BayesianFROC::dataList.Low,ite  = 1111)
  # pause()#9 /10  ----- draw a bi normal assumption----- Low AUC--------

  draw_bi_normal(fit.Low)

  # Demo for srsc finished !!




  # pause()
  viewdata(BayesianFROC::dataList.Chakra.Web)
  # pause()
  fit <- fit_Bayesian_FROC(BayesianFROC::dataList.Chakra.Web,ite = 1000,summary = TRUE,PreciseLogLikelihood = TRUE)
  # pause()
  DrawCurves_MRMC_pairwise(fit)
  # pause()
  DrawCurves_MRMC_pairwise(fit,modalityID = c(1,2),readerID = c(1))
  # pause()
  DrawCurves_MRMC_pairwise(fit,modalityID = c(3,4),readerID = c(1))
  # pause()
  DrawCurves_MRMC_pairwise(fit,modalityID = c(2,4),readerID = c(3))
  # pause()
  DrawCurves_MRMC_pairwise(fit,modalityID = c(3,1),readerID = c(1,2,3,4))




}# function
