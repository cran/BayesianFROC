
#' @title Fit a model
#' @description Fitting is done with single
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'
#' @return A fitted model object of class stanfitExtended
#' @export
#'
#' @examples
#'
#'\donttest{
#'
#'#----------------------------------------------------------------------------------------
#'#           d is data and 1111 is a number of MCMC iterations
#'#----------------------------------------------------------------------------------------
#'
#'
#'  d %>>% 1111
#'
#'
#'  }
`%>>%` <- function(dataList,ite=1111){

  if(missing(ite)==TRUE)  message(" please use .Last.value ")

  fit <-fit_Bayesian_FROC(dataList = dataList,ite = ite)

}#function
