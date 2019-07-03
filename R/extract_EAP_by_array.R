
#'@title Extract Etimates Preserving Array Format.
#'@description Extract posterior mean extimates (\strong{EAP})  by array format.
#'@details
#'If an estimate is an array,
#'then this function extract  estimated parameters preserving an array format.
#' The \code{rstan} also has such function,
#' i.e., \strong{\emph{\code{rstan::\link[rstan]{get_posterior_mean}}()}}.
#'  However this function does not extract paramter as an array but coerce to the class matrix.

#'@inheritParams validation.dataset_srsc
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@param  name.of.parameter An parameter name (given as a character string, should not surround by "").
#'The name of parameter which user want to extract.
#'Parameters are contained in the parameter block of each Stan file in the path: inst/extdata.

#'
#' @return A list of datalists from the posterior predictive distribution
#' @export
#'
#' @examples
#'  \donttest{
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#=================================The first example: MRMC case ========================
#'#----------------------------------------------------------------------------------------
#'#             MRMC case: Extract a estimates from fitted model objects
#'#----------------------------------------------------------------------------------------
#'
#'
#'# Make a fitted model object of class stanfitExtended
#'# which is inherited from the S4class stanfit.
#'# The following example, fitted model is the hierarchical Bayesian FROC model
#'# which is used to compare modality.
#'
#'  fit <- fit_Bayesian_FROC( ite  = 1111 ,
#'                            summary = FALSE   ,
#'                            dataList = dataList.Chakra.Web.orderd,
#'                            cha=1
#'                             )
#'
#'#  Extract one dimensional array "z = z[]",
#'
#'                   z   <- extract_EAP_by_array(
#'                                                fit,  # The above fitted model object
#'                                                z     # The parameter contained in "fit"
#'                                                )
#'
#'
#'
#'#  Extract two dimensional array "AA = AA[ , ]",
#'
#'                   AA  <- extract_EAP_by_array(
#'                                               fit,
#'                                               AA
#'                                               )
#'
#'
#'#  Extract three dimensional array "ppp = ppp[ , , ]",
#'
#'                   ppp <- extract_EAP_by_array(fit,ppp)
#'
#'

#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#================= The second example: singler reader and single modality ==============
#'#----------------------------------------------------------------------------------------
#'#             srsc case: Extract a estimates from fitted model objects
#'#----------------------------------------------------------------------------------------
#'
#'
#' #   Of course, for the case of srsc, it is also available.
#' #   We shall show the case of srsc in which case the parameters are not array,
#' #   but in such a case we can extract estimates preserving its format such as vector.
#'
#'  fit <- fit_Bayesian_FROC( ite  = 1111 ,
#'                            summary = FALSE   ,
#'                            dataList = dataList.Chakra.1,
#'                            cha=2
#'                             )
#'
#'#  To extract the posterior mean for parameter "A" representing AUC, we run the following;
#'
#'
#'           A <- extract_EAP_by_array(
#'                                     fit,
#'                                      A
#'                                      )
#'
#'
#'
#'
#'#  To extract the posterior mean for parameter "z" indicating decision thresholds;
#'
#'
#'           z <- extract_EAP_by_array(
#'                                      fit,
#'                                      z
#'                                      )
#'
#'
#'
#' # 2019.05.21 Revised.
#'
#'
#'
#'#----------------------------------------------------------------------------------------
#'#              name.of.parameter surrounded by double quote is also available
#'#----------------------------------------------------------------------------------------
#'
#'
#'#      Let fit be the above fitted model object.
#'#      Then the following two codes are same.
#'
#'
#'
#'                               extract_EAP_by_array( fit, "A" )
#'
#'                               extract_EAP_by_array( fit,  A  )
#'
#'
#'# The former is the case that  the variable is surrounded by the double quote,
#'# the later is raw, i.e., pseudo object.
#'# Note that the later case sometimes cause the R CMD check error which said
#'# that no visible binding, since object A is not defined.
#'
#'# I am not sure, does this package development make me happy?
#'# Back pain being due to an abnormality in my immune system, which is caused
#'# my exposure to surfactants or latex (not LaTeX).
#'

#'}# Revised 2019 Jun 19

#'

extract_EAP_by_array <-function(StanS4class,
                                name.of.parameter
){

  # This detect the length of array
  if (class("name.of.parameter")=="character"){
    name.of.parameter <-substitute(name.of.parameter)
  }
    fit <- methods::as(StanS4class, "stanfit")
  extract.expression.dim <- paste( "length(dim(extract(fit,par=c(name.of.parameter ))[[1]]))-1" ,sep = "")
  foo.dim <- parse(text = extract.expression.dim)
  dim<- eval(foo.dim)
  # dim means e.g.,
  # real z[5 ] --dim=1
  # real z[4,5]---dim = 2
  # real z[5,6,7]---dim = 3

  if (dim==0) {
    extract.expression <- paste( "mean (extract(fit)$",name.of.parameter, ",)" ,sep = "")
  }


  if (dim==1) {
    extract.expression <- paste( "apply(extract(fit)$",name.of.parameter, ", MARGIN = 2, mean)" ,sep = "")
  }


  if (dim==2){
    extract.expression <- paste( "apply(extract(fit)$",name.of.parameter, ", MARGIN = c(2,3), mean)" ,sep = "")
  }

  if (dim==3){
    extract.expression <- paste( "apply(extract(fit)$",name.of.parameter, ", MARGIN = c(2,3,4), mean)" ,sep = "")
  }

  if (dim==4){
    extract.expression <- paste( "apply(extract(fit)$",name.of.parameter, ", MARGIN = c(2,3,4,5), mean)" ,sep = "")
  }

  foo <- parse(text = extract.expression )
  e<- eval(foo)

  return(e)
}



# n <-array(NA,dim = c(5,2,3))
# for (cd in 1:5) {
#   for (md in 1:2) {
#     for (qd in 1:3) {
#       n[cd,md,qd] <- paste("[",cd,",",md, ",", qd , "]",sep = ""  )
#
#
#   }
#   }
# }

