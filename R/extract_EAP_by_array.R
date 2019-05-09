
#'@title Extract Etimates Preserving Array Format.
#'@description Extract EAP extimates by array format.
#'@details
#'If an estimate is an array, then this function extract as an array. The \code{rstan} also has such function, i.e., \code{ rstan::get_posterior_mean()}. However this function does not extract as an array.

#'@inheritParams validation.dataset_srsc
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@param  name.of.parameter The name of parameter which user want to extract

#'
#' @return A list of datalists from the posterior predictive distribution
#' @export
#'
#' @examples
#'  \donttest{

#'
#' # Make a stanfit object. More precisely its extended class.
#'
#'  fit <- fit_Bayesian_FROC( ite  = 1111 ,
#'                            summary = FALSE   ,
#'                            dataList = dataList.Chakra.Web.orderd,cha=1
#'                             )
#'
#'#  One dimensional array
#'
#'                   z   <- extract_EAP_by_array(fit,z)
#'
#'
#'
#'#  Two dimensional array
#'
#'                   AA  <- extract_EAP_by_array(fit,AA)
#'
#'
#'#  Three dimensional array
#'
#'                   ppp <- extract_EAP_by_array(fit,ppp)
#'
#'

#'
#'# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#' #     Of course, for the case of srsc, it is also available.
#' #   We shall show the case of srsc in which case the parameters are not array,
#' #   but in such a case we can extract estimates preserving its format such as vector.
#'
#'  fit <- fit_Bayesian_FROC( ite  = 1111 ,
#'                            summary = FALSE   ,
#'                            dataList = dataList.Chakra.1,
#'                            cha=2
#'                             )
#'
#'#  To extract the posterior mean for parameter A, we run the following;
#'
#'   A <- extract_EAP_by_array(fit,A)
#'
#'#  To extract the posterior mean for parameter z, we run the following;
#'
#'
#'  z <- extract_EAP_by_array(fit,z)
#'

#'}# dottest

#'

extract_EAP_by_array <-function(StanS4class,
                                name.of.parameter
){

  # This detect the length of array
  name.of.parameter <-substitute(name.of.parameter)
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

