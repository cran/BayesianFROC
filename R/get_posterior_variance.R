


#' @title (UNDER CONSTRUCTION) Alternative of \code{rstan::get_posterior_mean()}
#' @description This function is underconstruction. I validate only the example of this function.
#' For MRMC case, I have to write or modify code. 2019 Sept 6
#'
#'@inheritParams extract_EAP_by_array
#'
#' @return variance or posterior parameters, if it is an array, then return is also an array.
#' @export
#'
#' @examples
#'
#'
#'
#' \donttest{
#'
#'         fit <- fit_Bayesian_FROC(BayesianFROC::dd)
#'
#'
#'
#'
#'              e <- rstan::extract(fit)
#'
#'
#'
#'
#'  # Check the retrun value is desired one.
#'
#'
#'    apply(e$z, 2, var) ==  get_posterior_variance(fit,z)
#'
#'              var(e$m) ==  get_posterior_variance(fit,m)
#'
#'              var(e$v) ==  get_posterior_variance(fit,v)
#'
#'
#'
#'
#'
#'
#'  }#donttest
#'
#'
#'
#'
#'
get_posterior_variance <-function(StanS4class,
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
    extract.expression <- paste( "stats::var (extract(fit)$",name.of.parameter, ",)" ,sep = "")
  }


  if (dim==1) {
    extract.expression <- paste( "apply(extract(fit)$",name.of.parameter, ", MARGIN = 2, stats::var)" ,sep = "")
  }

#
#   if (dim==2){
#     extract.expression <- paste( "apply(extract(fit)$",name.of.parameter, ", MARGIN = c(2,3), mean)" ,sep = "")
#   }
#
#   if (dim==3){
#     extract.expression <- paste( "apply(extract(fit)$",name.of.parameter, ", MARGIN = c(2,3,4), mean)" ,sep = "")
#   }
#
#   if (dim==4){
#     extract.expression <- paste( "apply(extract(fit)$",name.of.parameter, ", MARGIN = c(2,3,4,5), mean)" ,sep = "")
#   }

  foo <- parse(text = extract.expression )
  e<- eval(foo)

  return(e)
}
