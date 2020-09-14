# max.rhat <-  round( max(summary(f)$summary[,"Rhat"]) ,digits = 5)




#' @title   Chage S4 class to stanfit
#'
#' @inheritParams DrawCurves
#'
#' @return A fitted model object whose S4 class is the stanfit
#' @export
#'
#' @examples
#' \dontrun{
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                   Draw   a trace plot for a paramter whose R hat is largest
#'#========================================================================================
#'
#'
#'#  Fit a model to data
#'#______________________
#'
#'
#'
#'       f <- fit_Bayesian_FROC(
#'                          ite  = 111,
#'                           cha = 1,
#'                      dataList = d)
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'# Change the class of the above object "f" to stanfit from its inherited class
#'#____________________________________________________________________
#'
#'
#'
#'   stanfit_from_its_inherited_class(f)
#'
#'
#'}
#'
stanfit_from_its_inherited_class <- function(StanS4class) {

  f    <-StanS4class
  f    <- methods::as(f,"stanfit")

  return(f)
}




















#' @title Arg Max: Extract a subscript corresponding component is a max
#' @description The non-negative valued function of a vector, which returns
#' a subscript whose component is the maximal component of the vector.
#'
#'  If the maximal component is not unique, then the lowest is chosen
#'
#'
#'Namely, for an arbitrary  \code{vector},
#'
#' \code{argMax(vector) = i}
#'
#'  if and only if \code{i} is the smallest number such that
#'
#'    \code{vector[i] >= vector[j]} for all \code{j}.
#'
#' @param numeric_vector A vector, each component is a real number (an object of class numeric).
#' @param verbose A logical, if TRUE, then verbose summary is printed in R or R studio console.
#' @return A non-negative integer, indicating a subscript, corresponding component is the maximum component.
#'
#' @details This function is very fundamental and so,,,
#' Is there a same function in the package \pkg{base}?
#' @export
#'
#' @examples
#'
#'      argMax(c(0,0,0,0,0,0,0,0,0,0))
#'      argMax(c(11,0,0,0,0,0,0,0,0,0))
#'      argMax(c(0,22,0,0,0,0,0,0,0,0))
#'      argMax(c(0,0,33,0,0,0,0,0,0,0))
#'      argMax(c(0,0,0,44,0,0,0,0,0,0))
#'      argMax(c(0,0,0,0,55,0,0,0,0,0))
#'      argMax(c(0,0,0,0,0,66,0,0,0,0))
#'      argMax(c(0,0,0,0,0,0,77,0,0,0))
#'      argMax(c(0,0,0,0,0,0,0,88,0,0))
#'      argMax(c(0,0,0,0,0,0,0,0,99,0))
#'
#'      # If the maximal component is not unique, then the lowest is chosen
#'      argMax(c(0,0,0,44,0,0,44,0,0,0))
#'
#'
#'      argMax(c(NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN))
#'      argMax(c(11,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN))
#'      argMax(c(NaN,22,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN))
#'      argMax(c(NaN,NaN,33,NaN,NaN,NaN,NaN,NaN,NaN,NaN))
#'      argMax(c(NaN,NaN,NaN,44,NaN,NaN,NaN,NaN,NaN,NaN))
#'      argMax(c(NaN,NaN,NaN,NaN,55,NaN,NaN,NaN,NaN,NaN))
#'      argMax(c(NaN,NaN,NaN,NaN,NaN,66,NaN,NaN,NaN,NaN))
#'      argMax(c(NaN,NaN,NaN,NaN,NaN,NaN,77,NaN,NaN,NaN))
#'      argMax(c(NaN,NaN,NaN,NaN,NaN,NaN,NaN,88,NaN,NaN))
#'      argMax(c(NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,99,NaN))
#'      argMax(c(NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,100))
#'
#'
#'      argMax(c(NaN,NaN,NaN,22,NaN,55,NaN,NaN,NaN,NaN))
#'      argMax(c(NaN,44,NaN,11,NaN,NaN,NaN,NaN,NaN,NaN))
#'      argMax(c(NaN,NaN,33, 33, 33, 33,NaN,NaN,NaN,NaN))

#'
argMax  <- function(numeric_vector,verbose = FALSE) {
  vvv<- numeric_vector
  leng <- length(vvv)

  # count <- 1
  # s<- vvv[1]


  # vvv <-c(NaN,NaN,NaN)

  # vvv <-c(NaN,222,3)
  s<-NaN
  i<-1
  while(is.nan(s)&&(i<=length(vvv))){
    s <- vvv[i]
   if(verbose) cat("s = ",s,", i = ",i,"\n")
    i<-i+1
  }
if( verbose&&!is.nan(s)) cat("The first non NaN component is the ",i-1, "-th \n")
count <- i-1

if (!is.nan(s)) {

  for (xxx in (i-1):leng) {
    if ( !is.nan(vvv[xxx])  ) {
      if(verbose)    cat("xxx=",xxx,", !is.nan(vvv[",xxx,"])=",!is.nan(vvv[xxx]),",")
       # browser()
      if(vvv[xxx] >s){
          s <- vvv[xxx]
          count <- xxx
        }

      if(verbose)    cat(" count=",count,",\n")

        }

    # if (  is.nan(vvv[xxx])  )  count <- xxx
  }
  return(count)

}


if (is.nan(s)) {
  if(verbose) print("All componenets are NaN.")
  return(NaN)
}


}



































# max.rhat <-  round( max(summary(f)$summary[,"Rhat"]) ,digits = 5)


#' @title Arg Min: Extract a subscript corresponding component is a minimal
#' @description The non-negative valued function of a vector, which returns
#' a subscript whose component is the minimal component of the vector.
#'Namely,
#'
#' \code{argMin(vector) = i}
#'
#'  if and only if
#'
#'    \code{vector[i] <= vector[j]} for all \code{j}.
#'
#' @param numeric_vector A vector, each component is a real number (an object of class numeric).
#' @param verbose A logical, if TRUE, then verbose summary is printed in R or R studio console.
#' @return A non-negative integer, indicating a subscript, corresponding component is the maximum component.
#' @seealso  \code{ \link{argMax}()}
#' @details This function is very fundamental and so,,,
#' Is there a same function in the package \pkg{base}?
#' @export
#'
#' @examples
#'
#'      argMin(c(11,99,99,99,99,99,99,99,99,99))
#'      argMin(c(99,22,99,99,99,99,99,99,99,99))
#'      argMin(c(99,99,33,99,99,99,99,99,99,99))
#'      argMin(c(99,99,99,44,99,99,99,99,99,99))
#'      argMin(c(99,99,99,99,55,99,99,99,99,99))
#'      argMin(c(99,99,99,99,99,66,99,99,99,99))
#'      argMin(c(99,99,99,99,99,99,77,99,99,99))
#'      argMin(c(99,99,99,99,99,99,99,88,99,99))
#'      argMin(c(99,99,99,99,99,99,99,99,99,99))
#'
#'      argMin(c(NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN))
#'      argMin(c(11,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN))
#'      argMin(c(NaN,22,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN))
#'      argMin(c(NaN,NaN,33,NaN,NaN,NaN,NaN,NaN,NaN,NaN))
#'      argMin(c(NaN,NaN,NaN,44,NaN,NaN,NaN,NaN,NaN,NaN))
#'      argMin(c(NaN,NaN,NaN,NaN,55,NaN,NaN,NaN,NaN,NaN))
#'      argMin(c(NaN,NaN,NaN,NaN,NaN,66,NaN,NaN,NaN,NaN))
#'      argMin(c(NaN,NaN,NaN,NaN,NaN,NaN,77,NaN,NaN,NaN))
#'      argMin(c(NaN,NaN,NaN,NaN,NaN,NaN,NaN,88,NaN,NaN))
#'      argMin(c(NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,99,NaN))
#'      argMin(c(NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,NaN,100))
#'
#'
#'      argMin(c(NaN,NaN,NaN,22,NaN,55,NaN,NaN,NaN,NaN))
#'      argMin(c(NaN,44,NaN,11,NaN,NaN,NaN,NaN,NaN,NaN))
#'      argMin(c(NaN,NaN,33, 33, 33, 33,NaN,NaN,NaN,NaN))

#'
argMin  <- function(numeric_vector,verbose = FALSE) {
  vvv<- numeric_vector
  leng <- length(vvv)

  # count <- 1
  # s<- vvv[1]


  # vvv <-c(NaN,NaN,NaN)

  # vvv <-c(NaN,222,3)
  s<-NaN
  i<-1
  while(is.nan(s)&&(i<=length(vvv))){
    s <- vvv[i]
    if(verbose) cat("s = ",s,", i = ",i,"\n")
    i<-i+1
  }
  if( verbose&&!is.nan(s)) cat("The first non NaN component is the ",i-1, "-th \n")
  count <- i-1

  if (!is.nan(s)) {

    for (xxx in (i-1):leng) {
      if ( !is.nan(vvv[xxx])  ) {
        if(verbose)    cat("xxx=",xxx,", !is.nan(vvv[",xxx,"])=",!is.nan(vvv[xxx]),",")
        # browser()
        if(vvv[xxx] <s){
          s <- vvv[xxx]
          count <- xxx
        }

        if(verbose)    cat(" count=",count,",\n")

      }

      # if (  is.nan(vvv[xxx])  )  count <- xxx
    }
    return(count)

  }


  if (is.nan(s)) {
    if(verbose) print("All componenets are NaN.")
    return(NaN)
  }


}



































#' @title Extract name from a real vector whose component is the maximal one
#' @description Extracts an object of class character from a named vector.
#' The component whose name is the extracted one is the maximal component of vector.
#'
#'@inheritParams argMax
#'
#' @return A character, indicating a name of some component of vector.
#'  The corresponding component is the minimal component.
#' @export
#'
#' @examples
#'
#'
#'
#'
#' v<-c(11,22,33,22)
#' names(v)<-c("1-st","2-nd","3-rd","4-th")
#' names_argMax(v)
#'
#'
#'
#'
#'
#' v<-c(11,NaN,33,22)
#' names(v)<-c("1-st","2-nd","3-rd","4-th")
#' names_argMax(v)
#'
#'
#'
#'
#'
#'
#' \dontrun{
#'
#'
#'  f <- fit_Bayesian_FROC(
#'                          ite  = 111,
#'                           cha = 1,
#'                      dataList = d)
#'
#'
#'  a <- summary(f)$summary[,"Rhat"]
#'
#'  names_argMax(a)
#'
#'
#'
#'
#' }
#'
names_argMax <- function(numeric_vector) {
  vvv<- numeric_vector
  subscript <- argMax(vvv)
  if(!is.nan(subscript)){
    name      <- names(vvv[subscript])
    return(name)
  }

  if(is.nan(subscript)){
    print("Name cannot extract correctly, cuz all of them are NaN.")
    return("lp__")
  }


}





#' @title Extract a name of parameter from StanfitExtended object (or stanfit object.)
#'
#' @inheritParams DrawCurves
#' @return An object of class "character" indicating a parameter whose chain has the maximal R hat over all chains of MCMC parameters.
#' @export
#'
#' @examples
#' \dontrun{
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                   Draw   a trace plot for a paramter whose R hat is largest
#'#========================================================================================
#'
#'
#'#  Fit a model to data
#'#______________________
#'
#'
#'
#'       f <- fit_Bayesian_FROC(
#'                          ite  = 111,
#'                           cha = 1,
#'                      dataList = d)
#'
#'
#'# Extract a name of parameter whose R hat is maximal over all parameters
#'#____________________________________________________________________
#'
#'
#'
#'   name <- name_of_param_whose_Rhat_is_maximal(f)
#'
#'
#'
#'# Change the S4 class of fitted model object to apply the rstan package
#'#____________________________________________________________________
#'
#'
#'
#'    # f <- methods::as(f,"stanfit")
#'    # for unknown error in R CMD check, the author put # before the code
#'
#'# Show trace plot of a parameter whose R hat is the worst
#'#____________________________________________________________________
#'
#'
#'
#'    # rstan::stan_trace(f,pars=name)
#'    # for unknown error in R CMD check, the author put # before the code
#'
#' }
#'
name_of_param_whose_Rhat_is_maximal  <- function(StanS4class) {

  f <- methods::as(StanS4class,"stanfit")


  a <- summary(f)$summary[,"Rhat"]

  name <-  names_argMax(a)

 return( name)

}








#' @title   a trace plot for a paramter whose R hat is largest
#'
#' @inheritParams DrawCurves
#'
#' @return none
#' @export
#'
#' @examples
#' \dontrun{
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                   Draw   a trace plot for a paramter whose R hat is largest
#'#========================================================================================
#'
#'
#'#  Fit a model to data
#'#______________________
#'
#'
#'
#'       f <- fit_Bayesian_FROC(
#'                          ite  = 111,
#'                           cha = 1,
#'                      dataList = d)
#'
#'
#'# Extract a name of parameter whose R hat is maximal over all parameters
#'#____________________________________________________________________
#'
#'
#'
#'  stan_trace_of_max_rhat(f)
#'
#'
#'}
#'
stan_trace_of_max_rhat <- function(StanS4class) {

  f    <-StanS4class
  name <- name_of_param_whose_Rhat_is_maximal(f)
  f    <- methods::as(f,"stanfit")
  rstan::stan_trace(f,pars=name)

}






