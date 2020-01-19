
#'@title Array of hits and false alarms; 2019 Jun 18
#'@description
#'Return value is a three dimensional array of
#'type \strong{\emph{[C,M,Q]}}
#'representing the number of confidence levels
#' and modalities and readers,
#'  respectively.
#'This array includes the number of hit
#'and the number of false alarms.
#'
#'
#'  Revised 2019 Nov. 20

#'@details The author also implemented this
#' in the \code{ \link{metadata_to_fit_MRMC}} which is an old version.
#'However, the old version uses "\code{for}" sentences,
#' and it is not so better.
#'On the other hand,
#'this function use
#'the function \code{\link[base]{aperm}}()
#'and   \code{\link[base]{array}}() and they are better
#'than "\code{for}" sentence.
#'
#' Revised 2019 Nov. 20
#' Revised 2019 Dec. 12

#'@inheritParams metadata_to_fit_MRMC
#'
#' @return A list,
#'  whose components are arrays of the number of hits  \code{h} and
#'   the number of false alarms \code{f} of dimension \code{ [c,M,Q]}.
#'    Do not confuse \code{ [c,Q,M]} or \code{ [M,Q,C]}, etc.
#'   Revised 2019 Nov. 20

#' @export
#' @seealso
#'  \code{ \link{Chi_square_goodness_of_fit_in_case_of_MRMC_Posterior_Mean}}

#' @examples
#'#--------------------------------------------------------------------------------------
#'#                        Validation of program
#'#--------------------------------------------------------------------------------------
#'
#'
#'  h1 <- array_of_hit_and_false_alarms_from_vector(dd)$harray
#'  h2 <- metadata_to_fit_MRMC(dd)$harray
#'
#'  h1 == h2
#'
#'
#'
#'
#'  f1 <- array_of_hit_and_false_alarms_from_vector(dd)$farray
#'  f2 <- metadata_to_fit_MRMC(dd)$farray
#'
#'  f1 == f2
#'
#'#--------------------------------------------------------------------------------------
#'#                       subtraction for ( hit - hit.expected)
#'#--------------------------------------------------------------------------------------
#'# Chi square calculation, we need to subtract expected value of hit from hit rate,
#'# thus the author made this function.
#'
#'
#' \donttest{
#'
#'
#'# Prepare data
#'
#'     dd <- BayesianFROC::dd
#'
#'
#'# Fit a data
#'
#'
#'     fit <- fit_Bayesian_FROC(  dataList = dd )
#'
#'
#'# Extract a expected hits by array
#'
#'
#'
#'     harray.expected  <-  extract_EAP_by_array(fit,ppp)*dd$NL
#'
#'
#'
#'# Prepare hits data by array
#'
#'
#'      harray <- array_of_hit_and_false_alarms_from_vector(dd)$harray
#'
#'
#'
#'
#'# Calculate the difference of hits and its expectation..
#'
#'
#'
#'  Difference <- harray - harray.expected
#'
#'
#'# Such calculation is required in the chi square goodness of fit
#'
#'
#'
#'
#'#======================================================================================
#'#                           array format hit and false
#'#======================================================================================
#'
#'
#'
#'
#'    harray <- array_of_hit_and_false_alarms_from_vector(dataList = ddd)$harray
#'    farray <- array_of_hit_and_false_alarms_from_vector(dataList = ddd)$farray
#'
#'
#'
#'
#'
#'}
#'
#'
array_of_hit_and_false_alarms_from_vector <- function(dataList){

  C<-dataList$C;Q<-dataList$Q;M<-dataList$M;

  A <- array(dataList$h,c(C,Q,M)) # A is dimension [C,Q,M]
  B <- aperm(A, c(1, 3, 2)) # B is dimension [C,M,Q]
  harray <- B


  A <- array(dataList$f,c(C,Q,M))
  B <- aperm(A, c(1, 3, 2))
  farray <- B

  invisible(list(harray=harray,farray=farray))

}












