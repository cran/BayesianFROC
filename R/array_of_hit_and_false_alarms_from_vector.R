
#'@title Array of hits and false alarms; 2019 Jun 18
#'@description
#'Return value is an array of type \strong{\emph{[C,M,Q]}} representing hit and false alarms.
#'
#'
#'
#'Who read this ? I am not sure, who read but if someone,....who read and where I will go? I love you.
#'@details The author also implemented this in the \code{ \link{metadata_to_fit_MRMC}} which we shall  call old version.
#'However, old version use \code{for} sentence, and it is not so better.
#'On the other hand,
#'this function use
#'the function \code{\link[base]{aperm}}()
#'and   \code{\link[base]{array}}() and which is better than \code{for} sentence.
#'
#'@inheritParams metadata_to_fit_MRMC
#'
#' @return A list of array, contains  \code{h} and \code{f} of dimension \code{ [c,M,Q]}. Do not confuse \code{ [c,Q,M]} or \code{ [M,Q,C]}  or etc.
#' @export
#' @seealso
#'  \code{ \link{Chi_square_goodness_of_fit_in_case_of_MRMC_Posterior_Mean}}
#'@author Issei Tsunoda

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












