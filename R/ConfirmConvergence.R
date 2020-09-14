#' @title  Check R hat criterion
#' @description  Calculates the maximum and the minimal values of R hat over all parameters.
#'  In addition, it returns a loginal \R object whether R hat is good (\code{TRUE}) or bad (\code{FALSE}).
#'@details Evaluates convergence criterion based on only the R hat statistics for a fitted model object. Revised Nov 23.
#'@references  Gelman A. &  Rubin, D.B. (1992). Inference from Iterative Simulation Using Multiple Sequences, Statistical Science, Volume 7, Number 4, 457-472.
#'@param StanS4class An S4 object of the class \strong{\emph{\code{stanfit}}}. No need that it is the S4 class \strong{\code{ \link{stanfitExtended}}}.
#'@param digits A positive integer, indicating digits for R hat statistics.
#'@return Logical: \code{TRUE} of \code{FALSE}. If model converges ( all R hat are closed to 1) then it is \code{TRUE}, and if not( some R hat is far from 1), then  \code{FALSE}.
#'@importFrom rstan traceplot summary
#'@seealso \code{check_rhat()}, which is made by Betanalpha.
#'@inheritParams fit_Bayesian_FROC
#'

#'@examples
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'\dontrun{

#'#================The first example======================================
#'
#'             #((Primitive way)).
#' #1) Build the data for a singler reader and a single modality  case.
#'
#' dat <- list(c=c(3,2,1),    #Confidence level
#'             h=c(97,32,31), #Number of hits for each confidence level
#'             f=c(1,14,74),  #Number of false alarms for each confidence level
#'
#'             NL=259,       #Number of lesions
#'             NI=57,        #Number of images
#'             C=3)          #Number of confidence level
#'

#'# where, c denotes Confidence level,
#'#        h denotes number of Hits for each confidence level,
#'#        f denotes number of False alarms for each confidence level,
#'#        NL denotes Number of Lesions,
#'#        NI denotes Number of Images,
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' #2) Fit the FROC model.
#'   #Since the above dataset  "dat" are single reader and single modality,
#'   #the following function fit the non hierarchical model.
#'
#'           fit <-   BayesianFROC::fit_Bayesian_FROC(dat,ite=1111)
#'
#'
#'
#'#  Where, the variable "ite" specifies the iteration of MCMC samplings.
#'#  Larger iteration is better.
#'
#'
#'
#' #3.1) Confirm whether our estimates converge.
#'
#'
#'
#'
#'           ConfirmConvergence(fit)
#'
#'
#'
#'# By the above R script,
#'# the diagnosis of convergence will be printed in the R (R-studio) console.
#'# The diagnosis is based on only the R hat statistic.

#' # It also return the logical vector indicating whether or not the MCMC converge,
#' # if MCMC converges, then the return value is TRUE and if not, then FALSE.
#'
#' # This logical return value is used in this package development
#' # and the user should not be interested.
#'
#' # The following was useful for programming.
#' #3.2) The return value is TRUE or FALSE.
#'
#'      x <- ConfirmConvergence(fit)
#'
#' #3.3) If you do not want to print the results in the R (Studio) console, then
#'
#'       x <- ConfirmConvergence(fit,summary=FALSE)
#'
#'
#'
#' # 2019.05.21 Revised.
#' # 2019.12.02 Revised.


#'
#'
#'}# dontrun



#' @export ConfirmConvergence

ConfirmConvergence<-function(StanS4class,summary=TRUE,digits=2){
  # fit <-StanS4class
  fit <-  methods::as(StanS4class, "stanfit")


  max.rhat <-  round( max(summary(fit)$summary[,"Rhat"],na.rm = TRUE) ,digits = digits)
  min.rhat <-  round( min(summary(fit)$summary[,"Rhat"],na.rm = TRUE) ,digits = digits)

if (max.rhat < 1.1) {
  message("\n\n * max R-hat = ", crayon::bgBlack$bold$yellow$underline$italic(  max.rhat ) , ", which is achieved by the parameter ", crayon::bgBlack$bold$yellow$underline$italic(  name_of_param_whose_Rhat_is_maximal(fit) ) )
  # message("\n * min R-hat = ", min.rhat)
}

  if (max.rhat >= 1.1 && min.rhat  <= 1.1) {
    message("\n\n * max R-hat = ", crayon::red$bgWhite$bold$underline$italic(paste( max.rhat," ") )    )
    # message("\n * min R-hat = ", min.rhat)
  }

  if (max.rhat >= 1.1 && min.rhat > 1.1) {
    message("\n\n * max R-hat = ", crayon::red( max.rhat )    )
    # message("\n * min R-hat = ", crayon::red( min.rhat )    )
  }







  A <- all(as.data.frame(rstan::summary(fit))$summary.Rhat <=1.01,  na.rm=T)
  B <- all(as.data.frame(rstan::summary(fit))$summary.Rhat <=1.10,  na.rm=T)
  C <- all(as.data.frame(rstan::summary(fit))$summary.Rhat <=1.50,  na.rm=T)

  Rhat <-as.data.frame(rstan::summary(fit))$summary.Rhat



  if (A) {
    message(crayon::silver("Very Good!\n"))
    invisible(A)

  }else if(B){
    message(crayon::silver("Good!\n"))

    invisible(B)
  }else if(C){
    message(crayon::silver("Now, subtle. Check trace plot. If trace plot is good, then more MCMC iterations may help. \n"))

    invisible(FALSE)
  }else {
    message(crayon::silver("Bad!\n"))

    invisible(FALSE)
  }

}

