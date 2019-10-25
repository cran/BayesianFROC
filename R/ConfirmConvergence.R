#' @title  Check R hat
#' @description  Calculates maximum and minimal R hat over all parameters and also return a \R loginal object whether R hat is good or bad.
#'@details This function evaluate convergence based on only the R hat statistics for a fitted model object.
#'@references  Gelman A. &  Rubin, D.B. (1992). Inference from Iterative Simulation Using Multiple Sequences, Statistical Science, Volume 7, Number 4, 457-472.
#'@param StanS4class An S4 object of the class \strong{\emph{\code{\link[rstan]{stanfit}}}}. No need that it is the S4 class \strong{\code{ \link{stanfitExtended}}}.
#'@param digits digits of R hat
#'@return Logical: \code{TRUE} of \code{FALSE}. If model converges then it is \code{TRUE}, and if not, then  \code{FALSE}.
#'@importFrom rstan traceplot summary
#'@seealso \code{check_rhat()} made by Betanalpha.
#'@inheritParams fit_Bayesian_FROC
#'

#'@examples
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'\donttest{

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
#'#  Where, the variable "ite" is the iteration of MCMC sampling.
#'#  More larger iteration is better.
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
#'# Which diagnosis is based on only the R hat statistic.
#'# So someone might consider it is not sufficient, and use the
#'# Simulation based cariblation (SBC) or other things to diagnosis of the
#'# convergence or bias.
#'# Now, I try to implement SBC and it almost be made, but the randomized is not
#'# sufficient caused seed, so, I have to say it is under construction.
#'# I am tired,...
#'
#'
#'
#'

#' # It also return the logical vector whether the MCMC converge,
#' # if MCMC converges, then the return is TRUE and if not, then FALSE.
#'
#' # This logical return value is used in this package development
#' # and the user should not be interested.
#'
#' # The following was useful for programming.
#' #3.2) The return value is TRUE or FALSE.
#'
#'      x <- ConfirmConvergence(fit)
#'
#' #3.3) If you do not want to print the results, then
#'
#'       x <- ConfirmConvergence(fit,summary=FALSE)
#'
#'
#'
#' # 2019.05.21 Revised.

#'
#'
#'}# dottest



#' @export ConfirmConvergence

ConfirmConvergence<-function(StanS4class,summary=TRUE,digits=2){
  # fit <-StanS4class
  fit <-  methods::as(StanS4class, "stanfit")


  max.rhat <-  round( max(summary(fit)$summary[,"Rhat"],na.rm = TRUE) ,digits = digits)
  min.rhat <-  round( min(summary(fit)$summary[,"Rhat"],na.rm = TRUE) ,digits = digits)

if (max.rhat < 1.1) {
  message("\n\n * max R-hat = ", max.rhat)
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



  if(requireNamespace("crayon",quietly = TRUE)){
    `%c+%` <- utils::getFromNamespace("%+%", "crayon") # changed to not break other things
      cyan <- utils::getFromNamespace("cyan", "crayon")
      blue <- utils::getFromNamespace("blue", "crayon")
    silver <- utils::getFromNamespace("silver", "crayon")

    red <- utils::getFromNamespace("red", "crayon")
    bold <- utils::getFromNamespace("bold", "crayon")
    blurred <- utils::getFromNamespace("blurred", "crayon")
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

