#' @title  Diagnosis For Convergence
#'
#'@description This function evaluate convergence based on only the R hat statistics for a fitted model object.
#'@references  Gelman A. &  Rubin, D.B. (1992). Inference from Iterative Simulation Using Multiple Sequences, Statistical Science, Volume 7, Number 4, 457-472.
#'@param StanS4class An S4 object of the class \strong{\emph{\code{\link[rstan]{stanfit}}}}. No need that it is the S4 class \strong{\code{ \link{stanfitExtended}}}.
#'@return Logical: \code{TRUE} of \code{FALSE}. If model converges then it is TRUE, and if not FALSE.
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
#' #1) Build the data for singler reader and single modality  case.
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

ConfirmConvergence<-function(StanS4class,summary=TRUE){
  # fit <-StanS4class
  fit <-  methods::as(StanS4class, "stanfit")

  if(requireNamespace("crayon",quietly = TRUE)){
    `%c+%` <- utils::getFromNamespace("%+%", "crayon") # changed to not break other things
    cyan <- utils::getFromNamespace("cyan", "crayon")
    blue <- utils::getFromNamespace("blue", "crayon")
    silver <- utils::getFromNamespace("silver", "crayon")

    red <- utils::getFromNamespace("red", "crayon")
    bold <- utils::getFromNamespace("bold", "crayon")
    blurred <- utils::getFromNamespace("blurred", "crayon")
  }

  #
  #   if(requireNamespace("crayon",quietly = TRUE)){
  #     `%>%`  <- utils::getFromNamespace("%>%", "magrittr")
  #   }

  A <- all(as.data.frame(rstan::summary(fit))$summary.Rhat <=1.01,  na.rm=T)
  B <- all(as.data.frame(rstan::summary(fit))$summary.Rhat <=1.10,  na.rm=T)
  C <- all(as.data.frame(rstan::summary(fit))$summary.Rhat <=1.50,  na.rm=T)

  Rhat <-as.data.frame(rstan::summary(fit))$summary.Rhat

  if(A==TRUE){
    if(summary==TRUE) {
      if(requireNamespace("crayon",quietly = TRUE)){


        message(
          crayon::silver('* Very Good Convergence !! ',"\n" %c+%
                           silver$blurred('\n\n     R hat  < 1.01 \n\n'),"\n" %c+%
                           silver$bold('* Your model converged, ')  %c+% 'that is:',"\n" %c+%
                           silver$bold('* Each R hat is less than or equal to 1.01 for all parameters')),"\n"
        )
      }else{
        message(" * Very Good Convergence !!   \n")
        message(" * R hat =< 1.01 for all parameters. \n")
        message(" * Your model converged, that is: \n")
        message(" * Each R hat is less than or equal to 1.01 for all parameters\n ")
      }#if(requireNamespace("crayon",quietly = TRUE)){
    }

    invisible(A)


  }else{
    if(B==TRUE){


      if(summary==TRUE) {

        if(requireNamespace("crayon",quietly = TRUE)){
          ite <- fit@sim$iter
          message(
            crayon::silver('* Good Convergence !!   \n\n     1.01 <  R hat < 1.10 \n\n' %c+%
                             silver$blurred('* Some R hat is greater than 1.01 for some parameters, however at most 1.10 for all parameters. \n') %c+%
                             silver$bold('* Raising the number of [ite] more than now (ite =',ite,') you would get more reliable values.\n')
            ) )
        }else{
          message("* Good Convergence !!  \n\n   1.01 <  R hat < 1.10 \n  \n")
          message("* Some R hat is greater than 1.01 for some parameters, however at most 1.10. \n")
          message("* Raising the number 'ite', you would get more reliable values.\n ")
        }#if(requireNamespace("crayon",quietly = TRUE)){
      }
      invisible(B)



    }else{
      if(C==TRUE){
        if(summary==TRUE) {
          if(requireNamespace("crayon",quietly = TRUE)){
            ite <- fit@sim$iter
            message(
              crayon::cyan('* Almost Convergence !! \n* You need some effort to reduce R hat !! \n \n    1.10 <  R hat < 1.50   \n  \n' %c+%
                             cyan$blurred('* Your R hat is at most 1.50 for all parameters, but greater than 1.10 for some parameter. \n') %c+%
                             cyan$bold('* Raising the number of [ite] more than now (ite =',ite,') you might get convergence.\n')
              ) )
          }else{
            message("* Almost Convergence !! \n You need some effort to reduce R hat !! \n\n  1.10 <  R hat < 1.50   \n\n")
            message("* Your R hat is at most 1.50 for all parameters, but greater than 1.10 for some parameter. So, by raising the Hamiltonian Monte Carlo sampling number 'ite', you might get convergence.\n")
          }#if(requireNamespace("crayon",quietly = TRUE)){


        }
















        invisible(C)












      }else{
        # requireNamespace("rstan", quietly = TRUE)
        if(summary==TRUE) {
          print(rstan::traceplot(fit, pars=c("A")))


          if(requireNamespace("crayon",quietly = TRUE)){
            message(
              crayon::cyan(
                red$underline$bold$bgWhite( '* Not Convergence !!  1.50 <  R hat \n') %c+%
                  '* Try the following !!',"\n" %c+%
                  '***', bold('1) Changing the seed !!'), '\n     That is, change the variable [see] to different natural number.  \n'  %c+%
                  '***', bold('2) Raise the number of iterations !!'), '\n     That is, the variable [ite] = more greater.  \n'  %c+%
                  red$underline$bold$bgWhite( '* Your R hat is greater than 1.50 for some parameters. \n'), 'So, you should check trace plot of your model to confirm it has trend or not. If chains have trend, changing the seed you would get convergence.\n'  %c+%
                  '* We draw the trace plot of AUCs, denoted by A[m] for the m th modality. From this trace plot, verify what occurs in your data. \n' %c+%
                  '*  If you can see clear non-overlapped sample paths, then change the seed. \n' %c+%
                  '*  If you can see clear overlapped sample paths, then raise the number of iterations !!. \n'%c+%
                  '*  If the so-called label switching problem occurs, then cut the number of iterations !!. \n'

              ))
          }else{

            message("* Not Convergence !!  1.50 <  R hat \n  \n")
            message("* Try the following !! \n  \n")
            message("** 1) Changing the seed !! (That is, change the variable [see] to different natural number. )   \n")
            message("** 2) Raise the number of iterations !! (That is, the variable [ite] = more greater. )   \n")
            message("\n* Your R hat is greater than 1.50 for some parameters. So, you should check trace plot of your model to confirm it has trend or not. If chains have trend, changing the seed you would get convergence.\n")
            message("\n* We draw the trace plot of AUCs, denoted by A[m] for the m th modality. From this trace plot, verify what occurs in your data.\n")
            message("\n*  If you can see clear non-overlapped sample paths, then change the seed.\n")
            message("\n*  If you can see clear overlapped sample paths, then raise the number of iterations !!.\n")
            message("*  If the so-called label switching problem occurs, then cut the number of iterations !!. \n")
            message("******************************************************** \n")
            message("******************************************************** \n")
            message("**                                                    ** \n")
            message("** * This model is meaningless!!                      **\n")
            message("**                                                    ** \n")
            message("** * A convergence criteria does not hold !!          ** \n")
            message("**                                                    ** \n")
            message("** * Changing the seed !! e.g., see =12 or see=1234 ..** \n")
            message("******************************************************** \n")
            message("******************************************************** \n")
          }#if(requireNamespace("crayon",quietly = TRUE))
        }
        D<-FALSE
        invisible(D)

      }
    }
  }
}

