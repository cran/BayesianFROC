#' @title  Diagnosis of MCMC sampling
#'
#'@description This function evaluate \eqn{R} hat statistics for any fitted model object of class \code{stanfit}.
#'@details It evaluate whether or not r hat statistics are all near 1.
#'@references  Gelman A. \&  Rubin, D.B. (1992). Inference from Iterative Simulation Using Multiple Sequences, Statistical Science, Volume 7, Number 4, 457-472.
#'@return  Logical, that is \code{TRUE} or \code{FALSE}. If model converges then \code{TRUE}, and if not \code{FALSE}.
#'@importFrom rstan traceplot summary
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#' @param digits a positive integer, indicating the digit of R hat printed in R/R-studio console

#'@export
#'@author  \strong{betanalpha}, so not my function. But I modified it. So, alphanbetan is one of the standeveloper, so his function will has consensus, thus I use it.


# Checks the potential scale reduction factors
check_rhat <- function(StanS4class, summary=FALSE,digits = 3) {
  message( crayon::silver("\n* One of the Stan developer \"betanalpha\" made the check_rhat() and it says that "))

  fit <-methods::as(StanS4class, "stanfit")
  fit_summary <- summary(fit, probs = c(0.5))$summary
  N <- dim(fit_summary)[[1]]

  no_warning <- TRUE
  for (n in 1:N) {
    rhat <- fit_summary[,6][n]
    if (rhat > 1.1 || is.infinite(rhat) || is.nan(rhat)) {
      cat(
          "\n* Rhat for parameter",
          crayon::red$bold$underline$bgWhite( rownames(fit_summary)[n]),
          "is",
          crayon::red$bold$underline$bgWhite(   signif(rhat, digits = digits)   ) ,"."

        )
      no_warning <- FALSE
    }
  }
  if (no_warning)
   message(crayon::silver("Rhat looks reasonable for all parameters.\n\n"))
  else
    cat(
      crayon::red$bold$underline$bgWhite(
        '\n* Inequality [ Rhat > 1.1 ] indicates that the chains very likely have not mixed'
        )
      )


  if(summary) cat("Max R hat: \n")
  if(summary) message(  paste( R_hat_max(fit) , crayon::silver(" achieved by the param \"",name_of_param_whose_Rhat_is_maximal(fit), "\"")  ,sep = "")  )
}
