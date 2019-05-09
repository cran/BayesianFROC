#' @title  Diagnosis for convergence
#'
#'@description This function evaluate your model's R hat statistics.
#'@references  Gelman A. \&  Rubin, D.B. (1992). Inference from Iterative Simulation Using Multiple Sequences, Statistical Science, Volume 7, Number 4, 457-472.
#'@param StanS4class This is a return value of the function \code{rstan::stan()}, i.e., an object of the S4 class the so-called stanfit.
#'@return TRUE of FALSE. If model converges then TRUE, and if not FALSE.
#'@importFrom rstan traceplot summary
#'@inheritParams fit_Bayesian_FROC
#'@export
#'@author  betanalpha. This is not my function. But I modified it.


# Checks the potential scale reduction factors
check_rhat <- function(StanS4class) {
  message( crayon::silver("\n* One of the Stan developer \"betanalpha\" makes the check_rhat() and it says that "))

  fit <-methods::as(StanS4class, "stanfit")
  fit_summary <- summary(fit, probs = c(0.5))$summary
  N <- dim(fit_summary)[[1]]

  no_warning <- TRUE
  for (n in 1:N) {
    rhat <- fit_summary[,6][n]
    if (rhat > 1.1 || is.infinite(rhat) || is.nan(rhat)) {
      cat(
          "\n* Rhat for parameter", rownames(fit_summary)[n], "is", crayon::red$bold$underline$bgWhite(
rhat ) ,"."

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
}
