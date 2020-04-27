

#' @title Error message \strong{on a plot plane} (imaging device)
#' @description
#'
#'
#'
#' Since, shiny board fix user interface, and it let me make this;
#' in graphical device, the error message should be shown on its device.
#' So, usual functions such as \code{message()} or \code{cat()} cannot use in Shiny board.
#' Since, the UI is already made and it is graphical device!
#'
#' If a fitted model converges, then the error message is none and thus only in R console,
#' the message is printed such as "A model converged." and does not print error message on a plot plane.
#'
#' @details This is for non-convergent fitted model object, where convergence criteiron is R hat statistics for each model parameters.
#'
#' @inheritParams DrawCurves
#' @export
#' @param digits digits to round r hat
#' @param verbose A logical. if \code{TRUE}, then the maximal R hat is printed in the R cosole.
#'
#'
#'
#' @examples
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#   Non convergent fitting and error on it via a graphic device
#'#========================================================================================
#'  \dontrun{
#'
#'  # Creat a fitted model object which does not converge with R hat criterion:
#'
#'  fit <- fit_Bayesian_FROC( ite  = 111,
#'                             cha = 1,
#'                         summary = TRUE,
#'                 Null.Hypothesis  = FALSE,
#'                        dataList = dd # Here, non convergent data
#'                          )
#'
#'
#' # Nothing is plotted:
#'
#' plot(0,0,
#'      type ="n",
#'      axes =FALSE,
#'      ann=FALSE
#'      )
#'
#'
#'  # Error message on the above graphic device:
#'
#' error_message_on_imaging_device_rhat_values(fit)
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#      Plot
#'#========================================================================================
#'
#'
#'         DrawCurves(fit)
#'
#'
#'
#'# It does not work , and it is ,,, Ok since when non converges I will want to see
#'# plot, so this function is no need.
#'
#'  # 2019 August 18
#' }#dontrun


error_message_on_imaging_device_rhat_values <- function(
  StanS4class, verbose = TRUE,
  digits = 3
){

  fit <-StanS4class
  convergence <- fit@convergence

  max.rhat <-  round( max(summary(fit)$summary[,"Rhat"]) ,digits = digits)
  min.rhat <-  round( min(summary(fit)$summary[,"Rhat"]) ,digits = digits)

if(convergence ==FALSE){





  suppressWarnings(graphics::par(new=TRUE));
  graphics::text(0.1,0.9,paste( "Caution: Model did not converge!",
                                sep = ""),
                 col="white",cex = 2)


  suppressWarnings(graphics::par(new=TRUE));
  graphics::text(0.1,0.8,paste( "Caution: Model did not converge!",
                                sep = ""),
                 col="red",cex = 2)


  suppressWarnings(graphics::par(new=TRUE));
  graphics::text(0.1,0.5,  substitute(paste( hat(R)[max] ," = ", max.rhat),list(max.rhat = max.rhat)  ),
                 col="red",cex = 2)

  suppressWarnings(graphics::par(new=TRUE));
  graphics::text(0.1,0.3,  substitute(paste( hat(R)[min] ," = ", min.rhat),list(min.rhat = min.rhat)  ),
                 col="red",cex = 2)




}


if(convergence ==TRUE && verbose)message("\n * max R-hat = ", max.rhat,"  (converged)\n")
  # message("\n * min R-hat = ", min.rhat)



  }#function
