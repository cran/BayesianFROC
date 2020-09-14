



#' @title Pairs plot for divergent transition
#' @description If divergent transition occurs, the author often forget the variable par or pars. So, I made this to avoid such confusion.
#'
 #' @param character.representing.paramter Character, surrounded by "", indicating the paramter of model.
#'@inheritParams DrawCurves_MRMC_pairwise


#' @export
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####

#' @examples
#' \dontrun{
#'
#' # Create a fitted model object of class stanfitExtended  inherited from stanfit.
#'
#'  fit <- fit_Bayesian_FROC( ite  = 1111,
#'                            summary = FALSE,
#'                            cha = 1,
#'                            Null.Hypothesis = FALSE,
#'                            dataList = dd )
#'
#' # Pairs plot to examine the divergent transition.
#'
#'
#'
#'           # pairs_plot_if_divergent_transition_occurred(fit)
#'
#' # R CMD check launched error that pkg cannot be found, but it exsits
#' # Moreover it is available without errors from R console.  but I put # here
#' # to proceed futher steps in R CMD checks, what a lovely, pretty cute R CMD check is!
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'     Close_all_graphic_devices()
#'
#'     }
#'
#'
pairs_plot_if_divergent_transition_occurred <- function(StanS4class, character.representing.paramter = "z"){

  fit <- StanS4class

  fit.stan <-methods::as(fit,"stanfit")
  # grDevices::dev.new()
  graphics::pairs(fit.stan,pars=c(character.representing.paramter))

}
