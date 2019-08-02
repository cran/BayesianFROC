


#' @title Curve and signal distribution and noise  d log Phi() for single reader and single modality
#' @description Draws FROC curve and signal and noise ( \eqn{d \log \Phi}) are drawn in a \strong{same} plain.
#' The author of this pacakage developed the FROC theory, and find that
#' the noise distribution is not the so-called bi normal assumption.
#' But instead, we use the differential logarithmic Gaussian for the noise distribution.
#'
#'\emph{ Note that MRMC data is not allowed.}
#' @seealso
#' \code{\link{DrawCurves}}
#'
#' \code{\link{draw_bi_normal_version_UP}}
#'
#' @inheritParams DrawCurves
#' @return None
#' @export
#' @details This function is made to pass this plot to Shiny.
#' If someone knows how to divide the main panel of Shiny, please tell me!!
#' I cannot, thus I divide the plot before passing to Shiny.
#'
#'
#'
#' With pain from all my body, but today 2019 July 23 is good.
#'  Neuralgia or muscle aches makes my feeling down and down.
#'  If I can transform into Anpanman, then I want to give my head.
#'
#'  I fails, this is very small plot, so I cannot use this function for my package.
#' I will remove this function or extende plot region for more confortable exhibition.
#'
#' @examples
#'
#' \donttest{
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#----------------------------------------------------------------------------------------
#'#            1)             Build the data
#'#----------------------------------------------------------------------------------------
#'
#'# For singler reader and single modality  case.
#'
#' dat <- list(c=c(3,2,1),    #Confidence level. Note that c is ignored.
#'             h=c(97,32,31), #Number of hits for each confidence level
#'             f=c(1,14,74),  #Number of false alarms for each confidence level
#'
#'             NL=259,        #Number of lesions
#'             NI=57,         #Number of images
#'             C=3)           #Number of confidence level
#'
#'
#'
#'
#'#  where,
#'#      c denotes confidence level, i.e., rating of reader.
#'#                3 = Definitely deseased,
#'#                2 = subtle,.. deseased
#'#                1 = very subtle
#'#      h denotes number of hits (True Positives: TP) for each confidence level,
#'#      f denotes number of false alarms (False Positives: FP) for each confidence level,
#'#      NL denotes number of lesions,
#'#      NI denotes number of images,

#'
#'
#'# For example, in the above example data,
#'#  the number of hits with confidence level 3 is 97,
#'#  the number of hits with confidence level 2 is 32,
#'#  the number of hits with confidence level 1 is 31,
#'
#'#  the number of false alarms with confidence level 3 is 1,
#'#  the number of false alarms with confidence level 2 is 14,
#'#  the number of false alarms with confidence level 1 is 74,
#'
#'
#'
#'#--------------------------------------------------------------------------------------
#'#                         2)       Fit the FROC model.
#'#--------------------------------------------------------------------------------------
#'
#'
#'
#'   #Since dataset named dat are single reader and single modality,
#'   #the function build the such model by running the following code.
#'
#'
#'
#'
#'
#'           fit <-   BayesianFROC::fit_Bayesian_FROC(
#'                                dat,       # dataset
#'                                ite=1111,  #To run in time <5s.
#'                                cha=1      # number of chains, it is better more large.
#'                                )
#'
#'
#'
#'
#'
#'
#'#--------------------------------------------------------------------------------------
#'#             3)  Draw the FROC curve and signal and noise (logarithmic Gaussian)
#'#--------------------------------------------------------------------------------------
#'
#'
#'#   Using fitted model object of class stanfitExtended, we can draw curves.
#'
#'     plot_curve_and_hit_rate_and_false_rate_simultaneously(fit)
#'
#'}
#'
plot_curve_and_hit_rate_and_false_rate_simultaneously <- function(StanS4class){

  fit <-StanS4class
  graphics::split.screen(figs = c(1, 2))
  graphics::split.screen(figs = c(2, 1), screen = 2)

graphics::screen(1)
DrawCurves(fit,new.imaging.device = F,Colour = F)
graphics::screen(3)
draw_bi_normal_version_UP(fit,hit.rate = T,false.alarm.rate = F,both.hit.and.false.rate = F,dark_theme = F,new.imaging.device = F)
graphics::screen(4)
draw_bi_normal_version_UP(fit,hit.rate = F,false.alarm.rate = T,both.hit.and.false.rate = F,dark_theme = F,new.imaging.device = F)
}
