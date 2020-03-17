#'@title   Definition of a method for the inherited class stanfitExtended from stanfit
#'@description This is a function for a method for a generic function \code{print()} for class "\code{\link{stanfitExtended}}"
#'@param x This is an \R object of an S4 class named \code{\link{stanfitExtended}}  inherited  class from the stanfit in the rstan package.
#'@export print_stanfitExtended
#'
#'@details
#'
#' Print of stanfit has many parameters, but one of them, the AUC is the most important parameter. Thus in particular, we explain how to interprete the print out messages for AUCs.
#'
#'
#'
#'
#'    --------   Print of \code{stanfit} object  -----------------------------------------------
#'
#'
#'   * The  AUC denoted by \code{AA[modalityID , readerID]} are shown by the function \code{print()} with a stanfit object.
#'
#'   * The column of 2.5\% and 97.5\% means the lower and upper bounds of the 95% Credible Interval of AUCs.
#'
#'   * For example, \code{AA[2,3]} means the AUC of the 2 nd modality and the 3 rd reader.
#'

#'
print_stanfitExtended<-function (x)
{
  if(x@studyDesign=="srsc.per.image"||x@studyDesign=="srsc.per.lesion"){ summary_EAP_CI_srsc(x)}
  # if(x@studyDesign=="MRMC"){ summary_AUC_comparison_MRMC(x)}
  if(x@studyDesign=="MRMC"){ summarize_MRMC(x)}
  message("\n\n\n\n")

  print(  methods::as(x, "stanfit"))
}


















#'@title   A method for a generic function \code{print()} for class "\code{\link{stanfitExtended}}"
#'@description This is a method for print and \code{\link{stanfitExtended}} S4 class.
#'@param x An S4 object of  class \code{\link{stanfitExtended}} inherited  from the class stanfit in the rstan package.
#' @examples
#'  \donttest{

# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'# How to use a new method for generic function "print".

#'#=============================The First Example======================================
#'
#'
#'#(1)First, we prepare the example data from this package.
#'
#'
#'
#'                       dat  <- BayesianFROC::dataList.Chakra.1
#'
#'
#'
#'# The R object named dat is a list which contains the hits and false alarms representing
#'# an FROC dataset. To confirm it, the function viewdata() can be used;
#'
#'
#'
#'                               viewdata(dat)
#'
#'
#'
#'
#'#(2)Second, we run fit_Bayesian_FROC() in which the rstan::sampling() is implemented.
#'#Fit to data named "dat"   the author's Bayesian model by
#'
#'
#'
#'                          fit <-  fit_Bayesian_FROC(dat)
#'
#'
#'
#'
#' #(3)Thirdly, we obtain the R object fit of  S4 class
#' # named stanfitExtended that is an inherited class from the  S4 class stanfit
#' # defined in the package rstan.
#' # For the S4 class stanfitExtended defined in this package, we can use
#' # the generic function print for this new S4 class.
#'
#'
#'
#'                                print(fit)
#'
#'
#'
#'# To use the generic functin print() as a  object of class "stanfit",
#'#  we coerce class of fit into stanfit from stanfitExtended as follows;
#'
#'
#'
#'
#'                             fitt <- methods::as(fit,"stanfit")
#'
#'
#'
#'
#'# THe R object "fitt" is a fitted model object of class stanfit,
#'# thus we can also apply the generic function print() as follows:
#'
#'
#'
#'                                print(fitt)

#'
#'
#'

#'#=============================The Second Example======================================
#'
#'
#'#(1)First, we prepare the example data from this package.
#'
#'                       dat  <- BayesianFROC::dataList.Chakra.Web
#'
#'
#'#(2)Second, we run fit_Bayesian_FROC() in which the rstan::sampling() is implemented.
#'#Fit to data named "dat"   the author's Bayesian model by
#'
#'
#'                         fit <-  fit_Bayesian_FROC(dat)
#'
#' #(3)Thirdly, we obtain the R object fit of  S4 class
#' # named stanfitExtended that is an inherited class from the  S4 class stanfit
#' # defined in the package rstan.
#' # For the S4 class stanfitExtended defined in this package, we can use
#' # the generic function print for this new S4 class.
#'
#'
#'
#'                             print(fit)
#'
#'
#'
#'
#'
#' # 2019.05.21 Revised.
#'
#'
#'}# dottest
#'
#'
#'




# @description generic function

# @docType methods
# @rdname print-methods
 # @name  print-methods

 # methods::setGeneric("print")

#' @title Method for generic function \code{print()}
# @description Associeate generic with method
# @name print
# @rdname printt
# @aliases print,ANY,ANY-method
methods::setMethod("print",
                   signature = "stanfitExtended",
                   definition =print_stanfitExtended )









#---------------------------------------------------------------
#'@include methods_print.R stanfitExtended.R
NULL
# Since print are used the definition of stanfitExtended, we have to load the file stanfitExtended.R beforhand.
# To do so, this code is requiered and without it, it causes error.
#------------------------------------------------------------











