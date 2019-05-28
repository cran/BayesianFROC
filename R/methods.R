#'@title   Definition of a method for the inherited class stanfitExtended from stanfit

#'
#'
#'@description This is a function for a method for a generic function \code{print()} for class "\code{\link{stanfitExtended}}"
#'@param x This is an object of an S4 class named stanfitExtended which is an inherited S4 class from the stanfit S4 class in the rstan package.
#' @export print_stanfitExtended
 print_stanfitExtended<-function (x)
{
   if(x@studyDesign=="srsc.per.image"||x@studyDesign=="srsc.per.lesion"){ summary_EAP_CI_srsc(x)}
   # if(x@studyDesign=="MRMC"){ summary_AUC_comparison_MRMC(x)}
   if(x@studyDesign=="MRMC"){ summarize_MRMC(x)}
message("\n\n\n\n")

  print(  methods::as(x, "stanfit"))
}



 #'@title   This is a method for a generic function \code{print()} for class "\code{\link{stanfitExtended}}"
 #'
 #'
 #'@description This is a method for print and stanfitExtended S4 class.
 #' @export
 #'@param x This is an object of an S4 class named stanfitExtended which is an inherited S4 class from the stanfit S4 class in the rstan package.
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




 setMethod("print",
           signature = "stanfitExtended",
           definition =print_stanfitExtended )

























 #'@title  # Definition of a method for the inherited class stanfitExtended from stanfit

 #'
 #'
 #'@description This is a function for a method in the generic function plot.
 #'@param x This is an object of an S4 class named stanfitExtended which is an inherited S4 class from the stanfit S4 class in the rstan package.
 #' @export
 plot_test<-function (x)
 {
   if(x@studyDesign=="srsc"){ plot(x@plotdata$x.FROC,x@plotdata$y.FROC )
      message("If you want to use plot as stan function, then please use plot( methods::as(  fitted model to your data , \"stanfit\")). ")

      message( "plot(fit, \"stanfitExtended\") is also available.")

# interactive choice is better.

      }
   if(x@studyDesign=="MRMC"){ message("Now prepare this method.")}

 }


 #'@title  A method for a generic function plot()
 #'
 #'
 #'@description This is a  method \code{print()} for a new  S4 class \strong{\code{ \link{stanfitExtended}}}.
 #' @details This is a inherited methods from a method of stanfit, plot as an stanfit object and further plot an FROC curve if not MRMC.
 #'
 #' @export
 #'@param x This is an object of an S4 class named \strong{\code{ \link{stanfitExtended}}} which is an inherited S4 class from the S4 class \strong{\emph{\code{\link[rstan]{stanfit}}}}  in the rstan package.

 setMethod("plot",
           signature(x = "stanfitExtended",y="missing"),
           definition = function(x){
             # grDevices::dev.new()
             #  xx <- methods::as(x,"stanfit")
             #  plot(xx)

              DrawCurves(x)

             message("\n* If you want to use the generic function \"plot()\" for the S4 class stanfit, then please use the code ", crayon::cyan$bold(" \"fit <- methods::as(fit,\"stanfit\")\" "), " to coerce S4 class from inherited class stanfitExtended to the class stanfit." )
             }

           )


#
#
# setMethod("summary",
#           signature(x = "stanfitExtended",y="missing"),
#           definition = function(x){  #plot( x@plotdata$x.AFROC, x@plotdata$y.AFROC)
#             DrawCurves(x)
#           }
#
#  )

 ##plot(fit, "stanfitExtended")

 # summary is used in old class, if we define a summary for
 # new class, then the codes use summary as new class leads us to errors.
 # summary and print is similar, so it seems better not to use the summary.
 # setMethod("summary",
 #           signature( object="stanfitExtended"),
 #           definition = function(object){
 #             if(object@studyDesign=="srsc"){ summary_EAP_CI_srsc(object)}
 #             if(object@studyDesign=="MRMC"){ summary_AUC_comparison_MRMC(object)}
 #
 #             print(  methods::as(object, "stanfit"))
 #
 #
 #           }
 #          )


###################################################
#
# plot_Bayesian_FROC <- function(x,...)UseMethod("plot_Bayesian_FROC",x)
#
# plot_Bayesian_FROC.stanfitExtended <- function(object,
#                                                         modalityID,
#                                                         readerID,
#                                                         mesh.for.drawing.curve=10000,
#                                                         Colour=TRUE,
#                                                         DrawFROCcurve=TRUE,
#                                                         DrawAFROCcurve=FALSE,
#                                                         DrawCFPCTP=TRUE,
#                                                         Draw.Flexible.upper_y=TRUE,
#                                                         Draw.Flexible.lower_y=TRUE){
#   if(StanS4class@studyDesign=="MRMC"){ DrawCurves_MRMC_pairwise(
#     object,
#     modalityID,
#     readerID,
#     mesh.for.drawing.curve=10000,
#     Colour=TRUE,
#     DrawFROCcurve=TRUE,
#     DrawAFROCcurve=FALSE,
#     DrawCFPCTP=TRUE,
#     Draw.Flexible.upper_y=TRUE,
#     Draw.Flexible.lower_y=TRUE
#
#
#   )}
#   if(StanS4class@studyDesign=="srsc"){ fit_Bayesian_FROC(StanS4class@dataList)}
#
# }#function

####################################################################









 # setMethod("aaa",
 #           signature=c(
 #             "stanfitExtended",
 #                  "numeric",
 #                   "numeric",
 #                    "numeric",
 #                  "logical",
 #                 "logical",
 #             "logical",
 #                  "logical",
 #                 "logical",
 #                 "logical"
 #             #
 #             #               # object="stanfitExtended",
 #             #               # modalityID ="numeric",
 #             #               # readerID   ="numeric",
 #             #               # mesh.for.drawing.curve="numeric",
 #             #               # Colour="logical",
 #             #               # DrawFROCcurve="logical",
 #             #               # DrawAFROCcurve="logical",
 #             #               # DrawCFPCTP="logical",
 #             #               # Draw.Flexible.upper_y="logical",
 #             #               # Draw.Flexible.lower_y="logical"
 #
 #           ),
 #           definition = function(object,
 #                                 modalityID,
 #                                 readerID,
 #                                 mesh.for.drawing.curve=10000,
 #                                 Colour=TRUE,
 #                                 DrawFROCcurve=TRUE,
 #                                 DrawAFROCcurve=FALSE,
 #                                 DrawCFPCTP=TRUE,
 #                                 Draw.Flexible.upper_y=TRUE,
 #                                 Draw.Flexible.lower_y=TRUE){
 #             if(StanS4class@studyDesign=="MRMC"){ DrawCurves_MRMC_pairwise(
 #               object,
 #               modalityID,
 #               readerID,
 #               mesh.for.drawing.curve=10000,
 #               Colour=TRUE,
 #               DrawFROCcurve=TRUE,
 #               DrawAFROCcurve=FALSE,
 #               DrawCFPCTP=TRUE,
 #               Draw.Flexible.upper_y=TRUE,
 #               Draw.Flexible.lower_y=TRUE
 #
 #
 #             )}
 #             if(StanS4class@studyDesign=="srsc"){ fit_Bayesian_FROC(StanS4class@dataList)}
 #
 #           }#function
 # )
 #
 #
 #
 #
