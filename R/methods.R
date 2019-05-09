#'@title  # Definition of a method for the inherited class stanfitExtended from stanfit

#'
#'
#'@description This is a function for a method in the generic function print.
#'@param x This is an object of an S4 class named stanfitExtended which is an inherited S4 class from the stanfit S4 class in the rstan package.
#' @export print_stanfitExtended
 print_stanfitExtended<-function (x)
{
   if(x@studyDesign=="srsc"){ summary_EAP_CI_srsc(x)}
   # if(x@studyDesign=="MRMC"){ summary_AUC_comparison_MRMC(x)}
   if(x@studyDesign=="MRMC"){ summarize_MRMC(x)}

  print(  methods::as(x, "stanfit"))
}



 #'@title   This is a method for a generic function print
 #'
 #'
 #'@description This is a method for print and stanfitExtended S4 class.
 #' @export
 #'@param x This is an object of an S4 class named stanfitExtended which is an inherited S4 class from the stanfit S4 class in the rstan package.
#' @examples
#'  \donttest{

#'
 #'#================The First Example======================================
 #'
 #'
 #'#(1)First, we prepare the data from this package.
 #'
 #'          dat  <- BayesianFROC::dataList.Chakra.1
 #'
 #'
 #'#(2)Second, we run fit_Bayesian_FROC() in which the rstan::stan() is implemented.
 #'#with data named "dat"  and the author's Bayesian model.
 #'
 #'
 #'          fit <-  fit_Bayesian_FROC(dat)
 #'
 #' #(3)Thirdly, we obtain the return value fit which is an object from an S4 class
 #' # named stanfitExtended that is an inherited class from rstan S4 class stanfit.
 #' # For the S4 class stanfitExtended defined in this package, we can use
 #' # the generic function print for this new S4 class.
 #'
 #'        print(fit)
 #'
 #'
 #'

 #'#================The Second Example======================================
 #'
 #'
 #'#(1)First, we prepare the data from this package.
 #'
 #'          dat  <- BayesianFROC::dataList.Chakra.Web
 #'
 #'
 #'#(2)Second, we run fit_Bayesian_FROC() in which the rstan::stan() is implemented.
 #'#with data named "dat"  and the author's Bayesian model.
 #'
 #'
 #'          fit <-  fit_Bayesian_FROC(dat)
 #'
 #' #(3)Thirdly, we obtain the return value fit which is an object from an S4 class
 #' # named stanfitExtended that is an inherited class from rstan S4 class stanfit.
 #' # For the S4 class stanfitExtended defined in this package, we can use
 #' # the generic function print for this new S4 class.
 #'
 #'        print(fit)
 #'
 #'
 #'
 #'
 #'
 #'
 #'
 #'# Adding a new method for generic function "print".
 #'
 #'}# dottest



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
 #'@title   This is a method for a generic function print
 #'
 #'
 #'@description This is a method for print and stanfitExtended S4 class.
 #' @export
 #'@param x This is an object of an S4 class named stanfitExtended which is an inherited S4 class from the stanfit S4 class in the rstan package.

 setMethod("plot",
           signature(x = "stanfitExtended",y="missing"),
           definition = function(x){
             # grDevices::dev.new()
             #  xx <- methods::as(x,"stanfit")
             #  plot(xx)

              DrawCurves(x)

             message("\n* If you want to use the generic function \"plot()\" for the S4 class stanfit, then please use the code \"fit <- methods::as(fit,\"stanfit\")\" to convert S4 class." )
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
