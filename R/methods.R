
#---------------------------------------------------------------
#'@include methods.R stanfitExtended.R
NULL
# Since print are used the definition of stanfitExtended, we have to load the file stanfitExtended.R beforhand.
# To do so, this code is requiered and without it, it causes error.
#------------------------------------------------------------


 #' @title  A  generic function \code{plot()}
 #' @docType  methods
 #' @param x An \R object of the S4 class (\code{\link{stanfitExtended}})
 #' @param y An \R object of the S4 class   \strong{\emph{\code{\link[methods]{missing-class}}}}.

 #' @param ... Additional arguments
 #'
#' @aliases plot,ANY,ANY-method ======
 methods::setMethod("plot",
           signature(x = "stanfitExtended",y="missing"),
           definition = function(x,...){
             # grDevices::dev.new()
             #  xx <- methods::as(x,"stanfit")
             #  plot(xx)

              DrawCurves(x,
                         new.imaging.device = F, # If TRUE, then Shiny cannot plot, so to plot it in shiny it should be FALSE
                         ...
                         )



             message("\n* If you want to use the generic function \"plot()\" for the S4 class stanfit, then please use the code ", crayon::cyan$bold(" \"fit <- methods::as(fit,\"stanfit\")\" "), " to coerce S4 class from inherited class stanfitExtended to the class stanfit." )
             }

           )






 #'@title  # Definition of a method for the inherited class stanfitExtended from stanfit
 #'@description This is a function for a method in the generic function plot.
 #'@param x This is an object of an S4 class named stanfitExtended which is an inherited S4 class from the stanfit S4 class in the rstan package.
 #' @export
 plot_test<-function (x)
 {
    if(x@studyDesign=="srsc"){ plot(x@plotdata$x.FROC,x@plotdata$y.FROC )
       message("If you want to use plot as stan function, then please use plot( methods::as(  fitted model to your data , \"stanfit\")). ")

       message( "plot(fit, \"stanfitExtended\") is also available.")

    }
    if(x@studyDesign=="MRMC"){ message("Now prepare this method.")}

 }

