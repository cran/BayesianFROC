
##########################################################
#
#
# tryCatch({
#   library(ggmcmc)
# }, error = function(e) {
#   install.packages("ggmcmc")
#   library(ggmcmc)
# })
#
#
# if("ggmcmc" %in% installed.packages()[,1]){
#   library(ggmcmc)
# }else{
#   install.packages("ggmcmc")
#   library(ggmcmc)
# }

########################################################
#
#
# tryCatch({
#   DrawCurves(f, new.imaging.device = F)
# }, error = function(e) {
#
#   error_plot()
#   })



#' @title Check whether class is \emph{stanfitExtended} for any \R object
#'
#' @param any_R_object any R object
#'
#' @return logical
#' @export
#'
# @examples
#'
#'
is_stanfitExtended <- function(any_R_object){
class(any_R_object)=="stanfitExtended"
  }

error_plot <- function(){

    small_margin()
  dark_theme(2)
  suppressWarnings(graphics::par(new=FALSE));

    plot(0,0,type="n", axes=FALSE,xlim=c(0,1),ylim =c(0,1),xaxt="n", yaxt="n",xlab="Please fix inconsistent data",ylab="",
         main="Error")

    graphics::text(0.5,0.5,
                   c("
   o o          ooo
 o    o        o   o
  o    oooo   o
   o              o
  o     |        |    o
 o         *        o
  o                o
   oooooo


I intended to use
this in Shiny, but now,
I did not success
to exhibite this in GUIs.


                   "  ),col="red",cex =    0.7  )


  }










#
#
# tryCatch({
#  fit_Bayesian_FROC( ite  = 111, summary = TRUE,  cha = 1, dataList = dd ,see = 3129)
#     }, error = function(e) {
#
#   message("aaaaaaaaaa")
# })
#
