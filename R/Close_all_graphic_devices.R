

#' @title Close the Graphic Device
#'
#'@description
#'  Close the graphic device to avoid errors in R CMD check.
#' @export
#'
#' @examples
#'
#' \dontrun{

#'  #    Open the graphic devices
#'
#'    grDevices::dev.new();plot(stats::runif(100),stats::runif(100))
#'    grDevices::dev.new();plot(stats::runif(100),stats::runif(100))
#'    grDevices::dev.new();plot(stats::runif(100),stats::runif(100))
#'    grDevices::dev.new();plot(stats::runif(100),stats::runif(100))
#'    grDevices::dev.new();plot(stats::runif(100),stats::runif(100))
#'    grDevices::dev.new();plot(stats::runif(100),stats::runif(100))
#'
#'
#' #    Close the graphic device
#'
#' ##       Close_all_graphic_devices()
#'
#'
#'#'}# dottest

Close_all_graphic_devices <- function(){


# if (grDevices::dev.cur()>=4) {

     # for (i in 3:grDevices::dev.cur()-1)  {
     #   message("\n*  The ",i,"-th graphic device is omitted.\n" ,sep = "")
       while (!is.null(grDevices::dev.list()))   grDevices::dev.off()
     # }
# }
#   if (grDevices::dev.cur()==1||grDevices::dev.cur()==2){
#     message("\n*  It is sufficient to run R CMD check.")
# }
  }# function
