
#' @title Fit with GUI via Shiny
#' @description First, please execute, then user will understand what it is.
#' This function is the one of the most important function in this package.
#' I do not assume  that the user is familiar with R script but FROC analysis. So, I made this function to provide the Graphical User Interface (GUI) for users.
#' I hope it helps someone in the world.
#'
#' @param display.mode Logical, passing to \code{runApp}.
#'  Default is \code{FALSE} corresponding to "normal",
#'   and if \code{TRUE}, then "showcase" which shows code.
#'   The author made this, but it did not work or ignored, that is, showcase did not work.
#'   Why???
#'@author Issei Tsunoda

#'
#' @return None
#' @export
#' @examples
#'
#' \donttest{
#'
#'  #No need to consider the variables, it is sufficient in  default values.
#'  fit_GUI()
#'
#'}#'
#'
#'
fit_GUI <- function(display.mode = FALSE) {

  appDir <- system.file("myapp", package = "BayesianFROC")
  if (system.file("myapp", package = "BayesianFROC") == "") {
    stop("Could not find myapp. Try re-installing `BayesianFROC`.", call. = FALSE)
  }
  # shiny::runApp(list(ui=ui, server=server))

  if(display.mode==FALSE)  shiny::runApp(appDir, display.mode = "normal")
  if(display.mode==TRUE)  shiny::runApp(appDir, display.mode = "showcase")

}




#' @title Fit with GUI via Shiny
#' @description First, please execute, then user will understand what it is.
#' This function is the one of the most important function in this package.
#' I do not assume the user is familiar with R script but FROC analysis. So, I made this function to provide the Graphical User Interface (GUI) for users.
#' I hope it helps someone in the world.

#'
#' @return None
#' @export
#'
fit_GUI_MRMC <- function() {

  appDir <- system.file("myappp", package = "BayesianFROC")
  if (system.file("myappp", package = "BayesianFROC") == "") {
    stop("Could not find myappp. Try re-installing `BayesianFROC`.", call. = FALSE)
  }
  # shiny::runApp(list(ui=ui, server=server))

  shiny::runApp(appDir, display.mode = "normal")
}
