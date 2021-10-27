
#' @title Fit with GUI via Shiny
#' @description First, please execute, then user will understand what this fucking program is.
#' This function is the one of the most important function in this package.
#' I do not assume  that the user is familiar with R script but FROC analysis. So, I made this function to provide the Graphical User Interface (GUI) for users.
#' I hope it helps someone in the world.
#'
#' @param display.mode Logical, passing to \code{runApp}.
#'  Default is \code{FALSE} corresponding to "normal",
#'   and if \code{TRUE}, then "showcase" which shows code.
#'   The author made this, but it did not work or ignored, that is, showcase did not work.
#'   Why???

#'
#' @return None
#' @export
#' @examples
#'
#' \dontrun{
#'## Only run examples in interactive R sessions
#'if (interactive()) {
#'  #No need to consider the variables, it is sufficient in  default values.
#'  #fit_GUI()
#'}### Only run examples in interactive R sessions
#'}#'
#'
#'
fit_GUI <- function(display.mode = FALSE) {

  appDir <- system.file("myapp",#Here I specified the directory myappp in which there are ui.R and server.R.
                        package = "BayesianFROC")
  if (system.file("myapp",#Here I specified the directory myappp in which there are ui.R and server.R.
                  package = "BayesianFROC") == "") {
    stop("Could not find myapp. Try re-installing `BayesianFROC`.", call. = FALSE)
  }
  # shiny::runApp(list(ui=ui, server=server))

  if(display.mode==FALSE)  shiny::runApp(appDir, display.mode = "normal")
  if(display.mode==TRUE)  shiny::runApp(appDir, display.mode = "showcase")

}



























#' @title Fit (very bad, MCMC not converge) ROC model with GUI via Shiny
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

#'
#' @return None
#' @export
#' @examples
#'
#' \dontrun{
#'## Only run examples in interactive R sessions
#'if (interactive()) {
#'  #No need to consider the variables, it is sufficient in  default values.
#'  #fit_GUI()
#'}### Only run examples in interactive R sessions
#'}#'
#'
#'
fit_GUI_ROC <- function(display.mode = FALSE) {

  appDir <- system.file("myappROC",#Here I specified the directory myappp in which there are ui.R and server.R.
                        package = "BayesianFROC")
  if (system.file("myappROC",#Here I specified the directory myappp in which there are ui.R and server.R.
                  package = "BayesianFROC") == "") {
    stop("Could not find myappROC Try re-installing `BayesianFROC`.", call. = FALSE)
  }
  # shiny::runApp(list(ui=ui, server=server))

  if(display.mode==FALSE)  shiny::runApp(appDir, display.mode = "normal")
  if(display.mode==TRUE)  shiny::runApp(appDir, display.mode = "showcase")

}




