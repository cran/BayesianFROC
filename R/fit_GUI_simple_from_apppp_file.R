

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
# @export
#' @examples
#'
#' \dontrun{
#'## Only run examples in interactive R sessions
#'if (interactive()) {
#'  #No need to consider the variables, it is sufficient in  default values.
#'  #fit_GUI_simple_from_apppp_file()
#'

#'}### Only run examples in interactive R sessions
#'}#'
#'
#'




fit_GUI_simple_from_apppp_file <- function(display.mode = FALSE) {


  appDir <- system.file("myapppp",
                        package = "BayesianFROC")
  if (system.file("myapppp",
                  package = "BayesianFROC") == "") {
    stop("Could not find myapppp. Try re-installing `BayesianFROC`.", call. = FALSE)
  }
  # shiny::runApp(list(ui=ui, server=server))

  if(display.mode==FALSE)  shiny::runApp(appDir, display.mode = "normal")
  if(display.mode==TRUE)  shiny::runApp(appDir, display.mode = "showcase")

}
