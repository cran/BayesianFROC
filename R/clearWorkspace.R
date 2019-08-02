#' @title  Clear Work Space
#'
#'
#'@description   If functions are masked in global environment, I use this.
#' this function has no variables.
#'@author Issei Tsunoda

# devtools::document();help("clearWorkspace") # Confirm reflection

#  @export clearWorkspace
clearWorkspace = function () {

  # Clear global workspace
  rm( list = ls( envir = globalenv() ), envir = globalenv() )
  message("Clear global workspace")
}

# clearAll = function(){
#   # Clear console, plots, and workspace
#   clearConsole()
#   clearPlots()
#   clearWorkspace()
# }
