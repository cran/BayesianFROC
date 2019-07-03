



runExample <- function() {


  # if (system.file("myapp", package = "BayesianFROC") == "") {
  #   stop("Could not find myapp. Try re-installing `BayesianFROC`.", call. = FALSE)
  # }

  appDir <- system.file("myapp", package = "BayesianFROC")
  if (system.file("myapp", package = "BayesianFROC") == "") {
    stop("Could not find myapp. Try re-installing `BayesianFROC`.", call. = FALSE)
  }
  # shiny::runApp(list(ui=ui, server=server))

  shiny::runApp(appDir, display.mode = "normal")
}
