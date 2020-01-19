
#' @title Fit with GUI via Shiny in case of MRMC
#' @description First, please execute, then user will understand what it is.
#' This function is the one of the most important function in this package.
#' I do not assume the user is familiar with R script but FROC analysis. So, I made this function to provide the Graphical User Interface (GUI) for users.
#' I hope it helps someone in the world.
#' @param M No. of modalities
#' @param Q No. of readers
#' @param C No. of confidence levels
#' revised 2019 Nov. 21
#' @return None
#'
# @export
#'
fit_GUI_MRMC <- function(M=2,Q=3,C=4) {

  # DF<-data.frame(
  #   m =m_q_c_vector_from_M_Q_C(M,Q,C)$m,
  #   q=m_q_c_vector_from_M_Q_C(M,Q,C)$q,
  #   c=m_q_c_vector_from_M_Q_C(M,Q,C)$c
  #   # h=ddd$h,
  #   # f=ddd$f
  # )


  appDir <- system.file("myappp", #Here I specified the directory myappp in which there are ui.R and server.R.
                        package = "BayesianFROC")
  if (system.file("myappp",
                  package = "BayesianFROC") == "") {
    stop("Could not find myappp. Try re-installing `BayesianFROC`.", call. = FALSE)
  }
  # shiny::runApp(list(ui=ui, server=server))

  shiny::runApp(appDir, display.mode = "normal")
}
