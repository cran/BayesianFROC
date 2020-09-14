


#' @title Compile all stanfiles in pkg BayesianFROC
#'
#' @return none
#' @export
#'
#' @examples
#' \dontrun{
#' # compile_all_models_in_pkg_BayesianFROC()
#' }
#'
compile_all_models_in_pkg_BayesianFROC  <- function() {



    stanfile <- dir( system.file("extdata","", package="BayesianFROC"))

    NNN <- length(stanfile)

    for (id in 2:NNN) {

      scr <- system.file("extdata",stanfile[id] , package="BayesianFROC")
      rstan::stan_model(scr)
      print(paste("Th ", id, "-th stan-file is compiled.",sep = ""))

    }



}
