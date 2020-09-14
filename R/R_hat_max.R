
#' @title Max R hat
#'
#' @param  StanS4class A stanfit object.
#'
#' @return A real number, indicating the maximal R hat over all parameters.
#' @export
#'
# @examples
R_hat_max <- function(StanS4class){

  f <- methods::as(StanS4class,"stanfit")

max.rhat <-  round( max(summary(f)$summary[,"Rhat"]) ,digits = 5)

return(max.rhat)
}
