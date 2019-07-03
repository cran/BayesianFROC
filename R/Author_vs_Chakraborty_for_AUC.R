
#' @title validation of AUC calculation
#' @description This is for the author.
#'
#' @return AUCs
# @export
#'
#'@inheritParams DrawCurves
Author_vs_classic_for_AUC <- function(StanS4class){
fit <- methods::as(StanS4class,"stanfit")
a<-get_posterior_mean(fit,pars=c("a"))[1:fit@sim$chains]
b<-get_posterior_mean(fit,pars=c("b"))[1:fit@sim$chains]
a<-mean(a)
b<-mean(b)

f <-function(x) { 1-stats::pnorm(b*stats::qnorm( 1-x )-a)}

True_AUC <- stats::integrate(Vectorize(f), 0, 1)
A<-rstan::get_posterior_mean(fit,pars=c("A"))[1:fit@sim$chains]

AUC_of_my_manner<-1-stats::pnorm(-a/sqrt(1+b^2),0,1)
AUC_of_Chakraborty_manner<-stats::pnorm(a/sqrt(1+b^2),0,1)

return(
  list(
    True_AUC=True_AUC,
  AUC_of_my_manner=AUC_of_my_manner,
  AUC_of_Chakraborty_manner=AUC_of_Chakraborty_manner
  )
)
}#function
