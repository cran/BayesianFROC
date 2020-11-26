






#' @title Calculation of P values are visualized
#' @description The author suspect the p value calculation in this Pkg is wrong, cuz it is always accept the null hypothesis.
#' Why? So,,, I made this to examine the process of comparison between observed chisquare and that of post. pred. dist.
#' Hahh, why? My intuition is wrong? or  program is incorrect?? I'm not gonna write program.
#'
#' @param return_of_ppp_srsc An object of return value of \code{ppp_srsc}()
#'
#' @return NaN
#' @export
#'
#' @examples
#'
#'  \dontrun{
#'   f <- fit_Bayesian_FROC(
#'        ite  = 55,
#'         summary = TRUE,
#'         cha = 1,
#'          dataList = dataList.Chakra.1
#'          )
#'
#'   a<- ppp_srsc(f)
#'
#'   p_value_visualization(a)
#'
#' }
#'
#'
p_value_visualization  <- function(return_of_ppp_srsc) {

a <- return_of_ppp_srsc

dark_theme()
obs <- a$chisq_at_observed_data
ylim = c(min(obs),max(obs))

b <- a$chisq_Not_at_observed_data.list[[1]]

MMM <- max(max(obs),max(b))
mmm <- min(min(obs),min(b))
ylim = c(mmm,MMM)
title_of_plot <- "Yellow is observed chisq and red is chisq from Post. Pred. dist."
plot (1:length(obs),obs,type ="l",ylim = ylim, col ="yellow", xlab ="" , ylab ="",main = title_of_plot)
suppressWarnings(graphics::par(new=TRUE));plot (1:length(b),b,type ="l", col ="red",ylim = ylim,xlab ="", ylab ="",main = title_of_plot)

mean(b>obs)
graphics::abline(h=  mean(b), col="red",lty ="dashed",lwd ="2")
graphics::abline(h=  mean(obs), col="yellow",lty ="dashed",lwd ="2")


 plus <- (b>obs)
 plus <- as.integer(plus)
 # graphics::abline(v =( 1:length(obs))[b>obs], untf = FALSE, col="yellow",lty ="dotdash",lwd ="2");
 graphics::abline(v =( 1:length(obs))[b<=obs], untf = FALSE, col="gray",lty ="dotdash",lwd ="1");

 invisible(NaN)

}
