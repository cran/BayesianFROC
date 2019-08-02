

#' @title pnorm or qnorm
#' @description Thu author always forget which is cumulative distribution of Gaussia, so I made this and this tells me which is mmy desired one.
#' In this package, I often use \eqn{\Phi()} for the standard Gaussian, and it is \code{pnorm}.
#' I am very confuse, since probability density  has initial alphabet p, but \code{pnorm} is not it.

pnorm_or_qnorm <- function(){


  message(" cumulative distribution function is stats::pnorm")


  message("

  x <-stats::rnorm(10000)
  plot(x,stats::pnorm(x))

                  ")

   x <-stats::rnorm(10000)
  plot(x,stats::pnorm(x))


}

Phi <- stats::pnorm
Phi_inverse <- stats::qnorm
