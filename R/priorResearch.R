#' @title Research for Prior
#' @description The autor investigates prior
#'
#' @param m a real number, specifying the mean of signal Gaussian
#' @param sd a real number, specifying the standard deviation of signal Gaussian
#' @param z a real number, indicating \eqn{\theta_c}.
#' @param e a positive real number, indicating \eqn{\epsilon}.
#'
#' @return A real, to investigate prior
#'
#'  \deqn{ \mu   + \sigma \Phi^{-1}(1- \epsilon + \Phi( \frac{\theta_{c} - \mu }{\sigma} ) ) - \Phi^{-1}( \Phi( \theta) \exp(\epsilon ))   }
#'
#'  where,
#'  \code{m =  } \eqn{\mu},
#'  \code{sd =  } \eqn{\sigma},
#'  \code{z =  } \eqn{\theta},
#'  \code{e =  } \eqn{\epsilon}.
#'
#' @export
#'
#' @examples
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#            From this plot, we can evaluate the minimum value of x such that
#'#            the value is negative.
#'#========================================================================================
#'
#'
#'       x <- runif(100,-1,3 )  # Syntheisze 100 smaples from Uniform(-1,3)
#'       y <- priorResearch(x)
#'
#'      plot(x,y)
#'
#'
#'
#'
priorResearch <- function(z,m=6,sd=1,e =0.01) {

  message("m + sd*Phi_inv(e)      = ",m + sd*Phi_inv(e) )
  if (0==sum(z < m + sd*Phi_inv(e)) )return (message( "z should be smaller than ",m + sd*Phi_inv(e) ))
  message("1-e +Phi(  (z-m)/sd  ) = ",1-e +Phi(  (z-m)/sd  )  )
  message(" m+sd*Phi_inv(1-e +Phi(  (z-m)/sd  )) -Phi_inv(Phi(z)*exp(e)) = ",    m+sd*Phi_inv(1-e +Phi(  (z-m)/sd  )) -Phi_inv(Phi(z)*exp(e))  )
  y <- m+sd*Phi_inv(1-e +Phi(  (z-m)/sd  )) -Phi_inv(Phi(z)*exp(e))
  invisible(y)


}
