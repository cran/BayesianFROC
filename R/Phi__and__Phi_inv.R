

#' @title The Cumulative distribution function \eqn{\Phi(x)}
#'  of the Standard Gaussian, namely, mean = 0 and variance =1.

#'
#' @description
#'
#'  \deqn{\Phi(x):= \int_{-\infty}^x \frac{1}{\sqrt{2\pi}} e^{\frac{-x^2}{2}}   }

#'
#' @param x  A real. To be passed to
#' the function \code{stats::pnorm()}

#' @return \eqn{\Phi(x) := \int _{-\infty}^x Gaussian(z|0,1)dz   }
#' @seealso \code{\link{Phi_inv}()}
#' @export
#'
#' @examples
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#            1)             validation of this function
#'#========================================================================================
#'#'
#'   x<-0.2
#'  Phi(x)==stats::pnorm(x)
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#            1)             Build the data
#'#========================================================================================
#'#'
#'
#'  a  <-  0.1;
#'  NX <-  222;
#'  x  <-  runif(100,-11,11)
#'  y  <-  Phi_inv(exp(a/NX) *Phi(x))-x
#'  plot(x,y)
#'
#'  a  <-  0.1;
#'  NX <-  222;
#'  x  <-  runif(100,0,11)
#'  y  <-  Phi_inv(exp(a/NX) *Phi(x))-x
#'  plot(x,y)
#'
#'
#'  a  <-  0.1;
#'  NX <-  222;
#'  x  <-  runif(100,2,4)
#'  y  <-  Phi_inv(exp(a/NX) *Phi(x))-x
#'  plot(x,y)
#'
#'  a  <-  0.01;
#'  NX <-  222;
#'  x  <-  runif(100,2,4);
#'  y  <-  Phi_inv(exp(a/NX) *Phi(x))-x
#'  plot(x,y)
#'
#'
#'
#'
#'  a  <-  0.01;
#'  NX <-  222;
#'  x  <-  runif(100,3.5,4);
#'  y  <-  Phi_inv(exp(a/NX) *Phi(x))-x
#'  plot(x,y)
#'
#'


Phi  <- function(x) {
  y<-  stats::pnorm(q=x)
  return(y)
}







# @describeIn Phi
# @rdname  Phi
#' @title Inverse function of the Cumulative distribution
#'  function \eqn{\Phi(x)} of the Standard Gaussian.
#'  where \eqn{x} is a  real number.
#' @description
#' The author is confused  \code{stats::qnorm()}
#'  with  \code{stats::pnorm()} and thus he made this.
#'
#' @param x  A real. To be passed
#' to the function \code{stats::qnorm()}
#' @seealso \code{\link{Phi}()},  \code{\link{inv_Phi}()}

#' @return A real number: \eqn{\Phi^{-1}(x)}
#' @export
#' @details In Stan file, it is \code{inv_Phi()}
#' and not \code{inv_phi}.
#'
#' Since  \eqn{\Phi(x)} is monotonic, it follows that
#'  \eqn{\frac{d}{dx}\Phi^{-1} = (\frac{d}{dx}\Phi)^{-1}  >0},
#'  and thus  \eqn{\Phi^{-1}(x)} is also monotonic.
#' @examples
#'
#'
#'
#'
#'          x <- runif(100)

#'
#'  Phi_inv(x) == stats::qnorm(x)
#'
#'  inv_Phi(x) == stats::qnorm(x)
#'
#'
#'
#'
#'
#'



Phi_inv <- function(x) {
  y<-  stats::qnorm(x)
  return(y)
}




# @describeIn Phi
# @rdname  Phi
#' @title Inverse function of the Cumulative distribution
#'  function \eqn{\Phi(x)} of the Standard Gaussian.
#'  where \eqn{x} is a  real number.
#' @description
#' The author is confused  \code{stats::qnorm()}
#'  with  \code{stats::pnorm()} and thus he made this.
#'
#' @param x  A real. To be passed
#' to the function \code{stats::qnorm()}
#' @seealso \code{\link{Phi}()},  \code{\link{Phi_inv}()}

#' @return A real number: \eqn{\Phi^{-1}(x)}
#' @export
#' @details In Stan file, it is \code{inv_Phi()}
#' and not \code{inv_phi}.
#'
#' Since  \eqn{\Phi(x)} is monotonic, it follows that
#'  \eqn{\frac{d}{dx}\Phi^{-1} = (\frac{d}{dx}\Phi)^{-1}  >0},
#'  and thus  \eqn{\Phi^{-1}(x)} is also monotonic.
#' @examples
#'
#'
#'
#'
#'          x <- runif(100)

#'
#'  Phi_inv(x) == stats::qnorm(x)
#'
#'  inv_Phi(x) == stats::qnorm(x)
#'
#'
#'
#'
#'


inv_Phi <- function(x) {
  y<-  stats::qnorm(x)
  return(y)
}
