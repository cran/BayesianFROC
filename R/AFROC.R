# @title AFROC -----
#' @title AF\strong{ROC} curve (alternative free-response \strong{ROC} curve)
#' @description An AFROC curve is a plane curve whose area under the curve (AUC) indicates an observer performance ability.
#'  In the following, \eqn{\Phi()} denotes the cumulative distribution function
#'  on the standard Gaussian disribution.
#'
#'
#' The so-called \emph{AFROC} curve is defined by
#'
#'
#' \deqn{  (\xi(t),\eta(t) ) =(1-e^{-t}, \Phi( b\Phi^{-1}(\exp(-t) )- a   ) )}
#'
#' for all \eqn{t >0} and some fixed real numbers \eqn{a,b}.
#'
#'
#'
#' By specifying  two real numbers \eqn{a} and \eqn{b}, we can plot  an AFROC curve.
#'
#'
#'
#' The are under the AFROC curve, or  breafly AUC,  is calculated as follows, whic
#' are used to evaluate how  physicians detect lesions in radiographs.
#'
#' \deqn{  AUC = \int \eta(t) d\xi(t) = \frac{ a }{ \sqrt{1+  b^2} }. }
#'
#' Note that the so-called FROC curve can be interpreted as the curve of expectations of data points.
#' On the other hand, AFROC curve cannot be interpreted as the fitted curve, but its AUC is finite.
#' Because AFROC can be obtained by modifying FROC curve, it reflects obeserver performance.
#'
#'
#' @param t A real number which moves in the domain  of FROC curve
#' @param a,b One of the parameter of model which characterize AFROC curve
# @param b One of the parameter of model which characterize AFROC curve
#'
#' @param x.coordinate.also Logical, whether a vector of \code{1-exp(-t)}
#'   is included in a return value.
#' @return
#' if \code{x.coordinate.also =TRUE}, then
#' A list, contains two vectors as x,y cooridinates of the AFROC curve
#'  for drawing curves.
#'  if \code{x.coordinate.also =FALSE}, then
#'  return is  a vector as y coodinates of the AFROC curve exclueded its x-coordinates.
#'   (x coodinates is omitted.)
#' @export
#'
#' @examples
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#             Plot AFROC curve
#'#========================================================================================
#'
#' tt <- seq(0, 1, length.out = 111)
#' ttt <- stats::runif(1000,0.001,100)
#' t <- c(tt,ttt)
#' a <-  AFROC(t,x.coordinate.also=TRUE)
#'
#' plot(a$x,a$y)
#'
#' # We note that the x-coordinates of AFROC curve is not t but x = 1 - exp(-t).
#' # To emphasize that x-coordinates is not t, we prepare the another example
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#             Plot AFROC curve
#'#========================================================================================
#'
#' tt <- seq(0, 1, length.out = 111)
#' ttt <- stats::runif(1000,0.001,100)
#' t <- c(tt,ttt)
#' y <-  AFROC(t,x.coordinate.also=FALSE)
#'
#' plot(1-exp(-t),y)
#'
#'
#'

AFROC <- function(
  t,
  a =  0.14,
  b =  0.19,
  x.coordinate.also = FALSE

){


 y<- 1 - stats::pnorm(b * stats::qnorm( exp(-t)) - a)




if (  x.coordinate.also == TRUE) {
            return(list(
                x=1-exp(-t),
                y=y
              ))
            }
 if (  x.coordinate.also == FALSE) {
   return(y)

 }
}



# @describeIn AFROC
#' @title Draw the Region of AUC of AFROC
#' @description An AFROC curve has two parameter denoted by \eqn{a,b}.
#' By specifying   \eqn{a,b}, we can draw  an AFROC curve.
#'
#'
#'
#' Def of AFROC
#'
#' \deqn{  (\xi(t),\eta(t) ) =(1-e^{-t}, \Phi( b\Phi^{-1}(\exp(-t) )- a   ) ).}
#'
#'
#' Def of AUC of AFROC
#'
#' \deqn{  AUC = \int \eta d \xi = \frac{ a }{ \sqrt{1+  b^2} }. }
#'
#'
#' @details
#'
#' We define the so-called FROC curve as a map from 1-dimensional Euclidean space to
#' 2-dimensional Euclidean space, mapping each \eqn{t>0} to
#'
#' \deqn{  (x(t),y(t) ) =(t, \Phi(\frac{ \Phi^{-1}(\exp(-t)) - \mu}{\sigma})   ) }
#'
#'
#' Sine \eqn{x(t)=t,t>0} is not bounded, the area under the FROC curve is infinity.
#'
#' To calculates aleternative notion of AUC in the ordinal ROC theory, we define the so-called
#' AFROC curve:
#'
#' \deqn{  (\xi(t),\eta(t) ) =(1-e^{-t}, \Phi(\frac{ \Phi^{-1}(\exp(-t)) - \mu}{\sigma})   ) }
#'
#'
#' which contained in the rectangular space \eqn{[0,1]^2}.
#' Introducing new parameter \eqn{a:= \mu / \sigma} and \eqn{b:= 1 / \sigma}, we also write
#' \deqn{  (\xi(t),\eta(t) ) =(1-e^{-t}, \Phi( b\Phi^{-1}(\exp(-t) )- a   ) )}

#'
#' The area Under the (AFROC) curve  (breifly, we call it AUC) represents the observer performance.
#' For example, if radiologist detects more lesions with small False Positives (FPs), then AUC would be high.
#'
#'Using the parameter of the signal distribution, we express AUC as follows,
#'
#' \deqn{  AUC = \frac{ \mu / \sigma}{  \sqrt{1+  1/\sigma^2}  }.}
#'
#'
#' Using new parameter \eqn{a:= \mu / \sigma} and \eqn{b:= 1 / \sigma}, we also write
#'
#'
#' \deqn{  AUC = \frac{ a }{ \sqrt{1+  b^2} }. }
#'
#' @param a One of the parameter of model which characterize AFROC curve
#' @param b One of the parameter of model which characterize AFROC curve
#'
#' @inheritParams fit_srsc
# @inheritParams AFROC

#' @return none.
#' @export
#'
#' @examples
#'
#'
#'
#'         Draw_AUC()
#'
#'
Draw_AUC <- function(
  a =  0.13,
  b =  0.19,
  mesh.for.drawing.curve=2222
){
 dark_theme()

  set.seed(1);ll<- stats::rchisq(mesh.for.drawing.curve, 1)
  lll<- 0.99+ll

  l0<-pracma::logspace(-0.5, -222, mesh.for.drawing.curve)
  l2<-pracma::linspace(0, 1.5, mesh.for.drawing.curve)
  l3<-pracma::logspace(0,3, mesh.for.drawing.curve)

  l4<-append(l0,l2)
  la<-append(l4,l3)

  lb<-append(ll,lll)

  l <- append(la,lb)
 x<-1-exp(-l)
  y_buttom<- rep(0,length(x))
y2 <- AFROC(l)
suppressWarnings(graphics::par(new=TRUE));
plot(1-exp(-l),y2 , xlim = c(0,3 ),
     ylim = c(0,2))
graphics::segments( x,y_buttom, x,y2, col="gray",
                    xlim = c(0,3 ),
                    ylim = c(0,2)
                    )


}#function


