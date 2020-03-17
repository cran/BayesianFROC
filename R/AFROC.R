# @title AFROC -----
#' @title AFROC curve
#' @description An AFROC curve is a plane curve and
#' it has two parameters denoted by \eqn{a,b}.
#'
#'
#' \strong{Def of AFROC}
#'
#'
#' \deqn{  (\xi(t),\eta(t) ) =(1-e^{-t}, \Phi( b\Phi^{-1}(\exp(-t) )- a   ) )}
#'
#' for all \eqn{t >0}.
#'
#'
#' By specifying   \eqn{a,b}, we can draw  an AFROC curve.
#'
#'
#'
#' Def of AUC of AFROC
#'
#' \deqn{  AUC = \frac{ a }{ \sqrt{1+  b^2} }. }
#'
#'
#' @param x A real number which moves in the domain  of FROC curve
#' @param a,b One of the parameter of model which characterize AFROC curve
# @param b One of the parameter of model which characterize AFROC curve
#'
#' @param x.coordinate.also Logical, whether a vector of \code{1-exp(-x)}
#'   is included in a return value.
#' @return
#' if \code{x.coordinate.also =TRUE}, then
#' A list, contains two vectors of x,y cooridinates
#'  for drawing curves.
#'  if \code{x.coordinate.also =FALSE}, then
#'  return is  a vector, consisting of y coodinate only,
#'   (x coodinates is omitted.)
#' @export
#'
#' @examples
#'
#'    x <- stats::runif(1000,0.001,100)
#'    a <-  AFROC(x,x.coordinate.also=TRUE)
#'
#'    plot(a$x,a$y)
#'

AFROC <- function(
  x,
  a =  0.14,
  b =  0.19,
  x.coordinate.also = FALSE

){


 y<- 1 - stats::pnorm(b * stats::qnorm( exp(-x)) - a)




if (  x.coordinate.also == TRUE) {
            return(list(
                x=1-exp(-x),
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


