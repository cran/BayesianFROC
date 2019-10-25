
#' @title AFROC curve
#'
#' @param x  A vector, for which AFROC curve is calculateld.
#' @param a A number, for which AFROC curve is calculateld.
#' @param b A number, for which AFROC curve is calculateld.
#' @param x.coordinate.also Logical, whether a vector of \code{1-exp(-x)}
#'   is included in a return value.
#' @return A list, contains two vectors of x,y cooridinates
#'  for drawing curves. Or A vector, of y coodinate only,
#'   (x coodinates is omitted.)
#' @export
#'
#' @examples
#'
#'
#'      AFROC(stats::runif(1000,3,100),x.coordinate.also=TRUE)
#'
#'
#'

AFROC <- function(
  x,
  a =  0.13,
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




#' @title draw AUC of AFROC
#'
#' @param a A number, for which AFROC curve is calculateld.
#' @param b A number, for which AFROC curve is calculateld.
#' @inheritParams fit_srsc

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


