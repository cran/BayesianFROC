#' @title Empirical FROC curve via ggplot2
#'
#'@inheritParams fit_Bayesian_FROC
#'
#' @return none
#' @export
#'
#' @examples
#'
#'  Empirical_FROC_via_ggplot(
#'                            dataList = d
#'                            )
#'
#'
Empirical_FROC_via_ggplot <- function(dataList){


  h <- dataList$h
  f <- dataList$f
  NL<- dataList$NL
  NI<- dataList$NI

  TPF <- cumsum(h)/NL
  FPF <- cumsum(f)/NI


  df <- data.frame(FPF =c(0,FPF),TPF=c(0,TPF) )

  ggplot2::ggplot(df, ggplot2::aes(x = FPF, y = TPF)) +

    ggplot2::geom_area(colour = "black",
                 fill = "blue",
                alpha = .2)+

    ggplot2::annotate("text",
              x = FPF[2]*1.2,
              y = TPF[2]*0.5,
              parse = TRUE,
              label = paste0("frac(1,", NL,")"),
              fontface = "bold.italic",
              colour = "red")+

    ggplot2::scale_colour_brewer(palette = "Set1")+

    ggplot2::geom_line(linetype = "dashed",
              size = 1,
              colour = "blue")+

    ggplot2::geom_point(size = 4,
               shape = 21,
               colour = "darkred",
               fill = "pink")



  # geom_point(size = 4, shape = 21, fill = "white")

  # Also use a point with a color fill


}#function


#' @title FROC curve as an embedding map
#'
#' @param x A real number moves in domain  of FROC curve
# @param a parameter of FROC curve, defining familiy of FROC curve
# @param b parameter of FROC curve, defining familiy of FROC curve
#'
#' @return none
#' @export
#'
#' @examples
#'
#' # I love you!
#'
#'
#'
FROC_curve <- function(x){
  a<-1
  b<-1
   1-stats::pnorm( b*stats::qnorm(exp(-x))- a)


}



#' @title FROC curve as an embedding map
#' @details Technique of plotting AFROC is difficult
#'  because it has two points in which
#'  the gradients are infinity
#'   and it causes the following warinings. Revised 2019 Nov. 20
#'
#' Warning messages:
#' 1: In stats::qnorm(exp(1 - x)) : NaNs produced
#' 2: In stats::qnorm(exp(1 - x)) : NaNs produced
#' 3: Removed 50 rows containing missing values (geom_path).
#'
#' @param x A real number which moves in the domain  of FROC curve
#' @param a a generated parameter of model which characterize AFROC curve
#' @param b a generated parameter of model which characterize AFROC curve
#'
#' @return none
#' @export
#'
#' @examples
#'
#' # This function is under construction.
#' x <- runif(1000,1,10)
#' y <- AFROC_curve(x)
#' plot(x,y)
#'
#'
AFROC_curve <- function(x,a=0.13,b=0.19){
  # a<-0.13
  # b<-0.19
  # 1-stats::pnorm( b*stats::qnorm(exp(1-x))- a)
  1-Phi( b*Phi_inv(exp(1-x))- a)


}


ggplot2::ggplot(data.frame(x = c(0, 1)), ggplot2::aes(x = x))+

  ggplot2::stat_function(fun = AFROC_curve,
                         geom = "line",
                         colour = "blue")
# +geom_area(colour = "black", fill = "blue", alpha = .2)



# ggplot() with dummy data
p <- ggplot2::ggplot(data.frame(x = c(2, 0)), ggplot2::aes(x = x))

p +
  ggplot2::stat_function(fun = AFROC_curve,
                         geom = "area",
                         fill = "blue",
                         alpha = 0.2) +
  ggplot2::stat_function(fun = AFROC_curve)











ggplot2::ggplot(data.frame(x = c(0, 20)), ggplot2::aes(x = x))+

  ggplot2::stat_function(fun = FROC_curve,
                         geom = "line",
                         colour = "blue")
# +geom_area(colour = "black", fill = "blue", alpha = .2)















# Return dnorm(x) for 0 < x < 2, and NA for all other x
dnorm_limit <- function(x) {
  y <- stats::dnorm(x)
  y[x < 0  |  x > 2] <- NA
  return(y)
}

# ggplot() with dummy data
p <- ggplot2::ggplot(data.frame(x = c(-3, 3)), ggplot2::aes(x = x))

p +
  ggplot2::stat_function(fun = dnorm_limit, geom = "area", fill = "blue", alpha = 0.2) +
  ggplot2::stat_function(fun = dnorm)
