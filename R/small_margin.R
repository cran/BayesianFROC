
#' @title Margin
#' @description If each variable is smaller, then the margin of it is smaller, so plot region become larger.
#' But title and x axis title will be vanish.
#'
#'@details  To show FROC curve or signal and noise distributions in Shiny Graphical devices,
#'the author write down this function \code{small_margin}. By taking margin too small, we gives more larger plot regions
#'in Shiny Graphicl devices. 2019 August 6
#'
#' @param Down.oma smaller gives larger plot region
#' @param Left.oma smaller gives larger plot region
#' @param Top.oma smaller gives larger plot region
#' @param Right.oma smaller gives larger plot region
#' @param Down.mar smaller gives larger plot region
#' @param Left.mar smaller gives larger plot region
#' @param Top.mar smaller gives larger plot region
#' @param Right.mar smaller gives larger plot region
#'
#'
#'
#'@seealso
#'
#'\code{\link{draw_latent_signal_distribution}()}
#'
#'\code{\link{draw_latent_noise_distribution}()}
#'
#'\code{\link{DrawCurves}()}
#'
#'\code{\link{DrawCurves_srsc}()}

#'
#'
#'
#' @return NONE
#' @export
#'
#' @examples
#'
#' small_margin()
#' graphics::plot(1:3,1:3)
#'
#'
#' small_margin(2,2,2,2)
#' graphics::plot(1:3,1:3)
#'
#' small_margin(2,2,2,2,4,4,4,4)
#' graphics::plot(1:3,1:3)
#' colors()
#' graphics::rect(
#' par()$usr[1],
#' par()$usr[2],
#' par()$usr[3],
#' par()$usr[4],
#'
#'  col = "steelblue3",
#'  border = NA)
#'
#' small_margin(0.1,0.1)
#' graphics::plot(1:2,1:2, type="n")
#'
#'
#'
small_margin <- function(
  Down.oma = 1 ,
  Left.oma =  1,
  Top.oma  = 1 ,
  Right.oma = 1,
  Down.mar = 1 ,
  Left.mar =  1,
  Top.mar  = 1 ,
  Right.mar = 1

  ){



###################################2019 August 5 for margin
oldpar <- graphics::par(oma=c(
 Down.oma = Down.oma ,
 Left.oma=  Left.oma,
 Top.oma  = Top.oma ,
 Right.oma = Right.oma
),
mar=c(
  Down.mar = Down.mar ,
  Left.mar =  Left.mar,
  Top.mar  = Top.mar ,
  Right.mar = Right.mar

  ))#;plot(c(1,2),c(1,2))
# par(oldpar )
#############################################


}
