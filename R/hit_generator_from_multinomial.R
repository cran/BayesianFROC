
#' @title Under Const
#'
#' @inheritParams hits_rate_creator
#' @details The algorithm of \code{rmultinom()} explained in \code{?rmultinom} is quite same as mine code.
#' So, I do not need to write this code. OK.
#'
#' @return A vector of non-negative integers
#' @export
#'
# @examples
hit_generator_from_multinomial  <- function(
  z.truth  = c(0.1,0.2,0.3,0.4,0.5),
  mu.truth = 1,
  v.truth  = 2
) {

  hit_rate <-  hits_rate_creator (
                          z.truth  = z.truth,
                          mu.truth = mu.truth,
                          v.truth  = v.truth,

                          is_hit_rate_adjusted = FALSE
                        )

  stats::rmultinom(10, size = 12, prob = z.truth)

}
