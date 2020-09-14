

#' @title get treedepth threshold
#' @description From \code{rstan:::get_treedepth_threshold}.
#'
#' @inheritParams DrawCurves
#'
#' @return A non-negative integer
#' @author Some Stan developer. Not the author of this package!
#' @export
#'
# @examples
get_treedepth_threshold <- function (StanS4class)
{
  object <-  methods::as(StanS4class,"stanfit")
  # stopifnot(rstan:::is.stanfit(object))
  max_depth <- object@stan_args[[1]]$control$max_treedepth
  if (is.null(max_depth)) {
    max_depth <- 10
  }
  return(max_depth)
}
