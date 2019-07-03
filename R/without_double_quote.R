

#' @title without double quote
#' @description  wati
#' @param X sequence of
#'
#'

#'
foo  <- function(X){ Y <-substitute(X);cat(Y)}


#' @title taboo or
#' @description wait
#'
#'
 fooo <-function(){
   A <-1 # This does not work but only to remove the note in the R CMD check
  foo(A)
}
