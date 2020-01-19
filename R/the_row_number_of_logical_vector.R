

#' @title Extract the row number from a logical vector
#'
#' @param vector.logical vector with logical component
#'
#' @return the row number of logical component
#' @export
#'@author Issei Tsunoda

#'
#' @examples
#'
#'
#'  a <-c(TRUE,FALSE,FALSE,TRUE,TRUE)
#'
#'
#'  b <-  the_row_number_of_logical_vector(a)
#'
#' # Then, return value object, b is a vector of
#'
#' #> b
#' #  1, 4, 5
#'
#' # From this, we can count the TRUE, as following manner:
#'
#'  Number.of.TRUE <- length(b)
#'
#' # Of course, it is:
#' #> Number.of.TRUE
#' #  3
#'
#' length(b) == sum(a)
#'
#'
#'
#'
the_row_number_of_logical_vector <- function(vector.logical){
  L <- length(vector.logical)
  return.vector <- vector()
  s <- 0
   for (ld in 1:L) {
    if (vector.logical[ld]==TRUE) {
      s <- s+1
      return.vector[s] <- ld

    }
}
 return( return.vector )



}





#' @title Count \code{TRUE} in a Vector whose components are all Logical \R objects
#' @description For the posterior predictive p value.
#'
#'@inheritParams the_row_number_of_logical_vector
#'
#' @return A positive integer.
#' @export
#'
#' @examples
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                         Revised 2019 oct. This is same as sum(), I did not know this
#'#========================================================================================
#'
#'  a <-c(TRUE,FALSE,FALSE,TRUE,TRUE)
#'
#'  TRUE.Counter.in.vector(a)
#'
#' # Of course, it is:
#' #> Number.of.TRUE
#' #  3
#'
#'
#' sum(a) ==  TRUE.Counter.in.vector(a)
#'
#' # I did not know this equality,... no longer this function is needed
#'
#'
TRUE.Counter.in.vector <- function(vector.logical){

  return( length(the_row_number_of_logical_vector(vector.logical)))
}
