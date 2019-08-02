

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
#'  a <-c(TRUE,FALSE,FALSE,TRUE)
#'
#'
#' the_row_number_of_logical_vector(a)
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
