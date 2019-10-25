



#' @title Example array
#' @description Make a three dim array whose component is its index. For example
#'
#'  \code{a[2,3,4] = 234}
#'
#' @param I natural number less than 10
#' @param J natural number less than 10
#' @param K natural number less than 10
#'
#' @return An array of three dimension.
#' @export
#'
#' @examples
#'
#'
#'
#'    a <-array_easy_example(2,3,4)
#'
#'
#'
array_easy_example <- function(I=2,J=3,K=4){
  array <- array(0, c(I,J,K))

  for (id in 1:I) {
    for (jd in 1:J) {
      for (kd in 1:K) {

        array[id,jd,kd] <- 100*id + 10*jd +kd
      }
    }

  }

  return(array)


}
