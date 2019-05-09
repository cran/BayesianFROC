
#' @title Variance of a List of Arrays
#'@description Then the function calculates the variance over all list for each array component.
#'@details Of course variance can change to \code{sum} or \code{mean} or any other functions whose entry is a vector.One can find this function in the Stack over flow, since I ask there, and thus the example given in here can also find also there. In my hierarchical Bayesian Model, the estimates has the format arrays. For example the hit rate are array whose subscript is confidence level, modality, and reader. So, when one desire to validate the estimates, it needs to calculate such variance of arrays. When I validate the estimates, I used the function.


#' @param x A List of Arrays. The dimension of array is fixed for all list component.
#' @param name.of.function This is an operator, such as \code{mean}, \code{var}, \code{sum},... Note that user no need to surround the input by "". For example, mean instead of "mean".

#'
#' @return An array  being reduced form use input list of array via user input operator such as \code{mean}, \code{var}, \code{sum},...
#' @export
#'
#' @examples
#'
#' #Suppose that x is the following list of arrays:
#'
#'   a <- array(1,c(2,3,4));
#'   b <- array(2,c(2,3,4));
#'   c <- array(3,c(2,3,4));
#'   d <- array(4,c(2,3,4));
#'   x <- list(a=a,b=b,c=c,d=d)
#'
#' foo_of_a_List_of_Arrays(x,sum)
#' foo_of_a_List_of_Arrays(x,mean)
#' foo_of_a_List_of_Arrays(x,stats::var)
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'
#' #Note that the component of list can be vectors with fixed same length.
#'
#'   y <- list(c(1,2,3),
#'             c(11,22,33),
#'             c(1111,2222,3333))
#'
#'
#'   a <- foo_of_a_List_of_Arrays(y,sum)
#'
#'
foo_of_a_List_of_Arrays <- function(x,name.of.function){
  name.of.function <-substitute(name.of.function)
  name.of.function <-deparse(name.of.function)

  if ( is.vector(x[[1]])==TRUE) {
    expression.pre <-  paste(
      "xx <- matrix(unlist(x), nrow = length(x), ncol = length(x[[1]]),byrow = T);xxx <- apply(xx, 2,",  name.of.function ,")",
      sep=""
    )

    expression <- parse(text = expression.pre )
    e<- eval(expression)
    return(e)
  }


  if ( is.array(x[[1]])==TRUE) {
        expression.pre <-  paste(
      "apply(array(unlist(x),append( dim(x[[1]]), length(x) )  ),1:length(dim(x[[1]]))," , name.of.function,")",
      sep=""
      )
  }






  expression <- parse(text = expression.pre )
  e<- eval(expression)
  return(e)

}
