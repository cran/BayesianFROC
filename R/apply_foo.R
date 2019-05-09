#  @title function
#'
#  @param x variable
#  @param name.of.function function
#'
#  @return fun(x)
#'
#'
# @examples
#'
#'
#'  BayesianFROC:::apply_foo(4,sqrt) == sqrt(4)
#'
#'
#' apply_foo <- function(x,name.of.function){
#'   name.of.function <-substitute(name.of.function)
#'   name.of.function <-deparse(name.of.function)
#'
#'   x <-substitute(x)
#'   x <-deparse(x)
#'
#'      expression.pre <-  paste(
#'  name.of.function, "(" ,x,")",
#'       sep=""
#'     )
#'
#'
#'
#'
#'   expression <- parse(text = expression.pre )
#'   e<- eval(expression)
#'   return(e)
#'
#' }
#'
#'
#' fooo <- function(x,foo){
#'   foo <-substitute(foo)
#'   browser()
#'   apply_foo(x, foo)
#'
#' }
