


#' @title Detect NA in a vector
#'
#' @param vect A vector, whose components are allowed to be numeric, character, etc.
#'
#' @return A logical. If \code{vect} contains at least one \code{NA}, then \code{TRUE}. Otherwise \code{FALSE}.
#' @export
#'
#' @examples
#'
#'  is_na_in_vector(  c(NA,1)      )
#'
#'  is_na_in_vector(  c(6,1)      )
#'
#'  is_na_in_vector(  c(1:5,NA,1)      )
#'
#'  is_na_in_vector(  c(1:5,1)      )
#'
#'  is_na_in_vector(  c(1:5,NA,1,NA)      )
#'
#'  is_na_in_vector(  c(1:51,"asdfg")      )
#'
#'  is_na_in_vector(  c(1:5,NA,1,NA,"asdfg")      )
#'
is_na_in_vector <- function(vect) {
  !as.logical(  prod( !is.na(vect  )  )   )
}









#' @title is.logical(0)
#' @description When object is created by the codes \code{ x <- integer(); y <- list(); z <- logical()},
#' and if the values is not substituted, then this function return \code{TRUE}.
#' This function determine whether the value is assigned or not according to the object size.
#'
#'  2020 Sept 25
#' @param integer_object An object of class integer
#'
#' @return A logical
#' @export
#'
#' @examples
#'
#' a <- integer()
#'
#' is_logical_0(a)
#'
#' is_logical_0(1)
#'
#' a <- integer()
#'
#' is_logical_0(a)
#'
#' is_logical_0(TRUE)

#'
#'
is_logical_0 <- function(integer_object){

  utils::object.size(integer_object )==48
}



#' @title Is argument of length zero ?
#' @description When object is created by the codes \code{ x <- integer(); y <- list(); z <- logical()},
#' and if the values is not substituted, then this function return \code{TRUE}.
#' This function determine whether the value is assigned or not according to the object size.
#'
#'  2020 Oct 6
#' @param integer_object An object of class integer
#'
#' @return A logical
#' @export
#'
#' @examples
#'
#' a <- integer()
#'
#' is_length_zero(a)
#'
#' is_length_zero(1)
#'
#' a <- list()
#'
#' is_length_zero(a)
#'
#' is_length_zero(TRUE)

#'
#'
is_length_zero <- function(integer_object){

  length(integer_object )==48
}











