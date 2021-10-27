

#' @title Size of R object
#' @description This return value can add each other or any number by  the manner: return + number of \R object
#'
#' @param object Any R object, whose size is measured.
#' @param summary A logical, whether the result is printed.
#' @param is_return_value  A logical, printed word is used as " return value " if it is \code{TRUE}.
#' @param base_size This value is added to the return value, namely,  object size + \code{base_size} is the return value. This is for the package developer.
# @param dig A positive integer, digit
#' @param col A logical, wheter print is colored.
#' @return return value of \code{utils::object.size()}
#' @export
#'
# @examples
#'
#'
size_of_return_value <- function(object,
                                 summary=TRUE,
                                 is_return_value =TRUE,
                                 base_size=0,
                                 col = FALSE
                                 # ,dig =3

                                 ){

if(summary==TRUE){
 if (is_return_value)   message(crayon::silver("\n* size of the return value: "))
 if (!is_return_value)   message(crayon::silver("\n* size of the current object: "))

  # print( utils::object.size(object),unit="MB")
  aaa <- utils::object.size(object) + base_size
  bbb <- aaa/1000000
 # print( message(crayon::cyan(bbb),crayon::cyan("Mb"),"\n") )
 if(!col)  message(crayon::cyan( round( bbb,digits = 1)  ),crayon::cyan("  Mb"),"\n")
 if(col) {   cat("\n-------------------------------------------")
          message(crayon::bgYellow$red$bold$underline( signif( bbb,digits = 1)  ),crayon::cyan("  Mb"),"\n")
}
 # print( message(crayon::cyan(bbb),crayon::cyan("Mb"),"\n") )

}
  if(summary==FALSE){
    aaa <- utils::object.size(object)
    bbb <- aaa/1000000
  }
invisible(  aaa   )

  }
