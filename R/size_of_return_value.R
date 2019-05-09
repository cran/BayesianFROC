

size_of_return_value <- function(object,summary=TRUE){

if(summary==TRUE){
  message(crayon::silver("\n* size of the return value: "))
  # print( utils::object.size(object),unit="MB")
  aaa <- utils::object.size(object)
  bbb <- aaa/1000000
 # print( message(crayon::cyan(bbb),crayon::cyan("Mb"),"\n") )
 message(crayon::cyan(bbb),crayon::cyan("Mb"),"\n")
 # print( message(crayon::cyan(bbb),crayon::cyan("Mb"),"\n") )

}
  if(summary==FALSE){
    aaa <- utils::object.size(object)
    bbb <- aaa/1000000
  }
invisible(  aaa    )

  }
