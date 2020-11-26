
#' @title message with colored item
#'
#' @param words Characters
#' @param ... Characters
#' @param type An integer
#' @param print_debug A logical, whether prints a message
#'
#' @return NULL or print
#' @export
#'
#' @examples
#'
#'  color_message("aaaaa","bbbbb",type = 2,print_debug = TRUE)
#'
color_message  <- function(words,..., type = 1, print_debug = TRUE) {
  red<-  crayon::bgRed$yellow$bold$underline$italic(" ")
  yellow <- crayon::bgYellow$red$bold$underline$italic(" ")
  white <- crayon::bgWhite$red$bold$underline$italic(" ")
  magenta <- crayon::bgMagenta$red$bold$underline$italic(" ")
  green <- crayon::bgGreen$red$bold$underline$italic(" ")
  blue <- crayon::bgBlue$red$bold$underline$italic(" ")
  black <- crayon::bgBlack$red$bold$underline$italic(" ")
  cyan <- crayon::bgCyan$red$bold$underline$italic(" ")

  if(print_debug){

  if(type == 1){
  message( paste(
    green,white,yellow,green," ", paste( words,...)
      ,sep = "" ) )
  }else if (type == 2){
    message( paste(
      red," ",red,yellow," ",paste( words,...)
      ,sep = "" ) )
  }else if (type == 3){
    message( paste(
      white," ",red,yellow," ",paste( words,...)
      ,sep = "" ) )
  }else if (type == 4){
    message( paste(
      green," ",white,yellow," ",paste( words,...)
      ,sep = "" ) )
  }else if (type == 5){
    message( paste(
      blue," ",white,yellow," ",paste( words,...)
      ,sep = "" ) )
  }else if (type == 6){
    message( paste(
      black," ",white,black," ",paste( words,...)
      ,sep = "" ) )
  }else if (type == 7){
    message( paste(
      cyan," ",white,black," ",paste( words,...)
      ,sep = "" ) )
  }


}else
  invisible(NULL)



}
