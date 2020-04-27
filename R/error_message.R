#' @title Error Message for Data Format
#' @description Plot error messages to let user know his or her data format is wrong.
#' @details If \code{sum(h) > NL}, then an error message will appear.
#' The reason why the author uses the
#' generic funtion \code{plot} for error messages instead
#' of such as \code{message()} or \code{cat()}
#' is to preserve GUIs in \pkg{Shiny}.
#' So, this error message is shown in
#' some plot plane in the Graphical User Interface of \pkg{Shiny} in which  \code{message()} or \code{cat()} cannot use.
#' @param h A non-negative integer vector
#' @param NL A positive integer, indicating Number of lesions
#' @return Plot of an error message by the generic function \code{plot()} for Shiny GUI.
#' @seealso \code{ \link{fit_GUI}() }
#' @export
#'
#' @examples
#'#========================================================================================
#'#            If   number of hits > number of lesion,  then an error message appears.
#'#========================================================================================
#'
#'  # Make an example such that sum(h) > NL, that is, the sum of the number of hits is
#'  # greater than the number of lesion, then, it launches an error message.
#'
#'          h  <- c(50,30,20)
#'          NL <- 3
#'
#'
#'          error_message(h,NL)
#'
#'  # Then, in an imaging device, an error message appears, because sum(h) = 100 > 3 = NL.
#'  # In Shiny, even if plot cannot be done causing some error, Graphical User Interface
#'  # can not change (now,... I can but.), so I have to use the graphical user interface.
#'  # Thus. in such case, I use this function rather than the message() or cat().
#'
#'  # Who read this? My heart will be more empty when I wrote this mannual.
#'
#'  # This function is made in 2019 July, 6.
#'  # Doc is reviesed in 2020 Feb
#'
#'
#'


#'
error_message <- function(h,NL){
  if (sum(h)> NL) {

  h.string <- as.character(h)
  for (cd  in 1:length(h.string)) {
    if (cd==1){ s<-""; s <- paste(h.string[cd],sep = "+")}
    if ( !cd==1)s <- paste(s,h.string[cd],sep = "+")

  }#for
  sum.of.h <- s
  sum.of.h <- paste(sum.of.h,"=",as.character( sum(h) ) )

  small_margin()

  plot(0,0,type="n", axes=FALSE,xlim=c(0,1),ylim =c(0,1),xaxt="n", yaxt="n",xlab="Please fix inconsistent data",ylab="",main="Error:   Inconsistent Data \n In baseball game, \n batter's number of hits can not  be greater than his number of at-bats")

  graphics::text(0.5,0.8,c("*Now, Sum of the number of hits is greater than that of lesions; \n\n", expression(paste(h1+h2+h3+... , "         >         Number of Lesions")) ),col="blue",cex =    1.4  )
  graphics::text(0.5,0.65,paste("In the current inputed data, it is the following: " ),col="black",cex =  1.5  )
  graphics::text(0.5,0.5,paste(  sum.of.h , "      >       ",NL,sep = ""),col="red",cex = 2)

  graphics::text(0.5,0.3,c("* Please fix so that the following inequality holds \n", expression(paste(h1+h2+h3+... , "       <        Number of lesions"))),col="blue",cex =  1.5    )
  graphics::text(0.5,0.1,c("* Shoud decrease the number of hits  or \n  Shoud increase the number of lesions"),col="blue",cex = 1.5  )


}


  }#function error messsage
