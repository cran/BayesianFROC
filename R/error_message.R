#' @title Error Message for Data Format
#' @description This function is excellent! I fear my own great genius. How great I am ,... Is there someone?
#' Who read this? I always feel vanity when write this mannual, who read? ha,.... This function is only reture plot to let user know the data format is error.
#' @details Why the author use the generic funtion \code{plot} instead of such as \code{message()} or \code{cat()} is for \emph{Shiny}.
#' So, this error message is shown in the plot plane in the Graphical User Interface, so in there,  \code{message()} or \code{cat()} cannot use.
#' Ha,..who read? I feel empty. In mathematics empty set is very vain. My heart is now, empty set. ha,,, I love you.
#'
#' @param h A non-negative integer vector
#' @param NL A positive number, indicating Number of lesions
#'@author Issei Tsunoda

#' @return Plot of error message by the generic function \code{plot()}. So, return value is not required.
#' @seealso \code{ \link{fit_GUI}() }
#' @export
#'
#' @examples
#'#----------------------------------------------------------------------------------------
#'#            If   number of hits > number of lesion,  then error message appears.
#'#----------------------------------------------------------------------------------------
#'
#'  # Make an example such that sum(h) > NL, that is, the sum of the number of hits is
#'  # greater than the number of lesion, then, it launced error message.
#'
#'  h <- c(50,30,20)
#'  NL <- 3
#'
#'
#'  error_message(h,NL)
#'
#'  # Then, in imaging device, the error message are shown.
#'  # In shiny, even if plot cannot be done causing some error, Graphical User Interface
#'  # can not change, so I have to use the graphical user interface.
#'  # Thus. in such case, I chose this function rather than the message() or cat().
#'
#'  # Who read this? My heart will be more empty when I wrote this mannual.
#'  # Now, today, my health is good, so I want to go to eat Sushi,...ha,  yari_ika_geso.
#'
#'  # This function is made in 2019 July, 6.


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

  plot(0,0,xlim=c(0,1),ylim =c(0,1),xaxt="n", yaxt="n",xlab="Please fix inconsistent data",ylab="",main="Error:   Inconsistent Data \n In baseball game, \n batter's number of hits can not  be greater than his number of at-bats")

  graphics::text(0.5,0.8,c("*Now, Sum of the number of hits is greater than that of lesion; \n\n", expression(paste(h1+h2+h3+... , "         >         Number of Lesions")) ),col="blue",cex =    1.4  )
  graphics::text(0.5,0.65,paste("In the current inputed data, it is the following: " ),col="black",cex =  1.5  )
  graphics::text(0.5,0.5,paste(  sum.of.h , "      >       ",NL,sep = ""),col="red",cex = 2)

  graphics::text(0.5,0.3,c("* Please fix so that the following inequality holds \n", expression(paste(h1+h2+h3+... , "       <        Number of lesions"))),col="blue",cex =  1.5    )
  graphics::text(0.5,0.1,c("* Shoud decrease the number of hits  or \n  Shoud increase the number of lesions"),col="blue",cex = 1.5  )


}


  }#function error messsage
