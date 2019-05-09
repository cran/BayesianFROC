

#' @title    Draw the FROC  curves
#'@description     Draw an FROC  curves and an AFROC curves.
#'@inheritParams DrawCurves
#'@param    Draw.inner.circle.for.CFPCTPs TRUE or FALSE. If true, then to plot the cumulative false positives and true positives the plot points is depicted by two way, one is a large circle and one is a small circle. By see the small circle, user can see the more precise position of these points.
#'@inheritParams DrawCurves_MRMC_pairwise
#'@inheritParams fit_Bayesian_FROC
#' @export
DrawCurves_srsc <- function(
  StanS4class,
  title=TRUE,
  indexCFPCTP=FALSE,
  upper_x,
  upper_y,
  new.imaging.device=TRUE,
    Drawcol = TRUE,
   DrawFROCcurve=TRUE,
  DrawAFROCcurve=FALSE,
  DrawCFPCTP=TRUE,
  Draw.inner.circle.for.CFPCTPs =TRUE
 ){

  fit <- StanS4class

  data <- fit@metadata

  ff <- data$ff
  hh <- data$hh
  C <- as.integer(data$C)
  f <- data$f
  h <- data$h
  NI <- data$NI
  NL <- data$NL




  convergence <- fit@convergence

   if(convergence ==FALSE){message("\n* Model has no mean, since R hat statistics are far from 1 !!\n")

     message("Continue to drawing ? \n
         1: Yes.\n
         2:  No.\n

         0: Exit\n")
     a<- readline("Please enter (1 or 2):")
     if(!(a ==1) ){ return( warning("\n*  Exit. \n"))}

  }
  if(convergence ==TRUE){
    if ( !is.nan( StanS4class@index )) {
    message("\n* The ",StanS4class@index, "-th estimated curve are shown.\n")
    }else {
    message("\n* R hat statistics criterion is OK, Let's draw curves.\n")

}
    }





  chisquare <- fit@chisquare


  l<- fit@plotdata$x.FROC

  x<- fit@plotdata$x.AFROC #AFROC
  y <-   fit@plotdata$y.FROC

  if(fit@studyDesign ==  "srsc.per.image"){ xlabel =  'mean of cumulative false positives per image'  }
  if(fit@studyDesign ==  "srsc.per.lesion"){ xlabel =  'mean of cumulative false positives per lesion'  }

  # browser()

  if(title==TRUE){
    if(fit@PreciseLogLikelihood==TRUE){   waic <- waic(fit)
 title <- paste("Lesions = ",NL,",  Images = ", NI, ", chi^2 =",chisquare, ", WAIC =",waic)}
    if(fit@PreciseLogLikelihood==FALSE){  title <- paste("Lesions = ",NL,",  Images = ", NI, ", chi^2 =",chisquare)}
  }


  if(title==FALSE){title <-""}
                            # if(  DrawFROCcurve == TRUE|| DrawCFPCTP==TRUE||DrawAFROCcurve==TRUE ){

  if (new.imaging.device==TRUE) {
    grDevices::dev.new()
  }

                            # }
                            # if( !( DrawFROCcurve == TRUE|| DrawCFPCTP==TRUE||DrawAFROCcurve==TRUE) ){
                            #   message("\n* We do not draw anything according to your input.\n")
                            # }



                           if( missing(upper_x)){  upper_x <- max(ff)}
                           if( missing(upper_y)){  upper_y <- 1.0    }

                            graphics::par(lwd = 2 )
                            suppressWarnings(graphics::par(new=TRUE));

           if(Drawcol==FALSE){
                              if(DrawAFROCcurve==TRUE){
                                suppressWarnings(graphics::par(new=TRUE)); plot(x,y,#AFROC
                                     xlim = c(0,upper_x),ylim = c(0,upper_y),
                                     col = 'black',
                                     cex=0.1,
                                     xlab = xlabel,
                                     ylab = 'cumulative hit per lesion'
                                )
                              }
                              if(DrawFROCcurve==TRUE){
                                suppressWarnings(graphics::par(new=TRUE));
                                plot(l,y,
                                     xlim = c(0,upper_x),ylim = c(0,upper_y),
                                     cex=0.3,
                                     col = 'black',
                                     xlab = '', ylab = '')
                              }
                              if(DrawCFPCTP==TRUE){
                                suppressWarnings(graphics::par(new=TRUE));plot(ff,hh,cex=3,
                                                          xlim = c(0,upper_x),ylim = c(0,upper_y),
                                                          col = 'black',
                                                          xlab = '', ylab = '')



                                if(Draw.inner.circle.for.CFPCTPs ==TRUE){
                                  suppressWarnings(graphics::par(new=TRUE));plot(ff,hh,cex=1,
                                                                                 xlim = c(0,upper_x),ylim = c(0,upper_y),
                                                                                 col = 'black',
                                                                                 xlab = '', ylab = '')

                                }







                              }
                            }#  if(Drawcol==FALSE



              if(Drawcol==TRUE){

                              graphics::par(bg= "gray12", #"gray27",#"gray40",#"black",# ,
                                            fg="gray",
                                            col.lab="bisque2" ,#"bisque" ,#  "antiquewhite1",
                                            col.axis="bisque2" ,##"bisque" "antiquewhite1",
                                            col.main="bisque2" ,
                                            cex.lab=1.5,
                                            cex.axis=1.3
                              )

                            if(DrawAFROCcurve==TRUE){

                              suppressWarnings(graphics::par(new=TRUE)); plot(x,y,
                                                              col ="antiquewhite1",
                                                              cex= 0.1 ,
                                                              xlim = c(0,upper_x ),ylim = c(0,upper_y),
                                                              xlab = xlabel,
                                                              ylab = 'cumulative hit per lesion'
                                                              ,main =title
                              );
                             }

                            if(DrawFROCcurve==TRUE){


                              #FROC
                              suppressWarnings(graphics::par(new=TRUE)); plot(l,y,
                                                              col ="antiquewhite1",
                                                              bg="gray",
                                                              fg="gray",
                                                              xlab = xlabel,
                                                              ylab = 'cumulative hit per lesion',
                                                              cex= 0.05,
                                                              xlim = c(0,upper_x ),
                                                              ylim = c(0,upper_y)
                                                              ,main = title

                              );

                            }


                            if(DrawCFPCTP==TRUE){
                                if (  indexCFPCTP==FALSE){


                              #CFP-CTP points
                              # pchh <-paste(md);
                              suppressWarnings(graphics::par(new=TRUE));plot(ff,hh,
                                                        xlim = c(0,upper_x ),
                                                        ylim = c(0,upper_y),

                                                        bg="gray",
                                                        fg="gray",
                                                        col ="antiquewhite1",
                                                        # pch =paste(md),
                                                        cex=3,# Size of Dots
                                                        xlab = '', ylab = '')




                                  if(Draw.inner.circle.for.CFPCTPs ==TRUE){
                                    suppressWarnings(graphics::par(new=TRUE));plot(ff,hh,
                                                                                   xlim = c(0,upper_x ),
                                                                                   ylim = c(0,upper_y),

                                                                                   bg="gray",
                                                                                   fg="gray",
                                                                                   col ="antiquewhite1",
                                                                                   # pch =paste(md),
                                                                                   cex=3,# Size of Dots
                                                                                   xlab = '', ylab = '')

                                  }









                                  }#  if   indexCFPCTP==FALSE

                              if (  !indexCFPCTP==FALSE){


                                #CFP-CTP points
                                C <- StanS4class@metadata$C
                                for (cd in 1:C)  {
                                suppressWarnings(graphics::par(new=TRUE));plot(ff[cd],hh[cd],
                                                                               xlim = c(0,upper_x ),
                                                                               ylim = c(0,upper_y),

                                                                               bg="gray",
                                                                               fg="gray",
                                                                               col ="antiquewhite1",
                                                                               pch =paste(cd),

                                                                               # pch =paste(indexCFPCTP),
                                                                               cex=1,# Size of Dots
                                                                               xlab = '', ylab = '')
                                        }
                                   }#    if (  !indexCFPCTP==FALSE){
                               } #  if(DrawCFPCTP==TRUE){
                            }#  if(Drawcol==TRUE
}#function end document
