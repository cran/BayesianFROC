

#' @title Visualization of Latent Gaussians
#'@inheritParams fit_Bayesian_FROC

#'@inheritParams DrawCurves_MRMC_pairwise
#' @param dark_theme TRUE or FALSE
#' @param dig Digit for print of the outputs in the R console.
#' @param mesh Mesh for painting the area
#' @param hit.rate whether draws it. Default is \code{TRUE}.
#' @param false.alarm.rate whether draws it. Default is \code{TRUE}.
#' @param both.hit.and.false.rate whether draws it. Default is \code{TRUE}.

#' @return Information of Latent Gaussians, such as mean and S.D. of the signal distributions and thresholds.
#' @export
#'
#' @examples
#' \donttest{

#'
#'#            ----- High AUC case --------
#'
#'      viewdata(dataList.High)
#'
#'      fit.High <- fit_Bayesian_FROC(dataList.High,ite=111)
#'
#'      draw_bi_normal(fit.High)
#'
#'
#'
#'
#'#            ----- Low AUC case --------
#'
#'      viewdata(dataList.Low)
#'
#'      fit.Low <- fit_Bayesian_FROC(dataList.Low)
#'
#'      draw_bi_normal(fit.Low)
#'

#'}# dottest


draw_bi_normal <- function( StanS4class,     dark_theme =TRUE, dig =3,mesh=1000, new.imaging.device = TRUE,
                            hit.rate = TRUE, false.alarm.rate = TRUE, both.hit.and.false.rate = TRUE){







  Colour1 <-  array(0, dim=c( 20)) #array(0, dim=c( M))
  Colour1[1]<-"antiquewhite1" # "gray0"  #"orange3"
  Colour1[2]<-"gray8"  #"orchid"
  Colour1[3]<-"antiquewhite1" #"coral1" #"deeppink4"  #"firebrick4"
  Colour1[4]<-"gray0"  #"aquamarine1"  #"darkcyan"
  Colour1[5]<-"antiquewhite1" #"blue4" #"deeppink4"  #" cyan4 " #"mediumvioletred" # "green4"##"darkgoldenrod4"
  Colour1[6]<-"gray0"#"darkolivegreen"
  Colour1[7]<-"antiquewhite1"
  Colour1[8]<-"gray0"
  for (cc in 9:20) {
    Colour1[cc] <- as.character(cc);
  }





  if (dark_theme ==TRUE){

  Colour1 <-  array(0, dim=c( 20)) #array(0, dim=c( M))
  Colour1[1]<-"antiquewhite1" # "gray0"  #"orange3"
  Colour1[2]<-"brown1"  #"orchid"
  Colour1[3]<-"dodgerblue1" #"coral1" #"deeppink4"  #"firebrick4"
  Colour1[4]<-"orange2"  #"aquamarine1"  #"darkcyan"
  Colour1[5]<-"yellowgreen" #"blue4" #"deeppink4"  #" cyan4 " #"mediumvioletred" # "green4"##"darkgoldenrod4"
  Colour1[6]<-"khaki1"#"darkolivegreen"
  Colour1[7]<-"darkorange4"
  Colour1[8]<-"slateblue4"
  for (cc in 9:20) {
    Colour1[cc] <- as.character(cc);
  }
  }# if  dark_theme ==TRUE



  fit <- StanS4class
  if ( fit@studyDesign == "srsc.per.image" || fit@studyDesign == "srsc.per.lesion" ) {

  }#if studyDesign
  m <- extract_EAP_by_array(fit,m)
  v <- extract_EAP_by_array(fit,v)
  z <- extract_EAP_by_array(fit,z)
  zeros <- rep(0,length(z))


  noise.col <- "black"
  signal.col <- "black"

  if (dark_theme ==TRUE){

    noise.col <- "white"
    signal.col <- "green"

  }


if(both.hit.and.false.rate==TRUE) {

            if (new.imaging.device == TRUE) grDevices::dev.new()

            if (dark_theme ==TRUE)   dark_theme()

            graphics::curve(stats::dnorm(x,0,1),min(m-3*v,0-1),max(m+3*v,0+1) ,col = noise.col
                            ,xlab = "Latent Gaussian and thresholds",
                            ylab = "Probability Density",
                            main="Bi-Normal"
            )

      graphics::curve(stats::dnorm(x,m,v),min(m-3*v,0-1),max(m+3*v,0+1), add = TRUE, col = signal.col,xlab = "",ylab = "")
      graphics::abline(v=z)


}#both.hit.and.false.rate==T

#-----------------------------------------------

#-------------------------------------
if (false.alarm.rate==TRUE){

                  if (new.imaging.device == TRUE) grDevices::dev.new()
                  if (dark_theme ==TRUE)dark_theme()
                        graphics::curve(stats::dnorm(x,0,1),min(m-3*v,0-1),max(m+3*v,0+1) ,col = noise.col
                              ,xlab = "Latent Gaussian and thresholds",
                              ylab = "Probability Density",
                              main="False Alarm Rate"######################################################
                  )

                  graphics::curve(stats::dnorm(x,m,v),min(m-3*v,0-1),max(m+3*v,0+1), add = TRUE, col = signal.col,xlab = "",ylab = "")
                  graphics::abline(v=z)
                  C <- length( z)

                  x<-list()
                  # mesh <- 100
                  for (cd in 1:C) {
                    if (!cd==C) {
                      x[[cd]]<- seq(z[cd],z[cd+1], length=mesh)
                      # y11 <- dnorm(x[[cd]],0,1)
                      y1<- rep(0,length(x[[cd]]))
                      y2 <- stats::dnorm(x[[cd]],0,1)
                      graphics::polygon( c(x[[cd]],rev(x[[cd]])), c(y1,rev(y2)), col=Colour1[cd+1])
                    }#    if (!cd==C) {


                    if (cd==C) {
                      x[[cd]]<- seq(z[cd],100, length=mesh)
                      # y11 <- dnorm(x[[cd]],0,1)
                      y1<- rep(0,length(x[[cd]]))
                      y2 <- stats::dnorm(x[[cd]],0,1)
                      graphics::polygon( c(x[[cd]],rev(x[[cd]])), c(y1,rev(y2)), col=Colour1[cd+1])
                    }#    if (!cd==C) {



                  }#for cd in 1:C


}#if false.alarm.rate==TRUE



#-------------------------------------------------------------------

 if(hit.rate==TRUE) {

                if (new.imaging.device == TRUE) grDevices::dev.new()
                if (dark_theme ==TRUE)dark_theme()
                graphics::curve(stats::dnorm(x,0,1),min(m-3*v,0-1),max(m+3*v,0+1) ,col = noise.col
                                ,xlab = "Latent Gaussian and thresholds",
                                ylab = "Probability Density",
                                main="Hit Rate" #####################################################################
                )

                graphics::curve(stats::dnorm(x,m,v),min(m-3*v,0-1),max(m+3*v,0+1), add = TRUE, col = signal.col ,xlab = "",ylab = "")
                graphics::abline(v=z)
                C <- length( z)

                x<-list()
                # mesh <- 100
                for (cd in 1:C) {
                  if (!cd==C) {
                    x[[cd]]<- seq(z[cd],z[cd+1], length=mesh)
                    # y11 <- dnorm(x[[cd]],0,1)
                    y1<- rep(0,length(x[[cd]]))
                    y2 <- stats::dnorm(x[[cd]],m,v)
                    graphics::polygon( c(x[[cd]],rev(x[[cd]])), c(y1,rev(y2)), col=Colour1[cd+1])
                  }#    if (!cd==C) {


                  if (cd==C) {
                    x[[cd]]<- seq(z[cd],100, length=mesh)
                    # y11 <- dnorm(x[[cd]],0,1)
                    y1<- rep(0,length(x[[cd]]))
                    y2 <- stats::dnorm(x[[cd]],m,v)
                    graphics::polygon( c(x[[cd]],rev(x[[cd]])), c(y1,rev(y2)), col=Colour1[cd+1])
                  }#    if (!cd==C) {



                }#for cd in 1:C

}#if hit.rate==TRUE

  #--------------------------------------------------------------




message("\n.......................................................")
message("\n--- Visualization of the bi-Gaussian distributions ---\n")
message("'''''''''''''''''''''''''''''''''''''''''''''''''''''''")


message("\n* The mean of the signal distribution:",crayon::cyan( signif(m,digits = dig)  )  )
message("\n* The standard deviation of the signal distribution:",crayon::cyan( signif(v,digits = dig)) )

message("\n*", crayon::bgWhite$red$bold$italic$underline("The vertical lines in the plot"), " mean the estimated thresholds. Each estimates is the posterior mean (expected a posterior estimates (EAPs))"  )
cat("\n* thresholds:");cat(crayon::cyan(  signif(z,digits = dig)) ,sep = " < ")


if (dark_theme ==TRUE)message("\n* ", crayon::green("Green")," curve indicates a signal distribution.")
# message("\n* ", crayon::red("Red")," curve indicates a signal distribution.")

message(crayon::silver("\n* False alarm rate is not the area between two thresholds in the noise distribution. But the area is intuitively indicate the false alarm rate, thus the author color such areas."))


# message("In the information geometrical view point, the two Gaussian distirbution is two points in the Poincare upper half plane. Thus we can also evaluate the observer performance by the distance. If the Signal distribution and the noise distribution is far, then we should consider that the observer performance is high, On the other hand, if this distane is small, then the observer performanse is low ability. This view is quite new. I also implement it   ")

Fisher.metric <- 2*log(
                       (sqrt( (m^2 +(1-v^2)^2) )
                     +  sqrt(  (m^2 +(1+v^2)^2)) )/2*v )

message(" The Fisher metric  of the signal and Noise Gaussian disributions as elements of  the Poincare upper half plane = ",Fisher.metric,"\n")




  e <- extract_EAP_CI(fit,"l",fit@dataList$C,summary = FALSE )
  x <-c(e$l.EAP)
  y <-e$l.EAP[2:fit@dataList$C]
  yy <- c(y,0)
  message("False alarm rate per image (or lesion) means the differences of parameter lambda")
  if(fit@studyDesign=="srsc.per.image") cat("\n* False alarm rate per image:"  , crayon::cyan( x-yy) )
  if(fit@studyDesign=="srsc.per.lesion") cat("\n* False alarm rate per lesion:", crayon::cyan( x-yy) )




  invisible(list(
    mean.of.signal.Gaussian = m,
    standard.deviation.of.signal.Gaussian =v,
    thresholds.of.signal.Gaussian = z


  ))

}
