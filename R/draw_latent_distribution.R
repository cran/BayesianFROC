


#' @title Curve and signal distribution and noise  d log Phi() for a single reader and a single modality
#' @description Draws FROC curve and signal and noise ( noise distribution is the differential of the logarithmic of the cumulative standard Gaussian denoted by \eqn{d \log \Phi}) are drawn in a \strong{same} plain.
#' The author of this pacakage developed the FROC theory, and find that
#' the noise distribution is not the so-called bi normal assumption.
#' But instead, we use the differential logarithmic Gaussian for the noise distribution.
#'
#'\emph{ Note that MRMC data is not allowed.}
#' @seealso
#' \code{\link{DrawCurves}}
#'
#' \code{\link{draw_latent_noise_distribution}}
#'
#' @inheritParams DrawCurves
#' @return None
#' @export
#' @details This function is made to pass this plot to Shiny.
#'
#'
#' With pain from all my body, but today 2019 July 23 is good.
#'  Neuralgia or muscle aches makes my feeling down and down.
#'  If I can transform into Anpanman, then I want to give my head.
#'
#'  I fails, this is very small plot, so I cannot use this function for my package.
#' I will remove this function or extende plot region for more confortable exhibition.
#'
#' @examples
#'
#' \dontrun{
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#            1)             Build the data
#'#========================================================================================
#'
#'# For singler reader and single modality  case.
#'
#' dat <- list(c=c(3,2,1),    #Confidence level. Note that c is ignored.
#'             h=c(97,32,31), #Number of hits for each confidence level
#'             f=c(1,14,74),  #Number of false alarms for each confidence level
#'
#'             NL=259,        #Number of lesions
#'             NI=57,         #Number of images
#'             C=3)           #Number of confidence level
#'
#'
#'
#'
#'#  where,
#'#      c denotes confidence level, i.e., rating of reader.
#'#                3 = Definitely deseased,
#'#                2 = subtle,.. deseased
#'#                1 = very subtle
#'#      h denotes number of hits (True Positives: TP) for each confidence level,
#'#      f denotes number of false alarms (False Positives: FP) for each confidence level,
#'#      NL denotes number of lesions,
#'#      NI denotes number of images,

#'
#'
#'# For example, in the above example data,
#'#  the number of hits with confidence level 3 is 97,
#'#  the number of hits with confidence level 2 is 32,
#'#  the number of hits with confidence level 1 is 31,
#'
#'#  the number of false alarms with confidence level 3 is 1,
#'#  the number of false alarms with confidence level 2 is 14,
#'#  the number of false alarms with confidence level 1 is 74,
#'
#'
#'
#'#--------------------------------------------------------------------------------------
#'#                         2)       Fit a model to the above data-set
#'#--------------------------------------------------------------------------------------
#'
#'
#'
#'   #Because dataset named dat is a single reader and a single modality,
#'   #the function fit such a model by running the following code.
#'
#'
#'
#'
#'
#'           fit <-   BayesianFROC::fit_Bayesian_FROC(
#'                                dat,       # dataset
#'                                ite=1111,  #To run in time <5s.
#'                                cha=1      # number of chains, it is better more large.
#'                                )
#'
#'
#'
#'
#'
#'
#'#--------------------------------------------------------------------------------------
#'#             3)  Draw the FROC curve and signal and noise (logarithmic Gaussian)
#'#--------------------------------------------------------------------------------------
#'
#'
#'#   Using the fitted model object of class stanfitExtended, we can draw curves.
#'
#'     plot_curve_and_hit_rate_and_false_rate_simultaneously(fit)
#'
#'
#'
#'      Close_all_graphic_devices() # 2020 August

#'}
#'
plot_curve_and_hit_rate_and_false_rate_simultaneously <- function(StanS4class){

  fit <-StanS4class
  graphics::split.screen(figs = c(1, 2))
  graphics::split.screen(figs = c(2, 1), screen = 2)

  graphics::screen(1)
  DrawCurves(fit,new.imaging.device  = FALSE,Colour = F)
  graphics::screen(3)
  draw_latent_noise_distribution(fit,hit.rate = TRUE,false.alarm.rate  = FALSE,both.hit.and.false.rate  = FALSE,dark_theme  = FALSE,new.imaging.device = F)
  graphics::screen(4)
  draw_latent_noise_distribution(fit,hit.rate  = FALSE,false.alarm.rate = TRUE,both.hit.and.false.rate  = FALSE,dark_theme  = FALSE,new.imaging.device = F)
}


#' @title Visualization of Latent Gaussians ( Signal Distribution)
#'
#' @description
#' Plot the posterior mean
#'  of  model parameter \eqn{\theta} and
#'   the parameter of the latent function, i.e.
#'  the normal distribution denoted by \eqn{ Gaussian(z|\mu,\sigma)}
#'  with posterior mean estimates of its mean \eqn{\mu} and
#'  standard deviation \eqn{\sigma}.
#'

#'
#'
#' @details  Our FROC model
#' use a latent Gaussian random variable to determine hit rates.
#' That is, each hit rate is defined as follows;
# \deqn{ p_5(z_1,...z_C; \mu, \sigma) = \int_{z_{5}}^{\infty} Gaussian(z|\mu,\sigma)dz}
# \deqn{ p_4(z_1,...z_C; \mu, \sigma) = \int_{z_{4}}^{5} Gaussian(z|\mu,\sigma)dz}
# \deqn{ p_3(z_1,...z_C; \mu, \sigma) = \int_{z_{3}}^{4} Gaussian(z|\mu,\sigma)dz}
# \deqn{ p_2(z_1,...z_C; \mu, \sigma) = \int_{z_{2}}^{3} Gaussian(z|\mu,\sigma)dz}
# \deqn{ p_1(z_1,...z_C; \mu, \sigma) = \int_{z_{1}}^{2} Gaussian(z|\mu,\sigma)dz}

#'
#'
#'
#' \deqn{ p_5(z_1,...z_C; \mu, \sigma) = \int_{z5}^{\infty} Gaussian(z|\mu,\sigma)dz}
#' \deqn{ p_4(z_1,...z_C; \mu, \sigma) = \int_{z4}^{z5} Gaussian(z|\mu,\sigma)dz}
#' \deqn{ p_3(z_1,...z_C; \mu, \sigma) = \int_{z3}^{z4} Gaussian(z|\mu,\sigma)dz}
#' \deqn{ p_2(z_1,...z_C; \mu, \sigma) = \int_{z2}^{z3} Gaussian(z|\mu,\sigma)dz}
#' \deqn{ p_1(z_1,...z_C; \mu, \sigma) = \int_{z1}^{z2} Gaussian(z|\mu,\sigma)dz}
#'
#'
#'
#'
#'  For example, in the following data,
#'  the number of hit data with the most
#'   highest confidence level 5 is regarded as an sample from the Binomial distribution of hit rate
  # \eqn{p_5(z_1,...z_C; \mu, \sigma) = \int_{z_{5}}^{\infty} Gaussian(z|\mu,\sigma)dz}
#'    \eqn{p_5(z_1,...z_C; \mu, \sigma) = \int_{z5}^{\infty} Gaussian(z|\mu,\sigma)dz}
#'    with
#'  Bernoulli trial number is \code{NL=142}.
#'
#'
# LaTeX errors found:
#   ! Double subscript.
# <argument> ..._1,...z_C; \mu , \sigma ) = \int _z_
# {5}^{\infty } Gaussian(z|\...
#                        l.6930 ...{5}^{\infty} Gaussian(z|\mu,\sigma)dz}{}
# with
# ! Double subscript.
# <argument> ..._1,...z_C; \mu , \sigma ) = \int _z_
# {5}^{\infty } Gaussian(z|\...
#                        l.7123 ...{5}^{\infty} Gaussian(z|\mu,\sigma)dz}{}
# with
# ! Paragraph ended before \Rd@code was complete.
# <to be read again>
#   \par
# l.16949
#'
#'
#'  So, this Gaussian distribution determines hit rate, and this function \code{draw_latent_signal_distribution()} plot
#'  this Gaussian distribution \eqn{ Gaussian(z|\mu,\sigma)}.
#'  And a reference distribution is  the standard Gaussian and do not confuse that
#'  it is not the noise distribution, but only reference.
#'
#'  The noise distribution (denoted by \eqn{d \log \Phi})  determines the
#'  False alarm rates in the similar manner and plotted by using a line of dots.
#'  The author thinks the standard Gaussian is more comfortable to compare or confirm the shape of  \eqn{ Gaussian(z|\mu,\sigma)} and
#'  thus, the author implement it in the \code{\link{draw_latent_signal_distribution}()}.
#'
#'  One would want to see the signal distribution and noise distribution simultaneously, then use the function \code{\link{draw_latent_noise_distribution}()}.
#'
#'
#'


#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves

#'@inheritParams DrawCurves_MRMC_pairwise
#' @param dark_theme TRUE or FALSE
#' @param dig A positive integer, indicating the digit for numbers in the R console.
#' @param mesh Mesh for painting the area
#' @param hit.rate whether draws it. Default is \code{TRUE}.
#' @param false.alarm.rate whether draws it. Default is \code{TRUE}.
#' @param both.hit.and.false.rate whether draws it. Default is \code{TRUE}.
#' @param density	 A natural number, indicating the density of shading lines, in lines per inch.
#' @param color A color region is selected from black and white only. For more colors, put \code{FALSE}. For publication, the mono color is allowed in many case, so the author made this for such publication.
#' @param mathmatical.symbols A logical, whether legend is in plot.
#'
#'
#'@seealso
#' \code{\link{draw_latent_noise_distribution}()}
#' Note that the difference of \code{\link{draw_latent_noise_distribution}()} and
#'   \code{draw_latent_signal_distribution()} is that the lator use the standard Gaussian for the reference distribution and
#'   former uses the \eqn{d \log \Phi()} for the reference distribution.
#'
#'   So, the  old version \code{draw_latent_signal_distribution()}  is also important and I like this old version also.
#'   Anyway who read this, I think my package size is very large,....ha,,,,I have to reduce it,....but how?

#' @return Information of Latent Gaussians, such as mean and S.D. of the signal distributions and thresholds.
#' @export
#'
#' @examples
#' \dontrun{
#'#========================================================================================
#'#   Shape of signal distribution strongly influences the value of AUC, so in the following
#'#   the author shows how it affects the estimates of AUCs.
#'#    We consider two data examples, one is a low AUC and the other is a high AUC.
#'#   In the high AUC case, the Signal Gaussain will be low variance and
#'#   in the low AUC case, the variance will desperse.  2019 August 4, 2019 Dec 17
#'#========================================================================================
#'
#'
#'#            ----- High AUC case --------
#'
#'      viewdata(dataList.High)
#'
#'      fit.High <- fit_Bayesian_FROC(dataList.High,ite=111)
#'
#'      draw_latent_signal_distribution(fit.High)
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
#'      draw_latent_signal_distribution(fit.Low)
#'
#'
#'
#'
#'#--------------------------------------------------------------------------------------
#'#                         2)      For submission (without color)
#'#--------------------------------------------------------------------------------------
#'
#'
#'
#'
#'
#'      fit <-    fit_Bayesian_FROC(
#'                                  dataList = dataList.Chakra.1.with.explantation
#'                                  )
#'
#'
#'
#'
#'# With legends
#'
#'
#'      draw_latent_signal_distribution(fit,
#'                    dark_theme  = FALSE,
#'                    color = TRUE,
#'                    density = 11
#'                    )
#'
#'
#'
#' #' Without legends
#' draw_latent_signal_distribution(fit,
#'                                 dark_theme           = FALSE,
#'                                 color               = TRUE,
#'                                 mathmatical.symbols = FALSE
#' )
#'              # 2019 Sept. 5
#'              # 2020 March 12
#'
#'
#'
#'      Close_all_graphic_devices() # 2020 August
#'
#'}# dottest


draw_latent_signal_distribution <- function( StanS4class,
                            dark_theme =TRUE,
                            dig =3,mesh=1000,
                            new.imaging.device = TRUE,
                            hit.rate = TRUE,
                            false.alarm.rate = FALSE,
                            both.hit.and.false.rate = FALSE,
                            density = 22,
                            color = TRUE,
                            mathmatical.symbols = TRUE,
                            type = 3,
                            summary =FALSE

                              ){






if (color==FALSE) {

  # Colour1 <-  array(0, dim=c( 20)) #array(0, dim=c( M))
  # Colour1[1]<-"antiquewhite1" # "gray0"  #"orange3"
  # Colour1[2]<-"gray8"  #"orchid"
  # Colour1[3]<-"antiquewhite1" #"coral1" #"deeppink4"  #"firebrick4"
  # Colour1[4]<-"gray0"  #"aquamarine1"  #"darkcyan"
  # Colour1[5]<-"antiquewhite1" #"blue4" #"deeppink4"  #" cyan4 " #"mediumvioletred" # "green4"##"darkgoldenrod4"
  # Colour1[6]<-"gray0"#"darkolivegreen"
  # Colour1[7]<-"antiquewhite1"
  # Colour1[8]<-"gray0"
  # for (cc in 9:20) {
  #   Colour1[cc] <- as.character(cc);
  # }

if(dark_theme ==FALSE){
  Colour1 <-  array(0, dim=c( 20)) #array(0, dim=c( M))
  Colour1[1]<-"gray8"  #"antiquewhite1" # "gray0"  #"orange3"
  Colour1[2]<-"gray0"  #"orchid"
  Colour1[3]<-"gray8"  #"antiquewhite1" #"coral1" #"deeppink4"  #"firebrick4"
  Colour1[4]<-"gray0"  #"aquamarine1"  #"darkcyan"
  Colour1[5]<-"gray8"  #"antiquewhite1" #"blue4" #"deeppink4"  #" cyan4 " #"mediumvioletred" # "green4"##"darkgoldenrod4"
  Colour1[6]<-"gray0"#"darkolivegreen"
  Colour1[7]<-"gray8"  #"antiquewhite1"
  Colour1[8]<-"gray0"
  for (cc in 9:20) {
    Colour1[cc] <- as.character(cc);
  }

}
  if(dark_theme ==TRUE){

    Colour1 <-  array(0, dim=c( 20)) #array(0, dim=c( M))
    Colour1[1]<-"antiquewhite1"  #"antiquewhite1" # "gray0"  #"orange3"
    Colour1[2]<-"antiquewhite1"  #"orchid"
    Colour1[3]<-"antiquewhite1"  #"antiquewhite1" #"coral1" #"deeppink4"  #"firebrick4"
    Colour1[4]<-"antiquewhite1"  #"aquamarine1"  #"darkcyan"
    Colour1[5]<-"antiquewhite1"  #"antiquewhite1" #"blue4" #"deeppink4"  #" cyan4 " #"mediumvioletred" # "green4"##"darkgoldenrod4"
    Colour1[6]<-"antiquewhite1"#"darkolivegreen"
    Colour1[7]<-"antiquewhite1"  #"antiquewhite1"
    Colour1[8]<-"antiquewhite1"
    for (cc in 9:20) {
      Colour1[cc] <- as.character(cc);
    }


  }





}#if color



  if (color==TRUE) {

  # if (dark_theme ==TRUE){


  Colour1 <-  array(0, dim=c( 21)) #array(0, dim=c( M))
  Colour1[1]<-"antiquewhite1" # "gray0"  #"orange3"
  Colour1[2]<-"brown1"  #"orchid"
  Colour1[3]<-"dodgerblue1" #"coral1" #"deeppink4"  #"firebrick4"
  Colour1[4]<-"orange2"  #"aquamarine1"  #"darkcyan"
  Colour1[5]<-"yellowgreen" #"blue4" #"deeppink4"  #" cyan4 " #"mediumvioletred" # "green4"##"darkgoldenrod4"
  Colour1[6]<-"khaki1"#"darkolivegreen"
  Colour1[7]<-"slateblue4"
  Colour1[8]<-"brown1"
  Colour1[9]<-"darkorange4"

  for (cc in 10:21) {
    Colour1[cc] <- as.character(cc);
  }
  # }# if  dark_theme ==TRUE
  }#if color



  fit <- StanS4class
  if ( fit@studyDesign == "srsc.per.image" || fit@studyDesign == "srsc.per.lesion" ) {

  }#if studyDesign
  m <- extract_EAP_by_array(fit,m)
  v <- extract_EAP_by_array(fit,v)
  z <- extract_EAP_by_array(fit,z)
  zeros <- rep(0,length(z))



  upper_x <- max( max(m+3*v,0+1), max(z)  )
  lower_x <- min( min(m-3*v,0-1), min(z)   )
  # min(m-3*v,0-1),max(m+3*v,0+1)



  noise.col <- "black"
  signal.col <- "black"

  if (dark_theme ==TRUE){

    noise.col <- "white"
    signal.col <- "green"

  }


if(both.hit.and.false.rate==TRUE) {

            if (new.imaging.device == TRUE) grDevices::dev.new()

            if (dark_theme ==TRUE)   dark_theme(type = type)


  small_margin()

            graphics::curve(stats::dnorm(x,0,1),
                            # min(m-3*v,0-1),max(m+3*v,0+1) ,
                            lower_x,upper_x,

                            col = noise.col
                            ,xlab = "Latent Gaussian and thresholds",
                            ylab = "Probability Density",
                            main="Bi-Normal"
            )

      graphics::curve(stats::dnorm(x,m,v),
                      # min(m-3*v,0-1),max(m+3*v,0+1),
                      lower_x,upper_x,

                      add = TRUE, col = signal.col,xlab = "",ylab = "")
      graphics::abline(v=z)

      ###############################2019 August 5
      C <- length( z)
      z.label <- 1:C
      for (cd in 1:C) {
        z.label[cd] <- paste( "z[", cd,"] = ", round(z[cd],3),   sep = "")
      }

      graphics::axis(1,at=z,labels = z.label,
                     hadj=0,
                     padj = 1,
                     col.axis ="blue",
                     col.ticks = "red",
                     cex.axis=1.2,# Font size
                     tck=0.4,
                     lwd.ticks = 1.5,
                     las=2
      )
      ###############################2019 August 5



}#both.hit.and.false.rate==T

#aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa

#aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
if (false.alarm.rate==TRUE){

                  if (new.imaging.device == TRUE) grDevices::dev.new()
                  if (dark_theme ==TRUE)dark_theme(type=type)

  small_margin()

                        graphics::curve(stats::dnorm(x,0,1),min(m-3*v,0-1),max(m+3*v,0+1) ,col = noise.col
                              ,xlab = "Latent Gaussian and thresholds",
                              ylab = "Probability Density",
                              main="False Alarm Rate"#aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
                  )

                        small_margin()

                  graphics::curve(stats::dnorm(x,m,v),

                                  # min(m-3*v,0-1),max(m+3*v,0+1),
                                  lower_x,upper_x,



                                  add = TRUE, col = signal.col,xlab = "",ylab = "")
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
                      graphics::polygon( c(x[[cd]],
                                           rev(x[[cd]])),
                                         c(y1,rev(y2)),
                                         col=Colour1[cd+1],

                                         density = density,# sparse color
                                         angle = -45,# sparse color
                                         border = "black"

                                         )
                    }#    if (!cd==C) {


                    if (cd==C) {
                                          x[[cd]]<- seq(z[cd],100, length=mesh)
                                          # y11 <- dnorm(x[[cd]],0,1)
                                          y1<- rep(0,length(x[[cd]]))
                                          y2 <- stats::dnorm(x[[cd]],0,1)
                      graphics::polygon( c(x[[cd]],
                                           rev(x[[cd]])),
                                         c(y1,rev(y2)),
                                         col=Colour1[cd+1],
                                         density = density,# sparse color
                                         angle = -45,# sparse color
                                         border = "black"
                                         )
                    }#    if (!cd==C) {



                  }#for cd in 1:C


}#if false.alarm.rate==TRUE



#aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa

 if(hit.rate==TRUE) {
         small_margin()

                if (new.imaging.device == TRUE) grDevices::dev.new()
                if (dark_theme ==TRUE)dark_theme(type=type)

                  ############################################################################# 2019 August 3
                     if(stats::dnorm(x=0,0,1) <= stats::dnorm(m,m,v) ) upper_y <- stats::dnorm(m,m,v)
                     if(stats::dnorm(x=0,0,1) > stats::dnorm(m,m,v) ) upper_y <- stats::dnorm(x=0,0,1)
                  #aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa#







                     small_margin()

 graphics::curve(stats::dnorm(x,0,1),
                 # min(m-3*v,0-1),
                 # max(m+3*v,0+1),
                 lower_x,upper_x,

                 col = noise.col,
                 xlab = "Latent Gaussian and thresholds",
                 ylab = "Probability Density",
                 ylim=c(0,   upper_y   ),#aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa 2019 July
                 lty = 3,#dot line  #line =1 and dot line =3
                 lwd =2, # Line WiDth
                   main="Hit Rate", #aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa

                cex.axis =1.3

                  )






 small_margin()

 graphics::curve(stats::dnorm(x,m,v),
                 # min(m-3*v,0-1),
                 # max(m+3*v,0+1),
                 lower_x,upper_x,

                 add = TRUE,
                 col = signal.col ,
                 xlab = "",
                 ylab = "")
 C <- length( z)


 if(mathmatical.symbols==TRUE){


 graphics::legend(
   # x=z[C]+z[C]/10,y=stats::dnorm(x=z[C],m,v)+stats::dnorm(x=z[C],m,v)*0.4,
   # x=z[C],y=  min(m-3*v,0-1) ,
   # x=z[C],y=  max(m+3*v,0+1),

   "topright",
   legend = c(expression( frac(1,1%*%sqrt(2*pi)) * exp(frac((x-0)^2,2%*%1^2))),
              substitute(
                frac(1,sigma%*%sqrt(2*pi)) * exp(frac((x-mu)^2,2%*%sigma^2)),
                list(mu  = round(m, digits = 2),
                     sigma = round(v, digits = 2)
                )
              )

   ),
   col=c( noise.col,signal.col  ), #Color of two line, one is line and the other is dot line
   lwd =2, # Line WiDth
   lty = c(3,1), #line =1 and dot line =3
   # pch = c(4, 3),
   xjust = 1,
   yjust = 1,
   bty="n", # Vanishing of frame
   title = paste(
     "Estimated Mean of Signal Gaussian =",round(m, digits = 2),
     "\nEstimated S.D. of Signal Gaussian =",round(v, digits = 2)
   )
 )

}
 # graphics::legend(
 #           "topright",
 #           legend = c(
 #                         substitute(
 #                           # expression( frac(1,sigma%*%sqrt(2*pi)) * exp(frac((x-mu)^2,2%*%sigma^2))),
 #                           frac(1,sigma%*%sqrt(2*pi)) * exp(frac((x-mu)^2,2%*%sigma^2)),
 #
 #                                   list(mu    = round(m, digits = 2),
 #                                        sigma = round(v, digits = 2)
 #                                   )
 #                         ),
 #
 #
 #                         substitute(
 #                                    # expression( frac(1,sigma%*%sqrt(2*pi)) * exp(frac((x-mu)^2,2%*%sigma^2)) ) ,
 #                           frac(1,sigma%*%sqrt(2*pi)) * exp(frac((x-mu)^2,2%*%sigma^2)),
 #
 #                                     list(mu    = 0,
 #                                          sigma = 1
 #                                          )
 #                         )
 #
 #
 #
 #
 #                       ),
 #
 #           col=c( noise.col,  signal.col), #Color of two line, one is line and the other is dot line
 #           lwd =2, # Line WiDth
 #           lty = 1:2, #line =1 and dot line =2
 #           xjust = 1,
 #           yjust = 1,
 #           bty="n", # Vanishing of frame
 #           title = paste(
 #             "Estimated Mean of Signal Gaussian =",round(m, digits = 2),
 #             "\nEstimated S.D. of Signal Gaussian =",round(v, digits = 2)
 #           )
 # )







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
                    graphics::polygon( c(x[[cd]],
                                         rev(x[[cd]])),
                                       c(y1,rev(y2)),
                                       col=Colour1[cd+1],
                                       density = density,# sparse color
                                       angle = -45,# sparse color
                                       border = "black"

                                       )
                  }#    if (!cd==C) {


                  if (cd==C) {
                    x[[cd]]<- seq(z[cd],100, length=mesh)
                    # y11 <- dnorm(x[[cd]],0,1)
                    y1<- rep(0,length(x[[cd]]))
                    y2 <- stats::dnorm(x[[cd]],m,v)
                    graphics::polygon( c(x[[cd]],
                                         rev(x[[cd]])),
                                       c(y1,rev(y2)),
                                       col=Colour1[cd+1],
                                       density = density,# sparse color
                                       angle = -45,# sparse color
                                       border = "black"

                                       )
                  }#    if (!cd==C) {



                }#for cd in 1:C

}#if hit.rate==TRUE

  #bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb




#if(summary) message("\n.......................................................")
#if(summary) message("\n--- Visualization of the bi-Gaussian distributions ---\n")
#if(summary) message("'''''''''''''''''''''''''''''''''''''''''''''''''''''''")


if(summary) message("\n* The mean of the signal distribution:",crayon::cyan( signif(m,digits = dig)  )  )
if(summary) message("\n* The standard deviation of the signal distribution:",crayon::cyan( signif(v,digits = dig)) )

if(summary) message("\n*", crayon::bgWhite$red$bold$italic$underline("The vertical lines in the plot"), " mean the estimated thresholds. Each estimates is the posterior mean (expected a posterior estimates (EAPs))"  )
if(summary)cat("\n* thresholds:");
if(summary)cat(crayon::cyan(  signif(z,digits = dig)) ,sep = " < ")


if (dark_theme&&summary) message("\n* ", crayon::green("Green")," curve indicates a signal distribution.")
# if(summary) message("\n* ", crayon::red("Red")," curve indicates a signal distribution.")

#message(crayon::silver("\n* False alarm rate is not the area between two thresholds in the noise distribution. But the area is intuitively indicate the false alarm rate, thus the author color the areas."))


# if(summary) message("In the information geometrical view point, the two Gaussian distirbution is two points in the Poincare upper half plane. Thus we can also evaluate the observer performance by the distance. If the Signal distribution and the noise distribution is far, then we should consider that the observer performance is high, On the other hand, if this distane is small, then the observer performanse is low ability. This view is quite new. I also implement it   ")

Fisher.metric <- 2*log(
                       (sqrt( (m^2 +(1-v^2)^2) )
                     +  sqrt(  (m^2 +(1+v^2)^2)) )/2*v )

if(summary) message(" The Fisher metric  of the signal and the standard Gaussian disributions as elements of  the Poincare upper half plane = ",Fisher.metric,"\n")




  e <- extract_EAP_CI(fit,"l",fit@dataList$C,summary = FALSE )
  x <-c(e$l.EAP)
  y <-e$l.EAP[2:fit@dataList$C]
  yy <- c(y,0)
  if(fit@studyDesign=="srsc.per.image"&&summary) {message("False alarm rate per image  means the differences of parameter lambda")
    cat("\n* False alarm rate per",  crayon::yellow("image:"), crayon::cyan( x-yy) )
  }

  if(fit@studyDesign=="srsc.per.lesion"&&summary){ message("False alarm rate per lesion  means the differences of parameter lambda")
    cat("\n* False alarm rate per",  crayon::yellow("lesion:"), crayon::cyan( x-yy) )
  }





  invisible(list(
    mean.of.signal.Gaussian = m,
    standard.deviation.of.signal.Gaussian =v,
    thresholds.of.signal.Gaussian = z


  ))

}











































#' @title Visualization of the Latent Gaussian for false rates
#' @description
#' Plot the posterior mean
#'  of  model parameter \eqn{\theta} and
#' and the latent function, i.e.
#'  the differential logarithmic Gaussian \eqn{d \log \Phi(z)}.
#'
#' @details
#' Our FROC model use a latent
#'  Gaussian random variable to determine false rates which are defined as follows;
#'
#' \deqn{ q_5(z_1,...z_C) = \int_{z5}^{\infty} d \log \Phi(z)dz}
#' \deqn{ q_4(z_1,...z_C) = \int_{z4}^{z5} d \log \Phi(z)dz}
#' \deqn{ q_3(z_1,...z_C) = \int_{z3}^{z4} d \log \Phi(z)dz}
#' \deqn{ q_2(z_1,...z_C) = \int_{z2}^{z3} d \log \Phi(z)dz}
#' \deqn{ q_1(z_1,...z_C) = \int_{z1}^{z2} d \log \Phi(z)dz}
#'
#'
#'  For example, in the following data, the number of false alarm data with confidence level 5 \strong{41} which
#'  is considered as an sample from the Poisson distribution of  its rate
#' \deqn{ q_5(z_1,...z_C) = \int_{z5}^{\infty} d \log \Phi(z)dz}
#'
#'
#
# LaTeX errors found:
#   ! Double subscript.
# <argument> ..._1,...z_C; \mu , \sigma ) = \int _z_
# {5}^{\infty } Gaussian(z|\...
#                        l.6930 ...{5}^{\infty} Gaussian(z|\mu,\sigma)dz}{}
# with
# ! Double subscript.
# <argument> ..._1,...z_C; \mu , \sigma ) = \int _z_
# {5}^{\infty } Gaussian(z|\...
#                        l.7123 ...{5}^{\infty} Gaussian(z|\mu,\sigma)dz}{}
# with
# ! Paragraph ended before \Rd@code was complete.
# <to be read again>
#   \par
# l.16949






#'  So, this Gaussian distribution determines false rate, and this function \code{draw_latent_noise_distribution()} plot
#'  this Gaussian distribution  \eqn{d \log \Phi}  and
#'   the density  \eqn{ Gaussian(z|\mu,\sigma)} is also plotted to compare hit rates and false rates.
#'  thus, the author implement it in the \code{\link{draw_latent_signal_distribution}()},
#'
#'
#'



#'\strong{\emph{ Example data:}}
#'
#'  \emph{            A single reader and single modality case   }
#'
#'------------------------------------------------------------------------------------------------------

#' \tabular{rccc}{
#' \code{NI=63,NL=124}   \tab \strong{ confidence level } \tab \strong{ No. of false alarms} \tab \strong{No. of hits}  \cr
#'  In R console ->      \tab \code{ c} \tab   \code{f }  \tab   \code{h}  \cr
#'   -----------------------\tab ----------------------- \tab ----------------------------- \tab ------------- \cr
#' \emph{definitely} present  \tab  5 \tab 1  \tab 41  \cr
#'  \emph{probably} present   \tab  4 \tab 2  \tab 22  \cr
#'  equivocal                 \tab  3 \tab 5  \tab 14  \cr
#'  subtle                    \tab  2 \tab 11 \tab 8   \cr
#'  \emph{very} subtle        \tab  1 \tab 13 \tab 1   \cr
#'  }
#'
#'---------------------------------------------------------------------------------------------------
#'
#'
#'*  \emph{false alarms} = False Positives = FP
#'
#'*  \emph{hits} = True Positives = TP
#'

#'@inheritParams fit_Bayesian_FROC
#'@inheritParams draw_latent_signal_distribution
#'@inheritParams DrawCurves

#'@inheritParams DrawCurves_MRMC_pairwise
#' @param dark_theme TRUE or FALSE
#' @param mesh Mesh for painting the area
#' @param hit.rate whether draws it. Default is \code{TRUE}.
#' @param false.alarm.rate whether draws it. Default is \code{TRUE}.
#' @param both.hit.and.false.rate whether draws it. Default is \code{TRUE}.
#'
#' @seealso
#' \code{\link{draw_latent_signal_distribution}()}
#' @return Information of Latent Gaussians, such as mean and S.D. of the signal distributions and thresholds.
#' @export
#'
#' @examples
#' \dontrun{

#'#========================================================================================
#'#   Shape of signal distribution strongly influences the value of AUC, so in the following
#'#   the author shows how it affects the estimates of AUCs.
#'#    We consider two dataset, one of which is a low AUC and the other is a high AUC.
#'#   In the high AUC case, the Signal Gaussain will be low variance and
#'#   in the low AUC case, the variance will desperse.  2019 August 4, 2019 Dec 17
#'#========================================================================================
#'
#'#            ----- High AUC case --------
#'
#'      viewdata(dataList.High)
#'
#'      fit.High <- fit_Bayesian_FROC(dataList.High,ite=111)
#'
#'      draw_latent_signal_distribution(fit.High)
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
#'      draw_latent_signal_distribution(fit.Low)
#'
#'
#'
#'      Close_all_graphic_devices() # 2020 August
#'

#'}# dottest


draw_latent_noise_distribution <- function( StanS4class,
                                       dark_theme =TRUE,
                                       dig =3,
                                       mesh=1000,
                                       new.imaging.device = TRUE,
                                       hit.rate = FALSE,
                                       false.alarm.rate = TRUE,
                                       both.hit.and.false.rate = FALSE,
                                       density = 22,
                                       color = TRUE,
                                       mathmatical.symbols = TRUE,
                                       type = 3,
                                       summary =FALSE


){

  Phi <- stats::pnorm
  Phi_inverse <- stats::qnorm

  fit <- StanS4class
  if ( fit@studyDesign == "srsc.per.image" )NX <- fit@dataList$NI
  if ( fit@studyDesign == "srsc.per.lesion")NX <- fit@dataList$NL


  if (color==FALSE) {

    # Colour1 <-  array(0, dim=c( 20)) #array(0, dim=c( M))
    # Colour1[1]<-"antiquewhite1" # "gray0"  #"orange3"
    # Colour1[2]<-"gray8"  #"orchid"
    # Colour1[3]<-"antiquewhite1" #"coral1" #"deeppink4"  #"firebrick4"
    # Colour1[4]<-"gray0"  #"aquamarine1"  #"darkcyan"
    # Colour1[5]<-"antiquewhite1" #"blue4" #"deeppink4"  #" cyan4 " #"mediumvioletred" # "green4"##"darkgoldenrod4"
    # Colour1[6]<-"gray0"#"darkolivegreen"
    # Colour1[7]<-"antiquewhite1"
    # Colour1[8]<-"gray0"
    # for (cc in 9:20) {
    #   Colour1[cc] <- as.character(cc);
    # }

    if(dark_theme ==FALSE){
      Colour1 <-  array(0, dim=c( 20)) #array(0, dim=c( M))
      Colour1[1]<-"gray8"  #"antiquewhite1" # "gray0"  #"orange3"
      Colour1[2]<-"gray0"  #"orchid"
      Colour1[3]<-"gray8"  #"antiquewhite1" #"coral1" #"deeppink4"  #"firebrick4"
      Colour1[4]<-"gray0"  #"aquamarine1"  #"darkcyan"
      Colour1[5]<-"gray8"  #"antiquewhite1" #"blue4" #"deeppink4"  #" cyan4 " #"mediumvioletred" # "green4"##"darkgoldenrod4"
      Colour1[6]<-"gray0"#"darkolivegreen"
      Colour1[7]<-"gray8"  #"antiquewhite1"
      Colour1[8]<-"gray0"
      for (cc in 9:20) {
        Colour1[cc] <- as.character(cc);
      }

    }
    if(dark_theme ==TRUE){

      Colour1 <-  array(0, dim=c( 20)) #array(0, dim=c( M))
      Colour1[1]<-"antiquewhite1"  #"antiquewhite1" # "gray0"  #"orange3"
      Colour1[2]<-"antiquewhite1"  #"orchid"
      Colour1[3]<-"antiquewhite1"  #"antiquewhite1" #"coral1" #"deeppink4"  #"firebrick4"
      Colour1[4]<-"antiquewhite1"  #"aquamarine1"  #"darkcyan"
      Colour1[5]<-"antiquewhite1"  #"antiquewhite1" #"blue4" #"deeppink4"  #" cyan4 " #"mediumvioletred" # "green4"##"darkgoldenrod4"
      Colour1[6]<-"antiquewhite1"#"darkolivegreen"
      Colour1[7]<-"antiquewhite1"  #"antiquewhite1"
      Colour1[8]<-"antiquewhite1"
      for (cc in 9:20) {
        Colour1[cc] <- as.character(cc);
      }


    }





  }#if color



  if (color==TRUE) {

    # if (dark_theme ==TRUE){


    Colour1 <-  array(0, dim=c( 21)) #array(0, dim=c( M))
    Colour1[1]<-"antiquewhite1" # "gray0"  #"orange3"
    Colour1[2]<-"brown1"  #"orchid"
    Colour1[3]<-"dodgerblue1" #"coral1" #"deeppink4"  #"firebrick4"
    Colour1[4]<-"orange2"  #"aquamarine1"  #"darkcyan"
    Colour1[5]<-"yellowgreen" #"blue4" #"deeppink4"  #" cyan4 " #"mediumvioletred" # "green4"##"darkgoldenrod4"
    Colour1[6]<-"khaki1"#"darkolivegreen"
    Colour1[7]<-"slateblue4"
    Colour1[8]<-"brown1"
    Colour1[9]<-"darkorange4"

    for (cc in 10:21) {
      Colour1[cc] <- as.character(cc);
    }
    # }# if  dark_theme ==TRUE
  }#if color



  #aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  d_log_Phi <-  function(x) stats::dnorm(x)/stats::pnorm(x)# 2019 July
  #aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa

  # browser()
  fit <- StanS4class
  if ( fit@studyDesign == "srsc.per.image" || fit@studyDesign == "srsc.per.lesion" ) {

  }#if studyDesign
  m <- extract_EAP_by_array(fit,m)
  v <- extract_EAP_by_array(fit,v)
  z <- extract_EAP_by_array(fit,z)
  zeros <- rep(0,length(z))


  upper_x <- max( max(m+3*v,0+1), max(z)  )
  lower_x <- min( min(m-3*v,0-1), min(z)   )


  noise.col <- "black"
  signal.col <- "black"

  if (dark_theme ==TRUE){

    noise.col <- "white"
    signal.col <- "green"

  }


  if(both.hit.and.false.rate==TRUE) {

    if (new.imaging.device == TRUE) grDevices::dev.new()

    if (dark_theme ==TRUE)   dark_theme(type=type)

    small_margin()

    graphics::curve( d_log_Phi(x), ########################### 2019 July
                     # min(m-3*v,0-1),max(m+3*v,0+1) ,
                     ,lower_x,upper_x,


                     col = noise.col, ylim=c(0, stats::dnorm(m,m,v))
                     ,xlab = "Latent Logarithmic Gaussian, Signal distribution  and thresholds",
                     ylab = "Probability Density",
                     main="Bi-Normal"
    )

    small_margin()

    graphics::curve(stats::dnorm(x,m,v),

                    # min(m-3*v,0-1),max(m+3*v,0+1),
                    lower_x,upper_x,

                    add = TRUE, col = signal.col,xlab = "",ylab = "")
    graphics::abline(v=z)


  }#both.hit.and.false.rate==T

  #bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb

  #bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
  if (false.alarm.rate==TRUE){

    if (new.imaging.device == TRUE) grDevices::dev.new()
    if (dark_theme ==TRUE)dark_theme(type = type)


    small_margin()

    graphics::curve(d_log_Phi(x)########################### 2019 July
                    , ylim=c(0, d_log_Phi(z[1]) + d_log_Phi(z[1])/10)######################## 2019 July
                    # ,min(m-3*v,0-1),max(m+3*v,0+1) ,
                    ,lower_x,upper_x,

                    col = noise.col
                    ,xlab = "Latent Logarithmic Gaussian, Signal distribution  and thresholds",
                    ylab = "Probability Density",
                    main="False Alarm Rate",#aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
                    cex.axis =1.3,
                    lty = 1,
                    pch =4#	 the plotting symbols


    )

    small_margin()

    graphics::curve(stats::dnorm(x,m,v),
                    # min(m-3*v,0-1),max(m+3*v,0+1),
                    lower_x,upper_x,

                    add = TRUE,
                    col = signal.col,
                    xlab = "",
                    ylab = "",
                    lty = 3,#dot line  #line =1 and dot line =3
                    lwd =2, # Line WiDth
                    pch =3#	 the plotting symbols
                    # col = "red"
    )



    if(mathmatical.symbols==TRUE){

      # This is a label and math symbols ######## 2019 Aug 14 ####### START
      graphics::legend(
        "topright",
        legend = c(expression(frac(d, dz) *phantom(0)*log*Phi(z)*","*phantom(0)*where*phantom(0)*Phi(z)*phantom(0)*"="*phantom(0)*integral( frac(1,1%*%sqrt(2*pi)) * exp(frac((x-0)^2,2%*%1^2)),-infinity,z)*dx),
                   substitute(
                     frac(1,sigma%*%sqrt(2*pi)) * exp(frac((x-mu)^2,2%*%sigma^2)),
                     list(mu  = round(m, digits = 2),
                          sigma = round(v, digits = 2)
                     )
                   )

        ),
        col=c( noise.col,signal.col  ), #Color of two line, one is line and the other is dot line
        lwd =2, # Line WiDth
        lty = c(1,3), #line =1 and dot line =3
        # pch = c(4, 3),
        xjust = 1,
        yjust = 1,
        bty="n", # Vanishing of frame
        title = paste(
          "Estimated Mean of Signal Gaussian =",round(m, digits = 2),
          "\nEstimated S.D. of Signal Gaussian =",round(v, digits = 2)
        )
      )
      # examples in help of legend is good reference.
      # Execute the code: ?legend
      # This is a label and math symbols ######## 2019 Aug 14 ####### START
    }# if mathmatical.symbols==TRUE

    graphics::abline(v=z)
    C <- length( z)

    x<-list()
    # mesh <- 100
    for (cd in 1:C) {
      if (!cd==C) {
        x[[cd]]<- seq(z[cd],z[cd+1], length=mesh)
        # y11 <- stats::dnorm(x[[cd]],0,1)
        y1<- rep(0,length(x[[cd]]))
        y2 <- d_log_Phi(x[[cd]])########################### 2019 July
        graphics::polygon( c(x[[cd]],rev(x[[cd]])),
                           c(y1,rev(y2)),
                           col=Colour1[cd+1],
                           density = density,# sparse color
                           angle = -45,# sparse color
                           border = "black"

        )
      }#    if (!cd==C) {


      if (cd==C) {
        x[[cd]]<- seq(z[cd],100, length=mesh)
        # y11 <- stats::dnorm(x[[cd]],0,1)
        y1<- rep(0,length(x[[cd]]))
        y2 <- d_log_Phi(x[[cd]]) ################################## 2019 July
        graphics::polygon( c(x[[cd]],rev(x[[cd]])),
                           c(y1,rev(y2)),
                           col=Colour1[cd+1],
                           density = density,# sparse color
                           angle = -45,# sparse color
                           border = "black"

        )
      }#    if (!cd==C) {



    }#for cd in 1:C








  }#if false.alarm.rate==TRUE




  if(hit.rate==TRUE) {

    if (new.imaging.device == TRUE) grDevices::dev.new()
    if (dark_theme ==TRUE)dark_theme(type = type)


    small_margin()


    graphics::curve(d_log_Phi(x),###################### 2019 July
                    ylim=c(0,    stats::dnorm(m,m,v)   )######################## 2019 July

                    # ,min(m-3*v,0-1),max(m+3*v,0+1) ,
                    ,lower_x,upper_x,
                    col = noise.col
                    ,xlab = "Latent Logarithmic Gaussian, Signal distribution  and thresholds",
                    ylab = "Probability Density",
                    main="Hit Rate" #aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    )


    small_margin()


    graphics::curve(stats::dnorm(x,m,v),
                    # min(m-3*v,0-1),max(m+3*v,0+1),
                    lower_x,upper_x,

                    add = TRUE, col = signal.col ,xlab = "",ylab = "")
    graphics::abline(v=z)
    C <- length( z)

    x<-list()
    # mesh <- 100
    for (cd in 1:C) {
      if (!cd==C) {
        x[[cd]]<- seq(z[cd],z[cd+1], length=mesh)
        # y11 <- stats::dnorm(x[[cd]],0,1)
        y1<- rep(0,length(x[[cd]]))
        y2 <- stats::dnorm(x[[cd]],m,v)
        graphics::polygon( c(x[[cd]],
                             rev(x[[cd]])),
                           c(y1,rev(y2)),
                           col=Colour1[cd+1],
                           density = density,# sparse color
                           angle = -45,# sparse color
                           border = "black"
        )
      }#    if (!cd==C) {


      if (cd==C) {
        x[[cd]]<- seq(z[cd],100, length=mesh)
        # y11 <- stats::dnorm(x[[cd]],0,1)
        y1<- rep(0,length(x[[cd]]))
        y2 <- stats::dnorm(x[[cd]],m,v)
        graphics::polygon( c(x[[cd]],
                             rev(x[[cd]])),
                           c(y1,rev(y2)),
                           col=Colour1[cd+1],
                           density = density,# sparse color
                           angle = -45,# sparse color
                           border = "black"
        )
      }#    if (!cd==C) {



    }#for cd in 1:C

  }#if hit.rate==TRUE





  if(summary) message("\n.......................................................")
  if(summary) message("          Visualization of Latent distributions          ")
  if(summary) message("'''''''''''''''''''''''''''''''''''''''''''''''''''''''")


  if(summary) message("\n* The mean of the signal distribution:",crayon::cyan( signif(m,digits = dig)  )  )
  if(summary) message("\n* The standard deviation of the signal distribution:",crayon::cyan( signif(v,digits = dig)) )

  if(summary) message("\n*", crayon::bgWhite$red$bold$italic$underline("The vertical lines in the plot"), " mean the estimated thresholds. Each estimates is the posterior mean (expected a posterior estimates (EAPs))"  )
  if(summary)cat("\n* thresholds:");
  if(summary)cat(crayon::cyan(  signif(z,digits = dig)) ,sep = " < ")


  if (dark_theme &&summary) message("\n* ", crayon::green("Green")," curve indicates a signal distribution.")
  # if(summary) message("\n* ", crayon::red("Red")," curve indicates a signal distribution.")

  if(summary)message(crayon::red("\n* False alarm rate is exactly the area between two thresholds in the differential logarithmic cumulative Gaussian distribution.The area intuitively indicates the false alarm rate, thus the author color the areas."))


  # if(summary) message("In the information geometrical view point, the two Gaussian distirbution is two points in the Poincare upper half plane. Thus we can also evaluate the observer performance by the distance. If the Signal distribution and the noise distribution is far, then we should consider that the observer performance is high, On the other hand, if this distane is small, then the observer performanse is low ability. This view is quite new. I also implement it   ")

  # Fisher.metric <- 2*log(
  #   (sqrt( (m^2 +(1-v^2)^2) )
  #    +  sqrt(  (m^2 +(1+v^2)^2)) )/2*v )
  #
  # if(summary) message(" The Fisher metric  of the signal and the standard Gaussian disributions as elements of  the Poincare upper half plane = ",Fisher.metric,"\n")
  #



  e <- extract_EAP_CI(fit,"l",fit@dataList$C,summary = FALSE )
  x <-c(e$l.EAP)
  y <-e$l.EAP[2:fit@dataList$C]
  yy <- c(y,0)
  if(fit@studyDesign=="srsc.per.image" &&summary){ message("False alarm rate per image  means the differences of parameter lambda")
    cat("\n* False alarm rate per",  crayon::yellow("image:"), crayon::cyan( x-yy) )
  }

  if(fit@studyDesign=="srsc.per.lesion"&&summary){ message("False alarm rate per lesion  means the differences of parameter lambda")
    cat("\n* False alarm rate per",  crayon::yellow("lesion:"), crayon::cyan( x-yy) )
  }

  invisible(list(
    mean.of.signal.Gaussian = m,
    standard.deviation.of.signal.Gaussian =v,
    thresholds.of.signal.Gaussian = z


  ))

}
