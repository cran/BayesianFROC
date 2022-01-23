#' @title    Draw FROC  curves
#'@description
#'
#'  plots FROC curves, AFROC curves and  \emph{FPF} and \emph{TPF}.
#'
#'@details
#' plots of the FROC curves and AFROC curves for user's specified modality and user's specified reader.
#' Using this function \strong{repeatedly}, we can draw the different reader and modality in a  \strong{same} plane simultaneously.
#' So, we can visualize
#'  the difference of modality ( or reader).
#'
#'
#'
#'@param StanS4class An S4 object of class \emph{\code{ \link{stanfitExtended}}} which is an inherited class from the S4 class  \code{stanfit}.
#' This \R object is a fitted model object
#'  as a return value of the function \code{\link{fit_Bayesian_FROC}()}.
#'
#' To be passed to \code{\link{DrawCurves}()} ... etc

#'
# @param --------
#'@param DrawFROCcurve Logical: \code{TRUE} of \code{FALSE}.  Whether or not FROC curves are shown.
#'@param DrawAFROCcurve Logical: \code{TRUE} of \code{FALSE}.  Whether or not AFROC curves are shown.
#'@param DrawCFPCTP Logical: \code{TRUE} of \code{FALSE}.  Whether or not the pairs of \emph{FPF} and \emph{TPF} are shown.
#'@param Draw.Flexible.upper_y Logical: \code{TRUE} of \code{FALSE}.  Whether or not the upper bounds of vertical axis are determined automatically.
#'@param Draw.Flexible.lower_y Logical: \code{TRUE} of \code{FALSE}.  Whether or not the lower bounds of vertical axis are determined automatically.

#'@param type_to_be_passed_into_plot "l" or "p".
#'@param modalityID A positive integer vector indicating modalityID. If it is not given, then the first modality is chosen.
#'@param readerID  A positive integer vector indicating  readerID. If it is not given, then the first reader is chosen.
#'
#'
#'@param Colour Logical: \code{TRUE} of \code{FALSE}.  whether Colour of curves is dark theme or not.
#'@param title Logical: \code{TRUE} of \code{FALSE}.  If \code{TRUE}  (default), then title of curves are drawn.


#'
#'@inheritParams fit_Bayesian_FROC
#'
#'@examples

# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#================The first example=======================================================
#' \dontrun{

#' # 1) Fit a model to data by the following:
#'
#'
#'
#'   fit <- fit_Bayesian_FROC(
#'   BayesianFROC::dataList.Chakra.Web,    # data to which fit the model
#'                            ite=1111  # iteration of MCMC is too small
#'                            )
#'
#' # Note that the return value "fit" is an object of an inherited S4 class from stanfit
#'
#'
#'
#'
#' # 2)
#' #  With the above S4 class object, we plot the curves.
#'
#'
#'
#'           DrawCurves(
#'                     fit,
#'                     modality = 1,
#'                       reader = 4)
#'

#' # From this code, an FROC curve is plotted
#' # for the first modality and the fourth reader.
#'
#'
#'
#'
#'
#'
#'
#' #3)
#'    # By changing, e.g., the modality, in the above,
#'    # we can draw the curves for different  modalities.
#'    # This shows the comparison of modalites.
#'    # In the following,
#'    # the first script plots a curve for the 2 nd modality and the fourth reader,
#'    # and the second script plots a curve for the 3rd modality and the 4 th reader,
#'    # respectively.
#'
#'
#'
#'             DrawCurves(fit,modality = 2,reader = 4)
#'             DrawCurves(fit,modality = 3,reader = 4)
#'
#'
#'
#'# Curves are overwritten in a single imaging device for the comparison.
#'
#'
#'
#' #4) By applying the function with respect to different modalities
#' #   in this manner, we can draw  AFROC (FROC) curves in the same plain.
#'
#'
#'
#' #5) If you want to draw the FROC curves
#' #for reader ID =1,2,3,4 and modality ID =1,2, then the code is as follows;
#'
#'                   DrawCurves(
#'                             fit,
#'                             modalityID = c(1,2,3,4),
#'                             readerID   = c(1,2)
#'                             )
#'# Each color of curves corresponds to the modality ID.
#'# So, the curves of "different" readers will have the "same" color,
#'# if their modalities are "same".
#'
#'
#'
#'
#'
#'
#'
#'# 6) To show only data points, i.e. FPF and TPF,
#'#    use DrawFROCcurve = F as follows;
#'
#' DrawCurves(fit,
#'            DrawCFPCTP    = TRUE,   # This implies data points are ploted.
#'            DrawFROCcurve = FALSE,  # From this, the curves are not drawn.
#'            modalityID    = c(1,2,3,4),
#'            readerID      = c(1)
#'            )
#'
#'
#'
#'
#'
#'
#'
#'#7) If you use the plot in submission and it is not allowed to use color, then
#'#   by Colour  = FALSE, you can get black and white plots, e.g.,
#'
#'
#'DrawCurves(fit,
#'           DrawCFPCTP    = TRUE,
#'           DrawFROCcurve = TRUE,
#'           modalityID    = c(1,2,3,4),
#'           readerID      = c(1),
#'           Colour = FALSE    # From this, you can get plots without colors.
#'           )
#'
#'
#'
#'
#'
#'
#'
#'#8)  For AFROC, use DrawAFROCcurve = T
#'
#'DrawCurves(fit,
#'           DrawFROCcurve  = FALSE,
#'           DrawAFROCcurve = TRUE,
#'           modalityID     = c(1,2,3,4),
#'           readerID       = c(1)
#'           )
#'
#'
#'
#'
#'
#'
#'#9)
#'
#'# In order to compare modality, we draw curves by each modality
#'# The 1-st modality with all readers 1,2,3,4:
#'
#'
#'DrawCurves(fit,modalityID = 1,readerID = 1:4, new.imaging.device = TRUE)
#'
#'#The 2-nd modality with all readers 1,2,3,4:
#'DrawCurves(fit,modalityID = 2,readerID = 1:4, new.imaging.device = FALSE)
#'
#'
#'#The 3-rd modality with all readers 1,2,3,4:
#'DrawCurves(fit,modalityID = 3,readerID = 1:4, new.imaging.device = FALSE)
#'
#'
#'#The 4-th modality with all readers 1,2,3,4:
#'DrawCurves(fit,modalityID = 4,readerID = 1:4, new.imaging.device = FALSE)
#'
#'
#'#The 5-th modality with all readers 1,2,3,4:
#'DrawCurves(fit,modalityID = 5,readerID = 1:4, new.imaging.device = FALSE)
#'
#'
#'
#'# Draw for all pairs of modalities and readers:
#'
#'             DrawCurves(
#'                         modalityID = 1:fit@dataList$M,
#'                           readerID = 1:fit@dataList$Q,
#'                        StanS4class = fit
#'                         )
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'# Changes the color by
#'
#'
#'                              DrawCurves(fit, type = 2)
#'                              DrawCurves(fit, type = 3)
#'                              DrawCurves(fit, type = 4)
#'                              DrawCurves(fit, type = 5)
#'                              DrawCurves(fit, type = 6)
#'                              DrawCurves(fit, type = 7)
#'
#'
#'
#'
#'
#'#================The Second Example======================================================
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'# This function is available in the case of a single reader and a single modality.
#'# The reason why the maintainer separate the function for two processes, one is
#'# the fitting and the second is to plot curves is, in MRMC case,
#'# it tooks a time to drawing, but in the a single reader and a single modality case, drawing
#'# the curve is very fast, so in fitting process the curves are also depicted, however
#'# by this function user can draw the FROC curves.
#'
#'
#'
#'
#'
#'#First, we prepare the data endowed with this package.
#'
#'
#'
#'
#'
#'                           dat  <- get(data("dataList.Chakra.1"))
#'
#'
#'
#'
#'#Second, we fit a model to data named "dat"
#'
#'
#'
#'
#'
#'                              fit <-  fit_srsc(dat)
#'
#'
#'
#'
#'
#'# Drawing the curves by
#'
#'
#'                              DrawCurves(fit)
#'
#'
#'
#'
#'
#'
#'# Changes the color by
#'
#'
#'                              DrawCurves(fit, type = 2)
#'                              DrawCurves(fit, type = 3)
#'                              DrawCurves(fit, type = 4)
#'                              DrawCurves(fit, type = 5)
#'                              DrawCurves(fit, type = 6)
#'                              DrawCurves(fit, type = 7)

#'
#' #      Close the graphic device to avoid errors in R CMD check.
#'
#'      Close_all_graphic_devices() # 2020 August
#'}# dottest




#' @export
#'@inheritParams DrawCurves_MRMC_pairwise
#'@inheritParams DrawCurves_srsc

#'@param indexCFPCTP TRUE of FALSE. If TRUE, then the cumulative false and hits are specified with its confidence level.
#'@param upper_x A non-negative real number. This is a upper bound for the axis of the horisontal coordinate of FROC curve.
#'@param upper_y A non-negative real number. This is a upper bound for the axis of the vertical coordinate of FROC curve.
#'@param lower_X A non-negative real number. This is a lower bound for the axis of the horisontal coordinate of FROC curve.
#'@param lower_y A non-negative real number. This is a lower bound for the axis of the vertical coordinate of FROC curve.
#'

#'
#'@param DrawAUC  TRUE of FALSE. If TRUE then area under the  AFROC curves are painted.
#'@param type An integer, for the color of background and etc.
#'@param color_is_changed_by_each_reader A logical, if \code{TRUE}, then the FROC curves, AFROC curves, and FPF, TPF are colored accordingly by each reader. The aim of FROC analysis is to compare the modality and not reader, so the default value is false, and curves and FPF and TPF are colored by each modalities.
# _________________________ -----
DrawCurves <- function(
  StanS4class,
  modalityID,
  readerID,
  title=TRUE,
  type_to_be_passed_into_plot = "l",
  indexCFPCTP=FALSE,
  upper_x,
  upper_y,
  lower_X=0,
  lower_y=0,
  new.imaging.device=TRUE,
  Colour=TRUE,
  DrawFROCcurve=TRUE,
  DrawAFROCcurve=FALSE,
  DrawAUC=TRUE,
  DrawCFPCTP=TRUE,
  Draw.Flexible.upper_y=TRUE,
  Draw.Flexible.lower_y=TRUE,
  summary=TRUE,
  type = 4,
  color_is_changed_by_each_reader=FALSE,
  Draw.inner.circle.for.CFPCTPs=TRUE

){

  fit <- StanS4class

  if(is_stanfitExtended(fit)){#if error occurs 2020 Nov 15



  if(!is.null(fit@dataList$M)&&fit@dataList$M==1)  color_is_changed_by_each_reader <- TRUE

  # if(grDevices::dev.cur() > 1) {
  #   message("\n* There are one more multiple graphics devices. I am afraid you confuse them. Please be carefull.")
  #   }
  if(fit@studyDesign=="srsc.per.image" || fit@studyDesign=="srsc.per.lesion"){
    # tryCatch({#tryCatch 2020 Nov 15

    DrawCurves_srsc(
      title=title,
      type_to_be_passed_into_plot=type_to_be_passed_into_plot,
      indexCFPCTP=indexCFPCTP,
      upper_x=upper_x,
      upper_y=upper_y,
      lower_X=lower_X,
      lower_y=lower_y,
      new.imaging.device=new.imaging.device,
      StanS4class=StanS4class,
      Drawcol = Colour, # Name is chainged!!??
      DrawFROCcurve=DrawFROCcurve,
      DrawAFROCcurve=DrawAFROCcurve,
      DrawCFPCTP=DrawCFPCTP,
      DrawAUC=DrawAUC,
      Draw.inner.circle.for.CFPCTPs=Draw.inner.circle.for.CFPCTPs,
      type = type

    )

     # }, error = function(e) {#tryCatch 2020 Nov 15
     #             error_plot()#tryCatch 2020 Nov 15
     #                       })#tryCatch 2020 Nov 15
  }

  if(fit@studyDesign=="MRMC"){


    if(missing(modalityID)){
      message("\n* WARNING:\n")
      message("\n* The variable  \"modalityID\" is missing, so we write the curve for the first modality only. If you want to write curves for, e.g., the first and the third modality, then it is accomplished by taking modalityID =c(1,3)")
      modalityID <-c(1)
      warning("* The variable  \"modalityID\" is missing, so we write the curve for the first modality only. If you want to write curves for, e.g., the first and the third modality, then it is accomplished by taking modalityID =c(1,3)")


    }

    if(missing(readerID)){
      message("*\n WARNING:\n")
      message("\n* The variable  \"readerID\" is missing, so we write the curve for the all readers. If you want to write curves for, e.g., the first and the third modality, then it is accomplished by taking readerID =c(1,3)")
      readerID <-1:fit@dataList$Q
      warning("* The variable  \"readerID\" is missing, so we write the curve forthe all readers. If you want to write curves for, e.g., the first and the third modality, then it is accomplished by taking readerID =c(1,3)")

    }


    DrawCurves_MRMC_pairwise (
      StanS4class=StanS4class,
      modalityID =modalityID,
      readerID=readerID,
      type_to_be_passed_into_plot=type_to_be_passed_into_plot,
      title=title,

      Colour=Colour,
      new.imaging.device = new.imaging.device,
      summary=summary,
      type = type,
      color_is_changed_by_each_reader=color_is_changed_by_each_reader,
      DrawFROCcurve=DrawFROCcurve,
      DrawAFROCcurve=DrawAFROCcurve,
      DrawCFPCTP=DrawCFPCTP,
      Draw.Flexible.upper_y=Draw.Flexible.upper_y,
      Draw.Flexible.lower_y=Draw.Flexible.lower_y
    )



  }


  }else{#if error occurs 2020 Nov 15
    color_message("Now, the class is ",class(fit)," but  should be stanfitExtended")
  warning("Object is not stanfitExtended")#if error occurs 2020 Nov 15
    error_plot()}#if error occurs 2020 Nov 15
}

















#' @title    Draw the FROC  curves
#'@description     Draw an FROC  curves and an AFROC curves.
#'@inheritParams DrawCurves
#@param lower_x A non-negative real number. This is a lower bound for the axis of the horisontal coordinate of FROC curve.
#@param lower_y A non-negative real number. This is a lower bound for the axis of the vertical coordinate of FROC curve.
#'
#'@param Draw.inner.circle.for.CFPCTPs TRUE or FALSE. If true, then to plot the cumulative false positives and true positives the plot points is depicted by two way, one is a large circle and one is a small circle. By see the small circle, user can see the more precise position of these points.
#'@inheritParams DrawCurves_MRMC_pairwise
#'@inheritParams fit_Bayesian_FROC
#' @export
# _________________________ -----

DrawCurves_srsc <- function(
  StanS4class,

  type = 4,
  type_to_be_passed_into_plot = "p",

  title=TRUE,
  indexCFPCTP=FALSE,
  upper_x,
  upper_y,
  lower_X =0,
  lower_y=0,
  new.imaging.device=TRUE,
  Drawcol = TRUE,
  DrawFROCcurve=TRUE,
  DrawAFROCcurve=FALSE,
  DrawCFPCTP=TRUE,
  Draw.inner.circle.for.CFPCTPs =TRUE,
  DrawAUC=TRUE

){

  # stopifnot(is(StanS4class, "stanfitExtended"))

  fit <- StanS4class
  data <- fit@metadata
  ff <- data$ff
  C <- as.integer(data$C)
  f <- data$f
  h <- data$h
  NI <- data$NI
  NL <- data$NL
  hh <- data$hh
  if( missing(upper_x)){  upper_x <- max(ff)}
  if( missing(upper_y)){  upper_y <- max(hh,1) }
  if( missing(lower_X)){  lower_X <- min(ff)}
  if( missing(upper_y)){  lower_y <- min(hh,1) }


  xlim <- c(lower_X,upper_x)
  ylim <- c(lower_y,upper_y)

  if( is.nan(lower_X) ||is.nan( upper_x )) xlim <- c(0,1)
  if( is.nan(lower_y) ||is.nan( upper_y )) ylim <- c(0,1)

  # browser()

  convergence <- fit@convergence
  chisquare   <- fit@chisquare
  pvalue      <- fit@posterior_predictive_pvalue_for_chi_square_goodness_of_fit#2020 Nov 17



  l<- fit@plotdata$x.FROC
  x<- fit@plotdata$x.AFROC #AFROC
  y <-   fit@plotdata$y.FROC

  PreciseLogLikelihood   <-  fit@PreciseLogLikelihood
  waic <- fit@WAIC


  ModifiedPoisson<-StanS4class@ModifiedPoisson
  if( is.na(ModifiedPoisson)){ModifiedPoisson<-FALSE
  warning("ModifiedPoisson is missing.")
  xlabel <- 'cumulative false positive per ?'
  }
  else if (ModifiedPoisson) xlabel <- 'cumulative false positive per nodule'
  # else if (!ModifiedPoisson&&!is.na(ModifiedPoisson)) xlabel <- 'cumulative false positive per image'
  if (!ModifiedPoisson ) xlabel <- 'cumulative false positive per image'
  # browser()

 chisquare <- signif(chisquare,digits = 3)
 waic <- signif(waic,digits = 3)

  if(PreciseLogLikelihood){  title_of_plot <-  substitute(paste("Post. Pred. Pvalue = ", pvalue ,", Goodness of fit  ",chi^2*(D*"|"* hat(theta)[EAP] ),  " = ",  chisquare , ",  WAIC =",waic," where,", hat(theta)[EAP], ":=", integral( theta*pi(theta*"|"*D)*d*theta, Theta, .  ), "."   ),list( pvalue=pvalue, chisquare=chisquare, waic=waic)  )} # 2019 Jun 22 demo(plotmath)
  # if(PreciseLogLikelihood){  title <- paste("chi^2 goodness of fit with posterior mean  = ", chisquare, ", smaller is better.  WAIC =",waic)}
  if(!PreciseLogLikelihood){  title_of_plot <-  substitute(paste("Post. Pred. Pvalue = ", pvalue ,",Goodness of fit",integral( chi^2*(D*"|"* theta)*pi(theta*"|"*D)*d*theta, Theta, .  )  , " = ",  chisquare , "," ),list( pvalue=pvalue,chisquare=chisquare)  )} # 2019 Jun 22 demo(plotmath)
  if(!title){title_of_plot <-""}




  if (new.imaging.device)  grDevices::dev.new()



  graphics::par(lwd = 2 )
  suppressWarnings(graphics::par(new=TRUE));
# color FALSE -------
  if(Drawcol==FALSE){
    if(DrawAFROCcurve){
      # small_margin()
      # small_margin(Top.mar = 2,Top.oma = 2)
      small_margin(Top.mar =  2,Top.oma = 1.4)

      suppressWarnings(graphics::par(new=TRUE)); plot(x,y,type=type_to_be_passed_into_plot,#AFROC
                                                      # xlim = c(0,upper_x),
                                                      # ylim = c(0,upper_y),
                                                      xlim = xlim,
                                                      ylim = ylim,
                                                      col = 'black',
                                                      cex=0.1,
                                                      xlab = xlabel,
                                                      ylab = 'cumulative hit per lesion',
                                                      cex.axis =1.3,
                                                      main =title_of_plot

      )


      if(DrawAUC){
        y_buttom<- rep(0,length(x))
        graphics::segments( x,y_buttom, x,y, col="gray",
                            xlim = c(0,upper_x ),
                            ylim = c(0,upper_y)

        )

        suppressWarnings(graphics::par(new=TRUE)); plot(x,y,type=type_to_be_passed_into_plot,#AFROC
                                                        # xlim = c(0,upper_x),ylim = c(0,upper_y),
                                                        xlim = xlim,
                                                        ylim = ylim,
                                                        col = 'black',
                                                        cex=0.5,
                                                        xlab = xlabel,
                                                        ylab = 'cumulative hit per lesion',
                                                        cex.axis =1.3,
                                                        main =title_of_plot

        )


        if (upper_y>1) graphics::abline(h=1)


      }

# error print --------------
      # if(convergence ==FALSE){
      #   suppressWarnings(graphics::par(new=TRUE));
      #   graphics::text(0.5,0.5,paste( "max Rhat = " ,max(summary(fit)$summary[,"Rhat"]) ,", min Rhat = ", min(summary(fit)$summary[,"Rhat"])  ,NL,sep = ""),col="red",cex = 2)
      # }




    }
    if(DrawFROCcurve){
      # small_margin()
      # small_margin(Top.mar = 2,Top.oma = 2)
      small_margin(Top.mar =  2,Top.oma = 1.4)

      suppressWarnings(graphics::par(new=TRUE));
      plot(l,y,type=type_to_be_passed_into_plot,
           # xlim = c(0,upper_x),
           # ylim = c(0,upper_y),
           xlim = xlim,
           ylim = ylim,
           cex=0.3,
           col = 'black',
           xlab = '',
           ylab = '',
           cex.axis =1.3,
           main =title_of_plot
      )
      if (upper_y>1) graphics::abline(h=1)

    }
    if(DrawCFPCTP){
      # CTP CFP Here 2019 Oct 2  ----

      # small_margin()
      # small_margin(Top.mar = 2,Top.oma = 2)
      small_margin(Top.mar =  2,Top.oma = 1.4)


      suppressWarnings(graphics::par(new=TRUE));
      plot(ff,hh,cex=3,
           # xlim = c(0,upper_x),
           # ylim = c(0,upper_y),
           xlim = xlim,
           ylim = ylim,
           col = 'black',
           xlab = '',
           ylab = '',
           cex.axis =1.3,
           main =title_of_plot
      )

      if (upper_y>1) graphics::abline(h=1)


      if(Draw.inner.circle.for.CFPCTPs ){


        # CTP CFP Here 2019 Oct 2  ----

        # small_margin()
        # small_margin(Top.mar = 2,Top.oma = 2)
        # small_margin(Top.mar =  1,Top.oma = 1.4)
        small_margin(Top.mar =  2,Top.oma = 1.4)

        suppressWarnings(graphics::par(new=TRUE));
        plot(ff,hh,cex=1,
             # xlim = c(0,upper_x),
             # ylim = c(0,upper_y),
             xlim = xlim,
             ylim = ylim,
             col = 'black',
             xlab = '',
             ylab = '',
             cex.axis =1.3,
             main =title_of_plot

        )
        if (upper_y>1) graphics::abline(h=1)

      }







    }
  }#  if(Drawcol==FALSE


  # color TRUE -------
  if(Drawcol){   dark_theme(type = type)


    if(DrawAFROCcurve){

      suppressWarnings(graphics::par(new=TRUE)); plot(x,y,type=type_to_be_passed_into_plot,
                                                      col ="antiquewhite1",
                                                      cex= 0.1 ,
                                                      # xlim = c(0,upper_x ),ylim = c(0,upper_y),
                                                      xlim = xlim,
                                                      ylim = ylim,
                                                      xlab = xlabel,
                                                      ylab = 'cumulative hit per lesion'
                                                      ,main =title_of_plot
      );
      if (upper_y>1) graphics::abline(h=1)

      if(DrawAUC){
        y_buttom<- rep(0,length(x))
        graphics::segments( x,y_buttom, x,y, col="gray",
                            # xlim = c(0,upper_x ),
                            # ylim = c(0,upper_y)
                            xlim = xlim,
                            ylim = ylim

        )
        if (upper_y>1) graphics::abline(h=1)

      }



    }#DrawAFROCcurve










    if(DrawFROCcurve){


      #FROC
      suppressWarnings(graphics::par(new=TRUE)); plot(l,y,type=type_to_be_passed_into_plot,
                                                      col ="antiquewhite1",
                                                      bg="gray",
                                                      fg="gray",
                                                      xlab = xlabel,
                                                      ylab = 'cumulative hit per lesion',
                                                      cex= 0.05,
                                                      # xlim = c(0,upper_x ),
                                                      # ylim = c(0,upper_y) ,
                                                      xlim = xlim,
                                                      ylim = ylim,
                                                      main = title_of_plot

      );
      if (upper_y>1) graphics::abline(h=1)

    }


    if(DrawCFPCTP){
      if (  indexCFPCTP==FALSE){


        #CFP-CTP points
        # pchh <-paste(md);
        suppressWarnings(graphics::par(new=TRUE));plot(ff,hh,
                                                       # xlim = c(0,upper_x ),
                                                       # ylim = c(0,upper_y),
                                                       xlim = xlim,
                                                       ylim = ylim,
                                                       bg="gray",
                                                       fg="gray",
                                                       col = "antiquewhite1",#"green",#"red",#"yellow",   #"antiquewhite1",
                                                       # pch =paste(md),
                                                       cex=3,# Size of Dots
                                                       xlab = '', ylab = '')


        if (upper_y>1) graphics::abline(h=1)


        if(Draw.inner.circle.for.CFPCTPs ){

          # CTP CFP Here 2019 Oct 2  ----
          # Draw inner circle
          suppressWarnings(graphics::par(new=TRUE));plot(ff,hh,
                                                         # xlim = c(0,upper_x ),
                                                         # ylim = c(0,upper_y),
                                                         xlim = xlim,
                                                         ylim = ylim,
                                                         bg="gray",
                                                         fg="gray",
                                                         col ="antiquewhite1",
                                                         # pch =paste(md),
                                                         cex=1,#<3 Size of circles and this makes it the inner circle since it is less than 3 which is the size of outer circle
                                                         xlab = '', ylab = '')

          if (upper_y>1) graphics::abline(h=1)


        }









      }#  if   indexCFPCTP==FALSE

      if (  !indexCFPCTP==FALSE){

        # CTP CFP Here 2019 Oct 2  ----
        #CFP-CTP points
        C <- StanS4class@metadata$C
        for (cd in 1:C)  {
          suppressWarnings(graphics::par(new=TRUE));plot(ff[cd],hh[cd],
                                                         # xlim = c(0,upper_x ),
                                                         # ylim = c(0,upper_y),
                                                         xlim = xlim,
                                                         ylim = ylim,
                                                         bg="gray",
                                                         fg="gray",
                                                         col ="antiquewhite1",
                                                         pch =paste(cd),

                                                         # pch =paste(indexCFPCTP),
                                                         cex=1,# Size of Dots
                                                         xlab = '', ylab = '')
          if (upper_y>1) graphics::abline(h=1)

        }
      }#    if (  !indexCFPCTP==FALSE){
    } #  if(DrawCFPCTP){
  }#  if(Drawcol



# error message on imaging device ------
  error_message_on_imaging_device_rhat_values(fit,verbose = FALSE)
}#function end document


































#' @title    Draw the FROC
#'  curves for all modalities and readers
#'@description     Draw the FROC
#'  curves and AFROC curves for all
#'  specified modalities and readers.
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#' @export DrawCurves_MRMC
#' @examples
#'
#' \dontrun{

#'   fit <- fit_Bayesian_FROC(
#'                            dataList.Chakra.Web.orderd,
#'                            ite = 1111,
#'                            summary =FALSE
#'                            )
#'
#'                DrawCurves_MRMC(fit)
#'
#'
#'
#'      Close_all_graphic_devices() # 2020 August
#'
#'}# dottest

#'
#  devtools::document()
# _________________________ -----

DrawCurves_MRMC<- function(
  StanS4class,
  type_to_be_passed_into_plot = "p",
  title=TRUE,

  type = 1
)
{

  ModifiedPoisson<-StanS4class@ModifiedPoisson
  if( is.na(ModifiedPoisson)){ModifiedPoisson<-FALSE
  warning("ModifiedPoisson is missing.")
  xlab <- 'cumulative false positive per ?'
  }
  else if (ModifiedPoisson) xlab <- 'cumulative false positive per nodule'
  else if (!ModifiedPoisson&&!is.na(ModifiedPoisson)) xlab <- 'cumulative false positive per image'



  data <-StanS4class@metadata
  ffarrayN<-data$ffarrayN;
  M<-as.integer(data$M)
  Q<-as.integer(data$Q)
  hharrayN<-data$hharrayN;


  xyl<- StanS4class@plotdataMRMC

  x<-xyl$x
  l<-xyl$l

  y<-xyl$y

  message("\n--------------------------------------------------  \n")
  message("Now, we draw the curves, please wait...   \n")
  message("--------------------------------------------------  \n")

  # upper_x <- 1.1
  # upper ------
  upper_x <-max(ffarrayN)
  upper_y <- 1.0
  #AFROC  Draw ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for(md in 1:M){
    for(qd in 1:Q){
      #par(new = TRUE);plot(ffarrayN[,md,qd],hharrayN[,md,qd],
      #                    #           col = (md-1)*Q+(qd-1),
      #                    cex=1+(md-1)*Q+(qd-1), xlim = c(0,upper_x ), ylim = c(0,upper_y) ,xlab = '', ylab = '');


      #AFROC
      graphics::par(new = TRUE); plot(x,y[,md,qd],type=type_to_be_passed_into_plot,
                                      #col = 1+(md-1)*Q+(qd-1)
                                      #cex=md/10,
                                      cex= 0.1 ,
                                      xlim = c(0,upper_x ),ylim = c(0,upper_y),
                                      xlab = xlab,
                                      ylab = 'cumulative hit per nodule'
      );
      #FROC
      graphics::par(new = TRUE); plot(l,y[,md,qd],type=type_to_be_passed_into_plot,
                                      #col =1+(md-1)*Q+(qd-1),
                                      #cex=md/20,
                                      xlab =xlab,
                                      ylab = 'cumulative hit per nodule',
                                      cex= 0.1,
                                      xlim = c(0,upper_x ),
                                      ylim = c(0,upper_y)
                                      #xlab = '', ylab = ''
      );
      #CFP-CTP points

      graphics::par(new=T);plot(ffarrayN[,md,qd],hharrayN[,md,qd],cex=2,xlim = c(0,upper_x ),
                                ylim = c(0,upper_y),
                                #col = MM,
                                xlab = '', ylab = '')



    }}

  #par(new=T);plot(mean(ff), mean(hh),bg = "black",fg = "blue",bty = "o",
  #              cex=6,xlim = c(0,2),ylim = c(0,1.5),col = 'blue', xlab = '', ylab = '')
  #colors()

  #MM<-M+1



}

























#' @title    Draw the FROC  curves with Colour
#'@description     Draw  \emph{FROC  curves} and \emph{AFROC curves} for user's specified modalities and user's specified readers.
#' Using this function \emph{\strong{repeatedly}}, we can draw the different reader and modality in a  \emph{\strong{same}} plane simultaneously.
#'
#'
#'@details By drawing different modality FROC curves in the same plane, we can compare the modality.
#' E.g., if some modality FROC curve is \code{upper} then other modality curves,
#'  then we may say that the upper modality is \code{better} observer performance, i.e., higher AUC.
#'
#'@param Draw.Flexible.upper_y Logical, that is \code{TRUE} or \code{FALSE}. Whether or not the upper bounds of vertical axis are determined automatically.
#'@param Draw.Flexible.lower_y Logical, that is \code{TRUE} or \code{FALSE}. Whether or not the lower bounds of vertical axis are determined automatically.
# @param StanS4class This is an \R  object of class \emph{\code{ \link{stanfitExtended}}} inherited from the S4 class  \strong{\emph{\code{stanfit}}}.
#'
#'
#'@param modalityID This is a vector indicating modalityID whose component is natural namber.
#'@param readerID  This is a vector indicating readerID whose component is natural namber.
#'
#'
#'@param Colour Logical, that is \code{TRUE} or \code{FALSE}. Whether plot  of curves are with dark theme. Default is \code{TRUE} indicating dark theme.
#'@author Issei Tsunoda

#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@examples
#'
#' \dontrun{
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####

#' #1) Fit a model to data by the following:
#'
#'
#'
#'   fit <- fit_Bayesian_FROC(dataList.Chakra.Web, ite = 1111)
#'
#'
#' #Note that the return value "fit" is an object of an inherited S4 class from stanfit
#'
#' #2)  Using the above S4 class object, we draw the curves.

#'
#'
#'
#'  DrawCurves_MRMC_pairwise(fit,
#'                           modality = 1,
#'                           reader = 4
#'                           )
#'
#'
#'
#' #3) By changing the modality (or reader),
#'    #we can draw the curves with respect to different  modalities.
#'    #This shows the comparison of modalites.
#'
#'
#'
#'  DrawCurves_MRMC_pairwise(fit,
#'                           modality = 2,
#'                           reader = 4
#'                           )
#'
#'  DrawCurves_MRMC_pairwise(fit,
#'                           modality = 3,
#'                           reader = 4
#'                           )
#'
#'
#' #4) By repeating in this manner for different modalities or readers,
#' #    we can draw  AFROC (FROC) curves in a single imaging device.
#' # Revised 2019 Nov 27
#'
#'
#'
#' #5) If you want to draw the FROC curves
#' #for reader ID =1,2,3,4 and modality ID =1,2, then the code is as follows;
#'
#' DrawCurves_MRMC_pairwise(
#'                             fit,
#'                             modalityID = c(1,2,3,4),
#'                             readerID = c(1,2)
#'                             )
#'# Each color of curves corresponds to the modality ID.
#'# So, even if curves are different readers and same modality, then color is same.
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
#'
#'        #   Close the graphic device
#'            Close_all_graphic_devices()
#'        } # dottest

#'@inheritParams fit_Bayesian_FROC
#' @export DrawCurves_MRMC_pairwise
#'
# _________________________ -----
DrawCurves_MRMC_pairwise<- function(
  StanS4class,
  modalityID,  type_to_be_passed_into_plot = "p",
  title=TRUE,

  readerID,
  Colour=TRUE,
  DrawFROCcurve=TRUE,
  DrawAFROCcurve=FALSE,
  DrawCFPCTP=TRUE,
  Draw.Flexible.upper_y=TRUE,
  Draw.Flexible.lower_y=TRUE,
  new.imaging.device = TRUE,
  summary=TRUE,
  color_is_changed_by_each_reader = FALSE,
  type = 1

)
{






  if(Colour){ DrawCurves_MRMC_pairwise_col(
    StanS4class = StanS4class,
    modalityID = modalityID,
    readerID = readerID,
    new.imaging.device = new.imaging.device,
    type_to_be_passed_into_plot=type_to_be_passed_into_plot,
title = title ,
    summary=summary,
    type = type,
    color_is_changed_by_each_reader=color_is_changed_by_each_reader,
    DrawFROCcurve = DrawFROCcurve,
    DrawAFROCcurve = DrawAFROCcurve,
    DrawCFPCTP = DrawCFPCTP,
    Draw.Flexible.upper_y = Draw.Flexible.upper_y,
    Draw.Flexible.lower_y = Draw.Flexible.lower_y
  )}

  if(Colour==FALSE){ DrawCurves_MRMC_pairwise_BlackWhite(
    StanS4class = StanS4class,
    modalityID = modalityID,
    readerID = readerID,
    new.imaging.device = new.imaging.device,
    summary=summary,
    type = type,
    type_to_be_passed_into_plot=type_to_be_passed_into_plot,
    title = title ,

    DrawFROCcurve = DrawFROCcurve,
    DrawAFROCcurve = DrawAFROCcurve,
    DrawCFPCTP = DrawCFPCTP,
    Draw.Flexible.upper_y = Draw.Flexible.upper_y,
    Draw.Flexible.lower_y = Draw.Flexible.lower_y
  )}

  if(!Colour==FALSE && !Colour ){ warning("Colour is TRUE or FALSE")}



}























#' @title    Draw the FROC  curves without colour
#'@description   Plot curves without colors (dark theme), that is, black and white (white backgroud with black curves).  Draw  FROC  curves and  AFROC curves for user's specified modality and user's specified reader.
#' Using this function \strong{repeatedly}, we can draw cueves simultaneously,
#' and we compare observer performance of the different reader and modality \strong{intuitively}.
#' So, we can visualize the difference of modality (reader).
#'
#' @export DrawCurves_MRMC_pairwise_BlackWhite
#  devtools::document();help("DrawCurves_MRMC_pairwise")
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves_MRMC_pairwise
# _________________________ -----

DrawCurves_MRMC_pairwise_BlackWhite<- function(
  StanS4class,
  modalityID ,
  readerID,  type_to_be_passed_into_plot = "p",
  title=TRUE,

  new.imaging.device = TRUE,
  DrawFROCcurve=TRUE,
  DrawAFROCcurve=FALSE,
  DrawCFPCTP=TRUE,
  Draw.Flexible.upper_y=TRUE,
  Draw.Flexible.lower_y=TRUE,
  summary=TRUE,
  type = 1

)
{


  ModifiedPoisson<-StanS4class@ModifiedPoisson
  if( is.na(ModifiedPoisson)){ModifiedPoisson<-FALSE
  warning("ModifiedPoisson is missing.")
  xlab <- 'cumulative false positive per ?'
  }
  else if (ModifiedPoisson) xlab <- 'cumulative false positive per nodule'
  else if (!ModifiedPoisson&&!is.na(ModifiedPoisson)) xlab <- 'cumulative false positive per image'

  # missing.modalityID <-missing(modalityID)
  # missing.readerID <-missing(readerID)
  # if(missing(modalityID)){modalityID <- 1  }
  # if(missing(readerID)){readerID <- 1:StanS4class@dataList$Q  }
  #
  #   if(!(missing.modalityID || missing.readerID) ){
  #     print(methods::as(StanS4class, "stanfit"),pars=c("AA"))
  #   }

  fit <-StanS4class



  data <-fit@metadata



  m<-data$m   ;S<-data$S;  NL<-data$NL;c<-data$c;q<-data$q;
  h<-data$h; f<-data$f;
  hh<-data$hh; hhN<-data$hhN;
  ff<-data$ff;ffN<-data$ffN;
  harray<-data$harray;    farray<-data$farray;
  hharray<-data$hharray;    ffarray<-data$ffarray;
  hharrayN<-data$hharrayN;    ffarrayN<-data$ffarrayN;

  C<-as.integer(data$C)
  M<-as.integer(data$M)
  N<-as.integer(data$N)
  Q<-as.integer(data$Q)



  if(missing(modalityID)){
    message("*\n WARNING:\n")
    message("\n* The variable  \"modalityID\" is missing, so we write the curve for the first modality only. If you want to write curves for, e.g., the first and the third modality, then it is accomplished by taking modalityID =c(1,3)")
    modalityID <-1
  }

  if(missing(readerID)){
    message("*\n WARNING:\n")
    message("\n* The variable  \"readerID\" is missing, so we write the curve for all readers. If you want to write curves for, e.g., the first and the third modality, then it is accomplished by taking readerID =c(1,3)")
    readerID <-1:StanS4class@dataList$Q
  }







  if( max( modalityID) >M ){
    message("\n \n Error: \n")
    message( "* Your input modality ID is not exists. \n* Your input modality ID should be in the range [1,",M,"].\n")
    return(message("* Please change the modalityID so that it is within the appropriate range [1,",M,"].\n"))

  } else{
    if(max(readerID) >Q){
      message("\n \n Error: \n")
      message( "* Your inputting reader ID does not exist. \n* Your input reader ID should be in the range [1,",Q,"].\n")
      return(message("* Please change the readerID so that it is within the appropriate range [1,",Q,"].\n"))
    }else{

    }
  }




  if(  DrawFROCcurve == TRUE|| DrawCFPCTP||DrawAFROCcurve ){
    if(new.imaging.device == TRUE) grDevices::dev.new()
  }
  if( !( DrawFROCcurve == TRUE|| DrawCFPCTP||DrawAFROCcurve) ){
    message("\n* We do not draw anything according to your input.\n")
  }






  war <- fit@sim$warmup
  cha <- fit@sim$chains
  ite <- fit@sim$iter

  #Draw the  AFROC curve~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xyl<- StanS4class@plotdataMRMC

  x<-xyl$x
  l<-xyl$l

  y<-xyl$y
  EAP_AA<-xyl$EAP_AA
  # chisquare<-xyl$chisquare

  upper_x <-max(ffarrayN)
  if(Draw.Flexible.upper_y==FALSE){
    upper_y <- 1.0
  }
  if(Draw.Flexible.upper_y){
    upper_y <- max(hharrayN)
  }

  if(Draw.Flexible.lower_y==FALSE){
    lower_y <- 0
  }
  if(Draw.Flexible.lower_y){
    lower_y <- min(hharrayN)
  }


  #AFROC  Draw ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  ssss<-paste("",sep = "")
  for (md in sort( modalityID)){
    if(md==min(modalityID)){ ssss<-paste(ssss,md,sep = "i.e., ")}
    if(!md==min(modalityID)){ ssss<-paste(ssss,", ",md,sep = "")}# in plot, each number is separated by "," interactively.
  }
  mainlabel <-paste(" Each Number (",ssss,") in the scatter plot means the modality ID.")
  if(!title){mainlabel <-""}
# browser()

  for (md in modalityID){
    for (qd in readerID){
      if(DrawAFROCcurve){

        #AFROC
        suppressWarnings(graphics::par(new=TRUE)); plot(x,y[,md,qd],type=type_to_be_passed_into_plot,
                                                        #col = 1+(md-1)*Q+(qd-1)
                                                        #cex=md/10,
                                                        cex= 0.1 ,
                                                        xlim = c(0,upper_x ),ylim = c(0,upper_y),
                                                        xlab =xlab,
                                                        ylab = 'cumulative hit per nodule',
                                                        main = mainlabel

        );
      }
      #FROC
      if(DrawFROCcurve){

        suppressWarnings(graphics::par(new=TRUE)); plot(l,y[,md,qd],type=type_to_be_passed_into_plot,
                                                        xlab =xlab,
                                                        ylab = 'cumulative hit per nodule',
                                                        cex= 0.1,
                                                        xlim = c(0,upper_x ),
                                                        ylim = c(0,upper_y),
                                                        main = mainlabel

        );
      }
      #CFP-CTP points
      if(DrawCFPCTP){

        suppressWarnings(graphics::par(new=TRUE));
        plot(ffarrayN[,md,qd],hharrayN[,md,qd],
             cex=1,
             xlim = c(0,upper_x ),
             ylim = c(0,upper_y),

             pch =paste(md),# I refer 2019 02.02.

             xlab =xlab,
             ylab = 'cumulative hit per nodule',
             main = mainlabel)
      } #DrawCFPCTP

      # if (  missing.modalityID   ){
      #   message("\n* WARNING:\n")
      #   message("\n* The variable  \"modalityID\" is missing, so we write the curve for the first modality only. If you want to write curves for, e.g., the first and the third modality, then it is accomplished by taking modalityID =c(1,3)")
      # }
      # if (  missing.readerID   ){
      #   message("\n* WARNING:\n")
      #   message("\n* The variable  \"readerID\" is missing, so we write the curve for the first readerID only. If you want to write curves for, e.g., the first and the third modality, then it is accomplished by taking readerID =c(1,3)")
      # }
      # if(missing.modalityID || missing.readerID) {
      #   return(message("Please specify the modality ID and reader ID for drawing curves."))
      # }

      AUC <- EAP_AA[md,qd]

      if (summary) {

        message("\n--------------------------------------------------  \n")
        message(" * The FROC and AFROC curves are depicted for the following: \n \n ")
        message("    ModalityID:", md," \n")
        message("    ReaderID  :", qd," \n")
        message( " \n")
        message(" * The expected a posterior estimate of the area under the AFROC curves with respect to above specified modality (",md,") and reader (",qd,") : \n \n ")
        message("     AUC=", AUC," \n")
        # message("The goodness of fit chi-square statistic: \n \n ")
        # message("     chi-square=", chisquare," \n")
        message("--------------------------------------------------  \n")
        message("\n * Note that this AUC is denoted by AA[",md,"," , qd,"]  for  modality (",md,") and reader (",qd,") in the fitted model object of class stanfit (more precisely inheritied class from stanfit, named stanfitExtended).\n")
      }
    }}# For sentence

  #par(new=T);plot(mean(ff), mean(hh),bg = "black",fg = "blue",bty = "o",
  #              cex=6,xlim = c(0,2),ylim = c(0,1.5),col = 'blue', xlab = '', ylab = '')
  #colors()
  if (summary) {

    message("\n--------------------------------------------------  \n")
    message("\n * Note that these curves are estimated by \"only one\" hierarchical model, more precisely we do not fit these curves by non hierarchical model for each reader and each modality.\n")
    message("\n * In the plot, the AFROC curves emanate from origin (0,0) to (1,1).\n")
    message("\n * In the plot, the numbers of the scatter plot mean modaltiy numbers. So, same reader has different numbers according to modality even if reader is same. \n")

    explanation_for_what_curves_are_drawn(  modalityID = modalityID,
                                            readerID   = readerID)
  }
}








#' @title    Draw the FROC  curves with Colour
#'@description     Draw an FROC  curves and an AFROC curves for user's specified modality and user's specified reader.
#' Using this function \strong{repeatedly}, we can draw the different reader and modality in a  \strong{same} plane simultaneously.
#' So, we can visualize the difference of modality (reader).
#'
# @details

#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves_MRMC_pairwise
#'@importFrom  grDevices dev.new


#' @export DrawCurves_MRMC_pairwise_col
#  devtools::document();help("DrawCurves_MRMC_pairwise_col")
# _________________________ -----

DrawCurves_MRMC_pairwise_col<- function(
  StanS4class,
  modalityID,
  readerID,  type_to_be_passed_into_plot = "p",
  title=TRUE,

  type = 1,
  color_is_changed_by_each_reader = FALSE,
  # mesh.for.drawing.curve=10000,
  new.imaging.device = TRUE,
  DrawFROCcurve=TRUE,
  DrawAFROCcurve=FALSE,
  DrawCFPCTP=TRUE,
  Draw.Flexible.upper_y=TRUE,
  Draw.Flexible.lower_y=TRUE,
  summary=TRUE
)
{



  ModifiedPoisson<-StanS4class@ModifiedPoisson
  if( is.na(ModifiedPoisson)){ModifiedPoisson<-FALSE
  warning("ModifiedPoisson is missing.")
  xlab <- 'cumulative false positive per ?'
  }
  else if (ModifiedPoisson) xlab <- 'cumulative false positive per nodule'
  else if (!ModifiedPoisson&&!is.na(ModifiedPoisson)) xlab <- 'cumulative false positive per image'



  fit <-StanS4class
  dataList <- fit@dataList
  M <-as.integer(dataList$M)
  Q <-as.integer(dataList$Q)

  if(missing(modalityID)){
    message("*\n WARNING:\n")
    message("\n* The variable  \"modalityID\" is missing, so we write the curve for the first modality only. If you want to write curves for, e.g., the first and the third modality, then it is accomplished by taking modalityID =c(1,3)")
    modalityID <-1
  }

  if(missing(readerID)){
    message("*\n WARNING:\n")
    message("\n* The variable  \"readerID\" is missing, so we write the curve for all readers. If you want to write curves for, e.g., the first and the third modality, then it is accomplished by taking readerID =c(1,3)")
    readerID <-1:StanS4class@dataList$Q
  }
  # library(base) #For stop()
  if( max( modalityID) >M ){
    message("\n \n Error: \n")
    message( "* Your input modality ID is not exists. \n* Your input modality ID should be in the range [1,",M,"].\n")
    return(message("* Please change the modalityID so that it is within the appropriate range [1,",M,"].\n"))

  } else{
    if(max(readerID) >Q){
      message("\n \n Error: \n")
      message( "* Your inputting reader ID does not exist. \n* Your input reader ID should be in the range [1,",Q,"].\n")
      return(message("* Please change the readerID so that it is within the appropriate range [1,",Q,"].\n"))
    }else{

    }
  }

  #--------- START
  data <-fit@metadata



  m<-data$m   ;S<-data$S;  NL<-data$NL;c<-data$c;q<-data$q;
  h<-data$h; f<-data$f;
  hh<-data$hh; hhN<-data$hhN;
  ff<-data$ff;ffN<-data$ffN;
  harray<-data$harray;    farray<-data$farray;
  hharray<-data$hharray;    ffarray<-data$ffarray;
  hharrayN<-data$hharrayN;    ffarrayN<-data$ffarrayN;

  C<-as.integer(data$C)
  M<-as.integer(data$M)
  N<-as.integer(data$N)
  Q<-as.integer(data$Q)
  #--------- fin




















  if(  DrawFROCcurve == TRUE|| DrawCFPCTP||DrawAFROCcurve ){
    if(new.imaging.device == TRUE) grDevices::dev.new()
  }
  if( !( DrawFROCcurve == TRUE|| DrawCFPCTP||DrawAFROCcurve) ){
    message("\n* We do not draw anything according to your input.\n")
  }





  war <- fit@sim$warmup
  cha <- fit@sim$chains
  ite <- fit@sim$iter

  #Draw the  AFROC curve~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xyl<- StanS4class@plotdataMRMC

  x<-xyl$x
  l<-xyl$l

  y<-xyl$y
  EAP_AA<-xyl$EAP_AA
  # chisquare<-xyl$chisquare



  # upper_x <- 1.1
  upper_x <-max(ffarrayN)
  if(Draw.Flexible.upper_y==FALSE){
    upper_y <- 1.0
  }
  if(Draw.Flexible.upper_y){
    upper_y <- max(hharrayN)
  }

  if(Draw.Flexible.lower_y==FALSE){
    lower_y <- 0
  }
  if(Draw.Flexible.lower_y){
    lower_y <- min(hharrayN)
  }



  #AFROC  Draw ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #par(new = TRUE);plot(ffarrayN[,md,qd],hharrayN[,md,qd],
  #                    #           col = (md-1)*Q+(qd-1),
  #                    cex=1+(md-1)*Q+(qd-1), xlim = c(0,upper_x ), ylim = c(0,upper_y) ,xlab = '', ylab = '');





  md <- modalityID
  qd <- readerID

  Colour1 <-  array(0, dim=c( 100)) #array(0, dim=c( M))
  Colour2 <-  array(0, dim=c( M)) #
  Colour1[1]<-"antiquewhite1" # "gray0"  #"orange3"
  Colour1[2]<-"brown1"  #"orchid"
  Colour1[3]<-"dodgerblue1" #"coral1" #"deeppink4"  #"firebrick4"
  Colour1[4]<-"orange2"  #"aquamarine1"  #"darkcyan"
  Colour1[5]<-"yellowgreen" #"blue4" #"deeppink4"  #" cyan4 " #"mediumvioletred" # "green4"##"darkgoldenrod4"
  Colour1[6]<-"khaki1"#"darkolivegreen"
  Colour1[7]<-"antiquewhite1" # "gray0"  #"orange3"
  Colour1[8]<-"brown1"  #"orchid"
  Colour1[9]<-"dodgerblue1" #"coral1" #"deeppink4"  #"firebrick4"
  Colour1[10]<-"orange2"  #"aquamarine1"  #"darkcyan"
  Colour1[11]<-"yellowgreen" #"blue4" #"deeppink4"  #" cyan4 " #"mediumvioletred" # "green4"##"darkgoldenrod4"
  Colour1[12]<-"khaki1"#"darkolivegreen"
  Colour1[13]<-"dodgerblue1" #"coral1" #"deeppink4"  #"firebrick4"
  Colour1[14]<-"orange2"  #"aquamarine1"  #"darkcyan"
  Colour1[15]<-"yellowgreen" #"blue4" #"deeppink4"  #" cyan4 " #"mediumvioletred" # "green4"##"darkgoldenrod4"
  Colour1[16]<-"khaki1"#"darkolivegreen"
  Colour1[17]<-"antiquewhite1" # "gray0"  #"orange3"
  Colour1[18]<-"brown1"  #"orchid"
  Colour1[19]<-"dodgerblue1" #"coral1" #"deeppink4"  #"firebrick4"
  Colour1[20]<-"orange2"  #"aquamarine1"  #"darkcyan"
  for (ccc in 21:100) {
    Colour1[ccc]<-"orange2"#2020 Jan
  }

  # Colour1[11]<-"yellowgreen" #"blue4" #"deeppink4"  #" cyan4 " #"mediumvioletred" # "green4"##"darkgoldenrod4"
  # Colour1[12]<-"khaki1"#"darkolivegreen"
  #
  # for (cc in 9:20) {
  #   Colour1[cc] <- as.character(cc-8);
  # }
  # main label ----

  if(!color_is_changed_by_each_reader){
  ssss<-paste("",sep = "")
  for (md in sort( modalityID)){
    if(md==min(modalityID)){ ssss<-paste(ssss,md,sep = "i.e., ")}
    if(!md==min(modalityID)){ ssss<-paste(ssss,", ",md,sep = "")}# in plot, each number is separated by "," interactively.
  }
  mainlabel <-paste(" Each Number (",ssss,") in the scatter plot means the modality ID.")
  }
  if(!title){mainlabel <-""}


   # main label ----
  if(color_is_changed_by_each_reader){

  ssss<-paste("",sep = "")
  for (md in sort( readerID)){
    if(md==min(readerID)){ ssss<-paste(ssss,md,sep = "i.e., ")}
    if(!md==min(readerID)){ ssss<-paste(ssss,", ",md,sep = "")}# in plot, each number is separated by "," interactively.
  }
  mainlabel <-paste(" Each Number (",ssss,") in the scatter plot means the reader ID.")

  }
  if(!title){mainlabel <-""}

  # message("hhhhhhhhhhhhhhhhhhh")
  # color -----
  dark_theme(type = type)
  # graphics::par(bg= "gray12", #"gray27",#"gray40",#"black",# ,
  #               fg="gray",
  #               col.lab="bisque2" ,#"bisque" ,#  "antiquewhite1",
  #               col.axis="bisque2" ,##"bisque" "antiquewhite1",
  #               col.main="bisque2" ,
  #               cex.lab=1.5,
  #               cex.axis=1.3
  # )

  for (md in modalityID){
    for (qd in readerID){
# color by md or qd ---------------
if(color_is_changed_by_each_reader)xxxd <- qd
if(!color_is_changed_by_each_reader)xxxd <- md

      if(DrawAFROCcurve){
        #AFROC

        suppressWarnings(graphics::par(new=TRUE));        plot(
          x,y[,md,qd],type=type_to_be_passed_into_plot,
          col =Colour1[xxxd],# color by md or qd ----
          cex= 0.1 ,
          xlim = c(0,upper_x ),ylim = c(lower_y,upper_y),
          xlab = xlab,
          ylab = 'cumulative hit per nodule',
          main = mainlabel
        );

      }

      if(DrawFROCcurve){

        #FROC
        suppressWarnings(graphics::par(new=TRUE)); plot(
          l,y[,md,qd],type=type_to_be_passed_into_plot,
          col =Colour1[xxxd],
          bg="gray",
          fg="gray",
          xlab = xlab,
          ylab = 'cumulative hit per nodule',
          cex= 0.1,
          xlim = c(0,upper_x ),
          ylim = c(lower_y,upper_y)
          ,main = mainlabel

        );
      }

pch <-paste(xxxd)# number in plot  md or qd ----
if(xxxd>9) pch <-xxxd -9
      if(DrawCFPCTP){
        #CFP-CTP points
        suppressWarnings(graphics::par(new=TRUE));plot(
          ffarrayN[,md,qd],hharrayN[,md,qd],
          xlim = c(0,upper_x ),
          ylim = c(lower_y,upper_y),
          bg="gray",
          fg="gray",
          col =Colour1[xxxd],
          pch =pch,
          cex=1,# Size of Dots
          xlab = xlab,
          ylab = 'cumulative hit per nodule',
          main = mainlabel

        )
      } #DrawCFPCTP


      AUC <- EAP_AA[md,qd]



      if (summary) {

      message("\n--------------------------------------------------  \n")
      message(" * The FROC and AFROC curves are depicted for the following: \n \n ")
      message("    ModalityID:", md," \n")
      message("    ReaderID  :", qd," \n")
      message( " \n")
      message(" * The expected a posterior estimate of the area under the AFROC curves with respect to above specified modality (",md,") and reader (",qd,") : \n \n ")
      message("     AUC=", AUC," \n")
      # message("The goodness of fit chi-square statistic: \n \n ")
      # message("     chi-square=", chisquare," \n")
      message("--------------------------------------------------  \n")
      message("\n * Note that this AUC is denoted by AA[",md,"," , qd,"]  for  modality (",md,") and reader (",qd,") in the fitted model object of class stanfit (more precisely inheritied class from stanfit, named stanfitExtended).\n")
}
    }}# For sentence

  #par(new=T);plot(mean(ff), mean(hh),bg = "black",fg = "blue",bty = "o",
  #              cex=6,xlim = c(0,2),ylim = c(0,1.5),col = 'blue', xlab = '', ylab = '')
  #colors()
  if (summary) {

  message("\n--------------------------------------------------  \n")
  message("\n * Note that these curves are estimated by \"only one\" hierarchical model, more precisely we do not fit these curves by non hierarchical model for each reader and each modality.\n")
  message("\n * In the plot, the AFROC curves emanate from origin (0,0) to (1,1).\n")
  message("\n * In the plot, the numbers of the scatter plot mean modaltiy numbers. So, same reader has different numbers according to modality even if reader is same. \n")

  explanation_for_what_curves_are_drawn(  modalityID = modalityID,
                                          readerID   = readerID)
}
}

























































#' @title Scatter Plot of FPFs and TPFs via Splitting Factor
#' @description Make a factor vector by which we plot FPF and TPF.
#' @param cex A positive real number, specifying the size of dots in the resulting plot.
#' @param dataList.MRMC A list, indicating FROC data of MRMC.
#' See also \code{dataList} which is a variable of the function \code{\link{fit_Bayesian_FROC}()}.
#' @param colored_by_modality A logical, if TRUE, then the color in the scatter plot means modality ID.
#'  If not, then the each color in the scatter plot indicates reader ID.
#'
#' @param numbered_by_modality A logical, if TRUE, then the number in the scatter plot means modality ID.
#'  If not, then the each number in the scatter plot indicates reader ID.
#' @inheritParams fit_Bayesian_FROC
#' @return A dataframe, which is added TPF and FPF, etc into \code{dataList.MRMC}.
#'
#'\strong{\emph{Added Vectors as Contents of the Data-frame}}
#'\describe{
#'\item{ \code{CFP}    }{ A vector of \strong{\emph{Cumulative False Positive}}  }
#'\item{ \code{CTP}    }{ A vector of \strong{\emph{Cumulative True Positive }}  }
#'\item{ \code{TPF}    }{ A vector of \strong{\emph{True Positive Fraction   }}  }
#'\item{ \code{FPF}    }{ A vector of \strong{\emph{False Positive Fraction  }} per image or per lesion according to the logical variable \code{ModifiedPoisson}  }
#'\item{ \code{factor}    }{What this means is trivial.}

#'}
#'\strong{\emph{Vectors as Contents of the Data-frame \code{dataList.MRMC}}}
#'
#' \describe{
#' \item{ \code{c }  }{A vector of positive integers,  representing  the \emph{\strong{confidence level}}. This vector must be made by \code{rep(rep(C:1), M*Q)} }
#' \item{ \code{m }  }{A vector of positive integers,  representing  the \emph{\strong{modality}} ID vector. }
#' \item{ \code{q }  }{A vector of positive integers,  representing  the \emph{\strong{reader}} ID vector.}
#' \item{ \code{h }  }{A vector of non-negative integers,  representing  the number of \emph{\strong{hits}}.   }
#' \item{ \code{f }  }{A vector of non-negative integers,  representing  the number of \emph{\strong{false alarm}}.  }
#'  }
#'
#'
#' @export
# examples -----
#' @examples
#'
#'#========================================================================================
#'#                               The 1st example
#'#========================================================================================
#'
#'
#' v  <- v_truth_creator_for_many_readers_MRMC_data(M=1,Q=37)
#' m  <- mu_truth_creator_for_many_readers_MRMC_data(M=1,Q=37)
#' d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#'
#' plot_FPF_TPF_via_dataframe_with_split_factor(d,
#'   colored_by_modality    = TRUE,
#'   numbered_by_modality   = TRUE)
#'
#' plot_FPF_TPF_via_dataframe_with_split_factor(d,
#'   colored_by_modality   = FALSE,
#'   numbered_by_modality   = TRUE)
#'
#' plot_FPF_TPF_via_dataframe_with_split_factor(d,
#'   colored_by_modality    = TRUE,
#'   numbered_by_modality  = FALSE)
#'
#'
#' plot_FPF_TPF_via_dataframe_with_split_factor(d,
#'   colored_by_modality   = FALSE,
#'   numbered_by_modality  = FALSE)
#'
#'#========================================================================================
#'#                               The 2-nd example
#'#========================================================================================
#'#
#'
#'
#' v  <- v_truth_creator_for_many_readers_MRMC_data(M=2,Q=37)
#' m  <- mu_truth_creator_for_many_readers_MRMC_data(M=2,Q=37)
#' d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#'
#' plot_FPF_TPF_via_dataframe_with_split_factor(d,
#'   colored_by_modality    = TRUE,
#'   numbered_by_modality   = TRUE)
#'
#' plot_FPF_TPF_via_dataframe_with_split_factor(d,
#'   colored_by_modality   = FALSE,
#'   numbered_by_modality   = TRUE)
#'
#' plot_FPF_TPF_via_dataframe_with_split_factor(d,
#'   colored_by_modality    = TRUE,
#'   numbered_by_modality  = FALSE)
#'
#'
#' plot_FPF_TPF_via_dataframe_with_split_factor(d,
#'   colored_by_modality   = FALSE,
#'   numbered_by_modality  = FALSE)
#'
#'
#'
#'#========================================================================================
#'#                               The 3rd example
#'#========================================================================================
#'
#'
#'
#'
#' v  <- v_truth_creator_for_many_readers_MRMC_data(M=3,Q=7)
#' m  <- mu_truth_creator_for_many_readers_MRMC_data(M=3,Q=7)
#' d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#'
#' plot_FPF_TPF_via_dataframe_with_split_factor(d,
#'   colored_by_modality    = TRUE,
#'   numbered_by_modality   = TRUE)
#'
#' plot_FPF_TPF_via_dataframe_with_split_factor(d,
#'   colored_by_modality   = FALSE,
#'   numbered_by_modality   = TRUE)
#'
#' plot_FPF_TPF_via_dataframe_with_split_factor(d,
#'   colored_by_modality   = TRUE,
#'   numbered_by_modality  = FALSE)
#'
#'
#' plot_FPF_TPF_via_dataframe_with_split_factor(d,
#'   colored_by_modality   = FALSE,
#'   numbered_by_modality  = FALSE)

#'#========================================================================================
#'#                               The 4th example
#'#========================================================================================
#'
#'
#'
#'
#' plot_FPF_TPF_via_dataframe_with_split_factor( dataList.MRMC = dd,
#'                                               colored_by_modality  = TRUE,
#'                                               numbered_by_modality = TRUE)
#'
#'
#'
#'#========================================================================================
#'#                               The 5th example
#'#========================================================================================
#'
#'\dontrun{
#' a <- plot_FPF_TPF_via_dataframe_with_split_factor(dd)
#'
#' p <- ggplot2::ggplot(a, ggplot2::aes(FPF, TPF,
#'                             group = factor(factor),
#'                             colour = factor(m)) ) +
#'    ggplot2::geom_line(size = 1.4)
#' print(p)
#'
#'
#'
#'
#'
#'#========================================================================================
#'#                               The 6th example
#'#========================================================================================
#'
#' a <- plot_FPF_TPF_via_dataframe_with_split_factor(dd,cex = 1.8)
#'
#'
#'#========================================================================================
#'#                               The 7th example
#'#========================================================================================
#'
#'
#' # Plot empirical FROC curve whose modality is specified as following manner
#'
#' a <- plot_FPF_TPF_via_dataframe_with_split_factor(dd)
#' aa <- a[a$m == c(2,3), ]
#'
#' p <- ggplot2::ggplot(aa, ggplot2::aes(FPF, TPF,
#'                             group = factor(factor),
#'                             colour = factor(m)) ) +
#'    ggplot2::geom_line(size = 1.4)
#' print(p)
#'
#'
#'
#' # Plot empirical FROC curve whose modality is specified as following manner
#'
#' a <- plot_FPF_TPF_via_dataframe_with_split_factor(dd)
#' aa <- a[a$m %in%  c(4,3), ]
#'
#' p <- ggplot2::ggplot(aa, ggplot2::aes(FPF, TPF,
#'                             group = factor(factor),
#'                             colour = factor(m)) ) +
#'    ggplot2::geom_line(size = 1.4)
#' print(p)
#'
#'
#' # Plot empirical FROC curve whose modality is specified as following manner
#'
#' a <- plot_FPF_TPF_via_dataframe_with_split_factor(dd)
#' aa <- a[a$m %in% c(3,4), ]
#'
#' p <- ggplot2::ggplot(aa, ggplot2::aes(FPF, TPF,
#'                             group = factor(factor),
#'                             colour = factor(m)) ) +
#'    ggplot2::geom_line(size = 1.4)
#' print(p)
#'
#'
#'    #     Close_all_graphic_devices()
#'}#dontrun
#'
#'
plot_FPF_TPF_via_dataframe_with_split_factor <- function(
  dataList.MRMC ,
  ModifiedPoisson =FALSE,
  colored_by_modality =TRUE,
  numbered_by_modality =TRUE,
  cex =1.3

){


  M <- dataList.MRMC$M
  Q <- dataList.MRMC$Q
  C <- dataList.MRMC$C
  m <- dataList.MRMC$m
  q <- dataList.MRMC$q
  c <- dataList.MRMC$c
  h <- dataList.MRMC$h
  f <- dataList.MRMC$f
  NI <- dataList.MRMC$NI
  NL <- dataList.MRMC$NL
  names(h) <-NULL
  names(f) <-NULL
  names(m) <-NULL
  names(q) <-NULL
  names(c) <-NULL
  names(M) <-NULL
  names(Q) <-NULL
  names(C) <-NULL
  names(NI) <-NULL
  names(NI) <-NULL


  if(ModifiedPoisson==FALSE){ NX <- NI;
  xlabb <- " per image"}
  if(ModifiedPoisson==TRUE){ NX <- NL;
  xlabb <- " per lesion"}

  factor <-rep(1:(M*Q),1,each =C)
  h<-data.frame(h=h)
  h<- split(x=h, f=factor)
  CTP <- lapply(h, cumsum)
  CTP <- unlist(CTP)
  TPF <- unlist(CTP)/NL


  f<-data.frame(f=f)
  f<- split(x=f, f=factor) # Very caution ----
  CFP <- lapply(f, cumsum)
  CFP <- unlist(CFP)
  FPF <- unlist(CFP)/NX
  names(CTP) <-NULL
  names(CFP) <-NULL
  names(TPF) <-NULL
  names(FPF) <-NULL

  df <- data.frame(
    m=dataList.MRMC$m,
    q=dataList.MRMC$q,
    c=dataList.MRMC$c,
    # h = h,
    # f  = FALSE,
    # NI = dataList.MRMC$NI,
    # NL = dataList.MRMC$NL,
    CTP=CTP,
    CFP=CFP,
    TPF=TPF,
    FPF=FPF,
    factor=factor
  )
  # dark_theme()

  if(colored_by_modality){ col <- m;
  col.main <- "modality"}
  if(!colored_by_modality) {col <-q
  col.main <- "reader" }

  if(numbered_by_modality) {number <- m
  num.main <- "modality"}
  if(!numbered_by_modality){ number <-q
  num.main <- "reader"}

  main <- paste("Color =", col.main, ",  Number = ",num.main )
  xlab = paste("FPF",xlabb)
  with(df, plot(FPF, TPF, type="n",main =main,xlab = xlab))
  with(df, text(FPF, TPF, number, col=col, cex=cex))

  # To plot empiciral FROC, the author should add zeros in FPF and TPF. 2020 Mar.


  interval.length <- C
  co_interval.length <- length(FPF)/interval.length
  FPF.ext <- as.vector(t(cbind(0, matrix(FPF, co_interval.length, byrow=T))))
  TPF.ext <- as.vector(t(cbind(0, matrix(TPF, co_interval.length, byrow=T))))
  e <- m_q_c_vector_from_M_Q_C(M,Q,C+1)
  m <- e$m
  q <- e$q
  c <- e$c

  factor <-rep(1:(M*Q),1,each =C+1)
  # browser()

  df.extended <- data.frame(
    m=m,
    q=q,
    c=c,
    TPF=TPF.ext,
    FPF=FPF.ext,
    factor=factor
  )

  # knitr::kable(df)
  # browser()

  return(df.extended)
}#function


# as.vector(t(cbind(0, matrix(x, interval.length, byrow=T))))


#' @title Plot empirical FROC Curves by traditional ways of \pkg{ggplot2}
#' @description  Plot empirical FROC Curves.
#' @inheritParams plot_FPF_TPF_via_dataframe_with_split_factor
#' @param modalityID A vector of integer, specifying modality ID to be drawn.
#' @param readerID A vector of integer, specifying modality ID to be drawn.

#' @return An object made by ggplot2, I am not sure what it is.
#' @export
#'
#' @examples
#' \dontrun{
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                               The 1-st example
#'#========================================================================================
#'
#'
#' plot_empirical_FROC_curves(dd,readerID = 1:4,modalityID = 1:5)
#'plot_empirical_FROC_curves(dd,readerID = 1,modalityID = c(4,3))
#'plot_empirical_FROC_curves(dd,readerID = 2,modalityID = c(4,3))
#'plot_empirical_FROC_curves(dd,readerID = 3,modalityID = c(4,3))
#'plot_empirical_FROC_curves(dd,readerID = 4,modalityID = c(4,3))
#'
#'
#'
#'
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                               The  example
#'#========================================================================================
#'
#'
#'      v  <- v_truth_creator_for_many_readers_MRMC_data(M=2,Q=37)
#'  m  <- mu_truth_creator_for_many_readers_MRMC_data(M=2,Q=37)
#'  d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#'
#'
#'     plot_empirical_FROC_curves(d,readerID = 1:14,modalityID = 1:2)
#'
#'
#'     plot_empirical_FROC_curves(d,readerID = 1:24,modalityID = 1:2)
#'
#'
#'     plot_empirical_FROC_curves(d,readerID = 1:34,modalityID = 1:2)
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                               The  example
#'#========================================================================================
#'
#'
#'
#' v  <- v_truth_creator_for_many_readers_MRMC_data(M=2,Q=7)
#' m  <- mu_truth_creator_for_many_readers_MRMC_data(M=2,Q=7)
#' d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#'
#'
#' plot_empirical_FROC_curves(d,readerID = 1,modalityID = 1:2)
#' plot_empirical_FROC_curves(d,readerID = 2,modalityID = 1:2)
#' plot_empirical_FROC_curves(d,readerID = 3,modalityID = 1:2)
#' plot_empirical_FROC_curves(d,readerID = 4,modalityID = 1:2)
#' plot_empirical_FROC_curves(d,readerID = 5,modalityID = 1:2)
#' plot_empirical_FROC_curves(d,readerID = 6,modalityID = 1:2)
#' plot_empirical_FROC_curves(d,readerID = 7,modalityID = 1:2)
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                               The  example
#'#========================================================================================
#'
#'
#' plot_empirical_FROC_curves(dd)
#' plot_empirical_FROC_curves(dd,modalityID = c(3,5))
#' plot_empirical_FROC_curves(dd,modalityID = c(3,5),readerID = c(1,4))
#' plot_empirical_FROC_curves(dd,modalityID = c(3,5),readerID = c(1,3))
#' plot_empirical_FROC_curves(dd,modalityID = c(3,5),readerID = c(1,2,3))
#' plot_empirical_FROC_curves(dd,modalityID = c(3,5),readerID = c(1,2,3))
#' plot_empirical_FROC_curves(dd,modalityID = c(3,5),readerID = c(3))
#' plot_empirical_FROC_curves(dd,modalityID = c(3,5),readerID = c(1,2,3))
#' plot_empirical_FROC_curves(dd,modalityID = c(3,5),readerID = c(1,2))
#' plot_empirical_FROC_curves(dd,modalityID = c(3,5),readerID = c(2))
#' plot_empirical_FROC_curves(dd,modalityID = c(3),readerID = c(2))
#' plot_empirical_FROC_curves(dd,modalityID = c(5),readerID = c(2))
#'
#'
#'
#'
#'
#'
#'
#'}
#'
plot_empirical_FROC_curves <- function(
  dataList.MRMC ,
  ModifiedPoisson =FALSE,
  colored_by_modality =TRUE,
  numbered_by_modality =TRUE,
  cex =1.3,
  modalityID =c(1, dataList.MRMC$M),
  readerID =c(1, dataList.MRMC$Q)


) {

  if(ModifiedPoisson) xlabel <- "False Positive Fraction per lesion"
  if(!ModifiedPoisson) xlabel <- "False Positive Fraction per image"

  a <- plot_FPF_TPF_via_dataframe_with_split_factor(
    dataList.MRMC =dataList.MRMC ,
    ModifiedPoisson =ModifiedPoisson,
    colored_by_modality =colored_by_modality,
    numbered_by_modality =numbered_by_modality,
    cex =cex
  )

  aa <- a[a$m %in% modalityID, ]
  aa <- aa[aa$q %in% readerID, ]


  upper.lim.x <- max(a$FPF)
  lower.lim.x <- min(a$FPF)

  upper.lim.y <- max(a$TPF)
  lower.lim.y <- min(a$TPF)
# aa$FPF
  p <- ggplot2::ggplot(aa, ggplot2::aes(aa$FPF, aa$TPF,
                                        group = factor(aa$factor),
                                        colour = factor(aa$m)) ) +
    ggplot2::geom_line(size = 1.4)+
    ggplot2::xlim(lower.lim.x,upper.lim.x)+
    ggplot2::ylim(lower.lim.y,upper.lim.y)+

    ggplot2::labs(
      # subtitle="Colored by modality ID.",
      y="True Positive Fraction",
      x= xlabel,
      title="Empirical (observed) FROC Curve (Colored by modality)"
      # caption = "Source: midwest"
    ) +

    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = 12),#label name
      axis.title.y = ggplot2::element_text(size = 12),#label name
      axis.text.x = ggplot2::element_text(size = 22),
      axis.text.y = ggplot2::element_text(size = 22),
      # panel.background = ggplot2::element_rect(fill = 'darkgray', colour = 'red'),
      plot.background = ggplot2::element_rect(fill = "gray"),
      #          axis.text =ggplot2::element_text(size=12),
      #          axis.title=ggplot2::element_text(size=33,face="bold"),
      legend.title = ggplot2::element_text(color = "blue", size = 10,face="bold"),
      legend.text = ggplot2::element_text(color = "black", size = 20,face="bold")
    )
  # theme_grey(
  #    base_size = 11,
  #    base_family = "",
  #    base_line_size = 11/22,
  #    base_rect_size = 11/22
  # )
  # p <- p + guides(colour=guide_legend(title="Modality ID"))
  p$labels$colour <- "Modality"

  print(p)


}
