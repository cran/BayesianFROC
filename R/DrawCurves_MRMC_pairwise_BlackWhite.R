#' @title    Draw the FROC  curves without colour
#'@description   Plot curves without any color (dark theme), that is, black and white (white backgroud with black curves).  Draw  FROC  curves and  AFROC curves for user's specified modality and user's specified reader.
#' Using this function \strong{repeatedly}, we can draw or compare the different reader and modality in a  \strong{same} plane simultaneously.
#' So, we can visualize the difference of modality (reader).
#'
#' @export DrawCurves_MRMC_pairwise_BlackWhite
#  devtools::document();help("DrawCurves_MRMC_pairwise")
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves_MRMC_pairwise
DrawCurves_MRMC_pairwise_BlackWhite<- function(
  StanS4class,
  modalityID ,
  readerID,
  new.imaging.device = TRUE,
  DrawFROCcurve=TRUE,
  DrawAFROCcurve=FALSE,
  DrawCFPCTP=TRUE,
  Draw.Flexible.upper_y=TRUE,
  Draw.Flexible.lower_y=TRUE
)
{



  missing.modalityID <-missing(modalityID)
  missing.readerID <-missing(readerID)
  if(missing(modalityID)){modalityID <- c(1)  }
  if(missing(readerID)){readerID <- c(1)  }

  if(!(missing.modalityID || missing.readerID) ){
    print(methods::as(StanS4class, "stanfit"),pars=c("AA"))
  }

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

  if( max( modalityID) >M ){
    message("\n \n Error: \n")
    message( "* Your input modality ID is not exists. \n* Your input modality ID should be in the range [1,",M,"].\n")
    return(message("* Please change the modalityID so that it is within the appropriate range [1,",M,"].\n"))

  } else{
    if(max(readerID) >Q){
      message("\n \n Error: \n")
      message( "* Your input reader ID is not exists. \n* Your input reader ID should be in the range [1,",Q,"].\n")
      return(message("* Please change the readerID so that it is within the appropriate range [1,",Q,"].\n"))
    }else{

    }
  }




  if(  DrawFROCcurve == TRUE|| DrawCFPCTP==TRUE||DrawAFROCcurve==TRUE ){
    if(new.imaging.device == TRUE) grDevices::dev.new()
  }
  if( !( DrawFROCcurve == TRUE|| DrawCFPCTP==TRUE||DrawAFROCcurve==TRUE) ){
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
  if(Draw.Flexible.upper_y==TRUE){
    upper_y <- max(hharrayN)
  }

  if(Draw.Flexible.lower_y==FALSE){
    lower_y <- 0
  }
  if(Draw.Flexible.lower_y==TRUE){
    lower_y <- min(hharrayN)
  }


  # upper_x <- 1.1
  #AFROC  Draw ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  ssss<-paste("",sep = "")
  for (md in sort( modalityID)){
    if(md==min(modalityID)){ ssss<-paste(ssss,md,sep = "i.e., ")}
    if(!md==min(modalityID)){ ssss<-paste(ssss," and ",md,sep = "")}
  }
  mainlabel <-paste(" Each Number (",ssss,") in the scatter plot means the modality ID.")



  for (md in modalityID){
    for (qd in readerID){
      if(DrawAFROCcurve==TRUE){

        #AFROC
        suppressWarnings(graphics::par(new=TRUE)); plot(x,y[,md,qd],
                                        #col = 1+(md-1)*Q+(qd-1)
                                        #cex=md/10,
                                        cex= 0.1 ,
                                        xlim = c(0,upper_x ),ylim = c(0,upper_y),
                                        xlab = 'mean of false positives per nodule',
                                        ylab = 'cumulative hit per nodule',
                                        main = mainlabel

                                        );
      }
      #FROC
      if(DrawFROCcurve==TRUE){

        suppressWarnings(graphics::par(new=TRUE)); plot(l,y[,md,qd],
                                        xlab = 'mean of false positives per nodule',
                                        ylab = 'cumulative hit per nodule',
                                        cex= 0.1,
                                        xlim = c(0,upper_x ),
                                        ylim = c(0,upper_y),
                                        main = mainlabel

        );
      }
      #CFP-CTP points
      if(DrawCFPCTP==TRUE){

        suppressWarnings(graphics::par(new=TRUE));
        plot(ffarrayN[,md,qd],hharrayN[,md,qd],
            cex=1,
            xlim = c(0,upper_x ),
            ylim = c(0,upper_y),

            pch =paste(md),# I refer 2019 02.02.

            xlab = 'mean of false positives per nodule',
            ylab = 'cumulative hit per nodule',
            main = mainlabel)
      } #DrawCFPCTP==TRUE

if (  missing.modalityID   ){
  message("\n* WARNING:\n")
  message("\n* modalityID is missing, so we write the curve for the first modality only. If you want to write curves for, e.g., the first and the third modality, then it accomplishes by taking modalityID =c(1,3)")
}
if (  missing.readerID   ){
  message("\n* WARNING:\n")
  message("\n* readerID is missing, so we write the curve for the first readerID only. If you want to write curves for, e.g., the first and the third modality, then it accomplishes by taking readerID =c(1,3)")
}
if(missing.modalityID || missing.readerID) {
  return(message("Please specify the modality ID and reader ID for drawing curves."))
}

      AUC <- EAP_AA[md,qd]
      # chisquare <- chisquare[md,qd]
      message("\n-----------   To read the tables   ---------------------------------  \n")
      message("\n * The  AUC denoted by AA[modalityID , readerID] are shown in the above table.\n")
      message("\n * The column of 2.5% and 97.5% means the lower and upper bounds of the 95% Credible Interval of AUCs.\n")

      message("\n * For example, AA[2,3] means the AUC of the 2 nd modality and the 3 rd reader.  \n")
      message("\n-------------------------------------------------  \n")
      message("\n * Especially, your specified pair of modality and reader is as follows:    \n")

      message("\n--------------------------------------------------  \n")
      message(" * The FROC and AFROC curves for the following: \n \n ")
      message("    ModalityID:", md," \n")
      message("    ReaderID  :", qd," \n")
      message( " \n")
      message(" * The expected a posterior estimate of the area under the AFROC curves with respect to above specified modality (",md,") and reader (",qd,") : \n \n ")
      message("     AUC=", AUC," \n")
      # message("The goodness of fit chi-square statistic: \n \n ")
      # message("     chi-square=", chisquare," \n")
      message("\n * Note that this AUC is denoted by AA[",md,"," , qd,"]  for  modality (",md,") and reader (",qd,") in the fitted model object of class stanfit (more precisely inheritied class from stanfit, called stanfitExtended).\n")
      message("--------------------------------------------------  \n")


      #par(new=T);plot(mean(ff), mean(hh),bg = "black",fg = "blue",bty = "o",
      #              cex=6,xlim = c(0,2),ylim = c(0,1.5),col = 'blue', xlab = '', ylab = '')
      #colors()

      #MM<-M+1

    }}
  explanation_for_what_curves_are_drawn(  modalityID = modalityID,
                                          readerID   = readerID)
}
