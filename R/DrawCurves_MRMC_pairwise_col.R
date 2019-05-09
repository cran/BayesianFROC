#' @title    Draw the FROC  curves with Colour
#'@description     Draw an FROC  curves and an AFROC curves for user's specified modality and user's specified reader.
#' Using this function \strong{repeatedly}, we can draw the different reader and modality in a  \strong{same} plane simultaneously.
#' So, we can visualize the difference of modality (reader).
#'
#'    --------   To read the tables in Stan S4 class  ----------------------------
#'
#'
#'   * The  AUC denoted by AA[modalityID , readerID] are shown.
#'
#'   * The column of 2.5\% and 97.5\% means the lower and upper bounds of the 95% Credible Interval of AUCs.
#'
#'   * For example, AA[2,3] means the AUC of the 2 nd modality and the 3 rd reader.
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves_MRMC_pairwise
#'@importFrom  grDevices dev.new


#' @export DrawCurves_MRMC_pairwise_col
#  devtools::document();help("DrawCurves_MRMC_pairwise_col")

DrawCurves_MRMC_pairwise_col<- function(
   StanS4class,
  modalityID,
  readerID,
  # mesh.for.drawing.curve=10000,
  new.imaging.device = TRUE,
  DrawFROCcurve=TRUE,
  DrawAFROCcurve=FALSE,
  DrawCFPCTP=TRUE,
  Draw.Flexible.upper_y=TRUE,
  Draw.Flexible.lower_y=TRUE
)
{






  fit <-StanS4class
  dataList <- fit@dataList
  M <-as.integer(dataList$M)
  Q <-as.integer(dataList$Q)

  if(missing(modalityID)){
    message("*\n WARNING:\n")
    message("\n* modalityID is missing, so we write the curve for the first modality only. If you want to write curves for, e.g., the first and the third modality, then it accomplishes by taking modalityID =c(1,3)")
    modalityID <-c(1)
  }

  if(missing(readerID)){
    message("*\n WARNING:\n")
    message("\n* readerID is missing, so we write the curve for the first readerID only. If you want to write curves for, e.g., the first and the third modality, then it accomplishes by taking readerID =c(1,3)")
    readerID <-c(1)
  }
  # library(base) #For stop()
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



  #AFROC  Draw ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #par(new = TRUE);plot(ffarrayN[,md,qd],hharrayN[,md,qd],
  #                    #           col = (md-1)*Q+(qd-1),
  #                    cex=1+(md-1)*Q+(qd-1), xlim = c(0,upper_x ), ylim = c(0,upper_y) ,xlab = '', ylab = '');





  md <- modalityID
  qd <- readerID

  Colour1 <-  array(0, dim=c( 20)) #array(0, dim=c( M))
  Colour2 <-  array(0, dim=c( M)) #
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


   ssss<-paste("",sep = "")
  for (md in sort( modalityID)){
    if(md==min(modalityID)){ ssss<-paste(ssss,md,sep = "i.e., ")}
    if(!md==min(modalityID)){ ssss<-paste(ssss," and ",md,sep = "")}
  }
mainlabel <-paste(" Each Number (",ssss,") in the scatter plot means the modality ID.")


 graphics::par(bg= "gray12", #"gray27",#"gray40",#"black",# ,
                fg="gray",
                col.lab="bisque2" ,#"bisque" ,#  "antiquewhite1",
                col.axis="bisque2" ,##"bisque" "antiquewhite1",
                col.main="bisque2" ,
                cex.lab=1.5,
                cex.axis=1.3
  )

  for (md in modalityID){
    for (qd in readerID){

      if(DrawAFROCcurve==TRUE){
        #AFROC

        suppressWarnings(graphics::par(new=TRUE));        plot(
          x,y[,md,qd],
          col =Colour1[md],
          cex= 0.1 ,
          xlim = c(0,upper_x ),ylim = c(lower_y,upper_y),
          xlab = 'mean of false positives per nodule',
          ylab = 'cumulative hit per nodule',
          main = mainlabel
        );

      }

      if(DrawFROCcurve==TRUE){

        #FROC
        suppressWarnings(graphics::par(new=TRUE)); plot(
          l,y[,md,qd],
          col =Colour1[md],
          bg="gray",
          fg="gray",
          xlab = 'mean of false positives per nodule',
          ylab = 'cumulative hit per nodule',
          cex= 0.1,
          xlim = c(0,upper_x ),
          ylim = c(lower_y,upper_y)
          ,main = mainlabel

        );
      }


      if(DrawCFPCTP==TRUE){
        #CFP-CTP points
        suppressWarnings(graphics::par(new=TRUE));plot(
          ffarrayN[,md,qd],hharrayN[,md,qd],
          xlim = c(0,upper_x ),
          ylim = c(lower_y,upper_y),
          bg="gray",
          fg="gray",
          col =Colour1[md],
          pch =paste(md),
          cex=1,# Size of Dots
          xlab = 'mean of false positives per nodule',
          ylab = 'cumulative hit per nodule',
          main = mainlabel

        )
      } #DrawCFPCTP==TRUE


      AUC <- EAP_AA[md,qd]
      # chisquare <- chisquare[md,qd]
      # print(StanS4class,pars=c("AA"))
      # message("\n-----------   To read the tables   ---------------------------------  \n")
      # message("\n * The  AUC denoted by AA[modalityID , readerID] are shown.\n")
      # message("\n * The column of 2.5% and 97.5% means the lower and upper bounds of the 95% Credible Interval of AUCs.\n")
      #
      # message("\n * For example, AA[2,3] means the AUC of the 2 nd modality and the 3 rd reader.  \n")
      # message("\n-------------------------------------------------  \n")
      # message("\n * Especially, your specified pair of modality and reader is as follows:    \n")

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
      message("\n * Note that this AUC is denoted by AA[",md,"," , qd,"]  for  modality (",md,") and reader (",qd,").\n")

    }}# For sentence

  #par(new=T);plot(mean(ff), mean(hh),bg = "black",fg = "blue",bty = "o",
  #              cex=6,xlim = c(0,2),ylim = c(0,1.5),col = 'blue', xlab = '', ylab = '')
  #colors()

  #MM<-M+1
  message("\n--------------------------------------------------  \n")
  message("\n * Note that these curves are estimated by \"only one\" hierarchical model, more precisely we do not fit these curves by non hierarchical model for each reader and each modality.\n")
  message("\n * In the plot, the AFROC curves emanate from origin (0,0) to (1,1).\n")
  message("\n * In the plot, the numbers of the scatter plot mean modaltiy numbers. So, same reader has different numbers according to modality even if reader is same. \n")

  explanation_for_what_curves_are_drawn(  modalityID = modalityID,
                                          readerID   = readerID)

}
