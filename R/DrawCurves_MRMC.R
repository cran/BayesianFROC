#' @title    Draw the FROC  curves for all modalities and readers
#'@description     Draw the FROC  curves and AFROC curves for all modalities and readers, if many  modalities and readers exists, then so very confused plots will be drawn.
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#' @export DrawCurves_MRMC
#'@author Issei Tsunoda
#' @examples
#'
#' \donttest{

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
#' # 2019.05.21 Revised.
#'
 #'}# dottest

#'
#  devtools::document()

DrawCurves_MRMC<- function(
                           StanS4class
                           )
{

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
upper_x <-max(ffarrayN)
upper_y <- 1.0
#AFROC  Draw ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for(md in 1:M){
for(qd in 1:Q){
    #par(new = TRUE);plot(ffarrayN[,md,qd],hharrayN[,md,qd],
    #                    #           col = (md-1)*Q+(qd-1),
    #                    cex=1+(md-1)*Q+(qd-1), xlim = c(0,upper_x ), ylim = c(0,upper_y) ,xlab = '', ylab = '');


#AFROC
  graphics::par(new = TRUE); plot(x,y[,md,qd],
                          #col = 1+(md-1)*Q+(qd-1)
                          #cex=md/10,
                          cex= 0.1 ,
                          xlim = c(0,upper_x ),ylim = c(0,upper_y),
                          xlab = 'mean of false positives per nodule',
                          ylab = 'cumulative hit per nodule'
    );
#FROC
  graphics::par(new = TRUE); plot(l,y[,md,qd],
                          #col =1+(md-1)*Q+(qd-1),
                          #cex=md/20,
                          xlab = 'mean of false positives per nodule',
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
