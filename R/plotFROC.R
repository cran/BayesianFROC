
#' @title  Draw FROC curves by two parameters a and b
#'@inheritParams DrawCurves_MRMC_pairwise
#'@inheritParams fit_Bayesian_FROC
#'@description Plot FROC curves based on two parameters a and b.
#'@details
#'FROC curve is the alternative notion of ROC curve in signal detection theory.
#'
#'
#'The definition of FROC curve is
#'
#'   \deqn{(x(t),y(t) ) = (t,   1 - \Phi(  b* \Phi^{-1}(exp(-t)) -a  )  ) }
#'
#'
#'where, \eqn{\Phi()} is the cumulative distribution
#'function of the standard Gaussian distribution and
#'\eqn{\Phi^{-1}()} is its inverse mapping.
#'
#'Revised 2019 NOv 27
#'
#'
#'@inheritParams ggplotFROC.EAP

#' @param upper_x A positive real number, indicating the frame size of drawing picture.
#' @param upper_y A positive real number, indicating the frame size of drawing picture.
#' @param lower_y A positive real number, indicating the frame size of drawing picture.
#' @export plotFROC
#'
#' @examples
#'
#' dark_theme()
#'
#' plotFROC(0.1,0.2)
#'

plotFROC <-function(a,b,mesh.for.drawing.curve=10000,
                    upper_x=1,upper_y=1,lower_y=0){


  if(missing(a)){
    message("\n* WARNING:\n")
    message("\n* a is missing, so we write the curve for a = 0.13")
    a <-0.13
warning("* a is missing, so we write the curve for a = 0.13")
  }

  if(missing(b)){
    message("\n* WARNING:\n")
    message("\n* b is missing, so we write the curve for b = 0.19")
   b <-0.19
    warning("* b is missing, so we write the curve for b = 0.19")
  }

  ll<- stats::rchisq(mesh.for.drawing.curve, 1)
  lll<- 0.99+stats::rchisq(mesh.for.drawing.curve, 1)
  l<-append(ll,lll) #This name l is duplicated!! CAUTION!!
  y <- 1 - stats::pnorm(  b*stats::qnorm(exp(-l)) -a  )
  plot(l,y, cex= 0.1 ,col = grDevices::gray(0.8),

       xlim = c(0,upper_x ),ylim = c(lower_y,upper_y)
)
}



#' @title  Draw FROC curves by two parameters a and b
#'@inheritParams DrawCurves_MRMC_pairwise
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams plotFROC
#'@inheritParams ggplotFROC.EAP

#'@description Plot FROC curves based on two parameters a and b.

#' @param upper_x A positive real number, indicating the frame size of drawing picture.
#' @param upper_y A positive real number, indicating the frame size of drawing picture.
#' @param lower_y A positive real number, indicating the frame size of drawing picture.
#' @export ggplotFROC
ggplotFROC<-function(a,b,mesh.for.drawing.curve=10000,
                       upper_x=1,upper_y=1,lower_y=0,
                     dataList,StanS4class
                     ){
  # if(missing(upper_x)) {upper_x <-max(ffarrayN)}
  # if(missing(upper_y)) {upper_y <-max(hharrayN)}
  # if(missing(lower_y)) {upper_x <-min(hharrayN)}

  ll<- stats::rchisq(mesh.for.drawing.curve, 1)
  lll<- 0.99+stats::rchisq(mesh.for.drawing.curve, 1)
  l<-append(ll,lll) #This name l is duplicated!! CAUTION!!

  y <- 1 - stats::pnorm(  b*stats::qnorm(exp(-l)) -a  )
  data <-data.frame(l=l,y=y)


  g1<-ggplot2::ggplot(data=data,
                      ggplot2::aes(x=l,y=y))
  g2<-g1+ggplot2::geom_point(ggplot2::aes(colour=md),
                    size=1,
                    alpha=0.5)+
    ggplot2::ylim(lower_y,upper_y)+
    ggplot2::xlim(0,upper_x)+
    ggplot2::theme_dark()+
    ggplot2::theme(text = ggplot2::element_text(family = "YuGo-Medium"),
    plot.background = ggplot2::element_rect(fill = "black"))+
    ggplot2::annotate("text", x = 50:50, y = 22,
               colour = c("#000000"),
               label = c(paste("Modality= ", 3)),
               parse = TRUE,size = 8, family = "Helvetica")

  print(g2)






  if(!missing(dataList)){
    metadata <- metadata_to_fit_MRMC(dataList)
    hharrayN<-metadata$hharrayN;
    ffarrayN<-metadata$ffarrayN;
    hhN<-metadata$hhN;
    ffN<-metadata$ffN;
    NL<- dataList$NL


    c<- dataList$c
    m<- dataList$m
    q<- dataList$q
    C<- as.integer(dataList$C)
    M<- as.integer(dataList$M)
    Q<- as.integer(dataList$Q)
    if(!missing(StanS4class)){
fit <-StanS4class
      war <- fit@sim$warmup
      cha <- fit@sim$chains
      ite <- fit@sim$iter
      MCMC=(ite-war)*cha  #Total Samples By MCMC
      ppp<-rstan::extract(fit)$ppp #samples of a by MCMC
      l<-rstan::extract(fit)$l #samples of b by MCMC
      EAP_ppp <-  array(0, dim=c( C,M,Q)) #
      EAP_l <-  array(0, dim=c( C)) #

      message("* Process \n")#Processsssssss
      for(cd in 1:C){
        for(md in 1:M){
          for(qd in 1:Q){
        EAP_ppp[cd,md,qd] <- 0
        EAP_l[cd] <- 0
        s<-0
        t<-0
        message("|")#Processsssssss

        for(mc in 1:MCMC){ #EAP
          s<-   EAP_ppp[cd,md,qd]
          EAP_ppp[cd,md,qd] <-  s+ ppp[mc,cd,md,qd]
          t<-  EAP_l[cd]
          EAP_l[cd] <-  t+ l[mc,cd]
        }
        EAP_ppp[cd,md,qd] <- EAP_ppp[cd,md,qd] /MCMC  #EAP of a
        EAP_l[cd] <-EAP_l[cd] /MCMC  #EAP of b
          }}#for md,qd
 message(paste("", round(cd/C,2)*100,"% \n"))#Processsssssss
        }#for cd 1:C

      EAP.ppp <-  array(0, dim=c( C*M*Q))
      EAP.l <-  array(0, dim=c( C*M*Q))


      for(cd in 1:C){for(md in 1:M){for(qd in 1:Q){
        EAP.ppp[cd+(md-1)*C*Q+(qd-1)*C]<-EAP_ppp[cd,md,qd]
        EAP.l[cd+(md-1)*C*Q+(qd-1)*C]<-EAP_l[cd]
         }}}
 }# if missing



 plotdata <- data.frame(#Cumulative.mean.f=Cumulative.mean.f,Cumulative.mean.h=Cumulative.mean.h,Cumulative.var.f=Cumulative.var.f,Cumulative.var.h=Cumulative.var.h,
   EAP.ppp=EAP.ppp,ffarrayN=ffarrayN ,hharrayN=hharrayN,hhN=hhN,ffN=ffN,c=c,m=m,q=q)

      gg1<-ggplot2::ggplot(plotdata,
                           ggplot2::aes(x=ffN,
                      y=hhN
                  ))+

        ggplot2::geom_errorbar(ggplot2::aes(
          ymin=hhN-EAP.ppp,
          ymax=hhN+EAP.ppp  ),
          colour=m,
          alpha=0.5,
          size=1)

      gg1<-gg1+ggplot2::geom_point(ggplot2::aes(colour=m),
                          size=1,
                          alpha=0.9)+
        ggplot2::theme_dark()+
        ggplot2::theme(text = ggplot2::element_text(family = "YuGo-Medium"),
                       plot.background = ggplot2::element_rect(fill = "black"))
       print(gg1)






  }
}














#' @title  Draw FROC curves by two parameters a and b
#'@inheritParams DrawCurves_MRMC_pairwise
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams plotFROC

#'@description Plot FROC curves based on two parameters a and b.
#' @param a An arbitrary real number.
#'  It is no need to require any assumption,
#'  but I use such as \code{a}=\eqn{\mu/\sigma},
#'  where \eqn{\mu} is a mean of signal distribution and \eqn{\sigma} is its standard deviation in the bi-normal assumption.
#' @param b An arbitrary positive real number.
#'  I use such as \code{b}=\eqn{1/\sigma},
#'  where \eqn{\sigma} is a standard deviation of signal distribution in the bi-noraml assumption.
#' @param upper_x A positive real number, indicating the frame size of drawing picture.
#' @param upper_y A positive real number, indicating the frame size of drawing picture.
#' @param lower_y A positive real number, indicating the frame size of drawing picture.
#' @export ggplotFROC.EAP
#'
ggplotFROC.EAP<-function(a,b,mesh.for.drawing.curve=10000,
                     upper_x=1,upper_y=1,lower_y=0,
                     dataList,StanS4class
){

  ll<- stats::rchisq(mesh.for.drawing.curve, 1)
  lll<- 0.99+stats::rchisq(mesh.for.drawing.curve, 1)
  l<-append(ll,lll) #This name l is duplicated!! CAUTION!!

  y <- 1 - stats::pnorm(  b*stats::qnorm(exp(-l)) -a  )
  if(!missing(dataList)){
    metadata <- metadata_to_fit_MRMC(dataList)
    hharrayN<-metadata$hharrayN;
    ffarrayN<-metadata$ffarrayN;
    hhN<-metadata$hhN;
    ffN<-metadata$ffN;
    NL<- dataList$NL


    c<- dataList$c
    m<- dataList$m
    q<- dataList$q
    C<- as.integer(dataList$C)
    M<- as.integer(dataList$M)
    Q<- as.integer(dataList$Q)
    if(!missing(StanS4class)){
fit<-StanS4class
      war <- fit@sim$warmup
      cha <- fit@sim$chains
      ite <- fit@sim$iter
      MCMC=(ite-war)*cha  #Total Samples By MCMC
      ppp<-rstan::extract(fit)$ppp #samples of a by MCMC
      l<-rstan::extract(fit)$l #samples of b by MCMC
      EAP_ppp <-  array(0, dim=c( C,M,Q)) #
      EAP_l <-  array(0, dim=c( C)) #

      message("* Process \n")#Processsssssss
      for(cd in 1:C){
        for(md in 1:M){
          for(qd in 1:Q){
            EAP_ppp[cd,md,qd] <- 0
            EAP_l[cd] <- 0
            s<-0
            t<-0
            message("|")#Processsssssss

            for(mc in 1:MCMC){ #EAP
              s<-   EAP_ppp[cd,md,qd]
              EAP_ppp[cd,md,qd] <-  s+ ppp[mc,cd,md,qd]
              t<-  EAP_l[cd]
              EAP_l[cd] <-  t+ l[mc,cd]
            }
            EAP_ppp[cd,md,qd] <- EAP_ppp[cd,md,qd] /MCMC  #EAP of a
            EAP_l[cd] <-EAP_l[cd] /MCMC  #EAP of b
          }}#for md,qd
        message(paste("", round(cd/C,2)*100,"% \n"))#Processsssssss
      }#for cd 1:C

      EAP.ppp <-  array(0, dim=c( C*M*Q))
      EAP.l <-  array(0, dim=c( C*M*Q))


      for(cd in 1:C){for(md in 1:M){for(qd in 1:Q){
        EAP.ppp[cd+(md-1)*C*Q+(qd-1)*C]<-EAP_ppp[cd,md,qd]
        EAP.l[cd+(md-1)*C*Q+(qd-1)*C]<-EAP_l[cd]
      }}}
    }# if missing
  }# if missing










  mean.h<-  array(0, dim=c( C*M*Q))
  var.h <-  array(0, dim=c( C*M*Q))
  mean.f<-  array(0, dim=c( C*M*Q))
  var.f <-  array(0, dim=c( C*M*Q))
  mean.h<-  EAP.ppp*NL
  var.h <-  EAP.ppp*NL*(1-EAP.ppp)
  mean.f<-  EAP.l
  var.f <-  EAP.l


  Cumulative.mean.h<-  array(0, dim=c( C*M*Q))
  Cumulative.var.h <-   array(0, dim=c( C*M*Q))
  Cumulative.mean.f<-   array(0, dim=c( C*M*Q))
  Cumulative.var.f <-  array(0, dim=c( C*M*Q))

  for(md in 1:M) {
    for(cd in 1:C) {
      for(qd in 1 : Q){
        for(n  in 1:cd){
          Cumulative.mean.f[cd+(md-1)*C*Q+(qd-1)*C]<-mean.f[cd+(md-1)*C*Q+(qd-1)*C]+mean.f[n+(md-1)*C*Q+(qd-1)*C]
          Cumulative.mean.h[cd+(md-1)*C*Q+(qd-1)*C]<-mean.h[cd+(md-1)*C*Q+(qd-1)*C]+mean.h[n+(md-1)*C*Q+(qd-1)*C]

          Cumulative.var.f[cd+(md-1)*C*Q+(qd-1)*C]<- var.f[cd+(md-1)*C*Q+(qd-1)*C]+var.f[n+(md-1)*C*Q+(qd-1)*C]
          Cumulative.var.h[cd+(md-1)*C*Q+(qd-1)*C]<- var.h[cd+(md-1)*C*Q+(qd-1)*C]+var.h[n+(md-1)*C*Q+(qd-1)*C]

        }}}}




  plotdata <- data.frame(Cumulative.mean.f=Cumulative.mean.f,Cumulative.mean.h=Cumulative.mean.h,Cumulative.var.f=Cumulative.var.f,Cumulative.var.h=Cumulative.var.h, ffarrayN=ffarrayN ,hharrayN=hharrayN,hhN=hhN,ffN=ffN,c=c,m=m,q=q)

  for (md in M){  for(qd in Q){
    gg3<-ggplot2::ggplot(plotdata,
                         ggplot2::aes(x=mean.f,
                    y=mean.h
                ))+

      ggplot2::geom_errorbar(ggplot2::aes(
        ymin=Cumulative.mean.h-Cumulative.var.h,
        ymax=Cumulative.mean.h+Cumulative.var.h  ),
        colour=m,
        alpha=0.5,
        size=1)

    gg3<-gg3+ggplot2::geom_point(ggplot2::aes(colour=m),
                        size=1,
                        alpha=0.9)+
      ggplot2::theme_dark()+
      ggplot2::theme(text = ggplot2::element_text(family = "YuGo-Medium"),
                     plot.background = ggplot2::element_rect(fill = "black"))
     print(gg3)
  }}



}#ggplotFROC.EAP









#  @title  Draw FROC curves by two parameters a and b
#  @inheritParams DrawCurves_MRMC_pairwise
#
#  @description Plot FROC curves based on 95% credible interval of two parameters a and b.
#  @export plotFROCcredibleInterval
# plotFROCcredibleInterval <-function(StanS4class,mesh.for.drawing.curve=10000,upper_x=1,upper_y=1,lower_y=0){
#
#   fit <-StanS4class
#   a.low <- as.data.frame(summary(fit)[[1]])["a","2.5%"]
#   a.hight <- as.data.frame(summary(fit)[[1]])["a","97.5%"]
#
#   b.low <- as.data.frame(summary(fit)[[1]])["b","2.5%"]
#   b.hight <- as.data.frame(summary(fit)[[1]])["b","97.5%"]
#   graphics::par(new = TRUE);
#   plotFROC(a.low,b.low)
#   graphics::par(new = TRUE);
#   plotFROC(a.hight,b.hight)
#
#   graphics::par(new = TRUE);
#   plotFROC(a.low,b.hight)
#   graphics::par(new = TRUE);
#   plotFROC(a.hight,b.low)
# }




#' @title  Draw FROC curves which means credible interval.
#'@inheritParams DrawCurves_MRMC_pairwise
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams plotFROC
#'@param StanS4class.fit_MRMC_versionTWO A return value of \code{fit_MRMC_versionTWO}.
#'@description Plot FROC curves based on two parameters a and b.

#' @export
Credible_Interval_for_curve <-function(dataList,
                                       StanS4class.fit_MRMC_versionTWO,
                                       mesh.for.drawing.curve=10000,
                                       upper_x=upper_x,
                                       upper_y=upper_y,
                                       lower_y=lower_y


                                       ){
  message("\n  Please wait... for credible curves...  \n")

fit <- StanS4class.fit_MRMC_versionTWO

if(missing(upper_x)||missing(lower_y)||missing(upper_y)){
  data <- metadata_to_fit_MRMC(dataList)
  hharrayN<-data$hharrayN;    ffarrayN<-data$ffarrayN;

  upper_x <-max(ffarrayN)
  upper_y <- max(hharrayN)
  lower_y <- min(hharrayN)
}




  #### Credible curves ##############################
  M <-dataList$M
  Q <-dataList$Q

  ll <-     stats::rchisq(mesh.for.drawing.curve, 1)
  lll<- 0.99+stats::rchisq(mesh.for.drawing.curve, 1)
  l<-append(ll,lll) #This name l is duplicated!! CAUTION!!

  war <- fit@sim$warmup
  cha <- fit@sim$chains
  ite <- fit@sim$iter

  MCMC=(ite-war)*cha  #Total Samples By MCMC

  a<-rstan::extract(fit)$a #samples of a by MCMC
  b<-rstan::extract(fit)$b #samples of b by MCMC
  yyy <-  array(0, dim=c(MCMC,length(l),  M)) #
  var.yy <-  array(0, dim=c(length(l),  M)) #

  #pb <- txtProgressBar(min = 1, max = length(l), style = 3)
for (md in 1:M) {
  for (ld in 1:length(l)) {     #utils::setTxtProgressBar(pb, md-1+ld)
    yyy[,ld,md]<- 1 - stats::pnorm(  b[,md] *stats::qnorm(exp(-l[ld])) -a[,md]  )
    var.yy[ld,md] <-stats::var(yyy[,ld,md])
  }
}


  y <-  array(0, dim=c(length(l),  M)) #
  y.lower <-  array(0, dim=c(length(l),  M)) #
  y.hight <-  array(0, dim=c(length(l),  M)) #

  EAP_a <-  array(0, dim=c( M)) #
  EAP_b <-  array(0, dim=c( M)) #


  for(md in 1:M){
      EAP_a[md] <- 0
      EAP_b[md] <- 0
      s<-0
      t<-0

      for(mc in 1:MCMC){ #EAP
        s<-  EAP_a[md]
        EAP_a[md] <-  s+ a[mc,md]
        t<-  EAP_b[md]
        EAP_b[md] <-  t+ b[mc,md]
      }
      EAP_a[md] <-EAP_a[md] /MCMC  #EAP of a
      EAP_b[md] <-EAP_b[md] /MCMC  #EAP of b
    }



    for(md in 1:M){
      for(ld in 1:length(l)){
        y[ld,md]<-1-stats::pnorm(EAP_b[md] *stats::qnorm(exp(-l[ld]))-EAP_a[md] )

        y.lower[ld,md] <- y[ld,md] - var.yy[ld,md]
        y.hight[ld,md] <- y[ld,md] + var.yy[ld,md]
        }}




 # invisible(list(x.axis.FROC = l, y.lower=y.lower,y.hight=y.hight ))

  for (md in 1:M) {


  graphics::par(new = TRUE);plot(l,y.lower[,md], cex= 0.1 ,
         col = grDevices::gray(0.8),
       xlim = c(0,upper_x ),
       ylim = c(lower_y,upper_y),
       xlab = '', ylab = ''

       )


  graphics::par(new = TRUE);plot(l,y.hight[,md], cex= 0.1 ,
                                 col = grDevices::gray(0.8),
                                 xlim = c(0,upper_x ),
                                 ylim = c(lower_y,upper_y),
                                 xlab = '', ylab = ''

  )



}#  for (md in 1:M)
}
