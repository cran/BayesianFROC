#' @title  Create metadata for MRMC data
#'
#'
#'@description  From data of number of hits and false alarms, we calculate the number of cumulative false positives and hits.
#' Since there are three subscripts, reader, modality, and image, we create array format and vector format etc...
#'

#'@inheritParams fit_Bayesian_FROC

#'@return A metadata such as number of cumulative false alarms and hits to create and draw the curve.
#'
#'
# devtools::document();help("metadata_to_DrawCurve_MRMC") # Confirm reflection
# data.MultiReaderMultiModality <-dataList.Chakra.Web
# devtools::use_data(data.MultiReaderMultiModality)
#' @export  metadata_to_DrawCurve_MRMC

#'@param StanS4class This is an output of \code{rstan::stan}.

metadata_to_DrawCurve_MRMC<- function(
                                      StanS4class,
                                      mesh.for.drawing.curve=5000)
{


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


  message(crayon::silver("\n* Metadata to draw the curves are callculating ... \n"))

  fit <-  methods::as(StanS4class, "stanfit")

  war <- fit@sim$warmup
  cha <- fit@sim$chains
  ite <- fit@sim$iter

  #Draw the  AFROC curve~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  MCMC=(ite-war)*cha  #Total Samples By MCMC

  ll<- stats::rchisq(mesh.for.drawing.curve, 1)
  lll<- 0.99+stats::rchisq(mesh.for.drawing.curve, 1)
  l<-append(ll,lll) #This name l is duplicated!! CAUTION!!
  x<- 1-exp(-l)   #AFROC-Curve
  y <-  array(0, dim=c(length(x),  M,Q)) #

  a<-rstan::extract(fit)$a #samples of a by MCMC
  b<-rstan::extract(fit)$b #samples of b by MCMC
  EAP_a <-  array(0, dim=c( M,Q)) #
  EAP_b <-  array(0, dim=c( M,Q)) #

  for(md in 1:M){
    for(qd in 1:Q){

      EAP_a[md,qd] <- 0
      EAP_b[md,qd] <- 0
      s<-0
      t<-0
      for(mc in 1:MCMC){ #EAP
        s<-  EAP_a[md,qd]
        EAP_a[md,qd] <-  s+ a[mc,md,qd]
        t<-  EAP_b[md,qd]
        EAP_b[md,qd] <-  t+ b[mc,md,qd]
      }
      EAP_a[md,qd] <-EAP_a[md,qd] /MCMC  #EAP of a
      EAP_b[md,qd] <-EAP_b[md,qd] /MCMC  #EAP of b
    }}

  # message("* Process  for calculating y coordinates of curves \n")#Processsssssss
  plotFROCdata  <- list()
  plotAFROCdata <- list()

  nameFROC  <- vector()
  nameAFROC  <- vector()

  #Vectorization   #Vectorization  #Vectorization   #Vectorization
  for(i in 1:length(x)){ #y-coord.of FROC and AFROC are same
    y[i,,]<-1-stats::pnorm(EAP_b *stats::qnorm(exp(-l[i]))-EAP_a )
  }
  #Vectorization   #Vectorization  #Vectorization   #Vectorization

  s <- 0
  for(qd in 1:Q){
    for(md in 1:M){
      s<-s+1
      # message("|")#Processsssssss


      plotFROCdata[[s]]  <- data.frame(x.coord = l, y.coord =y[,md,qd] )
      plotAFROCdata[[s]] <- data.frame(x.coord = x, y.coord =y[,md,qd] )

      names( plotFROCdata[[s]] ) <- c(paste("X.coord.for.FROC.curve.for.modality.",md,".and.reader.",qd,sep = ""),paste("Y.coord.for.FROC.curve.for.modality.",md,".and.reader.",qd,sep = ""))
      names( plotAFROCdata[[s]] ) <- c(paste("X.coord.for.AFROC.curve.for.modality.",md,".and.reader.",qd,sep = ""),paste("Y.coord.for.AFROC.curve.for.modality.",md,".and.reader.",qd,sep = ""))


       nameFROC[s] <- paste("FROC.data.for.modality.",md,".and.reader.",qd,sep = "")
      nameAFROC[s] <- paste("AFROC.data.for.modality.",md,".and.reader.",qd,sep = "")

      # names(list$plotAFROCdata)
       }
    # message(paste("", ceiling(round(qd/Q,2)*100/2),"% \n"))#Processsssssss

  }

  names(plotFROCdata) <-nameFROC
  names(plotAFROCdata) <-nameAFROC


  #-----------------------------
  #-- AUC for each modarlity and each reader


  AA<-rstan::extract(fit)$AA #samples of a by MCMC
  EAP_AA <-  array(0, dim=c( M,Q)) #

  for(qd in 1:Q){
    for(md in 1:M){
      EAP_AA[md,qd] <- mean(AA[,md,qd])
      # EAP_AA[md,qd] <- 0
      # s<-0
      # message("|")#Processsssssss
      # for(mc in 1:MCMC){ #EAP
      #   s<-  EAP_AA[md,qd]
      #   EAP_AA[md,qd] <-  s+ AA[mc,md,qd]
      # }
      # EAP_AA[md,qd] <-EAP_AA[md,qd] /MCMC  #EAP of a
    }#for md
    # message(paste("", ceiling(round(qd/Q,2)*100/2 +50),"% \n"))#Processsssssss
  }# for qd

  # #------------------------------------------
  #    #--------- chi ^2 -----------Start
  #    p<-rstan::extract(fit)$p
  #    lambda<-rstan::extract(fit)$l
  #    EAP_p <-  array(0, dim=c(   C,M,Q))
  #    EAP_l <-  array(0, dim=c(   C+1))
  #
  #    s <-  array(0, dim=c(   C,M,Q))
  #    t <-  array(0, dim=c(   C+1))
  #    for(mc in 1:MCMC){
  #      for(cd in 1:C){
  #        for(md in 1:M){
  #          for(qd in 1:Q){
  #        s[ cd,md,qd]<-  EAP_p[ cd,md,qd]
  #
  #        EAP_p[ cd,md,qd] <-  s[ cd,md,qd]+ p[mc,cd,md,qd]
  #      }
  #      for(cd in 0:C){
  #        t[ cd]<-  EAP_l[ cd]
  #        EAP_l[ cd] <-  t[ cd]+ lambda[mc,cd]
  #      }
  #    }}}
  #    EAP_p<-EAP_p/MCMC
  #    EAP_l<-EAP_l/MCMC
  #
  #    ss<-array(0, dim=c(   M,Q))
  #    tt<-array(0, dim=c(   M,Q))
  #    for(cd in 1:C){
  #      for(md in 1:M){
  #        for(qd in 1:Q){
  #      ss[md,qd]<-(hharray[C+1-cd,md,qd]-NL*EAP_p[cd,md,qd])^2/(NL*EAP_p[cd,md,qd])
  #      ## tt<-(ffarray[C+1-cd,md,qd]-NI*(EAP_l[cd ]-EAP_l[cd+1 ]))^2/(NI*(EAP_l[cd ]-EAP_l[cd+1 ]))
  #      tt[md,qd]<-(ffarray[C+1-cd,md,qd]-NL*(EAP_l[cd ]-EAP_l[cd+1 ]))^2/(NL*(EAP_l[cd ]-EAP_l[cd+1 ]))
  #
  #       }}}
  #    chisquare <- ss+tt
  #    chisquare
  #


  invisible(list(x=x,y=y,l=l,EAP_AA=EAP_AA ,plotFROCdata=plotFROCdata,plotAFROCdata=plotAFROCdata))

}




