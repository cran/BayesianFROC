#' @title  Create metadata for MRMC data.
#'
#'@description
#' The so-called \emph{false positive fraction (FPF)}
#' and the       \emph{true  positive fraction (TPF)}
#' are calculated from the number of hits (True Positives: TPs)
#' and the number of false alarms (False Positives: FPs)
#'
#'
#'
#'
#'
#'
#'
#'@details
#'  From data of number of hits (True Positive: TP)
#' and false alarms (False Positive: FP), we calculate the number
#' of cumulative false positives (FPF) and cumulative hits (TPF).
#'
#' Because there are three subscripts,
#'  reader, modality, and image,
#'  we create array format and vector format etc...
#'
#'@param dataList A list, should include
#'  \code{m,q,c,h,f,NL,C,M,Q} which means
#'
#'\code{c}  should be
#'created by \code{  c <-c(rep(C:1))},
#' where \code{C} is the number of confidence levels.
#'So, you should write down your hits
#'and false alarms vector so that it
#'is compatible with this
#' automatically created \code{c} vector.

#'
#'\code{h} means the number of hits
#'
#'\code{f} means the number of false alarm
#'
#'\code{NL} means the Total number of lesions for all images
#'
#'\code{C} means the highest number of confidence level




#'
#'@return A metadata such as number
#' of cumulative false alarms and
#'  hits to create and draw the curve.
#'
#'@examples
#' \dontrun{
#'#========================================================================================
#'#                      TP and FP
#'#========================================================================================
#'
#'
#'         dat  <- BayesianFROC::dataList.Chakra.Web
#'
#'
#'
#'#========================================================================================
#'#              Calculates  TPF and FPF from TP and FP
#'#========================================================================================
#'
#'
#'              metadata_srsc_per_image(dat)
#'
#'
#'
#'
#'
#'
#' # Revised 2019 Nov.
#'
#'
#'}# dottest
#' @export  metadata_srsc_per_image
#'@inheritParams fit_Bayesian_FROC

metadata_srsc_per_image<- function(dataList,ModifiedPoisson)
{
  if ( length(dataList)>=7)return(message("srsc Only. Your data is not srsc."))# This cause error in ReadMe, so It is better to omit this line. 2019 Sept

  h <- dataList$h
  f <- dataList$f
  NI <- dataList$NI
  NL <- dataList$NL
  C  <- as.integer(dataList$C)

if(ModifiedPoisson)NX <- NL
if(!ModifiedPoisson)NX <- NI


  # C <-length(h)
  c <-c(rep(C:1))






  if (    (length(h) >length(f))
          || (length(h) <length(f))
  ) {  return(message("Format error:\nIn your data, true positive and false positives are not same length.\n"))
  } else
    if ( (sum(h) >NL) )
    {return(message("Format error:\nIn your data, number of true positives are greater than that of lesions.\n"))
    } else

      N <- length(f)
  # hh <- numeric(N) #cumulative fraction
  # ff <- numeric(N) #cumulative fraction
  # for(cd in 1:C) {
  #   for(n in 1:cd) {
  #     hh[cd]<-hh[cd]+h[n]/NL #cumulative fraction  to examine  the fitness of  FROC
  #     ff[cd]<-ff[cd]+f[n]/NI #cumulative fraction  to examine  the fitness of  FROC
  #   }}
  # fff <- numeric(N)
  # for(cd in 1:C) {
  #   for(n in 1:cd) {
  #     fff[cd]<-fff[cd]+f[n] #cumulative fraction  to examine  the fitness of  FROC
  #   }}



  fff <- cumsum(f)
  hh <- cumsum(h)/NL
  ff <- fff/NX


  data <- list( N=N,NL=NL,NI=NI,C=as.integer(C),c=c,
                h=h,f=f,
                hh=hh,ff=ff,
                fff=fff)

  invisible(data)
}
































# title -----
#' @title  Create metadata for MRMC data
#'
#'
#'@description
#' The so-called \emph{false positive fraction (FPF)}
#' and the       \emph{true  positive fraction (TPF)}
#' are calculated from the number of hits (True Positives: TPs)
#' and the number of false alarms (False Positives: FPs)
#'
#'


#'

#'
#'
#'
#'
#'
#'

#'@details To fit a model to data,
#' we need a hit data and false data
#' formulated by both an array and a vector.
#'
#' It also calculates the so-called
#'   False Positive Fractions (FPF)
#' (resp. True Positive Fractions (TPF) )
#'  which are cumulative sums of false alarms (resp.
#'   hits)  over number of lesions or images.
#'
#'  From data of number of hits
#'   and false alarms,
#'    we calculate the number
#'     of cumulative false positives
#'     and hits per image or lesion, in other words,
#'     \emph{False Positive Fraction (FPF)}
#'     and  \emph{True Positive Fraction (TPF)}.
#' Since there are three subscripts,
#' \emph{reader}, \emph{modality},
#'  and \emph{image}, we can create array format
#'  or vector format etc...
#'

#' \strong{Abbreviations}
#'
#' \emph{FPF: false positive fraction }
#'
#' \emph{TPF: true positive fraction }
#'
#' \emph{hit : True Positive = TP}
#'
#' \emph{false alarms: False Positive = FP}
#'
#'
#'
#'
#'
#'
#'  The traditionaly, the so-called FPF;\emph{False Positive Fraction} and TPT:\emph{True Positive Fraction} are used.
#'  Recall that our data format:
#'
#'
#'
#'
#'  \emph{            A single reader and a single modality case   }
#'
#'------------------------------------------------------------------------------------------------------

#' \tabular{llll}{
#' \code{NI, NL }   \tab \strong{ confidence level } \tab \strong{ No. of false alarms} \tab \strong{No. of hits}  \cr
#'            \tab    \tab   (FP:False Positive)  \tab    (TP:True Positive) \cr
#'     -----------------------\tab ----------------------- \tab ----------------------------- \tab ------------- \cr
#' \emph{definitely} present  \tab   5 \tab  \eqn{F_5}   \tab   \eqn{H_5}  \cr
#'  \emph{probably} present   \tab   4 \tab  \eqn{F_4}  \tab   \eqn{H_4}   \cr
#'  equivocal                 \tab   3 \tab  \eqn{F_3}  \tab   \eqn{H_3}    \cr
#'  subtle                    \tab   2 \tab  \eqn{F_2}  \tab  \eqn{H_2}    \cr
#'  \emph{very} subtle        \tab   1 \tab  \eqn{F_1}  \tab \eqn{H_1}    \cr
#'  }
#'
#'---------------------------------------------------------------------------------------------------
#'
#' FPF is defined as follows;
#'
#'
#' \deqn{FPF(5):= \frac{F_5}{NI},}
#' \deqn{FPF(4):= \frac{F_4+F_5}{NI},}
#' \deqn{FPF(3):= \frac{F_3+F_4+F_5}{NI},}
#' \deqn{FPF(2):= \frac{F_2+F_3+F_4+F_5}{NI},}
#' \deqn{FPF(1):= \frac{F_1+F_2+F_3+F_4+F_5}{NI}.}
#'
#'
#' TPF is defined as follows;
#'
#'
#' \deqn{TPF(5):= \frac{H_5}{NL},}
#' \deqn{TPF(4):= \frac{H_4+H_5}{NL},}
#' \deqn{TPF(3):= \frac{H_3+H_4+H_5}{NL},}
#' \deqn{TPF(2):= \frac{H_2+H_3+H_4+H_5}{NL},}
#' \deqn{TPF(1):= \frac{H_1+H_2+H_3+H_4+H_5}{NL}.}
#'
#'
#'
#'
#'
#'
#'
#'


#'@param dataList A list, consisting of the following \R objects:\code{m,q,c,h,f,NL,C,M,Q} each of which means from the right
#'
#'\code{m } : A vector, indicating the modality ID = 1,2,... which does not include zero.
#'
#'\code{q } : A vector, indicating the reader ID = 1,2,... which does not include zero.
#'
#'\code{c }  : A vector, indicating the confidence = 1,2,... which does not include zero.
#'
#'\code{h }  : A vector, indicating the number of hits
#'
#'\code{f }  : A vector, indicating the number of false alarm
#'
#'\code{NL }  : An positive integer, indicating the number of lesions for all images
#'
#'\code{C } : An positive integer, indicating the highest number of confidence level
#'
#'\code{M } : An positive integer, indicating the number of modalities
#'
#'\code{Q } : An positive integer, indicating the number of readers.
#'
#'The detail of these dataset, please see the example datasets, e.g. \code{\link{dd}}.


# @return-----
#'@return A list, which includes arrays and vectors.
#'A metadata such as number of cumulative false alarms
#' and hits to create and draw the curve.
#'
#' The
#'  \emph{False Positive Fraction (FPF)} and
#'   \emph{True Positive Fraction (TPF)} are also calculated.
#'
#'\strong{ The components of list} \emph{ I rediscover it  at 2019 Jun 18, I am not sure it is useful? 2019 Dec 8}

#' \describe{
#'\item{ \code{ harray}     }{An array of hit, dimension \code{ [C,M,Q]}, where \code{C,M,Q} are a number of confidence level, modalities, readers, respectively.  }
#'\item{ \code{ farray}     }{An array of false alarms, dimension \code{ [C,M,Q]}, where \code{C,M,Q} are a number of confidence level, modalities, readers, respectively.  }
#'\item{ \code{ hharray}    }{An array of \strong{cumulative} hits, dimension \code{ [C,M,Q]}, where \code{C,M,Q} are a number of confidence level, modalities, readers, respectively.  }
#'\item{ \code{ ffarray}    }{An array of \strong{cumulative} false alarms, dimension \code{ [C,M,Q]}, where \code{C,M,Q} are a number of confidence level, modalities, readers, respectively.  }
#'\item{ \code{ hharrayN}   }{An array of TPF, dimension \code{ [C,M,Q]}, where \code{C,M,Q} are a number of confidence level, modalities, readers, respectively.  }
#'\item{ \code{ ffarrayN}   }{An array of FPF, dimension \code{ [C,M,Q]}, where \code{C,M,Q} are a number of confidence level, modalities, readers, respectively.  }
#'\item{ \code{ h}          }{An vector of hit, dimension \code{ [C*M*Q]}, where \code{C,M,Q} are a number of confidence level, modalities, readers, respectively.  }
#'\item{ \code{ f }         }{An vector of false alarms, dimension \code{ [C*M*Q]}, where \code{C,M,Q} are a number of confidence level, modalities, readers, respectively.  }
#'\item{ \code{ hh }        }{An vector of \strong{cumulative} hits, dimension \code{ [C*M*Q]}, where \code{C,M,Q} are a number of confidence level, modalities, readers, respectively.  }
#'\item{ \code{ ff }        }{An vector of \strong{cumulative} false alarms, dimension \code{ [C*M*Q]}, where \code{C,M,Q} are a number of confidence level, modalities, readers, respectively.  }
#'\item{ \code{ hhN}        }{An vector of TPF, dimension \code{ [C*M*Q]}, where \code{C,M,Q} are a number of confidence level, modalities, readers, respectively.  }
#'\item{ \code{ ffN}        }{An vector of FPF, dimension \code{ [C*M*Q]}, where \code{C,M,Q} are a number of confidence level, modalities, readers, respectively.  }
#'}
#'
#'
#' Revised Nov. 21

#'
#'
#'
#'@examples
#'#========================================================================================
#'#                     First, we prepare the data endowed with this package.
#'#========================================================================================
#'

#'
#'
#'              dat  <- get(data("dataList.Chakra.Web"))
#'
#'
#'#========================================================================================
#'#                           #Calculate FPFs and TPFs and etc.
#'#========================================================================================
#'
#'
#'
#'                              a <- metadata_to_fit_MRMC(dat)
#'
#'
#' #Now, we get  a meta-data object named "a".
#'
#'
#'#========================================================================================
#'#                                 Check of Definiion
#'#========================================================================================
#'
#'
#'                                  a$hh/dat$NL == a$hhN
#'
#'# Since all of aboves are TRUE, the hhN is a TPF per NL.
#'
#'
#'
#'
#'#========================================================================================
#'#                             Plot a FPFs and TPFs
#'#========================================================================================
#'#'
#'
#'
#'
#'                                  FPF = a$ffN
#'                                  TPF = a$hhN
#'
#'                                dark_theme()
#'                                plot(FPF,TPF)
#'
#'#========================================================================================
#'#                             Plot a FPFs and TPFs via ggplot
#'#========================================================================================
#'
#'                        length(dat$f)==length(FPF)
#'
#'             q  <- dat$q
#'             m  <- dat$m
#'             df <- data.frame(FPF,
#'                              TPF,
#'                              m,
#'                              q
#'                              )
#'
#'# ggplot2::ggplot(df, aes(x =FPF, y = TPF, colour = q, group = m)) + ggplot2::geom_point()
#'
#'# Revised 2019 Jun 18, Revised 2019 Sept 9
#'
#'
#'
#'
#'
# devtools::document();help("metadata_MRMC") # Confirm reflection
# devtools::use_data(dataList.high.ability)
#' @export  metadata_to_fit_MRMC
#'@inheritParams fit_Bayesian_FROC


metadata_to_fit_MRMC<- function(dataList,ModifiedPoisson=FALSE)
{
  # message("\n")
  # message("* Now, we calculated the metadata, e.g., cumulative hits and false alarms etc... \n")

  m <-dataList$m
  q<-dataList$q
  c<-dataList$c
  h<-dataList$h
  f<-dataList$f
  NI<-dataList$NI
  NL<-dataList$NL
  C<-dataList$C
  M<-dataList$M
  Q<-dataList$Q



  if(ModifiedPoisson==FALSE) NX = NI;
  if(ModifiedPoisson==TRUE) NX =NL;



  N <-C*M*Q
  #For Draw the Sample points on FROC curve. Assessment of Fit for FROC.
  hh <- numeric(N) #Initialization of Cumulative Hits
  ff <- numeric(N) #Initialization of Cumulative False alarm
  hharray<-array(0,dim=c(C,M,Q));#Cumulative
  ffarray<-array(0,dim=c(C,M,Q));#Cumulative
  harray<-array(0,dim=c(C,M,Q));#Non-Cumulative
  farray<-array(0,dim=c(C,M,Q)); #Non-Cumulative
  hCQ<-array(0,dim=c(C,Q));#Non-Cumulative, that is, merely format is only changed from vector to array.
  fCQ<-array(0,dim=c(C,Q)); #Non-Cumulative, that is, merely format is only changed from vector to array.
  hC<-array(0,dim=c(C));#Non-Cumulative, that is, merely format is only changed from vector to array.
  fC<-array(0,dim=c(C)); #Non-Cumulative, that is, merely format is only changed from vector to array.

  for(md in 1:M) {
    for(cd in 1:C) {
      for(qd in 1 : Q){
        for(n  in 1:cd){
          ff[cd+(md-1)*C*Q+(qd-1)*C]<-ff[cd+(md-1)*C*Q+(qd-1)*C]+f[n+(md-1)*C*Q+(qd-1)*C]
          hh[cd+(md-1)*C*Q+(qd-1)*C]<-hh[cd+(md-1)*C*Q+(qd-1)*C]+h[n+(md-1)*C*Q+(qd-1)*C]
        }
        ffarray[cd,md,qd] <-ff[cd+(md-1)*C*Q+(qd-1)*C]
        hharray[cd,md,qd] <-hh[cd+(md-1)*C*Q+(qd-1)*C]

        farray[cd,md,qd] <- f[cd+(md-1)*C*Q+(qd-1)*C]
        harray[cd,md,qd] <- h[cd+(md-1)*C*Q+(qd-1)*C]
      }}}

  for(cd in 1:C){
    for(qd in 1:Q){
      for(md in 1:M){
        hCQ[cd, qd]<-hCQ[cd, qd]+harray[cd,md,qd]
        fCQ[cd, qd]<-fCQ[cd, qd]+farray[cd,md,qd]
      }}}

  for(cd in 1:C){
    for(qd in 1:Q){
      for(md in 1:M){
        hC[cd]<-hC[cd]+harray[cd,md,qd]
        fC[cd]<-fC[cd]+farray[cd,md,qd]
      }}}



  S <-array(0,dim=c(M,Q));
  for(md in 1:M){
    for(qd in 1 : Q){
      S[md,qd] <- sum(harray[,md,qd])
    }}

  hharrayN<-hharray/NL
  ffarrayN<-ffarray/NX

  hhN<-hh/NL
  ffN<-ff/NX


  data <- list(N=N,Q=Q, M=M,m=m  ,C=C ,S=S,  NL=NL,NI=NI
               ,c=c,q=q,
               h=h, f=f,
               hh=hh, hhN=hhN,
               ff=ff,ffN=ffN,
               harray=harray,    farray=farray,
               hharray=hharray,    ffarray=ffarray,
               hharrayN=hharrayN,    ffarrayN=ffarrayN)

  return(data)

}




#' @title  Create metadata for MRMC data
#'@description  From data of number of hits and false alarms, we calculate the number of cumulative false positives and hits.
#' Since there are three subscripts, reader, modality, and image, we create array format and vector format etc...
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@return A metadata such as number of cumulative false alarms and hits to create and draw the curve.
#' @export  metadata_to_DrawCurve_MRMC


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


  # message(crayon::silver("\n* Metadata to draw the curves are callculating ... \n"))

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

  # a<-rstan::extract(fit)$a #samples of a by MCMC
  # b<-rstan::extract(fit)$b #samples of b by MCMC
  # EAP_a <-  array(0, dim=c( M,Q)) #
  # EAP_b <-  array(0, dim=c( M,Q)) #
  #
  # for(md in 1:M){
  #   for(qd in 1:Q){
  #
  #     EAP_a[md,qd] <- 0
  #     EAP_b[md,qd] <- 0
  #     s<-0
  #     t<-0
  #     for(mc in 1:MCMC){ #EAP
  #       s<-  EAP_a[md,qd]
  #       EAP_a[md,qd] <-  s+ a[mc,md,qd]
  #       t<-  EAP_b[md,qd]
  #       EAP_b[md,qd] <-  t+ b[mc,md,qd]
  #     }
  #     EAP_a[md,qd] <-EAP_a[md,qd] /MCMC  #EAP of a
  #     EAP_b[md,qd] <-EAP_b[md,qd] /MCMC  #EAP of b
  #   }}


  EAP_a <- extract_EAP_by_array(fit,"aa")
  EAP_b <- extract_EAP_by_array(fit,"bb")
  EAP_AA <-  extract_EAP_by_array(fit,"AA")


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


  #aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  #-- AUC for each modarlity and each reader

  # AA<-rstan::extract(fit)$AA #samples of a by MCMC
  # EAP_AA <-  array(0, dim=c( M,Q)) #
  #
  # for(qd in 1:Q){
  #   for(md in 1:M){
  #     EAP_AA[md,qd] <- mean(AA[,md,qd])
  #     # EAP_AA[md,qd] <- 0
  #     # s<-0
  #     # message("|")#Processsssssss
  #     # for(mc in 1:MCMC){ #EAP
  #     #   s<-  EAP_AA[md,qd]
  #     #   EAP_AA[md,qd] <-  s+ AA[mc,md,qd]
  #     # }
  #     # EAP_AA[md,qd] <-EAP_AA[md,qd] /MCMC  #EAP of a
  #   }#for md
  #   # message(paste("", ceiling(round(qd/Q,2)*100/2 +50),"% \n"))#Processsssssss
  # }# for qd

  # aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
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




