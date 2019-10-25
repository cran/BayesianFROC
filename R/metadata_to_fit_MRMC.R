#' @title  Create metadata for MRMC data
#'
#'
#'@description  From data of number of hits and false alarms, we calculate the number of cumulative false positives and hits, in other words, \emph{False Positive Fraction (FPF)} and  \emph{TRUE Positive Fraction (TPF)}.
#' Since there are three subscripts, \emph{reader}, \emph{modality}, and \emph{image}, we can create array format or vector format etc...
#'
#'@details To fit a model to data, we need False Positive Fraction and True Positive Fractions which are cumulative sums over number of lesions.




#'@param dataList An list, should include  the following \R objects:\code{m,q,c,h,f,NL,C,M,Q} which means from the right
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


#'
#'@return A list, which includes arrays and vectors. A metadata such as number of cumulative false alarms and hits to create and draw the curve. \emph{False Positive Fraction (FPF)} and  \emph{TRUE Positive Fraction (TPF)}.
#'
#'
#' \describe{
#'
#'\strong{ The following two are useful:} \emph{ I rediscover it  at 2019 Jun 18}

#'\item{ \code{ harray}  }{An array of hit, dimension \code{ [C,M,Q]}, where \code{C,M,Q} are a number of confidence level, modalities, readers, respectively.  }
#'
#'\item{ \code{ farray}   }{An array of false alarms, dimension \code{ [C,M,Q]}, where \code{C,M,Q} are a number of confidence level, modalities, readers, respectively.  }
#'
#'\item{ \code{ hharray}  }{An array of cumulative hits, dimension \code{ [C,M,Q]}, where \code{C,M,Q} are a number of confidence level, modalities, readers, respectively.  }
#'
#'\item{ \code{ ffarray}   }{An array of cumulative false alarms, dimension \code{ [C,M,Q]}, where \code{C,M,Q} are a number of confidence level, modalities, readers, respectively.  }
#'
#'
#'
#'\item{ \code{ harrayN}  }{An array of hit, dimension \code{ [C,M,Q]}, where \code{C,M,Q} are a number of confidence level, modalities, readers, respectively.  }
#'
#'\item{ \code{ farrayN}   }{An array of false alarms, dimension \code{ [C,M,Q]}, where \code{C,M,Q} are a number of confidence level, modalities, readers, respectively.  }
#'
#'\item{ \code{ hharrayN}  }{An array of TPF, dimension \code{ [C,M,Q]}, where \code{C,M,Q} are a number of confidence level, modalities, readers, respectively.  }
#'
#'\item{ \code{ ffarrayN}   }{An array of FPF, dimension \code{ [C,M,Q]}, where \code{C,M,Q} are a number of confidence level, modalities, readers, respectively.  }
#'
#'}
#'
#'
#'
#'
#'
#'
#'@examples
#'\donttest{
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#----------------------------------------------------------------------------------------
#'#           #First, we prepare the data endowed with this package.
#'#----------------------------------------------------------------------------------------
#'

#'
#'
#'              dat  <- get(data("dataList.Chakra.Web"))
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#----------------------------------------------------------------------------------------
#'#                           #Calculate FPFs and TPFs and etc.
#'#----------------------------------------------------------------------------------------
#'
#'
#'
#'                              a <- metadata_to_fit_MRMC(dat)
#'
#'
#' #Now, we get  a meta-data object named "a".
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#----------------------------------------------------------------------------------------
#'#                                 Check of Definiion
#'#----------------------------------------------------------------------------------------
#'
#'
#'                                  a$hh/dat$NL == a$hhN
#'
#'# Since all of aboves are TRUE, the hhN is a TPF per NL.
#'
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#----------------------------------------------------------------------------------------
#'#                             Plot a FPFs and TPFs
#'#----------------------------------------------------------------------------------------
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
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#----------------------------------------------------------------------------------------
#'#                             Plot a FPFs and TPFs via ggplot
#'#----------------------------------------------------------------------------------------
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
#'  ggplot(df, aes(x =FPF, y = TPF, colour = q, group = m)) +geom_point()
#'
#'# Revised 2019 Jun 18, Revised 2019 Sept 9
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
#'}# dottest

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




