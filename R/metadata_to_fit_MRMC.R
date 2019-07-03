#' @title  Create metadata for MRMC data
#'
#'
#'@description  From data of number of hits and false alarms, we calculate the number of cumulative false positives and hits.
#' Since there are three subscripts, reader, modality, and image, we create array format and vector format etc...
#'
#'@details To fit a model to data, we need False Positive Fraction and True Positive Fractions which are cumulative sums over number of lesions.




#'@param dataList An list, should include  the following \R objects:\code{m,q,c,h,f,NL,C,M,Q} which means from the right
#'
#'\code{m } means the modality ID vector
#'
#'\code{q } means the reader ID vector
#'
#'\code{c } means the confidence level
#'
#'\code{h } means the number of hits
#'
#'\code{f } means the number of false alarm
#'
#'\code{NL } means the Total number of lesions for all images
#'
#'\code{C } means the highest number of confidence level
#'
#'\code{M } means the number of modalities
#'
#'\code{Q } means the number of readers.
#'
#'The detail of these dataset, please see the example datasets, e.g. \code{\link{dd}}.


#'
#'@return A list. A metadata such as number of cumulative false alarms and hits to create and draw the curve.
#'
#'
#' \describe{
#'
#'\strong{ The following two are useful:} \emph{ I rediscover it  at 2019 Jun 18}

#'\item{ \code{ harray}  }{An array of hit, dimension \code{ [C,M,Q]}, where \code{C,M,Q} are a number of confidence level, modalities, readers, respectively.  }
#'
#'\item{ \code{ farray}   }{An array of false alarms, dimension \code{ [C,M,Q]}, where \code{C,M,Q} are a number of confidence level, modalities, readers, respectively.  }
#'
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

#'#First, we prepare the data endowed with this package.
#'
#'
#'
#'              dat  <- get(data("dataList.Chakra.Web"))
#'
#'
#'
#'
#'
#'              metadata_to_fit_MRMC(dat)
#'
#'
#'
#'
#'
#'
#' #Now, we get  meta-data.
#'
#'
#'# Revised 2019 Jun 18
#'
#'}# dottest

#'
# devtools::document();help("metadata_MRMC") # Confirm reflection
# devtools::use_data(dataList.high.ability)
#' @export  metadata_to_fit_MRMC
#'@inheritParams fit_Bayesian_FROC

metadata_to_fit_MRMC<- function(dataList)
{
  # message("\n")
  # message("* Now, we calculated the metadata, e.g., cumulative hits and false alarms etc... \n")

  m <-dataList$m
  q<-dataList$q
  c<-dataList$c
  h<-dataList$h
  f<-dataList$f
  # NI<-dataList$NI
  NL<-dataList$NL
  C<-dataList$C
  M<-dataList$M
  Q<-dataList$Q







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
  ffarrayN<-ffarray/NL

  hhN<-hh/NL
  ffN<-ff/NL


  data <- list(N=N,Q=Q, M=M,m=m  ,C=C ,S=S,  NL=NL,c=c,q=q,
               h=h, f=f,
               hh=hh, hhN=hhN,
               ff=ff,ffN=ffN,
               harray=harray,    farray=farray,
               hharray=hharray,    ffarray=ffarray,
               hharrayN=hharrayN,    ffarrayN=ffarrayN)

  return(data)

}




