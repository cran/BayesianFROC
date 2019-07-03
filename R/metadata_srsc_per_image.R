#' @title  Create metadata for MRMC data.
#'
#'
#'@description  From data of number of hits and false alarms, we calculate the number of cumulative false positives and hits.
#' Since there are three subscripts, reader, modality, and image, we create array format and vector format etc...
#'




#'@param dataList A list, should include  \code{m,q,c,h,f,NL,C,M,Q} which means from the right
#'

#'
#'\code{c } means the confidence level is not required,
#'however it is created by \code{  c <-c(rep(C:1))}, where \code{C} is the number of confidence levels.
#'So, you should write down your hits and false alarms vector so that it is compatible with this automatically created \code{c} vector.

#'
#'\code{h } means the number of hits
#'
#'\code{f } means the number of false alarm
#'
#'\code{NL } means the Total number of lesions for all images
#'
#'\code{C } means the highest number of confidence level




#'
#'@return A metadata such as number of cumulative false alarms and hits to create and draw the curve.
#'
#'@examples
#'
#'
#' \donttest{
#'#First, we prepare the data endowed with this package.
#'
#'         dat  <- BayesianFROC::dataList.Chakra.Web
#'
#'
#'
#'
#'#Second, we run the stan funtion
#'#with data named "dat"  and the author's Bayesian model.
#'
#'
#'              metadata_srsc_per_image(dat)
#'
#'
#'
#'
#'
#'
#' #Now, we get a metadata.
#'}# dottest

# devtools::document();help("metadata_MRMC") # Confirm reflection
#' @export  metadata_srsc_per_image
#'@inheritParams fit_Bayesian_FROC

metadata_srsc_per_image<- function(dataList)
{

  h <- dataList$h
  f <- dataList$f
  NI <- dataList$NI
  NL <- dataList$NL
  C  <- as.integer(dataList$C)


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
  ff <- fff/NI


  data <- list( N=N,NL=NL,NI=NI,C=as.integer(C),c=c,
                h=h,f=f,
                hh=hh,ff=ff,
                fff=fff)

  invisible(data)
}




