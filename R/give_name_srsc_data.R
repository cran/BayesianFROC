
#'@title Give a name for srsc data list component
#'@description Specifying the data,the names are given for each component vectors.
#'@details This is only available on singler reader and single modality case, not available on MRMC case.
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@inheritParams validation.dataset_srsc
#'@examples

#'#>dataList.Chakra.2
#'#$f
#'#[1]  4 13 44
#'#
#'#$h
#'#[1] 122  31  20
#'#
#'#$NL
#'#[1] 269
#'#
#'#$NI
#'#[1] 57
#'#
#'#$C
#'#[1] 3
#'
#'    dataList.with.name <- give_name_srsc_data(dataList.Chakra.2)
#'
#'
#'
#'
#'
#'#> dataList.with.name
#'# $f
#'# F(3) F(2) F(1)
#'# 4   13   44
#'#
#'# $h
#'# H(3) H(2) H(1)
#'# 122   31   20
#'#
#'# $NL
#'# Number of Lesions
#'# 269
#'#
#'# $NI
#'# Number of Images
#'# 57
#'#
#'# $C
#'# Number of Confidence levels
#'# 3
#'
#'
#' \dontrun{
#'}# dottest

#' @export

 give_name_srsc_data <- function(dataList) {
   d <-dataList
C<-dataList$C

f.name <-vector()
h.name <-vector()

for (cd in 1:C) {
  f.name[cd] <-paste("F(",C-cd+1,")",sep = "")
  h.name[cd] <-paste("H(",C-cd+1,")",sep = "")
}



 names(d$f) <-  f.name
 names(d$h) <-  h.name
 names(d$NI) <-c("Number of Images")
 names(d$NL) <-c("Number of Lesions")
 names(d$C) <-c("Number of Confidence levels")
 # devtools::use_data( name.of.data)

 return(d)

 }







#'@title Give a Name For CTP CFP vector
#'
#'@description
#' Give a Name for a vector representing cumulative true positives (CTPs) or cumulative false positives (CFPs).
#'@details
#' Some function in this package give the return values of vectors representing the CFP or CTPs. Using this function, we specify what the components of vector means. This is important since its order is not deterministic, that is, its order give two case, one is decreasing and one is increasing order. So, to avoid such confusion, the name should be specified. Of course this function is no needed for user to know or to use it.

#' @param vector A vector representing cumulative true positives (CTPs) or cumulative false positives (CFPs).
#' @param CFP.or.CTP "CFP" or "CTP". Default value is “CFP”.
#'@inheritParams fit_Bayesian_FROC
 #'@inheritParams DrawCurves
#' @return A vector representing cumulative true positives (CTPs) or cumulative false positives (CFPs) with its name.
#' @export
#'
#'
#' @examples
#'
#'    h <- BayesianFROC::dataList.Chakra.1$h
#'
#'    NL <- BayesianFROC::dataList.Chakra.1$NL
#'
#'    CTP.vector <- cumsum(h)/NL
#'
#'    CTP.vector.with.name <- give_name_srsc_CFP_CTP_vector(CTP.vector)
#'
#'
 give_name_srsc_CFP_CTP_vector <-     function( vector ,CFP.or.CTP ="CFP",ModifiedPoisson = FALSE){

   C <- length(vector)

 CFP.name <- vector()
 CTP.name  <- vector()
 for (cd in 1:C) {
   ssss <-""
   tttt <-""

   for (ccd in C:cd) {


  if(ssss==""){
       ssss <- paste(ssss,"H[",ccd,"]",sep = "")
       tttt <- paste(tttt,"F[",ccd,"]",sep = "")
     }else{
       ssss <- paste(ssss," + H[",ccd,"]",sep = "")
       tttt <- paste(tttt," + F[",ccd,"]",sep = "")
     }

     CTP.name [cd] <- paste( "( ",ssss," ) /","NL",sep = "")
    if (ModifiedPoisson == FALSE) CFP.name [cd] <- paste( "( ",tttt," ) /","NI",sep = "")
    if (ModifiedPoisson == TRUE ) CFP.name [cd] <- paste( "( ",tttt," ) /","NL",sep = "")




   }}

   if(CFP.or.CTP=="CFP"){ names(vector) <- CFP.name}


 if(CFP.or.CTP=="CTP"){ names(vector) <- CTP.name}

 return(vector)

 }#function
