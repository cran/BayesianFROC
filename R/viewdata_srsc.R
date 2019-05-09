#' @title  Build a table of data in the case of
#' Single reader and Single modality (srsc)
#'
#'
#'@description  In order to confirm your data, please view table.
#'my program makes new column of confidence levels which are used in
#'my program. So, it is possible that your order of confidence level and
#'Program's order of confidence level are inverse.
#'This function's result table are the one which are used in program.


#'@param dataList it should include  \code{f, h, NL, NI, C}.
#'The detail of these dataset, please see the endowed datasets.
#'Note that the maximal number of confidence  level, denoted by  \code{C}, are included,
#' however,
#'its each confidence level should not included  your data. So, to confirm
#'your false positives and hits are correctly correspondence
#'to confidence levels,
#'you should confirm the orders by the function

#'
#'@examples
#'\donttest{
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#First, we prepare the data endowed with this package.
#'#Note that this data should be formed as a single reader and single case (modality).
#'#If your data are a multiple reader and multiple case (modality), i.e.,MRMC-data,
#'#then another function named viewdataMRMC is available for MRMC-data.
#'
#'              dat  <- get(data("dataList.Chakra.1"))
#'
#'
#'
#'
#'#Second, we run the stan funtion
#'#with data named "dat";
#'
#'
#'              viewdata_srsc(dat)
#'
#'
#'
#'#The Reason why the author made this \code{viewdata_srsc} is
#'#the code does not refer your confidence level.
#'#More precisely, my program made the column vector of confidence levels
#'#from the its highest number,
#'#so, it may be occur the interpretaion of code for hits and false alarm
#'#are inverse order compared with your data.
#'
#'
#'}# dottest
#'
#' @importFrom knitr  kable
#'@param summary TRUE or FALSE, if true then results are printed, if FALSE this function do nothing.

# devtools::document();help("viewdataSRSC") # Confirm reflection

# devtools::use_package("rstan")



# devtools::use_data(dataList.high.ability)
#' @export viewdata_srsc
#'@inheritParams fit_Bayesian_FROC
viewdata_srsc <-function(dataList,summary=TRUE){
  C<-dataList$C
  if (summary==TRUE) {

# the output of kable is disturbed from the name of vector.
    names(dataList$f)<-NULL
    names(dataList$h)<-NULL

  data  <- data.frame(
    Confidence.Level=rep(C:1),
    False.Positives=dataList$f,
    True.Positives=dataList$h
    )


  print( knitr::kable(data, format = "pandoc"))
}
}
