#' @title  View MRMC data
#'
#'
#'@description  Build a table for data \code{dataList}.

#'@param dataList it should include  \code{m,q,c,h,f,NL,C,M,Q} which means from the right
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
#'The detail of these dataset, please see the endowed datasets.
#'Note that the maximal number of confidence  level, denoted by  \code{C}, are included,
#' however,
#'its each confidence level should not included  your data. So, to confirm
#'your false positives and hits are correctly correspondence
#'to confidence levels,
#'you should confirm the orders by the function \code{viewdata_MRMC}.
#'@param summary TRUE or FALSE, if true then results are printed, if FALSE this function do nothing.
#'@param head.only Logical: \code{TRUE} of \code{FALSE}. Whether  head part or entire. If \code{TRUE}, only head part are shown. Default is \code{FALSE}

#' @importFrom knitr  kable

# devtools::document();help("viewdata_MRMC") # Confirm reflection
#' @export viewdata_MRMC

viewdata_MRMC <-function(dataList,summary=TRUE, head.only =FALSE){
  if (summary==TRUE) {

  C<-dataList$C
  M<-dataList$M
  Q<-dataList$Q


  data  <- data.frame(ModalityID=dataList$m,
                      ReaderID=dataList$q,
                      Confidence.Level=rep(rep(C:1), M*Q),
                      False.Positives=dataList$f,
                      True.Positives=dataList$h)


message("
 -----  Interpretation of the Table ------

*   The first row shows that the number of hits ",crayon::bgBlack$bold$italic$underline$yellow(dataList$h[1]),"  and the number of false alarms  ",crayon::bgBlack$bold$italic$underline$yellow(dataList$f[1])," with the ",crayon::bgBlack$bold$italic$underline$yellow(dataList$q[1]),"-th reader under the imaging method ID ",crayon::bgBlack$bold$italic$underline$yellow(dataList$m[1])," with his confidence level ",crayon::bgBlack$bold$italic$underline$yellow(dataList$c[1]),".")


 if (head.only == FALSE)   print( knitr::kable(data, format = "pandoc"))
 if (head.only == TRUE)  { print( knitr::kable( utils::head(data,n=2*C), format = "pandoc"))
message("\n* We show the head part of data, i.e., first ", 2*C," rows  are shown. \n")
message("\n* To show all rows of data, use viewdata(dataList = ", crayon::bgBlack$cyan("Your data")   ,", head.only = ", crayon::bgBlack$cyan("FALSE")   ,")\n")

   }
  }# if (summary=TRUE)

}
