
#'@title make a Ranking for AUCs for MRMC data
#'@description print a modality ranking according to their AUCs.
#'@inheritParams DrawCurves
#'@details This is ranking. Sort a datafrmae involving AUC and corresponding modality IDs
#'@return Sorted table of AUC and modality ID
#'
#' @examples
#'
#'  \donttest{
#'
#'  fit <- fit_Bayesian_FROC(
#'  ite  = 1111,
#'  summary = FALSE,
#'  cha = 1,
#'  Null.Hypothesis = FALSE,
#'   dataList = dd )
#'
#'
#'
#'
#'
#'
#'  sortAUC(fit)
#'
#'  }
#'
#'
#'
# utils::globalVariables("extract_EAP_by_array")

sortAUC <- function(StanS4class){
  A <-1
  fit <- StanS4class

a<-extract_EAP_by_array(fit, name.of.parameter=A)
M <-fit@dataList$M

modalityID <-1:M

df <- data.frame(modalityID=modalityID,a=a)
df.sort<-with(df, df[order(a,-modalityID), ])

cat(crayon::bgWhite$black$bold$italic$underline(df.sort$modalityID),"\n" ,sep = paste(crayon::bgWhite$red$bold$italic$underline("-th"),crayon::bgGreen$white$bold$italic$underline(" modality"),"  < "))
cat("\n*",df.sort$a,sep = " < ")

invisible(df.sort)
}


