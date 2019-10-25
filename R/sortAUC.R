
#'@title Make a Ranking for AUCs for MRMC Data
#'@description print a modality ranking according to their AUCs.
#'@inheritParams DrawCurves
#'@details This is a ranking. Sort a data-frame involving AUC and corresponding modality IDs.
#'@return  A data-frame, representing sorted ranking of modality ID and its AUC. Revised 2019 Sept 9
#'@param digits To be passed to \code{round()} for AUC, to determine the significant digits of AUCs.
#' @export
#'
#' @examples
#'
#'  \donttest{
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#----------------------------------------------------------------------------------------
#'#            1)             Fit a model to an MRMC data-set named dd
#'#----------------------------------------------------------------------------------------
#'
#'                     fit <- fit_Bayesian_FROC(
#'                                                 ite  = 1111,
#'                                              summary = FALSE,
#'                                                  cha = 1,
#'                                             dataList = dd
#'                                              )
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#----------------------------------------------------------------------------------------
#'#            1)         Sort the AUC and make a ranking table
#'#----------------------------------------------------------------------------------------
#'
#'
#'
#'                               sortAUC(fit)
#'
#'
#'
#'# Then, a ranking table will appear.
#'
#'                                                                # Reviesed 2019 Sept 9
#'
#'  }
#'
#'
#'
# utils::globalVariables("extract_EAP_by_array")

sortAUC <- function(StanS4class, digits = 3){

  A <-1
  fit <- StanS4class
 if (!fit@studyDesign== "MRMC")message("MRMC Only.")

A<-extract_EAP_by_array(fit, name.of.parameter=A)
A <- round(A, digits = digits)

a<-sort(A, method = "shell", index.return = TRUE) # is stable

rank <- rev(a$ix)
AUC  <- rev(a$x)


# This is for only ranking table
rankk <- rank
 rankk[1] <- paste("(^o^)/ King!         :) ", rank[1])
 rankk[length(rank)] <- paste("('-_-) Sorry, beat it! ...   :'c ", rank[length(rank)])
  dff<- data.frame(modalityID=rankk, AUC=AUC )
  print(knitr::kable(dff,format = "pandoc", caption = "Ranking of Modality, based on AUC",align = c("r","c")))
# This does not relates the return value


df <- data.frame(modalityID=rank, AUC=AUC )

# cat(crayon::bgWhite$black$bold$italic$underline(rank),"\n" ,sep = paste(crayon::bgWhite$red$bold$italic$underline("-th"),crayon::bgGreen$white$bold$italic$underline(" modality"),"  > "))
# message("\n From the right, the most high performance modality \n")
# cat(  rank,sep = " > ")
# message("\n")
# cat(  AUC,sep = " > ")
# message("\n----------------------\n")




invisible(df)
}


