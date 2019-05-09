#' @title  Build a table of data in the case of
#' Single reader and Single modality (srsc)
#'
#'
#'@description  In order to confirm your data, please view table.
#'my program makes new column of confidence levels which are used in
#'my program. So, it is possible that your order of confidence level and
#'Program's order of confidence level are inverse.
#'This function's result table are the one which are used in program.



#'@param dataList
#'------------------------------------------------------
#'
#'
#'   Single reader and single modality (SRSC) case.
#'
#'
#'------------------------------------------------------
#'
#'In single reader and single modality case, it should include  \code{f, h, NL, NI, C}.
#'The detail of these dataset, see the datasets  endowed with this package.
#'Note that the maximal number of confidence level, denoted by  \code{C}, are included,
#' however,
#'its each confidence level should not included  your data. So, to confirm
#'your false positives and hits are correctly correspondence
#'to confidence levels,
#'you should confirm the orders by the function
#'
#'----------------------------------------------------------------------------
#'
#'   Multiple readers and multiple modalities case, i.e., MRMC case
#'
#'
#'----------------------------------------------------------------------------
#'
#'
#'In  multiple readers and multiple modalities case, i.e., MRMC case,
#' it should include  \code{m,q,c,h,f,NL,C,M,Q} which means from the right
#'
#'\code{C } means the highest number of confidence level, this is a scalar.
#'
#'\code{M } means the number of modalities
#'
#'\code{Q } means the number of readers.
#'
#'\code{c } means the confidence level vector. This vector must be made by \code{rep(rep(C:1), M*Q)}.
#'
#'
#'\code{m } means the modality ID vector.
#'
#'\code{q } means the reader ID vector.
#'
#'
#'\code{h } means the number of hits vector.
#'
#'\code{f } means the number of false alarm vector.
#'
#'\code{NL } means the Total number of lesions for all images, this is a scalar.
#'

#'The detail of these dataset, please see the endowed datasets.
#'Note that the maximal number of confidence level, denoted by  \code{C}, are included,
#' however,
#'its each confidence level vector also created in the program by \code{C}. So, to confirm
#'your false positives and hits are correctly correspond
#'to confidence levels,
#'you should confirm the orders by the function \code{viewdata_MRMC}.





#'
#'@examples
#' \donttest{

#'
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
#'              viewdata(dat)
#'
#'
#'
#'#The Reason why the author made this viewdataSRSC is
#'#the code does not refer your confidence level.
#'#More precisely, my program made the column vector of confidence levels
#'# from the its highest number,
#'#so, it may be occur the interpretaion of code for hits and false alarm
#'#are inverse order compared with your data.
#'
#'}# dottest

#'
# @importFrom   gridExtra tableGrob
# @importFrom  grid grid.draw
#' @importFrom  knitr kable
#' @export viewdata
#' @param summary TRUE or FALSE, if true then results are printed, if FALSE this function do nothing.

# devtools::document();help("viewdata") # Confirm reflection

# devtools::use_package("rstan")






viewdata <- function(dataList,summary=TRUE){



  # message(" This function named viewdata refer your datalist's column of modality ID, if such column does not exsit then this function consider your data are single reader and signle modality case, and in the other cases, this function regards your data as multiple reader and multiple case.\n")

  if ( length(dataList[["m"]])==0  ) {# the reader =0 occur even the case of MRMC

    # message("\n-------------------------------------\n")
    # message("\n* Your data:         ",deparse(substitute(dataList)) ," \n \n")
    message("\n* Number of Lesions: ",dataList$NL)
    message("\n* Number of Images : ",dataList$NI ,"\n")
    viewdata_srsc(dataList,summary)


  } else if (length(dataList[["m"]]) >= 1) {
    # message("\n-------------------------------------\n")
    # message("\n* Your data:         ",deparse(substitute(dataList)) ," \n \n")
    message("\n* Number of Lesions: ",dataList$NL)
    message("\n* Number of Images : ",dataList$NI ,". (This is not used for estimates).\n")
    viewdata_MRMC(dataList,summary)    } else

      message("Verify the  column vector of modality ID is named by  m  exactly in your data.")



}
