#' @title  Build a table of FROC data
#'
#'
#'@description
#' Create a tabular representation of FROC data from FROC data object.


#'@return Nothing
#'
#'In order to confirm your data, please view table before fitting.
#'Confidence level vector are created in my program regardless of user's confidence level vectors.



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



#'@param head.only Logical: \code{TRUE} of \code{FALSE}. Whether  head part or entire. If \code{TRUE}, only head part are shown. Default is \code{FALSE}


#'
#'@examples
#' \donttest{

#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####

#'# The first example, we prepare the data endowed with this package.

#'              dat  <- get(data("dataList.Chakra.1"))
#'
#'
#'
#'
#'              viewdata(dat)
#'
#'
#'# The second examle, we consider the multiple reader and multiple dataset
#'
#'              dat <-  get(data("dataList.Chakra.Web"))
#'
#'              viewdata(dat)
#'
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






viewdata <- function(dataList,summary=TRUE,head.only=FALSE){



  # message(" This function named viewdata refer your datalist's column of modality ID, if such column does not exsit then this function consider your data are single reader and signle modality case, and in the other cases, this function regards your data as multiple reader and multiple case.\n")

  if ( length(dataList[["m"]])==0  ) {# the reader =0 occur even the case of MRMC

    # message("\n-------------------------------------\n")
    # message("\n* Your data:         ",deparse(substitute(dataList)) ," \n \n")
    message("\n* Number of Lesions: ",  crayon::bgBlack$bold$italic$underline$yellow(dataList$NL)   )
    message("\n* Number of Images : ",  crayon::bgBlack$bold$italic$underline$yellow(dataList$NI) ,"\n")
    # viewdata_srsc(dataList,summary)

    tryCatch(
      {   viewdata_srsc(dataList,summary)  },
  error =function(e){message("Should confirm whether data format is correct. ")}
  )


    message(
      crayon::bold$italic$underline$silver(
        "\n\n* Higher number of confidence level indicates readers his higher confidence. In your case, the number "
        ),
        crayon::bgBlack$bold$italic$underline$yellow(dataList$C),
      crayon::bold$italic$underline$silver(
       " is the most high confidence level, i.e., we may say that confidence level "
      ),
      crayon::bgBlack$bold$italic$underline$yellow(dataList$C,
   crayon::bold$italic$underline$silver(
      " means that \"definitely lesion is present \" " )
   )
   )


  } else if (length(dataList[["m"]]) >= 1) {
    # message("\n-------------------------------------\n")
    # message("\n* Your data:         ",deparse(substitute(dataList)) ," \n \n")
    message("\n* Number of Lesions: ",crayon::bgBlack$bold$italic$underline$yellow(dataList$NL) )
    if (!is.null(dataList$NI))  message("\n* Number of Images : ",crayon::bgBlack$bold$italic$underline$yellow(dataList$NI) ,". (This is not used for estimates).\n")
    if (is.null(dataList$NI))  message(  crayon::bold$italic$underline$silver("\n* Number of Images is not assigned, not required to esimate. \n") )

     # viewdata_MRMC(dataList,summary)
     tryCatch(
       {   viewdata_MRMC(dataList,summary,head.only=head.only)   },
       error =function(e){message("Should confirm whether data format is correct. ")}
     )


     } else

      message("Verify the  column vector of modality ID is named by  m  exactly in your data.")



}
