

#' @title Multiple Reader and Multiple Modality Data
#' @description A list, representing FROC data of MRMC.
#' @details This data is based on in Chakraborty's JAFROC software in which example data exists. The author have calculated hits and false alarms from this Jafroc example data.
#'

#'
#'\strong{\emph{ Contents:  }}
#'
#'  \emph{          Multiple readers and Multiple modalities case, i.e., MRMC case}
#'
#'
#'
#'---------------------------------------------------------------------------------------------------
#' \tabular{ccccc}{
#'  \strong{ModalityID } \tab   \strong{ReaderID }  \tab  \strong{ Confidence levels} \tab   \strong{No. of false alarms} \tab   \strong{No. of hits}.\cr
#'   \code{q} \tab  \code{ m}  \tab   \code{c} \tab  \code{ f} \tab \code{ h}\cr
#'   -------------- \tab ------------- \tab ------------------------ \tab  ------------------- \tab ----------------\cr
#'1\tab 1\tab 5\tab  0\tab 50\cr
#'1\tab 1\tab 4\tab  4\tab 30\cr
#'1\tab 1\tab 3\tab 20\tab 11\cr
#'1\tab 1\tab 2\tab 29\tab 5\cr
#'1\tab 1\tab 1\tab 21\tab 1\cr
#'1\tab 2\tab 5\tab  0\tab 15\cr
#'1\tab 2\tab 4\tab  0\tab 29\cr
#'1\tab 2\tab 3\tab  6\tab 29\cr
#'1\tab 2\tab 2\tab 15\tab 1\cr
#'1\tab 2\tab 1\tab 22\tab 0\cr
#'1\tab 3\tab 5\tab  1\tab 39\cr
#'1\tab 3\tab 4\tab 15\tab 31\cr
#'1\tab 3\tab 3\tab 18\tab 8\cr
#'1\tab 3\tab 2\tab 31\tab 10\cr
#'1\tab 3\tab 1\tab 19\tab 3\cr
#'1\tab 4\tab 5\tab  1\tab 10\cr
#'1\tab 4\tab 4\tab  2\tab 8\cr
#'1\tab 4\tab 3\tab  4\tab 25\cr
#'1\tab 4\tab 2\tab 16\tab 45\cr
#'1\tab 4\tab 1\tab 17\tab 14\cr
#'2\tab 1\tab 5\tab  1\tab 52\cr
#'2\tab 1\tab 4\tab  1\tab 25\cr
#'2\tab 1\tab 3\tab 21\tab 13\cr
#'2\tab 1\tab 2\tab 24\tab 4\cr
#'2\tab 1\tab 1\tab 23\tab 1\cr
#'2\tab 2\tab 5\tab  1\tab 27\cr
#'2\tab 2\tab 4\tab  1\tab 28\cr
#'2\tab 2\tab 3\tab  5\tab 29\cr
#'2\tab 2\tab 2\tab 30\tab 1\cr
#'2\tab 2\tab 1\tab 40\tab 0\cr
#'2\tab 3\tab 5\tab  2\tab 53\cr
#'2\tab 3\tab 4\tab 19\tab 29\cr
#'2\tab 3\tab 3\tab 31\tab 13\cr
#'2\tab 3\tab 2\tab 56\tab 2\cr
#'2\tab 3\tab 1\tab 42\tab 4\cr
#'2\tab 4\tab 5\tab  2\tab 9\cr
#'2\tab 4\tab 4\tab  0\tab 16\cr
#'2\tab 4\tab 3\tab  2\tab 22\cr
#'2\tab 4\tab 2\tab 30\tab 43\cr
#'2\tab 4\tab 1\tab 32\tab 14\cr
#'3\tab 1\tab 5\tab  1\tab 43\cr
#'3\tab 1\tab 4\tab  7\tab 29\cr
#'3\tab 1\tab 3\tab 13\tab 11\cr
#'3\tab 1\tab 2\tab 28\tab 6\cr
#'3\tab 1\tab 1\tab 19\tab 0\cr
#'3\tab 2\tab 5\tab  0\tab 18\cr
#'3\tab 2\tab 4\tab  1\tab 29\cr
#'3\tab 2\tab 3\tab  7\tab 21\cr
#'3\tab 2\tab 2\tab  7\tab 0\cr
#'3\tab 2\tab 1\tab 31\tab 0\cr
#'3\tab 3\tab 5\tab  7\tab 43\cr
#'3\tab 3\tab 4\tab 15\tab 29\cr
#'3\tab 3\tab 3\tab 28\tab 6\cr
#'3\tab 3\tab 2\tab 41\tab 7\cr
#'3\tab 3\tab 1\tab  9\tab 1\cr
#'3\tab 4\tab 5\tab  0\tab 10\cr
#'3\tab 4\tab 4\tab  2\tab 14\cr
#'3\tab 4\tab 3\tab  5\tab 19\cr
#'3\tab 4\tab 2\tab 24\tab 32\cr
#'3\tab 4\tab 1\tab 31\tab 23\cr
#'4\tab 1\tab 5\tab  1\tab 61\cr
#'4\tab 1\tab 4\tab  4\tab 19\cr
#'4\tab 1\tab 3\tab 18\tab 12\cr
#'4\tab 1\tab 2\tab 21\tab 9\cr
#'4\tab 1\tab 1\tab 23\tab 3\cr
#'4\tab 2\tab 5\tab  1\tab 16\cr
#'4\tab 2\tab 4\tab  1\tab 29\cr
#'4\tab 2\tab 3\tab  0\tab 34\cr
#'4\tab 2\tab 2\tab 11\tab 1\cr
#'4\tab 2\tab 1\tab 35\tab 0\cr
#'4\tab 3\tab 5\tab  6\tab 52\cr
#'4\tab 3\tab 4\tab 14\tab 29\cr
#'4\tab 3\tab 3\tab 37\tab 10\cr
#'4\tab 3\tab 2\tab 36\tab 4\cr
#'4\tab 3\tab 1\tab 18\tab 3\cr
#'4\tab 4\tab 5\tab  0\tab 10\cr
#'4\tab 4\tab 4\tab  2\tab 16\cr
#'4\tab 4\tab 3\tab  4\tab 23\cr
#'4\tab 4\tab 2\tab 18\tab 43\cr
#'4\tab 4\tab 1\tab 25\tab 15\cr
#'5\tab 1\tab 5\tab  0\tab 35\cr
#'5\tab 1\tab 4\tab  2\tab 29\cr
#'5\tab 1\tab 3\tab 19\tab 18\cr
#'5\tab 1\tab 2\tab 23\tab 9\cr
#'5\tab 1\tab 1\tab 18\tab 0\cr
#'5\tab 2\tab 5\tab  0\tab 17\cr
#'5\tab 2\tab 4\tab  2\tab 27\cr
#'5\tab 2\tab 3\tab  6\tab 24\cr
#'5\tab 2\tab 2\tab 10\tab 0\cr
#'5\tab 2\tab 1\tab 30\tab 0\cr
#'5\tab 3\tab 5\tab  2\tab 34\cr
#'5\tab 3\tab 4\tab 25\tab 33\cr
#'5\tab 3\tab 3\tab 40\tab 7\cr
#'5\tab 3\tab 2\tab 29\tab 13\cr
#'5\tab 3\tab 1\tab 24\tab 2\cr
#'5\tab 4\tab 5\tab  1\tab 12\cr
#'5\tab 4\tab 4\tab  1\tab 16\cr
#'5\tab 4\tab 3\tab  4\tab 21\cr
#'5\tab 4\tab 2\tab 24\tab 35\cr
#'5\tab 4\tab 1\tab 32\tab 15}
#'---------------------------------------------------------------------------------------------------
#'
#' @seealso \code{\link{dataList.Chakra.Web.orderd} } \code{\link{d} }
#'
#' @name dataList.Chakra.Web
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#'
#' @references Example data of Jafroc software
#' @examples
#'
#'
#'     viewdata(BayesianFROC::dataList.Chakra.Web)
#'

NULL



































#' @title Multiple Reader and Multiple Modality Data
#' @description A list, representing FROC data of MRMC.
#' @details This data is based on in Chakraborty's JAFROC software in which example data exists. The author have calculated hits and false alarms from this Jafroc example data.

#'
#'\strong{\emph{ Contents:  }}
#'
#'  \emph{          Multiple readers and multiple modalities case, i.e., MRMC case   }
#'
#'
#'
#'
#'---------------------------------------------------------------------------------------------------
#' \tabular{ccccc}{
#'  \strong{ModalityID } \tab   \strong{ReaderID }  \tab  \strong{ Confidence levels} \tab   \strong{No. of false alarms} \tab   \strong{No. of hits}.\cr
#'   \code{q} \tab  \code{ m}  \tab   \code{c} \tab  \code{ f} \tab \code{ h}\cr
#'   -------------- \tab ------------- \tab ------------------------ \tab  ------------------- \tab ----------------\cr
#'1\tab 1\tab 5\tab  0\tab 50\cr
#'1\tab 1\tab 4\tab  4\tab 30\cr
#'1\tab 1\tab 3\tab 20\tab 11\cr
#'1\tab 1\tab 2\tab 29\tab 5\cr
#'1\tab 1\tab 1\tab 21\tab 1\cr
#'1\tab 2\tab 5\tab  0\tab 15\cr
#'1\tab 2\tab 4\tab  0\tab 29\cr
#'1\tab 2\tab 3\tab  6\tab 29\cr
#'1\tab 2\tab 2\tab 15\tab 1\cr
#'1\tab 2\tab 1\tab 22\tab 0\cr
#'1\tab 3\tab 5\tab  1\tab 39\cr
#'1\tab 3\tab 4\tab 15\tab 31\cr
#'1\tab 3\tab 3\tab 18\tab 8\cr
#'1\tab 3\tab 2\tab 31\tab 10\cr
#'1\tab 3\tab 1\tab 19\tab 3\cr
#'1\tab 4\tab 5\tab  1\tab 10\cr
#'1\tab 4\tab 4\tab  2\tab 8\cr
#'1\tab 4\tab 3\tab  4\tab 25\cr
#'1\tab 4\tab 2\tab 16\tab 45\cr
#'1\tab 4\tab 1\tab 17\tab 14\cr
#'2\tab 1\tab 5\tab  1\tab 52\cr
#'2\tab 1\tab 4\tab  1\tab 25\cr
#'2\tab 1\tab 3\tab 21\tab 13\cr
#'2\tab 1\tab 2\tab 24\tab 4\cr
#'2\tab 1\tab 1\tab 23\tab 1\cr
#'2\tab 2\tab 5\tab  1\tab 27\cr
#'2\tab 2\tab 4\tab  1\tab 28\cr
#'2\tab 2\tab 3\tab  5\tab 29\cr
#'2\tab 2\tab 2\tab 30\tab 1\cr
#'2\tab 2\tab 1\tab 40\tab 0\cr
#'2\tab 3\tab 5\tab  2\tab 53\cr
#'2\tab 3\tab 4\tab 19\tab 29\cr
#'2\tab 3\tab 3\tab 31\tab 13\cr
#'2\tab 3\tab 2\tab 56\tab 2\cr
#'2\tab 3\tab 1\tab 42\tab 4\cr
#'2\tab 4\tab 5\tab  2\tab 9\cr
#'2\tab 4\tab 4\tab  0\tab 16\cr
#'2\tab 4\tab 3\tab  2\tab 22\cr
#'2\tab 4\tab 2\tab 30\tab 43\cr
#'2\tab 4\tab 1\tab 32\tab 14\cr
#'3\tab 1\tab 5\tab  1\tab 43\cr
#'3\tab 1\tab 4\tab  7\tab 29\cr
#'3\tab 1\tab 3\tab 13\tab 11\cr
#'3\tab 1\tab 2\tab 28\tab 6\cr
#'3\tab 1\tab 1\tab 19\tab 0\cr
#'3\tab 2\tab 5\tab  0\tab 18\cr
#'3\tab 2\tab 4\tab  1\tab 29\cr
#'3\tab 2\tab 3\tab  7\tab 21\cr
#'3\tab 2\tab 2\tab  7\tab 0\cr
#'3\tab 2\tab 1\tab 31\tab 0\cr
#'3\tab 3\tab 5\tab  7\tab 43\cr
#'3\tab 3\tab 4\tab 15\tab 29\cr
#'3\tab 3\tab 3\tab 28\tab 6\cr
#'3\tab 3\tab 2\tab 41\tab 7\cr
#'3\tab 3\tab 1\tab  9\tab 1\cr
#'3\tab 4\tab 5\tab  0\tab 10\cr
#'3\tab 4\tab 4\tab  2\tab 14\cr
#'3\tab 4\tab 3\tab  5\tab 19\cr
#'3\tab 4\tab 2\tab 24\tab 32\cr
#'3\tab 4\tab 1\tab 31\tab 23\cr
#'4\tab 1\tab 5\tab  1\tab 61\cr
#'4\tab 1\tab 4\tab  4\tab 19\cr
#'4\tab 1\tab 3\tab 18\tab 12\cr
#'4\tab 1\tab 2\tab 21\tab 9\cr
#'4\tab 1\tab 1\tab 23\tab 3\cr
#'4\tab 2\tab 5\tab  1\tab 16\cr
#'4\tab 2\tab 4\tab  1\tab 29\cr
#'4\tab 2\tab 3\tab  0\tab 34\cr
#'4\tab 2\tab 2\tab 11\tab 1\cr
#'4\tab 2\tab 1\tab 35\tab 0\cr
#'4\tab 3\tab 5\tab  6\tab 52\cr
#'4\tab 3\tab 4\tab 14\tab 29\cr
#'4\tab 3\tab 3\tab 37\tab 10\cr
#'4\tab 3\tab 2\tab 36\tab 4\cr
#'4\tab 3\tab 1\tab 18\tab 3\cr
#'4\tab 4\tab 5\tab  0\tab 10\cr
#'4\tab 4\tab 4\tab  2\tab 16\cr
#'4\tab 4\tab 3\tab  4\tab 23\cr
#'4\tab 4\tab 2\tab 18\tab 43\cr
#'4\tab 4\tab 1\tab 25\tab 15\cr
#'5\tab 1\tab 5\tab  0\tab 35\cr
#'5\tab 1\tab 4\tab  2\tab 29\cr
#'5\tab 1\tab 3\tab 19\tab 18\cr
#'5\tab 1\tab 2\tab 23\tab 9\cr
#'5\tab 1\tab 1\tab 18\tab 0\cr
#'5\tab 2\tab 5\tab  0\tab 17\cr
#'5\tab 2\tab 4\tab  2\tab 27\cr
#'5\tab 2\tab 3\tab  6\tab 24\cr
#'5\tab 2\tab 2\tab 10\tab 0\cr
#'5\tab 2\tab 1\tab 30\tab 0\cr
#'5\tab 3\tab 5\tab  2\tab 34\cr
#'5\tab 3\tab 4\tab 25\tab 33\cr
#'5\tab 3\tab 3\tab 40\tab 7\cr
#'5\tab 3\tab 2\tab 29\tab 13\cr
#'5\tab 3\tab 1\tab 24\tab 2\cr
#'5\tab 4\tab 5\tab  1\tab 12\cr
#'5\tab 4\tab 4\tab  1\tab 16\cr
#'5\tab 4\tab 3\tab  4\tab 21\cr
#'5\tab 4\tab 2\tab 24\tab 35\cr
#'5\tab 4\tab 1\tab 32\tab 15}
#'---------------------------------------------------------------------------------------------------
#'
#' @seealso
#' \code{\link{dataList.Chakra.Web} }
#'  \code{\link{dataList.Chakra.Web.orderd} }
#'   \code{\link{d} }
#'
#' @name dd
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#'
#' @references Example data of Jafroc software
#' @examples
#'
#'
#'     viewdata(BayesianFROC::dd)
#'

NULL


























#' @title Multiple reader and Multiple modality data
#' @description This is used to build a hierarchical FROC model.
#' @details This data appeared in Chakraborty's JAFROC. I have  ordered so that the modality ID means the  order of AUC. For example modality ID = 1 means its AUC is the highest. modalityID = 2 means its AUC is the second.
#' @name dataList.Chakra.Web.orderd
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#'
#' @references Maximum likelihood analysis of free-response  receiver operating characteristic (FROC) data, Dev P. Chakraborty.
#'
#  @keywords Single reader and Single modality data. Non-hierarchical FROC data.
# devtools::document();help(dataList.Chakra.1)
NULL













#' @title Multiple reader and Multiple modality data
#' @description This is used to build a hierarchical FROC model.
#' @details This data is fictitious.
#' @name  data.hier.ficitious
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#'
#' @references The author' preprint
#  @keywords Single reader and Single modality data. Non-hierarchical FROC data.
# devtools::document();help(dataList.Chakra.1)
NULL


#' @title Multiple reader and Multiple modality data
#' @description This is used to build a hierarchical FROC model. This data is same as dataList.Chakra.Web.
#' @details This data appeared in Chakraborty's paper (1988)
#' @name  data.MultiReaderMultiModality
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#'
#' @references Maximum likelihood analysis of free-response  receiver operating characteristic (FROC) data, Dev P. Chakraborty.
#'
#  @keywords Single reader and Single modality data. Hierarchical FROC data.
# devtools::document();help(dataList.Chakra.1)
NULL



#' @title Multiple reader and one modality data for fit_MRMC_versionTWO
#' @description This is used to build a hierarchical FROC model.
#' @details This data contains only one modality. If see = 12, then the model has converged.
#' @name  dataList.one.modality
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#'
#' @references  Nothing in 2018
#'
#  @keywords Multiple readers and Single modality data.
# devtools::document();help(dataList.Chakra.1)
NULL







