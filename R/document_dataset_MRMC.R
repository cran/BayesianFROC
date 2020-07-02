

#' @title An FROC Data of Multiple-Reader and Multiple-Modality
#' @description A list,
#'  representing FROC data in case of MRMC.
#' @details This data is based on
#' an example data of Chakraborty's JAFROC software.
#' The author have calculated hits
#'  and false alarms from this
#'   example data formulated for Jafroc.
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
#  dataList.Chakra.Web ----
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
#' @description A list, representing FROC data of MRMC. This is same as \code{\link{dataList.Chakra.Web} }.
#' @details This data is based on in Chakraborty's JAFROC software
#' in which example data exists.
#'  The author have calculated hits and false alarms
#'  from this Jafroc example data.
#'
#'
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
# dd -------
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#'
#' @references Example data of Jafroc software
#' @examples
#'
#'
#'     viewdata(BayesianFROC::dd)
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#   dd  is same as dataList.Chakra.Web,  since the following code is all TRUE
#'#========================================================================================
#'
#'     dd$f==dataList.Chakra.Web$f
#'
#'
#'
#'
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                           Code to make the dataset dd
#'#========================================================================================
#'
#'
#'
#'
#'
#'
#' h<-c(
#'   50,30,11,5,1,15,29,29,1,0,39,31,8 ,10,3,10,8 ,25,45,14, # modality 1
#'   52,25,13,4,1,27,28,29,1,0,53,29,13,2 ,4,9 ,16,22,43,14, # modality 2
#'   43,29,11,6,0,18,29,21,0,0,43,29,6 ,7 ,1,10,14,19,32,23, # modality 3
#'   61,19,12,9,3,16,29,34,1,0,52,29,10,4 ,3,10,16,23,43,15, # modality 4
#'   35,29,18,9,0,17,27,24,0,0,34,33,7 ,13,2,12,16,21,35,15  # modality 5
#' )
#'
#' f <-c(
#'   0 ,4,20,29,21,0,0,6,15,22,1 ,15,18,31,19,1,2,4,16,17,# modality 1
#'   1 ,1,21,24,23,1,1,5,30,40,2 ,19,31,56,42,2,0,2,30,32,# modality 2
#'   1, 7,13,28,19,0,1,7, 7,31, 7,15,28,41,9 ,0,2,5,24,31,# modality 3
#'   1, 4,18,21,23,1,1,0,11,35, 6,14,37,36,18,0,2,4,18,25,# modality 4
#'   0, 2,19,23,18,0,2,6,10,30, 2,25,40,29,24,1,1,4,24,32)# modality 5
#'
#' a   <- m_q_c_vector_from_M_Q_C(5,4,5)
#'
#' m <- a$m
#' c <- a$c
#' q <- a$q
#'
#' NI<-199
#' NL <-142
#' C<-5
#' M<-5
#' Q<-4
#'
#' dd <- list(
#'   h=h,
#'   f=f,
#'   m=m,
#'   c=c,
#'   q=q,
#'   NI=NI,
#'   NL=NL,
#'   M=M,
#'   Q=Q,
#'   C=C
#' )
#'
#'
#'
#'
#'

NULL



















#' @title Multiple Reader and Multiple Modality Data
#' @description A list, representing FROC data of MRMC. This is same as \code{\link{dataList.Chakra.Web} }.
#' @details This data is based on in Chakraborty's JAFROC software
#' in which example data exists.
#'  The author have calculated hits and false alarms
#'  from this Jafroc example data.
#'  Moreover the author ordered it such that the modality ID  also means
#'  its observer performance, namely Modality ID = 1 means it has the most high AUC.
#'
#'
#' @name dd.orderd
# dd.orderd -------
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#'
#cat(tabular(data.frame(m=dd.orderd$m,q=dd.orderd$q,c=dd.orderd$c,h=dd.orderd$h,f=dd.orderd$f)))
#' @section contents:
#'  \tabular{ccccc}{
#'  \strong{ModalityID } \tab   \strong{ReaderID }  \tab  \strong{ Confidence levels} \tab   \strong{No. of hits} \tab \strong{No. of false alarms} \cr




#' \code{m} \tab \code{q} \tab \code{c} \tab \code{h} \tab  \code{f}\cr
#'   -------------- \tab ------------- \tab ------------------------ \tab  ------------------- \tab ----------------\cr

#'
#' 1 \tab 1 \tab 5 \tab 61 \tab  1\cr
#' 1 \tab 1 \tab 4 \tab 19 \tab  4\cr
#' 1 \tab 1 \tab 3 \tab 12 \tab 18\cr
#' 1 \tab 1 \tab 2 \tab  9 \tab 21\cr
#' 1 \tab 1 \tab 1 \tab  3 \tab 23\cr
#' 1 \tab 2 \tab 5 \tab 16 \tab  1\cr
#' 1 \tab 2 \tab 4 \tab 29 \tab  1\cr
#' 1 \tab 2 \tab 3 \tab 34 \tab  0\cr
#' 1 \tab 2 \tab 2 \tab  1 \tab 11\cr
#' 1 \tab 2 \tab 1 \tab  0 \tab 35\cr
#' 1 \tab 3 \tab 5 \tab 52 \tab  6\cr
#' 1 \tab 3 \tab 4 \tab 29 \tab 14\cr
#' 1 \tab 3 \tab 3 \tab 10 \tab 37\cr
#' 1 \tab 3 \tab 2 \tab  4 \tab 36\cr
#' 1 \tab 3 \tab 1 \tab  3 \tab 18\cr
#' 1 \tab 4 \tab 5 \tab 10 \tab  0\cr
#' 1 \tab 4 \tab 4 \tab 16 \tab  2\cr
#' 1 \tab 4 \tab 3 \tab 23 \tab  4\cr
#' 1 \tab 4 \tab 2 \tab 43 \tab 18\cr
#' 1 \tab 4 \tab 1 \tab 15 \tab 25\cr
#' 2 \tab 1 \tab 5 \tab 52 \tab  1\cr
#' 2 \tab 1 \tab 4 \tab 25 \tab  1\cr
#' 2 \tab 1 \tab 3 \tab 13 \tab 21\cr
#' 2 \tab 1 \tab 2 \tab  4 \tab 24\cr
#' 2 \tab 1 \tab 1 \tab  1 \tab 23\cr
#' 2 \tab 2 \tab 5 \tab 27 \tab  1\cr
#' 2 \tab 2 \tab 4 \tab 28 \tab  1\cr
#' 2 \tab 2 \tab 3 \tab 29 \tab  5\cr
#' 2 \tab 2 \tab 2 \tab  1 \tab 30\cr
#' 2 \tab 2 \tab 1 \tab  0 \tab 40\cr
#' 2 \tab 3 \tab 5 \tab 53 \tab  2\cr
#' 2 \tab 3 \tab 4 \tab 29 \tab 19\cr
#' 2 \tab 3 \tab 3 \tab 13 \tab 31\cr
#' 2 \tab 3 \tab 2 \tab  2 \tab 56\cr
#' 2 \tab 3 \tab 1 \tab  4 \tab 42\cr
#' 2 \tab 4 \tab 5 \tab  9 \tab  2\cr
#' 2 \tab 4 \tab 4 \tab 16 \tab  0\cr
#' 2 \tab 4 \tab 3 \tab 22 \tab  2\cr
#' 2 \tab 4 \tab 2 \tab 43 \tab 30\cr
#' 2 \tab 4 \tab 1 \tab 14 \tab 32\cr
#' 3 \tab 1 \tab 5 \tab 50 \tab  0\cr
#' 3 \tab 1 \tab 4 \tab 30 \tab  4\cr
#' 3 \tab 1 \tab 3 \tab 11 \tab 20\cr
#' 3 \tab 1 \tab 2 \tab  5 \tab 29\cr
#' 3 \tab 1 \tab 1 \tab  1 \tab 21\cr
#' 3 \tab 2 \tab 5 \tab 15 \tab  0\cr
#' 3 \tab 2 \tab 4 \tab 29 \tab  0\cr
#' 3 \tab 2 \tab 3 \tab 29 \tab  6\cr
#' 3 \tab 2 \tab 2 \tab  1 \tab 15\cr
#' 3 \tab 2 \tab 1 \tab  0 \tab 22\cr
#' 3 \tab 3 \tab 5 \tab 39 \tab  1\cr
#' 3 \tab 3 \tab 4 \tab 31 \tab 15\cr
#' 3 \tab 3 \tab 3 \tab  8 \tab 18\cr
#' 3 \tab 3 \tab 2 \tab 10 \tab 31\cr
#' 3 \tab 3 \tab 1 \tab  3 \tab 19\cr
#' 3 \tab 4 \tab 5 \tab 10 \tab  1\cr
#' 3 \tab 4 \tab 4 \tab  8 \tab  2\cr
#' 3 \tab 4 \tab 3 \tab 25 \tab  4\cr
#' 3 \tab 4 \tab 2 \tab 45 \tab 16\cr
#' 3 \tab 4 \tab 1 \tab 14 \tab 17\cr
#' 4 \tab 1 \tab 5 \tab 35 \tab  0\cr
#' 4 \tab 1 \tab 4 \tab 29 \tab  2\cr
#' 4 \tab 1 \tab 3 \tab 18 \tab 19\cr
#' 4 \tab 1 \tab 2 \tab  9 \tab 23\cr
#' 4 \tab 1 \tab 1 \tab  0 \tab 18\cr
#' 4 \tab 2 \tab 5 \tab 17 \tab  0\cr
#' 4 \tab 2 \tab 4 \tab 27 \tab  2\cr
#' 4 \tab 2 \tab 3 \tab 24 \tab  6\cr
#' 4 \tab 2 \tab 2 \tab  0 \tab 10\cr
#' 4 \tab 2 \tab 1 \tab  0 \tab 30\cr
#' 4 \tab 3 \tab 5 \tab 34 \tab  2\cr
#' 4 \tab 3 \tab 4 \tab 33 \tab 25\cr
#' 4 \tab 3 \tab 3 \tab  7 \tab 40\cr
#' 4 \tab 3 \tab 2 \tab 13 \tab 29\cr
#' 4 \tab 3 \tab 1 \tab  2 \tab 24\cr
#' 4 \tab 4 \tab 5 \tab 12 \tab  1\cr
#' 4 \tab 4 \tab 4 \tab 16 \tab  1\cr
#' 4 \tab 4 \tab 3 \tab 21 \tab  4\cr
#' 4 \tab 4 \tab 2 \tab 35 \tab 24\cr
#' 4 \tab 4 \tab 1 \tab 15 \tab 32\cr
#' 5 \tab 1 \tab 5 \tab 43 \tab  1\cr
#' 5 \tab 1 \tab 4 \tab 29 \tab  7\cr
#' 5 \tab 1 \tab 3 \tab 11 \tab 13\cr
#' 5 \tab 1 \tab 2 \tab  6 \tab 28\cr
#' 5 \tab 1 \tab 1 \tab  0 \tab 19\cr
#' 5 \tab 2 \tab 5 \tab 18 \tab  0\cr
#' 5 \tab 2 \tab 4 \tab 29 \tab  1\cr
#' 5 \tab 2 \tab 3 \tab 21 \tab  7\cr
#' 5 \tab 2 \tab 2 \tab  0 \tab  7\cr
#' 5 \tab 2 \tab 1 \tab  0 \tab 31\cr
#' 5 \tab 3 \tab 5 \tab 43 \tab  7\cr
#' 5 \tab 3 \tab 4 \tab 29 \tab 15\cr
#' 5 \tab 3 \tab 3 \tab  6 \tab 28\cr
#' 5 \tab 3 \tab 2 \tab  7 \tab 41\cr
#' 5 \tab 3 \tab 1 \tab  1 \tab  9\cr
#' 5 \tab 4 \tab 5 \tab 10 \tab  0\cr
#' 5 \tab 4 \tab 4 \tab 14 \tab  2\cr
#' 5 \tab 4 \tab 3 \tab 19 \tab  5\cr
#' 5 \tab 4 \tab 2 \tab 32 \tab 24\cr
#' 5 \tab 4 \tab 1 \tab 23 \tab 31
#' }
#'
#' @references Example data of Jafroc software
#' @examples
#'
#'
#'     viewdata(BayesianFROC::dd.orderd)
#'

#'
#'
#'
#'
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                           Code to make the dataset dd
#'#========================================================================================
#'
#'
#' h<-c(
#'   61,19,12,9,3,16,29,34,1,0,52,29,10,4 ,3,10,16,23,43,15, # modality 4 of dataset dd
#'   52,25,13,4,1,27,28,29,1,0,53,29,13,2 ,4,9 ,16,22,43,14, # modality 2 of dataset dd
#'   50,30,11,5,1,15,29,29,1,0,39,31,8 ,10,3,10,8 ,25,45,14, # modality 1 of dataset dd
#'   35,29,18,9,0,17,27,24,0,0,34,33,7 ,13,2,12,16,21,35,15,  # modality 5 of dataset dd
#'   43,29,11,6,0,18,29,21,0,0,43,29,6 ,7 ,1,10,14,19,32,23 # modality 3   of dataset dd
#'
#' )
#'
#' f <-c(
#'   1, 4,18,21,23,1,1,0,11,35, 6,14,37,36,18,0,2,4,18,25,# modality 4  of dataset dd
#'   1 ,1,21,24,23,1,1,5,30,40,2 ,19,31,56,42,2,0,2,30,32,# modality 2 of dataset dd
#'   0 ,4,20,29,21,0,0,6,15,22,1 ,15,18,31,19,1,2,4,16,17,# modality 1 of dataset dd
#'   0, 2,19,23,18,0,2,6,10,30, 2,25,40,29,24,1,1,4,24,32,# modality 5 of dataset dd
#'   1, 7,13,28,19,0,1,7, 7,31, 7,15,28,41,9 ,0,2,5,24,31# modality 3 of dataset dd
#'
#' )
#'
#' a   <- m_q_c_vector_from_M_Q_C(5,4,5)
#'
#' m <- a$m
#' c <- a$c
#' q <- a$q
#'
#' NI<-199
#' NL <-142
#' C<-5
#' M<-5
#' Q<-4
#'
#' dd.orderd <- list(
#'   h=h,
#'   f=f,
#'   m=m,
#'   c=c,
#'   q=q,
#'   NI=NI,
#'   NL=NL,
#'   M=M,
#'   Q=Q,
#'   C=C
#' )
NULL
























#' @title An FROC Data of Multiple-Reader and Multiple-Modality
#' @description To be fitted an FROC model.
#' @details This data was calculated from an example dataset which appears in Chakraborty's JAFROC.
#' The author has ordered
#' the dataset \code{\link{dataList.Chakra.Web}} (or  \code{\link{dd}} )
#'  so that the modality ID means the  order of AUC.
#' For example modality ID = 1 means its AUC is the highest.
#' modalityID = 2 means that
#'  its AUC is the secondly high AUC.
#'
#'
#' So, let \eqn{A_1,A_2,A_3,A_4,A_5} be the AUCs
#'  for the modality ID \eqn{1,2,3,4,5}, respectively.
#'
#'
#'     Then it follows that
#'
#'\deqn{A_1 >  A_2 > A_3 > A_4 > A_5.}
#'
#'
#' So, modality ID in this dataset corresponds
#'   the modality ID
#'    of \code{\link{dataList.Chakra.Web}} (or  \code{\link{dd}} )
#'     as  (4  2 1 5 3).
#'
#'
#' That is, let us denote the modality ID of this dataset
#'  (1',2',3',4',5') and
#'  let  modality ID
#'   of the dataset named \code{\link{dataList.Chakra.Web}} (or  \code{\link{dd}} ) be (1,2,3,4,5).
#'
#'
#'    Then we can write the correspondence as follows;
#'
#'
#'
#'\deqn{(1',2',3',4',5') = (4,  2, 1, 5, 3).}
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
#'   --------------------- \tab -------------------- \tab ----------------------------- \tab  ------------------------- \tab -----------------------\cr

#'1\tab 1\tab 5\tab  1\tab 61\cr
#'1\tab 1\tab 4\tab  4\tab 19\cr
#'1\tab 1\tab 3\tab 18\tab 12\cr
#'1\tab 1\tab 2\tab 21\tab 9\cr
#'1\tab 1\tab 1\tab 23\tab 3\cr
#'1\tab 2\tab 5\tab  1\tab 16\cr
#'1\tab 2\tab 4\tab  1\tab 29\cr
#'1\tab 2\tab 3\tab  0\tab 34\cr
#'1\tab 2\tab 2\tab 11\tab 1\cr
#'1\tab 2\tab 1\tab 35\tab 0\cr
#'1\tab 3\tab 5\tab  6\tab 52\cr
#'1\tab 3\tab 4\tab 14\tab 29\cr
#'1\tab 3\tab 3\tab 37\tab 10\cr
#'1\tab 3\tab 2\tab 36\tab 4\cr
#'1\tab 3\tab 1\tab 18\tab 3\cr
#'1\tab 4\tab 5\tab  0\tab 10\cr
#'1\tab 4\tab 4\tab  2\tab 16\cr
#'1\tab 4\tab 3\tab  4\tab 23\cr
#'1\tab 4\tab 2\tab 18\tab 43\cr
#'1\tab 4\tab 1\tab 25\tab 15\cr
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
#'3\tab 1\tab 5\tab  0\tab 50\cr
#'3\tab 1\tab 4\tab  4\tab 30\cr
#'3\tab 1\tab 3\tab 20\tab 11\cr
#'3\tab 1\tab 2\tab 29\tab 5\cr
#'3\tab 1\tab 1\tab 21\tab 1\cr
#'3\tab 2\tab 5\tab  0\tab 15\cr
#'3\tab 2\tab 4\tab  0\tab 29\cr
#'3\tab 2\tab 3\tab  6\tab 29\cr
#'3\tab 2\tab 2\tab 15\tab 1\cr
#'3\tab 2\tab 1\tab 22\tab 0\cr
#'3\tab 3\tab 5\tab  1\tab 39\cr
#'3\tab 3\tab 4\tab 15\tab 31\cr
#'3\tab 3\tab 3\tab 18\tab 8\cr
#'3\tab 3\tab 2\tab 31\tab 10\cr
#'3\tab 3\tab 1\tab 19\tab 3\cr
#'3\tab 4\tab 5\tab  1\tab 10\cr
#'3\tab 4\tab 4\tab  2\tab 8\cr
#'3\tab 4\tab 3\tab  4\tab 25\cr
#'3\tab 4\tab 2\tab 16\tab 45\cr
#'3\tab 4\tab 1\tab 17\tab 14\cr
#'4\tab 1\tab 5\tab  0\tab 35\cr
#'4\tab 1\tab 4\tab  2\tab 29\cr
#'4\tab 1\tab 3\tab 19\tab 18\cr
#'4\tab 1\tab 2\tab 23\tab 9\cr
#'4\tab 1\tab 1\tab 18\tab 0\cr
#'4\tab 2\tab 5\tab  0\tab 17\cr
#'4\tab 2\tab 4\tab  2\tab 27\cr
#'4\tab 2\tab 3\tab  6\tab 24\cr
#'4\tab 2\tab 2\tab 10\tab 0\cr
#'4\tab 2\tab 1\tab 30\tab 0\cr
#'4\tab 3\tab 5\tab  2\tab 34\cr
#'4\tab 3\tab 4\tab 25\tab 33\cr
#'4\tab 3\tab 3\tab 40\tab 7\cr
#'4\tab 3\tab 2\tab 29\tab 13\cr
#'4\tab 3\tab 1\tab 24\tab 2\cr
#'4\tab 4\tab 5\tab  1\tab 12\cr
#'4\tab 4\tab 4\tab  1\tab 16\cr
#'4\tab 4\tab 3\tab  4\tab 21\cr
#'4\tab 4\tab 2\tab 24\tab 35\cr
#'4\tab 4\tab 1\tab 32\tab 15\cr
#'5\tab 1\tab 5\tab  1\tab 43\cr
#'5\tab 1\tab 4\tab  7\tab 29\cr
#'5\tab 1\tab 3\tab 13\tab 11\cr
#'5\tab 1\tab 2\tab 28\tab 6\cr
#'5\tab 1\tab 1\tab 19\tab 0\cr
#'5\tab 2\tab 5\tab  0\tab 18\cr
#'5\tab 2\tab 4\tab  1\tab 29\cr
#'5\tab 2\tab 3\tab  7\tab 21\cr
#'5\tab 2\tab 2\tab  7\tab 0\cr
#'5\tab 2\tab 1\tab 31\tab 0\cr
#'5\tab 3\tab 5\tab  7\tab 43\cr
#'5\tab 3\tab 4\tab 15\tab 29\cr
#'5\tab 3\tab 3\tab 28\tab 6\cr
#'5\tab 3\tab 2\tab 41\tab 7\cr
#'5\tab 3\tab 1\tab  9\tab 1\cr
#'5\tab 4\tab 5\tab  0\tab 10\cr
#'5\tab 4\tab 4\tab  2\tab 14\cr
#'5\tab 4\tab 3\tab  5\tab 19\cr
#'5\tab 4\tab 2\tab 24\tab 32\cr
#'5\tab 4\tab 1\tab 31\tab 23}
#'---------------------------------------------------------------------------------------------------
#'
#' @seealso \code{\link{dataList.Chakra.Web} } \code{\link{d} }
#'
#' @name dataList.Chakra.Web.orderd
#  dataList.Chakra.Web.orderd ----------
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#'
#' @references Maximum likelihood analysis of free-response  receiver operating characteristic (FROC) data, Dev P. Chakraborty.
#'
#  @keywords A Single reader and A Single modality data. Non-hierarchical FROC data.
# devtools::document();help(dataList.Chakra.1)
NULL













#' @title Multiple reader and Multiple modality data
#' @description This is used to build a hierarchical FROC model.
#' @details This data is fictitious.
#' @name  data.hier.ficitious
# data.hier.ficitious ---------
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#'
#' @references The author' preprint
#  @keywords A Single reader and A Single modality data. Non-hierarchical FROC data.
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
# devtools::document();help(dataList.Chakra.1)
NULL



#' @title dataset of Multiple reader and one modality
#' @description This is used to build a hierarchical FROC model.
#' @details This data contains only one modality. If see = 12, then the model has converged.
#' @name  dataList.one.modality
# dataList.one.modality-------------
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#'
#' @references  Nothing in 2018
#'
#  @keywords Multiple readers and Single modality data.
# devtools::document();help(dataList.Chakra.1)
NULL







#' @title Multiple reader and Multiple modality data
#' @description This is a subset of \code{ \link{dd}}
#'
#' This dataset has different dimesion for each moality and reader and confidence levels.
#' To confirm my program is correct, the author made this.
#'
#' In  the following I emphasis
#' that this data set has distinct \code{C,M,Q}:
#'
#' \describe{
#'\item{ ddd$C   }{ 5 Confidence levels }
#'\item{ ddd$M   }{ 3 modalities   }
#'\item{ ddd$Q   }{ 4 readers}
#'}
#'
#'
#' So, all number, i.e. \code{M,C,Q} is \emph{different} each other and this is the reason why the author made this dataset.
#'
#'
#'
#'
#'@details The WAIC is finite which surprizes me,
#' because a dataset  \code{dd} has no finite WAIC. Why??
#'
#'I forgot when I wrote this and what model was fitted to this data, so
#'I am not sure the current model has finite WAIC.
#'
#'Revised 2019 Nov. 21
#'
#' \strong{Contents of dd}
#'
#'
#'  \code{NL}  = 142 (Number of Lesions)
#'
#'  \code{NI} = 199 (Number of Images)

#'
#'
#'
#'
#'
#'
#'
#'
#'---------------------------------------------------------------------------------------------------
#' \tabular{ccccc}{
#'  \strong{ModalityID } \tab   \strong{ReaderID }  \tab  \strong{ Confidence levels} \tab   \strong{No. of false alarms} \tab   \strong{No. of hits}.\cr
#'   \code{m} \tab  \code{ q}  \tab   \code{c} \tab  \code{ f} \tab \code{ h}\cr
#'     -------------- \tab ------------- \tab ------------------------ \tab  ------------------- \tab ----------------\cr
#' 1 \tab 1 \tab 5 \tab  0 \tab 50\cr
#' 1 \tab 1 \tab 4 \tab  4 \tab 30\cr
#' 1 \tab 1 \tab 3 \tab 20 \tab 11\cr
#' 1 \tab 1 \tab 2 \tab 29 \tab  5\cr
#' 1 \tab 1 \tab 1 \tab 21 \tab  1\cr
#' 1 \tab 2 \tab 5 \tab  0 \tab 15\cr
#' 1 \tab 2 \tab 4 \tab  0 \tab 29\cr
#' 1 \tab 2 \tab 3 \tab  6 \tab 29\cr
#' 1 \tab 2 \tab 2 \tab 15 \tab  1\cr
#' 1 \tab 2 \tab 1 \tab 22 \tab  0\cr
#' 1 \tab 3 \tab 5 \tab  1 \tab 39\cr
#' 1 \tab 3 \tab 4 \tab 15 \tab 31\cr
#' 1 \tab 3 \tab 3 \tab 18 \tab  8\cr
#' 1 \tab 3 \tab 2 \tab 31 \tab 10\cr
#' 1 \tab 3 \tab 1 \tab 19 \tab  3\cr
#' 1 \tab 4 \tab 5 \tab  1 \tab 10\cr
#' 1 \tab 4 \tab 4 \tab  2 \tab  8\cr
#' 1 \tab 4 \tab 3 \tab  4 \tab 25\cr
#' 1 \tab 4 \tab 2 \tab 16 \tab 45\cr
#' 1 \tab 4 \tab 1 \tab 17 \tab 14\cr
#' 2 \tab 1 \tab 5 \tab  1 \tab 52\cr
#' 2 \tab 1 \tab 4 \tab  1 \tab 25\cr
#' 2 \tab 1 \tab 3 \tab 21 \tab 13\cr
#' 2 \tab 1 \tab 2 \tab 24 \tab  4\cr
#' 2 \tab 1 \tab 1 \tab 23 \tab  1\cr
#' 2 \tab 2 \tab 5 \tab  1 \tab 27\cr
#' 2 \tab 2 \tab 4 \tab  1 \tab 28\cr
#' 2 \tab 2 \tab 3 \tab  5 \tab 29\cr
#' 2 \tab 2 \tab 2 \tab 30 \tab  1\cr
#' 2 \tab 2 \tab 1 \tab 40 \tab  0\cr
#' 2 \tab 3 \tab 5 \tab  2 \tab 53\cr
#' 2 \tab 3 \tab 4 \tab 19 \tab 29\cr
#' 2 \tab 3 \tab 3 \tab 31 \tab 13\cr
#' 2 \tab 3 \tab 2 \tab 56 \tab  2\cr
#' 2 \tab 3 \tab 1 \tab 42 \tab  4\cr
#' 2 \tab 4 \tab 5 \tab  2 \tab  9\cr
#' 2 \tab 4 \tab 4 \tab  0 \tab 16\cr
#' 2 \tab 4 \tab 3 \tab  2 \tab 22\cr
#' 2 \tab 4 \tab 2 \tab 30 \tab 43\cr
#' 2 \tab 4 \tab 1 \tab 32 \tab 14\cr
#' 3 \tab 1 \tab 5 \tab  1 \tab 43\cr
#' 3 \tab 1 \tab 4 \tab  7 \tab 29\cr
#' 3 \tab 1 \tab 3 \tab 13 \tab 11\cr
#' 3 \tab 1 \tab 2 \tab 28 \tab  6\cr
#' 3 \tab 1 \tab 1 \tab 19 \tab  0\cr
#' 3 \tab 2 \tab 5 \tab  0 \tab 18\cr
#' 3 \tab 2 \tab 4 \tab  1 \tab 29\cr
#' 3 \tab 2 \tab 3 \tab  7 \tab 21\cr
#' 3 \tab 2 \tab 2 \tab  7 \tab  0\cr
#' 3 \tab 2 \tab 1 \tab 31 \tab  0\cr
#' 3 \tab 3 \tab 5 \tab  7 \tab 43\cr
#' 3 \tab 3 \tab 4 \tab 15 \tab 29\cr
#' 3 \tab 3 \tab 3 \tab 28 \tab  6\cr
#' 3 \tab 3 \tab 2 \tab 41 \tab  7\cr
#' 3 \tab 3 \tab 1 \tab  9 \tab  1\cr
#' 3 \tab 4 \tab 5 \tab  0 \tab 10\cr
#' 3 \tab 4 \tab 4 \tab  2 \tab 14\cr
#' 3 \tab 4 \tab 3 \tab  5 \tab 19\cr
#' 3 \tab 4 \tab 2 \tab 24 \tab 32\cr
#' 3 \tab 4 \tab 1 \tab 31 \tab 23
#' }
#'---------------------------------------------------------------------------------------------------
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'@examples
#' ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#              make an object ddd from an object dd
#'#========================================================================================
#'
#'
#'
#'            ddd  <-  data.frame(m=dd$m,q=dd$q,c=dd$c,h=dd$h,f=dd$f)
#'
#'            dddd <-  ddd[ddd$m <4,]  #  Reduce the dataset ddd, i.e., dd
#'
#'ddd <- list(
#'            m=dddd$m,
#'            q=dddd$q,
#'            c=dddd$c,
#'            h=dddd$h,
#'            f=dddd$f,
#'            NL=142,
#'            NI=199, # 2020 April 6
#'            C=max(dddd$c),
#'            M=max(dddd$m),
#'            Q=max(dddd$q)
#'         )
#'
#'
#'
#' @name ddd
# ddd---------------
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#'
#' @references  Nothing in 2018
#'
#  @keywords Multiple readers and Single modality data.
# devtools::document();help(dataList.Chakra.1)
NULL


















#' @title  One reader and Multiple modality data
#' @description This is a subset of \code{ \link{dd}}. For this dataset, the function \code{\link{fit_Bayesian_FROC}() } well works.
#' So, even if the number of reader is one, my programm is available. Even if not available, I think it does not cause my model but my programming.
#'
#'
#' \describe{
#'\item{ dddd$M   }{  5 modalities   }
#'\item{ dddd$C   }{   5 Confidence levels }
#'\item{ dddd$Q   }{    1 readers}
#'}
#'
#'
#'
#'
#'@details Model converged in 2019 Jun 21.
#'
#'
#'
#'
#' \strong{Contents of dddd}
#'
#'
#'  \code{NL}  = 142 (Number of Lesions)
#'
#'  \code{NI} = 199 (Number of Images)
#'
#'
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
#'2\tab 1\tab 5\tab  1\tab 52\cr
#'2\tab 1\tab 4\tab  1\tab 25\cr
#'2\tab 1\tab 3\tab 21\tab 13\cr
#'2\tab 1\tab 2\tab 24\tab 4\cr
#'2\tab 1\tab 1\tab 23\tab 1\cr
#'3\tab 1\tab 5\tab  1\tab 43\cr
#'3\tab 1\tab 4\tab  7\tab 29\cr
#'3\tab 1\tab 3\tab 13\tab 11\cr
#'3\tab 1\tab 2\tab 28\tab 6\cr
#'3\tab 1\tab 1\tab 19\tab 0\cr
#'4\tab 1\tab 5\tab  1\tab 61\cr
#'4\tab 1\tab 4\tab  4\tab 19\cr
#'4\tab 1\tab 3\tab 18\tab 12\cr
#'4\tab 1\tab 2\tab 21\tab 9\cr
#'4\tab 1\tab 1\tab 23\tab 3\cr
#'5\tab 1\tab 5\tab  0\tab 35\cr
#'5\tab 1\tab 4\tab  2\tab 29\cr
#'5\tab 1\tab 3\tab 19\tab 18\cr
#'5\tab 1\tab 2\tab 23\tab 9\cr
#'5\tab 1\tab 1\tab 18\tab 0\cr}
#'---------------------------------------------------------------------------------------------------
#'
#' @seealso
#' \code{\link{dataList.Chakra.Web} }
#'  \code{\link{dataList.Chakra.Web.orderd} }
#'   \code{\link{dd} }
#'
#' @name dddd
#'
# dddd -----------
#' @docType data
#' @details The reason why the author made this data \code{dddd} is it has only one reader.
#' My program well works for more than two reader and more than two modality case. However,
#' the only one modality or only one reader case is very special for programming perspective,
#' and thus the author had to confirm whether my program well works in such cases.
#' For this dataset, the function \code{\link{fit_Bayesian_FROC}() } well works.
#' So, even if in a single reader case, my programm is available. Even if not available, I think it does not cause my model but my programming.
#'
#'
#' @references Example data of Jafroc software
#' @examples
#'
#'
#'#========================================================================================
#'#                        Show data by table
#'#========================================================================================
#'
#'
#'
#'                  viewdata(BayesianFROC::dddd)
#'
#'
#'
#'
#'#========================================================================================
#'#              make an object dddd from an object dd
#'#========================================================================================
#'
#'
#'
#'            ddd  <-  data.frame(m=dd$m,q=dd$q,c=dd$c,h=dd$h,f=dd$f)
#'
#'            dddd <-  ddd[ddd$q < 2,]  #  Reduce the dataset ddd, i.e., dd
#'
#'ddd <- list(
#'            m=dddd$m,
#'            q=dddd$q,
#'            c=dddd$c,
#'            h=dddd$h,
#'            f=dddd$f,
#'            NL=142,
#'            NI=199, # 2020 April 6
#'            C=max(dddd$c),
#'            M=max(dddd$m),
#'            Q=max(dddd$q)
#'         )
#'
#'           dddd <-ddd
#'
#'
#'#========================================================================================
#'#              Fit model to the object dddd
#'#========================================================================================
#'#  Unfortunately, R CMD check require running time to be less than 5 which is difficult
#'#  for rstan::sampling(), thus, we cannot run the following from roxygen2 example.
#'#
#'#
#'#     For Fitting, execute the following R code;
#'#
#'#
#'#
NULL

































#' @title Data of MRMC; Model \strong{ \emph{ does  }} converge.
#' @name ddddd
#  ddddd ------

#' @description This is a subset of \code{ \link{dd}}.
#' In the past, this model did not converge
#'  in the \strong{Model_MRMC.stan},
#' thus I made a new stan file to get convergence estimates.
#'  The stan file named \emph{Model_Hiera_OneModalityMultipleReader_TargetFormulation.stan}.
#' Thus, even if the number of modalityt is 1, we can pool the AUCs over all readers by using this new model.
#' The author believes this pooling is the most natural, primitive, simple way.
#'
#'
#'
#' \describe{
#'\item{ ddddd$M   }{ \strong{ \emph{ 1  }}  modality  <---- ATTENTION!! }
#'\item{ ddddd$C   }{  \strong{ \emph{ 5 }}    Confidence levels }
#'\item{ ddddd$Q   }{   \strong{ \emph{ 4 }}   readers}
#'}
#'
#'@details The model \strong{ \emph{ did not }} converge both null model and alternative model in 2019 Jun 21.

#'
#'
#'
#'
#' \strong{Contents of dddd}
#'
#'
#'  \code{NL}  = 142 (Number of Lesions)
#'
#'  \code{NI} = 199 (Number of Images)#'
#'
#'
#'
#'
#'\strong{\emph{ Contents:  }}
#'
#'  \emph{          Multiple readers and multiple modalities case, i.e., MRMC case   }
#'
#'
#'
#'
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
#'1\tab 4\tab 1\tab 17\tab 14\cr}
#'---------------------------------------------------------------------------------------------------
#'
#' @seealso
#' \code{\link{dataList.Chakra.Web} }
#'  \code{\link{dataList.Chakra.Web.orderd} }
#'   \code{\link{dd} }
#'
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#'
#' @references Example data of Jafroc software
#' @examples
#'
#'
#'#========================================================================================
#'#                        Show data by table
#'#========================================================================================
#'
#'
#'
#'                         viewdata(BayesianFROC::ddddd)
#'
#'
#'
#'
#' ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                       make an object dddd from an object dd
#'#========================================================================================
#'
#'
#'
#'            ddd  <-  data.frame(m=dd$m,q=dd$q,c=dd$c,h=dd$h,f=dd$f)
#'
#'            dddd <-  ddd[ddd$m < 2,]  #  Reduce the dataset ddd, i.e., dd
#'
#'ddd <- list(
#'            m=dddd$m,
#'            q=dddd$q,
#'            c=dddd$c,
#'            h=dddd$h,
#'            f=dddd$f,
#'            NL=142,
#'            NI=199, # 2020 April 6
#'            C=max(dddd$c),
#'            M=max(dddd$m),
#'            Q=max(dddd$q)
#'         )
#'
#'           ddddd <-ddd
#'
#'
#'
#'
#'
#'
#'
NULL

















#' @title Multiple reader and one modality data
#' @name dddddd
# dddddd------------

#' @description This is a subset of \code{ \link{dd}}
#'
#'\strong{  This dataset is made, as a toy data,   } \emph{ which is a subset of data \code{dd} }
#' \describe{
#'\item{ dddddd$M   }{  2 modalities   }
#'\item{ dddddd$C   }{  3 Confidence levels }
#'\item{ dddddd$Q   }{  2 readers}
#'}
#'
#'@details The model did not converge both null model and alternative model in 2019 Jun 21.

#'
#'
#'
#'
#' \strong{Contents of dddddd}
#'
#'
#'  \code{NL}  = 142 (Number of Lesions)
#'
#'  \code{NI} = 199 (Number of Images)#'
#'
#'
#'
#'
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
#'1\tab 1\tab 3\tab 20\tab 11\cr
#'1\tab 1\tab 2\tab 29\tab 5\cr
#'1\tab 1\tab 1\tab 21\tab 1\cr
#'1\tab 2\tab 3\tab  6\tab 29\cr
#'1\tab 2\tab 2\tab 15\tab 1\cr
#'1\tab 2\tab 1\tab 22\tab 0\cr
#'2\tab 1\tab 3\tab 21\tab 13\cr
#'2\tab 1\tab 2\tab 24\tab 4\cr
#'2\tab 1\tab 1\tab 23\tab 1\cr
#'2\tab 2\tab 3\tab  5\tab 29\cr
#'2\tab 2\tab 2\tab 30\tab 1\cr
#'2\tab 2\tab 1\tab 40\tab 0\cr
#'}
#'---------------------------------------------------------------------------------------------------
#'
#' @seealso
#' \code{\link{dataList.Chakra.Web} }
#'  \code{\link{dataList.Chakra.Web.orderd} }
#'   \code{\link{dd} }
#'
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#'
#' @references Example data of Jafroc software
#' @examples
#'
#'
#'#========================================================================================
#'#                        Show data by table
#'#========================================================================================
#'
#'
#'
#'                         viewdata(dddddd)
#'
#'
#'
#'
#' ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                       make an object dddd from an object dd
#'#========================================================================================
#'
#'
#'
#' ddd  <-  data.frame(m=dd$m,q=dd$q,c=dd$c,h=dd$h,f=dd$f)
#' dddd <- ddd[ddd$q < 3,]
#'
#' # The following code extract the first and the second modality from dd
#' dddd <- dddd[dddd$m < 3,]  #  Reduce the dataset ddd, i.e., dd
#' dddd <- dddd[dddd$c <4,]
#' ddd <- list(
#'   m=dddd$m,
#'   q=dddd$q,
#'   c=dddd$c,
#'   h=dddd$h,
#'   f=dddd$f,
#'   NL=142,
#'   NI=199, # 2020 April 6
#'   C=max(dddd$c),
#'   M=max(dddd$m),
#'   Q=max(dddd$q)
#' )
#'
#' dddddd <-ddd
#'
#'
#'# This dataset is made in 2019 July 6, for the aim of easy exihibition
#'# This dataset is very minimum, and it is easy to view
#'
#'
NULL



































#' @title Multiple reader and 2 modalities data such that all modalities have same AUC.
#' @description This is a subset of \code{ \link{dataList.Chakra.Web.orderd}}
#' @details The author made this dataset to validate the scheme of Bayes factor well works in our Bayesian FROC models
#'
#'\strong{  This dataset is made for validation that wheter Bayes factor well work } \emph{ which is a subset of data \code{dataList.Chakra.Web.orderd} }
#'
#' \describe{
#'\item{ dddddd$M   }{  2 modalities of almost \strong{\emph{same}} AUC  }
#'\item{ dddddd$C   }{  3 Confidence levels }
#'\item{ dddddd$Q   }{  2 readers}
#'}
#'
#'If Bayes factor admit the null hypothesis that all modality are same, that is, 1-st and 2-nd modality of  \code{ \link{dataList.Chakra.Web.orderd}} are same,
#'then, the Bayes factor well works.
#'

#'
#'
#'
#'
#' \strong{Contents of dddddd}
#'
#'
#'  \code{NL}  = 142 (Number of Lesions)
#'
#'  \code{NI} = 199 (Number of Images)#'
#'
#'
#'
#'
#'
#'\strong{\emph{ Contents:  }}
#'
#'  \emph{          Multiple readers and multiple modalities case, i.e., MRMC case   }
#'
#'
#'

#' @seealso
#' Not \code{\link{dataList.Chakra.Web} }
#' But \code{\link{dataList.Chakra.Web.orderd} }
#'  Not \code{\link{dd} }
#'
#' @name ddddddd
# ddddddd ---------
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#'
#' @references Example data of Jafroc software
#' @examples
#'
#'
#'#========================================================================================
#'#                        Show data by table
#'#========================================================================================
#'
#'
#'
#'                         viewdata(ddddddd)
#'
#'
#'
#'
#' ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                       make an object dddd from an object dataList.Chakra.Web.orderd
#'#========================================================================================
#'
#'
#' ddd  <-  data.frame(m=dataList.Chakra.Web.orderd$m,
#'                     q=dataList.Chakra.Web.orderd$q,
#'                     c=dataList.Chakra.Web.orderd$c,
#'                     h=dataList.Chakra.Web.orderd$h,
#'                     f=dataList.Chakra.Web.orderd$f
#' )
#'
#' dddd <- ddd[ddd$q < 3,]
#'
#' # The following code extract the first and the second modality from dd
#' dddd <- dddd[dddd$m < 3,]  #  Reduce the dataset ddd, i.e., dd
#' dddd <- dddd[dddd$c <4,]
#' ddd <- list(
#'   m=dddd$m,
#'   q=dddd$q,
#'   c=dddd$c,
#'   h=dddd$h,
#'   f=dddd$f,
#'   NL=142,
#'   NI=199, # 2020 April 6
#'   C=max(dddd$c),
#'   M=max(dddd$m),
#'   Q=max(dddd$q)
#' )
#'
#' ddddddd <-ddd

#'
#'
#'# This dataset is made in 2019 July 6, for the aim of easy exihibition
#'# This dataset is very minimum, and it is easy to view
#'
#'
NULL










#' @title 36 readers and a sinle modality data
#' @description  An example data-set whose sample size is large.
#' @details Frequentist methods fails when a sample size is large.
#' Namely, p value  monotonically decreases when the sample size tends to large.
#'
#' On the other hands, in Bayesian methods, the large samples such as large readers in FROC context
#' fails the MCMC algorithm. Thus Bayesian methods is also not free from such large sample problem in this sense.
#'
#'\strong{  This dataset is made for validation that wheter Bayes factor well work } \emph{ which is a subset of data \code{dataList.Chakra.Web.orderd} }
#' \describe{
#'\item{ the number of modalities, denoted by \code{M} which is now     }{  1 modality  }
#'\item{ the number of Confidences, denoted by \code{C}  which is now   }{  5 Confidence levels }
#'\item{ the number of readers, denoted by \code{Q}  which is now       }{  36 readers}
#'}
#'
#'
#'
#' \strong{Contents of \code{data_of_36_readers_and_a_single_modality}}
#'
#'
#'  \code{NL}  = 142 (Number of Lesions)
#'
#'  \code{NI} = 57 (Number of Images)#'
#'
#'
#'
#'
#'
#'\strong{\emph{ Contents:  }}
#'
#'  \emph{          Multiple readers and multiple modalities case, i.e., MRMC case   }
#'
#' \tabular{ccccc}{
#'  \strong{ModalityID } \tab   \strong{ReaderID }  \tab  \strong{ Confidence levels} \tab   \strong{No. of false alarms} \tab   \strong{No. of hits}.\cr
#'   \code{m} \tab  \code{ q}  \tab   \code{c} \tab  \code{ f} \tab \code{ h}\cr
#'   -------------- \tab ------------- \tab ------------------------ \tab  ------------------- \tab ----------------\cr
#' 1 \tab  1 \tab 5 \tab  0 \tab 12\cr
#' 1 \tab  1 \tab 4 \tab  3 \tab 22\cr
#' 1 \tab  1 \tab 3 \tab  7 \tab 18\cr
#' 1 \tab  1 \tab 2 \tab 12 \tab 18\cr
#' 1 \tab  1 \tab 1 \tab  8 \tab 15\cr
#' 1 \tab  2 \tab 5 \tab  0 \tab 14\cr
#' 1 \tab  2 \tab 4 \tab  4 \tab 24\cr
#' 1 \tab  2 \tab 3 \tab  9 \tab 17\cr
#' 1 \tab  2 \tab 2 \tab 14 \tab 15\cr
#' 1 \tab  2 \tab 1 \tab 10 \tab  6\cr
#' 1 \tab  3 \tab 5 \tab  0 \tab 26\cr
#' 1 \tab  3 \tab 4 \tab  3 \tab 39\cr
#' 1 \tab  3 \tab 3 \tab  6 \tab 23\cr
#' 1 \tab  3 \tab 2 \tab 11 \tab 16\cr
#' 1 \tab  3 \tab 1 \tab  7 \tab  6\cr
#' 1 \tab  4 \tab 5 \tab  0 \tab  9\cr
#' 1 \tab  4 \tab 4 \tab  1 \tab 17\cr
#' 1 \tab  4 \tab 3 \tab  4 \tab 15\cr
#' 1 \tab  4 \tab 2 \tab  8 \tab 18\cr
#' 1 \tab  4 \tab 1 \tab  5 \tab 25\cr
#' 1 \tab  5 \tab 5 \tab  0 \tab  9\cr
#' 1 \tab  5 \tab 4 \tab  2 \tab 17\cr
#' 1 \tab  5 \tab 3 \tab  5 \tab 16\cr
#' 1 \tab  5 \tab 2 \tab  9 \tab 19\cr
#' 1 \tab  5 \tab 1 \tab  6 \tab 27\cr
#' 1 \tab  6 \tab 5 \tab  0 \tab 39\cr
#' 1 \tab  6 \tab 4 \tab  0 \tab 46\cr
#' 1 \tab  6 \tab 3 \tab  2 \tab 22\cr
#' 1 \tab  6 \tab 2 \tab 15 \tab 13\cr
#' 1 \tab  6 \tab 1 \tab  2 \tab  3\cr
#' 1 \tab  7 \tab 5 \tab  0 \tab  9\cr
#' 1 \tab  7 \tab 4 \tab  1 \tab 17\cr
#' 1 \tab  7 \tab 3 \tab  4 \tab 14\cr
#' 1 \tab  7 \tab 2 \tab  8 \tab 16\cr
#' 1 \tab  7 \tab 1 \tab  5 \tab 17\cr
#' 1 \tab  8 \tab 5 \tab  1 \tab 11\cr
#' 1 \tab  8 \tab 4 \tab  5 \tab 19\cr
#' 1 \tab  8 \tab 3 \tab 10 \tab 16\cr
#' 1 \tab  8 \tab 2 \tab 16 \tab 17\cr
#' 1 \tab  8 \tab 1 \tab 12 \tab 15\cr
#' 1 \tab  9 \tab 5 \tab  0 \tab 15\cr
#' 1 \tab  9 \tab 4 \tab  1 \tab 26\cr
#' 1 \tab  9 \tab 3 \tab  3 \tab 20\cr
#' 1 \tab  9 \tab 2 \tab  6 \tab 18\cr
#' 1 \tab  9 \tab 1 \tab  4 \tab 12\cr
#' 1 \tab 10 \tab 5 \tab  0 \tab 31\cr
#' 1 \tab 10 \tab 4 \tab  4 \tab 40\cr
#' 1 \tab 10 \tab 3 \tab  8 \tab 22\cr
#' 1 \tab 10 \tab 2 \tab 13 \tab 16\cr
#' 1 \tab 10 \tab 1 \tab  9 \tab  5\cr
#' 1 \tab 11 \tab 5 \tab  0 \tab 13\cr
#' 1 \tab 11 \tab 4 \tab  2 \tab 23\cr
#' 1 \tab 11 \tab 3 \tab  5 \tab 19\cr
#' 1 \tab 11 \tab 2 \tab  9 \tab 19\cr
#' 1 \tab 11 \tab 1 \tab  6 \tab 17\cr
#' 1 \tab 12 \tab 5 \tab  0 \tab  8\cr
#' 1 \tab 12 \tab 4 \tab  3 \tab 16\cr
#' 1 \tab 12 \tab 3 \tab  7 \tab 15\cr
#' 1 \tab 12 \tab 2 \tab 11 \tab 17\cr
#' 1 \tab 12 \tab 1 \tab  8 \tab 22\cr
#' 1 \tab 13 \tab 5 \tab  0 \tab 13\cr
#' 1 \tab 13 \tab 4 \tab  1 \tab 23\cr
#' 1 \tab 13 \tab 3 \tab  4 \tab 19\cr
#' 1 \tab 13 \tab 2 \tab  7 \tab 21\cr
#' 1 \tab 13 \tab 1 \tab  4 \tab 20\cr
#' 1 \tab 14 \tab 5 \tab  0 \tab 36\cr
#' 1 \tab 14 \tab 4 \tab  4 \tab 45\cr
#' 1 \tab 14 \tab 3 \tab  9 \tab 22\cr
#' 1 \tab 14 \tab 2 \tab 14 \tab 13\cr
#' 1 \tab 14 \tab 1 \tab 10 \tab  3\cr
#' 1 \tab 15 \tab 5 \tab  0 \tab 17\cr
#' 1 \tab 15 \tab 4 \tab  2 \tab 27\cr
#' 1 \tab 15 \tab 3 \tab  5 \tab 20\cr
#' 1 \tab 15 \tab 2 \tab  9 \tab 18\cr
#' 1 \tab 15 \tab 1 \tab  6 \tab 10\cr
#' 1 \tab 16 \tab 5 \tab  0 \tab  8\cr
#' 1 \tab 16 \tab 4 \tab  4 \tab 15\cr
#' 1 \tab 16 \tab 3 \tab  8 \tab 13\cr
#' 1 \tab 16 \tab 2 \tab 13 \tab 16\cr
#' 1 \tab 16 \tab 1 \tab  9 \tab 22\cr
#' 1 \tab 17 \tab 5 \tab  0 \tab  9\cr
#' 1 \tab 17 \tab 4 \tab  1 \tab 16\cr
#' 1 \tab 17 \tab 3 \tab  4 \tab 15\cr
#' 1 \tab 17 \tab 2 \tab  8 \tab 17\cr
#' 1 \tab 17 \tab 1 \tab  5 \tab 20\cr
#' 1 \tab 18 \tab 5 \tab  0 \tab 12\cr
#' 1 \tab 18 \tab 4 \tab  2 \tab 21\cr
#' 1 \tab 18 \tab 3 \tab  6 \tab 17\cr
#' 1 \tab 18 \tab 2 \tab 10 \tab 17\cr
#' 1 \tab 18 \tab 1 \tab  7 \tab 12\cr
#' 1 \tab 19 \tab 5 \tab  0 \tab 19\cr
#' 1 \tab 19 \tab 4 \tab  3 \tab 33\cr
#' 1 \tab 19 \tab 3 \tab  8 \tab 21\cr
#' 1 \tab 19 \tab 2 \tab 12 \tab 19\cr
#' 1 \tab 19 \tab 1 \tab  9 \tab 13\cr
#' 1 \tab 20 \tab 5 \tab  0 \tab  8\cr
#' 1 \tab 20 \tab 4 \tab  1 \tab 15\cr
#' 1 \tab 20 \tab 3 \tab  3 \tab 14\cr
#' 1 \tab 20 \tab 2 \tab  6 \tab 16\cr
#' 1 \tab 20 \tab 1 \tab  4 \tab 21\cr
#' 1 \tab 21 \tab 5 \tab  0 \tab 33\cr
#' 1 \tab 21 \tab 4 \tab  2 \tab 41\cr
#' 1 \tab 21 \tab 3 \tab  5 \tab 21\cr
#' 1 \tab 21 \tab 2 \tab  9 \tab 13\cr
#' 1 \tab 21 \tab 1 \tab  6 \tab  3\cr
#' 1 \tab 22 \tab 5 \tab  0 \tab 15\cr
#' 1 \tab 22 \tab 4 \tab  3 \tab 26\cr
#' 1 \tab 22 \tab 3 \tab  7 \tab 20\cr
#' 1 \tab 22 \tab 2 \tab 12 \tab 20\cr
#' 1 \tab 22 \tab 1 \tab  8 \tab 15\cr
#' 1 \tab 23 \tab 5 \tab  0 \tab  9\cr
#' 1 \tab 23 \tab 4 \tab  4 \tab 17\cr
#' 1 \tab 23 \tab 3 \tab  8 \tab 15\cr
#' 1 \tab 23 \tab 2 \tab 12 \tab 18\cr
#' 1 \tab 23 \tab 1 \tab  9 \tab 23\cr
#' 1 \tab 24 \tab 5 \tab  0 \tab 10\cr
#' 1 \tab 24 \tab 4 \tab  0 \tab 19\cr
#' 1 \tab 24 \tab 3 \tab  3 \tab 17\cr
#' 1 \tab 24 \tab 2 \tab  6 \tab 20\cr
#' 1 \tab 24 \tab 1 \tab  4 \tab 23\cr
#' 1 \tab 25 \tab 5 \tab  0 \tab  8\cr
#' 1 \tab 25 \tab 4 \tab  1 \tab 15\cr
#' 1 \tab 25 \tab 3 \tab  3 \tab 14\cr
#' 1 \tab 25 \tab 2 \tab  6 \tab 17\cr
#' 1 \tab 25 \tab 1 \tab  4 \tab 22\cr
#' 1 \tab 26 \tab 5 \tab  0 \tab 12\cr
#' 1 \tab 26 \tab 4 \tab  1 \tab 21\cr
#' 1 \tab 26 \tab 3 \tab  4 \tab 18\cr
#' 1 \tab 26 \tab 2 \tab  8 \tab 19\cr
#' 1 \tab 26 \tab 1 \tab  5 \tab 18\cr
#' 1 \tab 27 \tab 5 \tab  0 \tab 19\cr
#' 1 \tab 27 \tab 4 \tab  1 \tab 32\cr
#' 1 \tab 27 \tab 3 \tab  4 \tab 18\cr
#' 1 \tab 27 \tab 2 \tab  7 \tab 13\cr
#' 1 \tab 27 \tab 1 \tab  5 \tab  4\cr
#' 1 \tab 28 \tab 5 \tab  1 \tab 10\cr
#' 1 \tab 28 \tab 4 \tab  5 \tab 18\cr
#' 1 \tab 28 \tab 3 \tab  9 \tab 16\cr
#' 1 \tab 28 \tab 2 \tab 15 \tab 19\cr
#' 1 \tab 28 \tab 1 \tab 11 \tab 26\cr
#' 1 \tab 29 \tab 5 \tab  0 \tab 16\cr
#' 1 \tab 29 \tab 4 \tab  2 \tab 27\cr
#' 1 \tab 29 \tab 3 \tab  6 \tab 21\cr
#' 1 \tab 29 \tab 2 \tab 10 \tab 20\cr
#' 1 \tab 29 \tab 1 \tab  7 \tab 16\cr
#' 1 \tab 30 \tab 5 \tab  1 \tab  9\cr
#' 1 \tab 30 \tab 4 \tab  4 \tab 18\cr
#' 1 \tab 30 \tab 3 \tab  9 \tab 16\cr
#' 1 \tab 30 \tab 2 \tab 14 \tab 19\cr
#' 1 \tab 30 \tab 1 \tab 10 \tab 25\cr
#' 1 \tab 31 \tab 5 \tab  0 \tab 10\cr
#' 1 \tab 31 \tab 4 \tab  3 \tab 19\cr
#' 1 \tab 31 \tab 3 \tab  7 \tab 16\cr
#' 1 \tab 31 \tab 2 \tab 11 \tab 18\cr
#' 1 \tab 31 \tab 1 \tab  8 \tab 20\cr
#' 1 \tab 32 \tab 5 \tab  1 \tab 12\cr
#' 1 \tab 32 \tab 4 \tab  5 \tab 22\cr
#' 1 \tab 32 \tab 3 \tab 10 \tab 18\cr
#' 1 \tab 32 \tab 2 \tab 15 \tab 19\cr
#' 1 \tab 32 \tab 1 \tab 11 \tab 18\cr
#' 1 \tab 33 \tab 5 \tab  1 \tab 14\cr
#' 1 \tab 33 \tab 4 \tab  6 \tab 24\cr
#' 1 \tab 33 \tab 3 \tab 11 \tab 18\cr
#' 1 \tab 33 \tab 2 \tab 16 \tab 17\cr
#' 1 \tab 33 \tab 1 \tab 12 \tab 10\cr
#' 1 \tab 34 \tab 5 \tab  0 \tab 34\cr
#' 1 \tab 34 \tab 4 \tab  3 \tab 43\cr
#' 1 \tab 34 \tab 3 \tab  8 \tab 22\cr
#' 1 \tab 34 \tab 2 \tab 12 \tab 14\cr
#' 1 \tab 34 \tab 1 \tab  9 \tab  3\cr
#' 1 \tab 35 \tab 5 \tab  0 \tab  9\cr
#' 1 \tab 35 \tab 4 \tab  1 \tab 17\cr
#' 1 \tab 35 \tab 3 \tab  4 \tab 15\cr
#' 1 \tab 35 \tab 2 \tab  8 \tab 18\cr
#' 1 \tab 35 \tab 1 \tab  5 \tab 25\cr
#' 1 \tab 36 \tab 5 \tab  1 \tab 17\cr
#' 1 \tab 36 \tab 4 \tab  6 \tab 31\cr
#' 1 \tab 36 \tab 3 \tab 11 \tab 20\cr
#' 1 \tab 36 \tab 2 \tab 16 \tab 17\cr
#' 1 \tab 36 \tab 1 \tab 12 \tab  9
#' }
#'

#' @seealso
#' Not \code{\link{dataList.Chakra.Web} }
#' But \code{\link{dataList.Chakra.Web.orderd} }
#'  Not \code{\link{dd} }
#'
#' @name data_of_36_readers_and_a_single_modality
# data_of_36_readers_and_a_single_modality ---------
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#'
#' @references Example data of Jafroc software
#' @examples
#'
#'
#'#========================================================================================
#'#                        Show data by table
#'#========================================================================================
#'
#'
#'
#'                         viewdata(data_of_36_readers_and_a_single_modality)
#'
#'
#'plot_FPF_and_TPF_from_a_dataset(data_of_36_readers_and_a_single_modality)
#'
#' ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                       make this data from functions in this package
#'#========================================================================================
#'
#'
#'
#' v  <- v_truth_creator_for_many_readers_MRMC_data(M=1,Q=36)
#' m  <- mu_truth_creator_for_many_readers_MRMC_data(M=1,Q=36)
#' d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#'
#'
#' # The last object named d is the desired dataset.

#'
#'
NULL

















#' @title data: 2 readers, 2 modalities and 3 confideneces
#' @description    Example data-set which has small samples.
#' @details
#'
#' \describe{
#'\item{ the number of modalities, denoted by \code{M}.   }{ \code{M = } 2 modalities  }
#'\item{ the number of Confidences, denoted by \code{C}.   }{ \code{C = }   3 Confidence levels }
#'\item{ the number of readers, denoted by \code{Q}.  }{   \code{Q = } 2 readers}
#'}
#'
#'
#'
#' \strong{Contents }
#'
#'
#'  \code{NL}  = 142 (Number of Lesions)
#'
#'  \code{NI} = 57 (Number of Images)#'
#'
#'
#'
#'
#'
#'\strong{\emph{ Contents:  }}
#'
#'  \emph{          Multiple readers and multiple modalities case, i.e., MRMC case   }
#'
#' \tabular{ccccc}{
#'  \strong{ModalityID } \tab   \strong{ReaderID }  \tab  \strong{ Confidence levels} \tab   \strong{No. of false alarms} \tab   \strong{No. of hits}.\cr
#'   \code{m} \tab  \code{ q}  \tab   \code{c} \tab  \code{ f} \tab \code{ h}\cr
#'    -------------- \tab ------------- \tab ------------------------ \tab  ------------------- \tab ----------------\cr
#'   1 \tab 1 \tab 3 \tab 20 \tab 111\cr
#'   1 \tab 1 \tab 2 \tab 29 \tab  55\cr
#'   1 \tab 1 \tab 1 \tab 21 \tab  22\cr
#'   1 \tab 2 \tab 3 \tab  6 \tab 100\cr
#'   1 \tab 2 \tab 2 \tab 15 \tab  44\cr
#'   1 \tab 2 \tab 1 \tab 22 \tab  11\cr
#'   2 \tab 1 \tab 3 \tab  6 \tab  66\cr
#'   2 \tab 1 \tab 2 \tab 24 \tab  55\cr
#'   2 \tab 1 \tab 1 \tab 23 \tab   1\cr
#'   2 \tab 2 \tab 3 \tab  5 \tab  66\cr
#'   2 \tab 2 \tab 2 \tab 30 \tab  55\cr
#'   2 \tab 2 \tab 1 \tab 40 \tab  44\cr
#' }

#' @seealso
#' Not \code{\link{dataList.Chakra.Web} }
#' But \code{\link{dataList.Chakra.Web.orderd} }
#'  Not \code{\link{dd} }
#'
#' @name data_2modaities_2readers_3confidence
# data_2modaities_2readers_3confidence ---------
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#'
#' @references Example data of Jafroc software
#' @examples
#'
#'
#'#========================================================================================
#'#                        Show data by table
#'#========================================================================================
#'
#'
#'
#'                         viewdata(data_of_36_readers_and_a_single_modality)
#'
#'
#'plot_FPF_and_TPF_from_a_dataset(data_of_36_readers_and_a_single_modality)
#'
#' ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                       make this data from functions in this package
#'#========================================================================================
#'
#'
#'
#' v  <- v_truth_creator_for_many_readers_MRMC_data(M=1,Q=36)
#' m  <- mu_truth_creator_for_many_readers_MRMC_data(M=1,Q=36)
#' d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#'
#'
#' # The last object named d is the desired dataset.

NULL








#  \tabular{ccccc}{
#   \strong{ModalityID } \tab   \strong{ReaderID }  \tab  \strong{ Confidence levels} \tab   \strong{No. of false alarms} \tab   \strong{No. of hits}.\cr
#    \code{q} \tab  \code{ m}  \tab   \code{c} \tab  \code{ f} \tab \code{ h}\cr
#    -------------- \tab ------------- \tab ------------------------ \tab  ------------------- \tab ----------------\cr

# df<-data.frame(ddd$m,ddd$q,ddd$c,ddd$f,ddd$h)


tabular2 <- function(df, ...) {
  df<-data.frame(df$m,df$q,df$c,df$f,df$h)
  stopifnot(is.data.frame(df))

  align <- function(x) if (is.numeric(x)) "r" else "l"
  col_align <- vapply(df, align, character(1))

  cols <- lapply(df, format, ...)
  contents <- do.call("paste",
                      c(cols, list(sep = " \\tab ", collapse = "\\cr\n  ")))

  x <- paste("\\tabular{", paste(col_align, collapse = ""), "}{\n  ",
        contents, "\n}\n", sep = "")
  cat(x)
}
# cat(tabular(df))

# Using this return value, we obtain cat or message( return), then it available on roxygen2
