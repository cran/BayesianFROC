
#' @title Extract sub data frame from list of FROC data
#' @description Makes a dataframe
#'  from a list consisting of  vectors \code{m,q,c,h,f} and positive integers \code{NL,C,M,Q,NI}.
#'  The resulting data-frame is construceted by  vectors \code{m,q,c,h,f}.
#'
#' @param dataList A list of MRMC data.
#' @inheritParams fit_Bayesian_FROC
#'
#' @return A data frame consisting of  vectors \code{m,q,c,h,f}.
#' \describe{
#' \item{ \code{m }  }{A vector of positive integers,  representing  the \emph{\strong{modality}} ID vector. }
#' \item{ \code{q }  }{A vector of positive integers,  representing  the \emph{\strong{reader}} ID vector.}
#' \item{ \code{c }  }{A vector of positive integers,  representing  the \emph{\strong{confidence level}}. This vector must be made by \code{rep(rep(C:1), M*Q)} }
#' \item{ \code{h }  }{A vector of non-negative integers,  representing  the number of \emph{\strong{hits}}.   }
#' \item{ \code{f }  }{A vector of non-negative integers,  representing  the number of \emph{\strong{false alarms}}.  }
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#'#========================================================================================
#'#                      From example dataset named dddddd
#'#========================================================================================
#'
#'## Only run examples in interactive R sessions
#'if (interactive()) {
#'
#'  fit_GUI_Shiny_MRMC(DF=extract_data_frame_from_dataList_MRMC(dddddd))
#'
#'
#'}## Only run examples in interactive R sessions
#'
#'
#'}
#'
extract_data_frame_from_dataList_MRMC <- function(dataList,verbose =FALSE){


  DF=data.frame(
    m=as.integer( dataList$m),
    q=as.integer( dataList$q),
    c=as.integer( dataList$c),
    h=as.integer( dataList$h),
    f=as.integer( dataList$f)
  )

  # return(DF)
  if(verbose)  print.data.frame(DF, row.names=FALSE)
  invisible(DF)
}







#' @title  extract data frame from datalist in case of srsc
#' @description extract data frame from datalist in case of srsc
#'
#' @inheritParams  extract_data_frame_from_dataList_MRMC
#' @return data frame
#' @export
#'
#' @examples
#'
#' dat <- list(c=c(3,2,1),    #     Confidence level. Note that c is ignored.
#'             h=c(97,32,31), #     Number of hits for each confidence level
#'             f=c(1,14,74),  #     Number of false alarms for each confidence level
#'
#'             NL=259,        #     Number of lesions
#'             NI=57,         #     Number of images
#'             C=3)           #     Number of confidence level
#'
#'
#'  extract_data_frame_from_dataList_srsc(d)
#'
extract_data_frame_from_dataList_srsc  <- function(dataList) {

  # dataList <-give_name_srsc_data( dataList )

  DF=data.frame(
    h=as.integer( dataList$h),
    f=as.integer( dataList$f)
  )
  print.data.frame(DF, row.names=FALSE)
  invisible(DF)
}





