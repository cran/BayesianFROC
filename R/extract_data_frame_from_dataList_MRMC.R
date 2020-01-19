
#' @title Extract sub data frame from list of FROC data
#'
#' @param dataList MRMC data list.
#'
#' @return A data frame consisting of  modality ID vector, reader ID vector, confidence vector,
#' hit vector and false alarms vector.
#' @export
#'
#' @examples
#' \donttest{


#'
#'
#'#----------------------------------------------------------------------------------------
#'#            2)          use exsisting dataset, named dddddd
#'#----------------------------------------------------------------------------------------
#'
#'
#'
#'  fit_GUI_Shiny_MRMC(DF=extract_data_frame_from_dataList_MRMC(dddddd))
#'
#'
#'
#'
#'
#'}
#'
extract_data_frame_from_dataList_MRMC <- function(dataList){


  DF=data.frame(
    m=as.integer( dataList$m),
    q=as.integer( dataList$q),
    c=as.integer( dataList$c),
    h=as.integer( dataList$h),
    f=as.integer( dataList$f)
  )

  return(DF)
  }
