% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_data_frame_from_dataList_MRMC.R
\name{extract_data_frame_from_dataList_srsc}
\alias{extract_data_frame_from_dataList_srsc}
\title{extract data frame from datalist in case of srsc}
\usage{
extract_data_frame_from_dataList_srsc(dataList)
}
\arguments{
\item{dataList}{A list of MRMC data.}
}
\value{
data frame
}
\description{
extract data frame from datalist in case of srsc
}
\examples{

dat <- list(c=c(3,2,1),    #     Confidence level. Note that c is ignored.
            h=c(97,32,31), #     Number of hits for each confidence level
            f=c(1,14,74),  #     Number of false alarms for each confidence level

            NL=259,        #     Number of lesions
            NI=57,         #     Number of images
            C=3)           #     Number of confidence level


 extract_data_frame_from_dataList_srsc(d)

}
