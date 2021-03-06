% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sbc_new.R
\name{flat_one_par}
\alias{flat_one_par}
\title{Makes array names}
\usage{
flat_one_par(n, d, col_major = FALSE)
}
\arguments{
\item{n}{A character, \code{n} is an abbreviation of \code{name}}

\item{d}{A vector of integers, to be passed to \code{\link{seq_array_ind}()}}

\item{col_major}{A logical, to be passed to \code{\link{seq_array_ind}()}}
}
\value{
a vector of characters
}
\description{
Makes array names
}
\examples{

 a<-flat_one_par("a",1:3)

#  > a
#  [1] "a[1,1,1]" "a[1,1,2]" "a[1,1,3]" "a[1,2,1]" "a[1,2,2]" "a[1,2,3]"
}
\author{
Some Stan developer, I am not sure,..., who?
}
