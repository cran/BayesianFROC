% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ppp_vectorization.R
\name{plot_dataset_of_ppp}
\alias{plot_dataset_of_ppp}
\title{plot datasets using calculation of ppp}
\usage{
plot_dataset_of_ppp(
  StanS4class,
  summary = FALSE,
  verbose = FALSE,
  colorindex = 6
)
}
\arguments{
\item{StanS4class}{An S4 object of class \emph{\code{ \link{stanfitExtended}}} which is an inherited class from the S4 class  \code{stanfit}.
This \R object is a fitted model object
 as a return value of the function \code{\link{fit_Bayesian_FROC}()}.

To be passed to \code{\link{DrawCurves}()} ... etc}

\item{summary}{Logical: \code{TRUE} of \code{FALSE}. Whether to print the verbose summary. If \code{TRUE} then verbose summary is printed in the \R console. If \code{FALSE}, the output is minimal. I regret, this variable name should be verbose.}

\item{verbose}{A logical, whether title in plot is verbose or not.}

\item{colorindex}{A positive integer, indicating the color of scatter plots.}
}
\value{
null
}
\description{
plot datasets using calculation of ppp
}
\examples{
\dontrun{
#========================================================================================
#                              Now single reader and single modality case only
#========================================================================================
# Fit a model to data-set "d"
# In the resulting object contained samples from posterior predictive distribution (PPD)

f <- fit_Bayesian_FROC( ite  = 1111, summary = TRUE,  cha = 1, dataList = d )


# Plot samples synthesized from posterior predictive distribution (PPD)

plot_dataset_of_ppp(f)
plot_dataset_of_ppp(f,colorindex = 1)
plot_dataset_of_ppp(f,colorindex = 2)
plot_dataset_of_ppp(f,colorindex = 3)
plot_dataset_of_ppp(f,colorindex = 4)

}#dontrun

}
