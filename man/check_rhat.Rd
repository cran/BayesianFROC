% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_rhat.R
\name{check_rhat}
\alias{check_rhat}
\title{Diagnosis of MCMC sampling}
\usage{
check_rhat(StanS4class, summary = FALSE, digits = 3)
}
\arguments{
\item{StanS4class}{An S4 object of class \emph{\code{ \link{stanfitExtended}}} which is an inherited class from the S4 class  \code{stanfit}.
This \R object is a fitted model object
 as a return value of the function \code{\link{fit_Bayesian_FROC}()}.

To be passed to \code{\link{DrawCurves}()} ... etc}

\item{summary}{Logical: \code{TRUE} of \code{FALSE}. Whether to print the verbose summary. If \code{TRUE} then verbose summary is printed in the \R console. If \code{FALSE}, the output is minimal. I regret, this variable name should be verbose.}

\item{digits}{a positive integer, indicating the digit of R hat printed in R/R-studio console}
}
\value{
Logical, that is \code{TRUE} or \code{FALSE}. If model converges then \code{TRUE}, and if not \code{FALSE}.
}
\description{
This function evaluate \eqn{R} hat statistics for any fitted model object of class \code{stanfit}.
}
\details{
It evaluates whether or not r hat statistics are close to 1.
}
\references{
Gelman A. \&  Rubin, D.B. (1992). Inference from Iterative Simulation Using Multiple Sequences, Statistical Science, Volume 7, Number 4, 457-472.
}
\author{
\strong{betanalpha}, so not my function. But I modified it. So, alphanbetan is one of the standeveloper, so his function will has consensus, thus I use it.
}
