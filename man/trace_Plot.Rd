% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tracePlotFROC.R
\name{trace_Plot}
\alias{trace_Plot}
\title{Trace plot}
\usage{
trace_Plot(
  StanS4class,
  param_name = name_of_param_whose_Rhat_is_maximal(StanS4class),
  chains = 1:length(StanS4class@stan_args),
  type = 2,
  new.imaging.device = TRUE,
  omit_initial_iter = 13
)
}
\arguments{
\item{StanS4class}{An S4 object of class \emph{\code{ \link{stanfitExtended}}} which is an inherited class from the S4 class  \code{stanfit}.
This \R object is a fitted model object
 as a return value of the function \code{\link{fit_Bayesian_FROC}()}.

To be passed to \code{\link{DrawCurves}()} ... etc}

\item{param_name}{character, indicating param name.}

\item{chains}{A positive integer, indicating the number of chains in MCMC}

\item{type}{An integer, for the color of background and etc.}

\item{new.imaging.device}{Logical: \code{TRUE} of \code{FALSE}. If TRUE (default), then open a new device to draw curve.
Using this we can draw curves in same plain by new.imaging.device=FALSE.}

\item{omit_initial_iter}{A positive integer, except which from the first iteration, trace plot is drawn}
}
\description{
Trace plot
}
