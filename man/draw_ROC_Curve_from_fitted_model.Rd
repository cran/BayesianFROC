% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ROC.R
\name{draw_ROC_Curve_from_fitted_model}
\alias{draw_ROC_Curve_from_fitted_model}
\title{Title}
\usage{
draw_ROC_Curve_from_fitted_model(StanS4class, plot_empirical_curves = FALSE)
}
\arguments{
\item{StanS4class}{An S4 object of class \emph{\code{ \link{stanfitExtended}}} which is an inherited class from the S4 class  \code{stanfit}.
This \R object is a fitted model object
 as a return value of the function \code{\link{fit_Bayesian_FROC}()}.

To be passed to \code{\link{DrawCurves}()} ... etc}

\item{plot_empirical_curves}{A logical, if it is true, then the empirical curve is drawn in the same plane.}
}
\description{
Title
}
