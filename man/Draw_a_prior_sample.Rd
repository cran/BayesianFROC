% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Simulation_Based_Calibration.R
\name{Draw_a_prior_sample}
\alias{Draw_a_prior_sample}
\title{Draw One Sample from Prior}
\usage{
Draw_a_prior_sample(sd = 5, C = 5, seed.for.drawing.a.prior.sample = 1111)
}
\arguments{
\item{sd}{Standard deviation of priors. Very large number.}

\item{C}{No. of Confidence level}

\item{seed.for.drawing.a.prior.sample}{seed}
}
\value{
w, v, m, dz, z
}
\description{
Draw One Sample from Prior
}
\examples{

\dontrun{

     Draw.a.prior.sample <- Draw_a_prior_sample()

}# dottest

}
