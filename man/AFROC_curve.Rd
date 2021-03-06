% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FROC_via_ggplot.R
\name{AFROC_curve}
\alias{AFROC_curve}
\title{FROC curve as an embedding map}
\usage{
AFROC_curve(x, a = 0.13, b = 0.19)
}
\arguments{
\item{x}{A real number which moves in the domain  of FROC curve}

\item{a}{a generated parameter of model which characterize AFROC curve}

\item{b}{a generated parameter of model which characterize AFROC curve}
}
\value{
none
}
\description{
FROC curve as an embedding map
}
\details{
Technique of plotting AFROC is difficult
 because it has two points in which
 the gradients are infinity
  and it causes the following warinings. Revised 2019 Nov. 20

Warning messages:
1: In stats::qnorm(exp(1 - x)) : NaNs produced
2: In stats::qnorm(exp(1 - x)) : NaNs produced
3: Removed 50 rows containing missing values (geom_path).
}
\examples{

# This function is under construction.
x <- runif(1000,1,10)
y <- AFROC_curve(x)
plot(x,y)


}
