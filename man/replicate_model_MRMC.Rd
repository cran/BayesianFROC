% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/validation_MRMC_Create_dataList_MRMC_Hit_from_rate_etc.R
\name{replicate_model_MRMC}
\alias{replicate_model_MRMC}
\title{Replicate Models}
\usage{
replicate_model_MRMC(
  initial.seed = 123,
  mu.truth = BayesianFROC::mu_truth,
  v.truth = BayesianFROC::v_truth,
  z.truth = BayesianFROC::z_truth,
  NI = 200,
  NL = 142,
  ModifiedPoisson = FALSE,
  replication.number = 2,
  summary = FALSE,
  ite = 1111
)
}
\arguments{
\item{initial.seed}{The variable
\code{initial.seed} is used to replicate datasets.
That is, if you take initial.seed = 1234,
then the seed 1234, 1235, 1236, 1237, 1238,
etc are for the first replication,
the second replication,
the third replication,etc.
If the n-th model does not
converge for some n,
 then such model has
 no mean and thus the
  non-convergent models
  are omitted to calculate the errors.}

\item{mu.truth}{array of dimension (M,Q).
Mean of the signal distribution
of bi-normal assumption.}

\item{v.truth}{array of dimension (M,Q).
Standard Deviation of represents the signal
 distribution of bi-normal assumption.}

\item{z.truth}{This is a parameter of the
latent Gaussian assumption for the noise distribution.}

\item{NI}{Number of Images.}

\item{NL}{Number of Lesions.}

\item{ModifiedPoisson}{Logical, that is \code{TRUE} or \code{FALSE}.

If \code{ModifiedPoisson = TRUE},
then Poisson rate of false alarm is calculated \strong{\emph{per lesion}},
and a model is fitted
so that the FROC curve is an expected curve
 of points consisting of the pairs of TPF per lesion and FPF  \strong{\emph{per lesion}}.

Similarly,

If \code{ModifiedPoisson = TRUE},
then Poisson rate of false alarm is calculated \strong{\emph{per image}},
and a model is fitted
so that the FROC curve is an expected curve
 of points consisting of the pair of TPF per lesion and FPF  \strong{\emph{per image}}.



For more details, see the author's paper in which I explained \emph{per image} and \emph{per lesion}.
(for details of models, see   \href{https://cran.r-project.org/package=BayesianFROC}{ vignettes  }, now, it is omiited from this package, because the size of vignettes are large.)

If \code{ModifiedPoisson = TRUE},
 then the \emph{False Positive Fraction (FPF)} is defined as follows
 (\eqn{F_c} denotes the number of false alarms with confidence level \eqn{c} )


\deqn{ \frac{F_1+F_2+F_3+F_4+F_5}{N_L}, }

\deqn{ \frac{F_2+F_3+F_4+F_5}{N_L}, }

 \deqn{ \frac{F_3+F_4+F_5}{N_L}, }

  \deqn{ \frac{F_4+F_5}{N_L}, }

   \deqn{ \frac{F_5}{N_L}, }

where \eqn{N_L} is a number of lesions (signal).
To emphasize its denominator  \eqn{N_L},
we also call it the \emph{False Positive Fraction (FPF)} \strong{per lesion}.


On the other hand,


if \code{ModifiedPoisson = FALSE} (Default), then
\emph{False Positive Fraction (FPF)} is given by

\deqn{ \frac{F_1+F_2+F_3+F_4+F_5}{N_I}, }

\deqn{ \frac{F_2+F_3+F_4+F_5}{N_I}, }

 \deqn{ \frac{F_3+F_4+F_5}{N_I}, }

  \deqn{ \frac{F_4+F_5}{N_I}, }

   \deqn{ \frac{F_5}{N_I}, }

where \eqn{N_I} is the number of images (trial).
To emphasize its denominator \eqn{N_I},
we also call it the \emph{False Positive Fraction (FPF)} \strong{per image}.


The model is fitted so that
the estimated FROC curve can be ragraded
 as the expected pairs of   FPF per image and TPF per lesion (\code{ModifiedPoisson = FALSE })

 or as the expected pairs of   FPF per image and TPF per lesion  (\code{ModifiedPoisson = TRUE})

If \code{ModifiedPoisson = TRUE}, then FROC curve means the expected pair of FPF \strong{per lesion} and TPF.

On the other hand, if  \code{ModifiedPoisson = FALSE}, then FROC curve means the expected pair of \strong{FPF per image} and TPF.




So,data of FPF and TPF are changed thus, a fitted model is also changed whether  \code{ModifiedPoisson = TRUE} or \code{FALSE}.
In traditional FROC analysis, it uses only per images (trial). Since we can divide one image into two images or more images, number of
trial is not important. And more important is per signal. So, the author also developed FROC theory to consider FROC analysis under per signal.
One can see that the FROC curve is rigid with respect to change of a number of images, so, it does not matter whether \code{ModifiedPoisson = TRUE} or \code{FALSE}.
This rigidity of curves means that the number of images is redundant parameter for the FROC trial and
thus the author try to exclude it.


Revised 2019 Dec 8
Revised 2019 Nov 25
Revised 2019 August 28}

\item{replication.number}{For fixed number of lesions, images, the dataset of hits and false alarms are replicated, and the number of replicated datasets are specified by this variable.}

\item{summary}{Logical: \code{TRUE} of \code{FALSE}. Whether to print the verbose summary. If \code{TRUE} then verbose summary is printed in the \R console. If \code{FALSE}, the output is minimal. I regret, this variable name should be verbose.}

\item{ite}{A variable to be passed to the function \code{rstan::}\code{sampling}() of \pkg{rstan}  in which it is named \code{iter}. A positive integer representing  the  number of samples synthesized by Hamiltonian Monte Carlo method,
and, Default = 1111}
}
\value{
A list, each component is an S4 object of class \code{\link{stanfitExtended}}.

  Revised 2019 Nov 7
}
\description{
Replicate Models For
Replicated Data From True Distributions.
}
\examples{

\dontrun{
#========================================================================================
#   Plot  FROC curves  for a single model in the replicated models
#========================================================================================


  list.of.fitted.model.objects  <- replicate_model_MRMC(replication.number = 2)

 DrawCurves(StanS4class =   list.of.fitted.model.objects[[2]],
            modalityID  = 1:list.of.fitted.model.objects[[2]]@dataList$M,
            readerID    = 1:list.of.fitted.model.objects[[2]]@dataList$Q )


#  Revised 2019 Sept 9

}
}
