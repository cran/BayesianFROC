% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validation_error_srsc.R
\name{error_srsc}
\alias{error_srsc}
\title{Validation via replicated datasets from a model at a given model parameter}
\usage{
error_srsc(
  NLvector = c(100L, 10000L, 1000000L),
  ratio = 2,
  replicate.datset = 3,
  ModifiedPoisson = FALSE,
  mean.truth = 0.6,
  sd.truth = 5.3,
  z.truth = c(-0.8, 0.7, 2.38),
  ite = 2222,
  cha = 1,
  verbose = FALSE
)
}
\arguments{
\item{NLvector}{A vector of positive integers,
indicating a collection of numbers of Lesions.}

\item{ratio}{A positive \strong{\emph{rational}} number,
with which Number of Images is determined by the formula:
 (number of images) = \code{ratio} times (numbser of lesions).
Note that in calculation, it  rounds   \code{ratio * NLvector } to an integer.}

\item{replicate.datset}{A Number indicate
that how many you replicate dataset
from user's specified dataset.}

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

\item{mean.truth}{This is a parameter
of the latent Gaussian assumption
for the noise distribution.}

\item{sd.truth}{This is a parameter of the latent
Gaussian assumption for the noise distribution.}

\item{z.truth}{This is a parameter of the
latent Gaussian assumption for the noise distribution.}

\item{ite}{A variable to be passed to the function \code{rstan::}\code{sampling}() of \pkg{rstan}  in which it is named \code{iter}. A positive integer representing  the  number of samples synthesized by Hamiltonian Monte Carlo method,
and, Default = 1111}

\item{cha}{A variable to be passed to the function \code{rstan::}\code{sampling}() of \pkg{rstan}  in which it is named \code{chains}.  A positive integer representing   the number of chains generated by Hamiltonian Monte Carlo method,
and, Default = 1.}

\item{verbose}{A logical, if \code{TRUE}, then the redundant summary is printed in \R console.
If \code{FALSE}, it suppresses output from this function.}
}
\value{
Replicated datasets, estimates,
errors,...etc I made this program 1 years ago?
 and now I forget ... the precise return values.
When I see today, 2019 August. It retains too many return
values to explain all of them.
}
\description{
Print for a given true parameter,
  a errors of estimates from replicated dataset.

  Also print a standard error which is the variance  of estimates.



Suppose that \eqn{\theta_0} is a given true model parameter with a given number of images \eqn{N_I} and a given number of lesions \eqn{N_L}, specified by user.
\describe{
\item{  \strong{(I)}  }{}
\item{  \strong{(I.1)}   Synthesize a collection of dataset \eqn{D_k} (\eqn{k=1,2,...,K})  from a likelihood (model) at a given parameter \eqn{\theta_0}, namely  }{    \eqn{D_k  \sim}  likelihood( \eqn{\theta_0}).                                                                                 }
\item{   \strong{(I.2)}  Replicates \eqn{K}   models fitted to each dataset  \eqn{D_k} (\eqn{k=1,2,...,K}), namely, draw  MCMC samples  \eqn{\{ \theta_i (D_k);i=1,...,I\}} from each posterior of the dataset  \eqn{D_k}, namely         }{   \eqn{ \theta _i(D_k) \sim  \pi(|D_k)}.                                                                                       }
\item{   \strong{(I.3)}  Calculate  posterior means for the set of data \eqn{D_k} (\eqn{k=1,2,...,K}),  namely                                                     }{    \eqn{ \bar{\theta}(D_k) := \frac{1}{I} \sum_i \theta_i(D_k) }.                                                     }
\item{ \strong{(I.4)} Calculates error for each dataset \eqn{D_k}                                                 }{ \eqn{\epsilon_k}:= Truth - estimates =  \eqn{\theta_0   - \bar{\theta}(D_k)}.                                          }
\item{ \strong{(II)} Calculates mean of errors over all datasets \eqn{D_k} (\eqn{k=1,2,...,K})                                            }{ mean of errors  \eqn{ \bar{\epsilon}(\theta_0,N_I,N_L)}=  \eqn{ \frac{1}{K} \sum     \epsilon_k }.                    }
\item{  NOTE                                                                                                      }{ We note that if a fitted model does not converge,( namely R hat is far from one), then it is omiited from this calculation.}
\item{ \strong{(III) } Calculates mean of errors for various number of lesions and images                         }{ mean of errors  \eqn{ \bar{\epsilon}(\theta_0,N_I,N_L)}                                                               }
}

For example, if  \eqn{(N_I^1,N_L^1),(N_I^2,N_L^2),(N_I^3,N_L^3),...,(N_I^m,N_L^m)}, then
\eqn{ \bar{\epsilon}(\theta_0,N_I^1,N_L^1)},
\eqn{ \bar{\epsilon}(\theta_0,N_I^2,N_L^2)},
\eqn{ \bar{\epsilon}(\theta_0,N_I^3,N_L^3)},...,
\eqn{ \bar{\epsilon}(\theta_0,N_I^m,N_L^m)} are calculated.

To obtain precise error,
The number of replicated fitted models (denoted by \eqn{K}) should be large enough.
If \eqn{K} is small, then it causes a bias.
\eqn{K} = \code{replicate.datset}: a variable of the function \code{error_srsc}.










 Running this function, we can see that the error
  \eqn{ \bar{\epsilon}(\theta_0,N_I,N_L)} decreases
 monotonically as a given number of images \eqn{N_I}
  or a given number of lesions \eqn{N_L} increases.

 Also, the scale of error also will be found. Thus this function can show how our estimates are correct.
Scale of error differs for each componenet of model parameters.



Revised 2019  August 28
}
\details{
In Bayesian inference,
if sample size is large,
 then posterior tends to the Dirac measure.
 So, the error and variance of estimates
 should be tends to zero as sample size tends to infinity.

 This function check this phenomenen.

 If model has problem, then it contains some non-decreasing vias
 with respect to sample size.

  Revised 2019  Nov 1



Provides a reliability of our posterior mean estimates.
Using this function, we can
find what digit makes sence.

In the real world, the data for modality
 comparison or observer performan evaluation is
100 images or 200 images. In such scale data,
 any estimate of AUC will contain error at most 0.0113....
So, the value of AUC should round in 0.XXX
and not 0.XXXX or 0.XXXXX or more. Since
error is 0.00113... and hence 4 digit
 or more digit is meaningless. In such manner,
we can analyize the errors.

We note that if we increase the
number of images or lesinons,
the errors decrease.

For example, if we  use 20000 images in FROC trial,
 then the error of AUC will be 0.0005... and thus,
and so on. Thus large number of images gives
us more reliable AUC. However the radiologist
cannot read such large (20000) images.

 Thus, the error will be 0.00113...


  If the number of images are given before hand and moreover if we obtains the estimates,
  then we can run this function using these two, we can find the estimated errors by simulation.
  Of course, the esimates is not the truth, but roughly speaking, if we assume that
  the estimates is not so far from truth, and the error analysis is rigid with respect to
  changing the truth, then we can say using estimates as truth, the result of this error analysis can be regarded as an
  actual error.


I want to go home. Unfortunatly, my house is ...
}
\examples{
\dontrun{
#========================================================================================
#            0)            0-th example
#========================================================================================


   datasets <-error_srsc(
               NLvector = c(100,10000,1000000),
               ite = 2222
               )


 # By the following, we can extract only datasets whose
 # model has converged.
   datasets$convergent.dataList.as.dataframe




#========================================================================================
#            1)            1-st example
#========================================================================================
# Long width  is required in  R console.



datasets <-error_srsc(NLvector = c(
  50L,
  111L,
  11111L
  ),
  # NIvector,
  ratio=2,
  replicate.datset =3,
  ModifiedPoisson = FALSE,
  mean.truth=0.6,
  sd.truth=5.3,
  z.truth =c(-0.8,0.7,2.38),
  ite =2222
)



#========================================================================================
#            2)             Plot the error of AUC with respect to  NI
#========================================================================================


a <-error_srsc(NLvector = c(
  33L,
  50L,
  111L,
  11111L
  ),
  # NIvector,
  ratio=2,
  replicate.datset =3,
  ModifiedPoisson = FALSE,
  mean.truth=0.6,
  sd.truth=5.3,
  z.truth =c(-0.8,0.7,2.38),
  ite =2222
)






      aa <- a$Bias.for.various.NL


      error.of.AUC <-  aa[8,]
      y <- subset(aa[8,], select  = 2:length(aa[8,]))
      y <- as.numeric(y)
      y <- abs(y)
      upper_y <- max(y)
      lower_y <- min(y)

      x <- 1:length(y)



      plot(x,y, ylim=c(lower_y, upper_y))

#  From this plot, we cannot see whether the error has decreased or not.
#  Thus, we replot with the log y-axis, the we will see that the error
#  has decreased with respect to number of images and lesions.

      ggplot(data.frame(x=x,y=y), aes(x = x, y = y)) +
           geom_line() +
           geom_point() +
           scale_y_log10()

# Revised 2019 Sept 25


# General print of log scale
df<-data.frame(x=c(10,100,1000,10,100,1000),
               y=c(1100,220000,33000000,1300,240000,36000000),
               group=c("1","1","1","2","2","2")
)

ggplot2::ggplot(df, aes(x = x, y = y, shape = group)) +
  ggplot2::geom_line(position = position_dodge(0.2)) +           # Dodge lines by 0.2
  ggplot2::geom_point(position = position_dodge(0.2), size = 4)+  # Dodge points by 0.2
  ggplot2::scale_y_log10()+
  ggplot2::scale_x_log10()


#========================================================================================
#    2)   Add other param into plot plain of the error of AUC with respect to  NI
#========================================================================================











a <-error_srsc(NLvector = c(
  111L,
  11111L
  ),
  # NIvector,
  ratio=2,
  replicate.datset =3,
  ModifiedPoisson = FALSE,
  mean.truth=0.6,
  sd.truth=5.3,
  z.truth =c(-0.8,0.7,2.38),
  ite =2222
)
      aa <- a$Bias.for.various.NL


      error.of.AUC <-  aa[8,]
      y1 <- subset(aa[8,], select  = 2:length(aa[8,]))
      y1 <- as.numeric(y1)
      y1 <- abs(y1)

      LLL <-length(y1)

      y2 <- subset(aa[7,], select  = 2:length(aa[7,]))
      y2 <- as.numeric(y2)
      y2 <- abs(y2)

      y <- c(y1,y2)


      upper_y <- max(y)
      lower_y <- min(y)

    group <- rep(seq(1,2,1),1 , each=LLL)
    x <-  rep(seq(1,LLL,1),2 , each=1)
    group <-  as.character(group)
   df <-  data.frame(x=x,y=y,group=group)


                ggplot2::ggplot(df, aes(x = x, y = y, shape = group)) +
ggplot2::geom_line(position = position_dodge(0.2)) +           # Dodge lines by 0.2
  ggplot2::geom_point(position = position_dodge(0.2), size = 4)+  # Dodge points by 0.2
           ggplot2::scale_y_log10()
         # ggplot2::scale_x_log10()









#========================================================================================
#          Confidence level = 4
#========================================================================================





datasets <-error_srsc(NLvector = c(
  111L,
  11111L
  ),
  # NIvector,
  ratio=2,
  replicate.datset =3,
  ModifiedPoisson = FALSE,
  mean.truth=-0.22,
  sd.truth=5.72,
  z.truth =c(-0.46,-0.20,0.30,1.16),
  ite =2222
)





 error_srsc_variance_visualization(datasets)

#  The parameter of model is 7 in which the ggplot2 fails with the following warning:


# The shape palette can deal with a maximum of 6 discrete values because more than 6
# becomes difficult to
# discriminate; you have 7. Consider specifying shapes manually if you must have them.


#========================================================================================
#     NaN ... why? 2021 Dec
#========================================================================================

fits <- validation.dataset_srsc()

f <-fits$fit[[2]]
rstan::extract(f)$dl
sum(rstan::extract(f)$dl)
Is.nan.in.MCMCsamples <- as.logical(!prod(!is.nan(rstan::extract(f)$dl)))
rstan::extract(f)$A[525]
a<-rstan::extract(f)$a
b<-rstan::extract(f)$b



Phi(  a[525]/sqrt(b[525]^2+1)  )
a[525]/sqrt(b[525]^2+1)
Phi(  a/sqrt(b^2+1)  )
x<-rstan::extract(f)$dl[2]

a<-rstan::extract(f)$a
b<-rstan::extract(f)$b

a/(b^2+1)
Phi(a/(b^2+1))
mean( Phi(a/(b^2+1))  )

#'


}# dontrun
}
