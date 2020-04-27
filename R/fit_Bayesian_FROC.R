

#' @title Fit a model to data
#' @description Fit a model to data.
#' @details The author made a function
#' \code{\link{fit_Bayesian_FROC}()} which has
#' very redundant variables.
#' So, \code{fit_a_model_to()} is made by simplifying \code{\link{fit_Bayesian_FROC}()}
#' so that its variables is minimum.
#' To access full details,
#' see the help of \code{\link{fit_Bayesian_FROC}()}.
#'
#' This function aims to give a simple interface by ignoring unnecessarly parameters of \code{\link{fit_Bayesian_FROC}()}.

#' @inheritParams fit_Bayesian_FROC
# @param data A list of data to be fitted a model. This is same
#' @param number_of_chains_for_MCMC A positive integer, indicating the number of chains for MCMC. To be passed to the function \code{rstan::}\code{\link[rstan]{sampling}}() in \pkg{rstan}.
#' @param number_of_iterations_for_MCMC A positive integer, indicating the number of interations for MCMC. To be passed to the function \code{rstan::}\code{\link[rstan]{sampling}}() in \pkg{rstan}.
#' @param seed_for_MCMC A positive integer, indicating the seed for MCMC. To be passed to the function \code{rstan::}\code{\link[rstan]{sampling}}() in \pkg{rstan}.
#' @param ... Additional arguments

#'
#' @seealso \code{\link{fit_Bayesian_FROC}()}
#' @return An fitted model
#' object of the S4 class named  \code{\link{stanfitExtended}}  which is an inherited class from stanfit.
#' @export
#'
#' @examples
#' \dontrun{
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'# 1)             Build a data-set
#'#========================================================================================
#'
#'# For a single reader and a single modality  case.
#'
#'     data <- list(c=c(3,2,1), #     Confidence level. Note that c is ignored.
#'             h=c(97,32,31), #     Number of hits for each confidence level
#'             f=c(1,14,74),  #     Number of false alarms for each confidence level
#'
#'             NL=259,        #     Number of lesions
#'             NI=57,         #     Number of images
#'             C=3)           #     Number of confidence level
#'
#'
#'          viewdata(data)
#'
#'#  where,
#'#      c denotes confidence level, i.e., rating of reader.
#'#                3 = Definitely diseased,
#'#                2 = subtle,.. diseased
#'#                1 = very subtle
#'#      h denotes number of hits (True Positives: TP) for each confidence level,
#'#      f denotes number of false alarms (False Positives: FP) for each confidence level,
#'#      NL denotes number of lesions,
#'#      NI denotes number of images,
#'
#'
#'# For example, in the above example data,
#'#  the number of hits with confidence level 3 is 97,
#'#  the number of hits with confidence level 2 is 32,
#'#  the number of hits with confidence level 1 is 31,
#'
#'#  the number of false alarms with confidence level 3 is 1,
#'#  the number of false alarms with confidence level 2 is 14,
#'#  the number of false alarms with confidence level 1 is 74,
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'# 2)       Fit an FROC model to the above dataset.
#'#========================================================================================
#'
#'
#'
#'
#'
#'
#'           fit <-   BayesianFROC::fit_a_model_to(
#' #  Dataset to be fiited
#'                      dataList = data,
#'
#' #  To run in time <5s, MCMC iterations too small to obtain reliable estimates
#' number_of_iterations_for_MCMC = 1111,
#'
#' #  The number of chains, it is better  if larger.
#' number_of_chains_for_MCMC     = 1
#'                                )
  #'}#dontrun
  #'
  #'
  # ____________________------

  fit_a_model_to <- function(
  dataList,
  number_of_chains_for_MCMC = 1,
  number_of_iterations_for_MCMC = 1111,
  seed_for_MCMC =1234,
  ...


) {

  f <- fit_Bayesian_FROC(
                          dataList = dataList,
                          ite  = number_of_iterations_for_MCMC,
                          cha = number_of_chains_for_MCMC,
                          see = seed_for_MCMC,
                          ... )
  return(f)
}
























# title -------

#' @title Fit a model to  data
#'

# description -------
#'@description
#'
#' Creates a fitted model object of class \code{ \link{stanfitExtended}}: an inherited class from the S4 class \strong{\emph{\code{\link[rstan]{stanfit}}}} in \pkg{rstan}.
# details -------

#'@details
#'
#'Draw MCMC samples using \R package: \pkg{rstan}
#'
#'
#'It also plots FROC curves if a single reader and a single modality case.
#' For details, see   \href{https://cran.r-project.org/package=BayesianFROC}{ vignettes  }
#'
#'  Build the S4 object by Stan to fit the author's
#'   Bayesian models introduced in the author's paper
#'    (for details of models, see
#'     \href{https://cran.r-project.org/package=BayesianFROC}{ vignettes  }).
#'  The output of the \code{rstan::}\code{\link[rstan]{sampling}}() is an object of the S4 class called  \strong{\emph{\code{\link[rstan]{stanfit}}}}.
#'  But, in this package, we extended the \emph{stanfit} class to an S4 class named  \emph{stanfitExtended}.
#'  The new S4 class \strong{\code{ \link{stanfitExtended}}} included new slots for sequential analysis.
#'  So, the return value of the function is not the S4 class \emph{stanfit} but the new S4 class \strong{\code{ \link{stanfitExtended}}}.
#'  Thus, to apply the functions in the \pkg{rstan} package for fitted model objects , we have to change the class of the S4 fitted model objects using the function \code{methods::}\code{\link[methods]{as}}() such as
#'  by the code \code{methods::as( object = fitted.model.object, "stanfit")}.
#'
#' The following items are main substances of this function.

#'
#'
#'
#'
#' This function \code{fit_Bayesian_FROC} is available both a single reader and a single modality case and multiple readers
#'and multiple modality case.
#'  Confidence level vector is not required but it is implicitly referred as the decreasing order,
#' For example, if C=3, then it would be a form  \code{c=c(3,2,1,3,2,1,...)}.
#'  Even if you write your data according to the order
#' \code{c=c(1,2,3,1,2,3,...)}, the program does not consider as your order, but \code{c=c(3,2,1,3,2,1,...)} instead.


#'@param dataList  A list, specifying an FROC data to be fitted a model.
#' It consists of data of numbers
#'of TPs, FPs, lesions, images.
#'.In addition, if in case of  mutiple readers or mutiple modalities,
#'then modaity ID and reader ID are included also.
#'
#'  The \code{dataList} will be passed to the function \code{rstan::}\code{\link[rstan]{sampling}}() in \pkg{rstan}. This is a variable in the function \code{rstan::sampling()} in which it is named \code{data}.
#'
#'
#'
#'  For the single reader and a single modality data, the \code{dataList} is made by the following manner:
#'
#' \code{ dataList.Example <- list(       }
#'
#'\code{            h = c(41,22,14,8,1),  # number of hits for each confidence level }
#'
#' \code{            f = c(1,2,5,11,13),  # number of false alarms for each confidence level   }
#'
#' \code{            NL = 124,  # number of lesions (signals)   }
#'
#' \code{            NI = 63,  # number of images (trials)  }
#'
#' \code{            C = 5)   # number of confidence, .. the author thinks it can be calculated as the length of h or f ...? ha, why I included this. ha .. should be omitted.      }
#'
#'And using this object \code{dataList.Example}, we can apply \code{fit_Bayesian_FROC()} such as \code{fit_Bayesian_FROC(dataList.Example)}.
#'
#'
#'
#'
#'
#'
#' To make this \R object \code{dataList} representing FROC data, this package provides three functions:
#' \describe{
#' \item{  \code{ \link{convertFromJafroc}()}           }{ If  data is a           \emph{\strong{JAFROC xlsx}} formulation.}
#' \item{  \code{ \link{dataset_creator_new_version}()} }{ Enter TP and FP data    \emph{\strong{by table   }}.            }
#' \item{  \code{ \link{create_dataset}()}              }{ Enter TP and FP data by \emph{\strong{interactive}} manner.     }
#' }
#'
#' Before fitting a model,
#'  we can confirm our dataset is correctly formulated
#'  by using the function \strong{\code{ \link{viewdata}()}}.
#'
#'
#'----------------------------------------------------------------------------------------
#'
#'
#'   \strong{A Single reader and a single modality (SRSC) case.}
#'
#'
#'----------------------------------------------------------------------------------------
#'
#'In a single reader and a single modality case (srsc),
#' \code{dataList} is a list consisting of  \code{f, h, NL, NI, C} where
#'  \code{f, h} are numeric vectors
#'  and  \code{NL, NI, C} are positive integers.
#'
#' \describe{
#' \item{ \code{f}  }{Non-negative integer vector  specifying  number of false alarms    associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{h}  }{Non-negative integer vector  specifying  number  of Hits  associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{NL} }{A positive integer, representing  Number of Lesions.}
#' \item{ \code{NI} }{A positive integer, representing  Number of Images. }
#' \item{ \code{C}  }{A positive integer, representing  Number of Confidence level. }
#' }
#'
#'
#'
#'
#'The detail of these dataset, see the datasets  endowed with this package.
#''Note that the maximal number of confidence level, denoted by  \code{C}, are included,
#' however,
#' Note that confidence level vector \code{c } should not be specified. If specified, will be ignored , since it is created by \code{  c <-c(rep(C:1))} in the program and do not refer from user input data, where \code{C} is the highest number of confidence levels.
#'So, you should write down your hits and false alarms vector so that it is compatible with this automatically created \code{c} vector.
#'
#'

#'\strong{\emph{ data Format:}}
#'
#'  \emph{            A single reader and a single modality case   }
#'
#'------------------------------------------------------------------------------------------------------

#' \tabular{rccc}{
#' \code{NI=63,NL=124}   \tab \strong{ confidence level } \tab \strong{ No. of false alarms} \tab \strong{No. of hits}  \cr
#'  In R console ->      \tab \code{ c} \tab   \code{f }  \tab   \code{h}  \cr
#'   -----------------------\tab ----------------------- \tab ----------------------------- \tab ------------- \cr
#' definitely present  \tab  \code{c[1] = }5 \tab \code{f[1] = }\eqn{F_5} = 1 \tab  \code{h[1] = }\eqn{H_5} = 41 \cr
#'  probably present   \tab  \code{c[2] = }4 \tab \code{f[2] = }\eqn{F_4} = 2 \tab  \code{h[2] = }\eqn{H_4} = 22 \cr
#'  equivocal                 \tab  \code{c[3] = }3 \tab \code{f[3] = }\eqn{F_3} = 5 \tab  \code{h[3] = }\eqn{H_3} = 14  \cr
#'  subtle                    \tab  \code{c[4] = }2 \tab \code{f[4] = }\eqn{F_2} = 11 \tab \code{h[4] = }\eqn{H_2} = 8  \cr
#'  very subtle        \tab  \code{c[5] = }1 \tab \code{f[5] = }\eqn{F_1} = 13 \tab \code{h[5] = }\eqn{H_1} = 1  \cr
#'  }
#'
#'---------------------------------------------------------------------------------------------------
#'
#'
#'
#'*  \emph{false alarms} = False Positives = FP
#'
#'*  \emph{hits} = True Positives = TP
#'
#'Note that  in FROC data, all confidence level means \emph{present} (\emph{diseased, lesion}) case only, no confidence level indicating absent. Since each reader marks his suspicious location only if he thinks  lesions are \emph{present}, and marked positions generates the hits or false alarms, \emph{thus} each confidence level represents that lesion is \emph{present}.
#'In the absent case, reader dose not mark any locations and hence, the absent confidence level does not relate this dataset. So, if reader think it is no lesion, then in such case confidence level is not needed.
#'
#'
#' Note that the first column of confidence level vector \code{c } should not be specified. If specified, will be ignored , since it is created by \code{  c <-c(rep(C:1))} automatically in the program and do not refer from user input data even if it is specified explicitly, where \code{C} is the highest number of confidence levels.
#'So you should check the compatibility of your
#' data and the confidence  level vector  \code{  c <-c(rep(C:1))}
#' via a table which can be displayed by the function \code{\link{viewdata}()}.
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'---------------------------------------------------------------------------------------
#'
#'   \strong{Multiple readers and multiple modalities case, i.e., MRMC case}
#'
#'
#'---------------------------------------------------------------------------------------
#'
#'
#'In case of  multiple readers and multiple modalities, i.e., MRMC case,
#'in order to apply the function \code{fit_Bayesian_FROC()},
#' dataset represented by an \R list object representing FROC data
#'must contain components \code{m,q,c,h,f,NL,C,M,Q}.
#'
#' \describe{
#' \item{ \code{C }  }{A positive integer, representing  the \emph{\strong{highest}} number of confidence level, this is a scalar.}
#' \item{ \code{M }  }{A positive integer vector, representing  the number of \emph{\strong{modalities}}.  }
#' \item{ \code{Q }  }{A positive integer, representing  the number of \emph{\strong{readers}}. }
#' \item{ \code{m }  }{A vector of positive integers,  representing  the \emph{\strong{modality}} ID vector. }
#' \item{ \code{q }  }{A vector of positive integers,  representing  the \emph{\strong{reader}} ID vector.}
#' \item{ \code{c }  }{A vector of positive integers,  representing  the \emph{\strong{confidence level}}. This vector must be made by \code{rep(rep(C:1), M*Q)} }
#' \item{ \code{h }  }{A vector of non-negative integers,  representing  the number of \emph{\strong{hits}}.   }
#' \item{ \code{f }  }{A vector of non-negative integers,  representing  the number of \emph{\strong{false alarms}}.  }
#' \item{ \code{NL}  }{A positive integer, representing  the Total number of \emph{\strong{lesions}} for all images, this is a scalar.}
#' }
#'
#'
#'
#'Note that the maximal number of confidence level (denoted by  \code{C}) are included in
#'the above \R object.
#' However,
#' each confidence level vector is not included in the data,
#'  because it is created automatically from \code{C}.
#'   To confirm  false positives and hits
#' are correctly ordered with respect to
#'  the automatically generated confidence vector,
#'
#' the function \code{\link{viewdata}()} shows the table.

#' Revised 2019 Nov 27
#' Revised 2019 Dec 5
#'
#'
#'

#'\strong{\emph{ Example data. }}
#'
#'
#'
#'  \emph{ Multiple readers and multiple modalities ( i.e., MRMC) }
#'
#'
#'
#'
#'---------------------------------------------------------------------------------------------------
#'
#' \tabular{ccccc}{
#'  \strong{Modality ID } \tab   \strong{Reader ID }  \tab  \strong{ Confidence levels} \tab   \strong{No. of false alarms} \tab   \strong{No. of hits}.\cr
#'   \code{m} \tab  \code{ q}  \tab   \code{c} \tab  \code{ f} \tab \code{ h}\cr
#'    -------------- \tab ------------- \tab ------------------------ \tab  ------------------- \tab ----------------\cr
#'   1 \tab 1 \tab 3 \tab 20 \tab 111\cr
#'   1 \tab 1 \tab 2 \tab 29 \tab  55\cr
#'   1 \tab 1 \tab 1 \tab 21 \tab  22\cr
#'   1 \tab 2 \tab 3 \tab  6 \tab 100\cr
#'   1 \tab 2 \tab 2 \tab 15 \tab  44\cr
#'   1 \tab 2 \tab 1 \tab 22 \tab  11\cr
#'   2 \tab 1 \tab 3 \tab  6 \tab  66\cr
#'   2 \tab 1 \tab 2 \tab 24 \tab  55\cr
#'   2 \tab 1 \tab 1 \tab 23 \tab   1\cr
#'   2 \tab 2 \tab 3 \tab  5 \tab  66\cr
#'   2 \tab 2 \tab 2 \tab 30 \tab  55\cr
#'   2 \tab 2 \tab 1 \tab 40 \tab  44\cr
#' }

#'---------------------------------------------------------------------------------------------------
#'
#'

#'
#'
#'*  \emph{false alarms} = False Positives = FP
#'
#'*  \emph{hits} = True Positives = TP
#'
#'
#'
#'
#'
#'
#'
#'@inheritParams fit_srsc
#'
#'
# param -------
#' @param ... Additional arguments

#'@param model_reparametrized A logical, if TRUE, then a model under construction is used.
#'@param Model_MRMC_non_hierarchical  A logical.
#'If \code{TRUE}, then the model of multiple readers and multiple modalities consits of
#'no hyper parameters.
#'The reason why the author made this parameter is that the hyper parameter make the MCMC posterior samples be unstable.
#'And also, my hierarachical model is not so good in theoretical perspective.
#'Thus, I made this. The Default is \code{TRUE}.


#'@param zz A real number  specifying one of the parameter of prior
#'@param verbose A logical, if \code{TRUE}, then the redundant summary is printed in \R console.
#'If \code{FALSE}, it suppresses output from this function.


#'@param cha  To be passed to the function \code{rstan::}\code{\link[rstan]{sampling}}() in \pkg{rstan}. An argument of \code{rstan::}\code{\link[rstan]{sampling}}()  in which it is named \code{chains}.  A positive integer representing   the number of chains generated by Hamiltonian Monte Carlo method,
#'and, Default = 1.



#'@param ite  To be passed to the function \code{rstan::}\code{\link[rstan]{sampling}}() in \pkg{rstan}. An argument of \code{rstan::}\code{\link[rstan]{sampling}}()  in which it is named \code{iter}. A positive integer representing  the  number of samples synthesized by Hamiltonian Monte Carlo method,
#'and, Default = 10000. If your model could not converge, then raise this number. Must be greater for more reliable estimates.


#'@param dig  To be passed to the function \code{rstan::}\code{\link[rstan]{sampling}}() in \pkg{rstan}. An argument of \code{rstan::}\code{\link[rstan]{sampling}}()  in which it is named \code{...??}.   A positive integer representing   the Significant digits, used in stan Cancellation.
#'Default = 5,
#'


#'@param war  To be passed to the function \code{rstan::}\code{\link[rstan]{sampling}}() in \pkg{rstan}. An argument of \code{rstan::}\code{\link[rstan]{sampling}}()  in which it is named \code{warmup}.  A positive integer representing the Burn in period, which must be less than \code{ite}. Defaults to
#'war = floor(ite/5)=10000/5=2000,


#'@param see   To be passed to the function \code{rstan::}\code{\link[rstan]{sampling}}() in \pkg{rstan}. An argument of \code{rstan::}\code{\link[rstan]{sampling}}()  in which it is named \code{seed}.  A positive integer representing  seed used in stan,
#' Default = 1234567.


#'@param PreciseLogLikelihood  Logical, that is \code{TRUE} or \code{FALSE}. If \code{PreciseLogLikelihood  = TRUE}(default), then Stan calculates the precise log likelihood with target formulation.


#'If \code{PreciseLogLikelihood  = FALSE}, then Stan calculates the log likelihood by dropping the constant terms in the likelihood function.
#'In past, I distinct the stan file, one is target formulation and the another is not. But non-target formulation cause some Jacobian warning,
#' thus I made all stanfile with target formulation when I uploaded to CRAN.
#'  Thus this variable is now meaningless.
#'
#'
#'@param Null.Hypothesis Logical, that is \code{TRUE} or \code{FALSE}.
#' If \code{Null.or.Alternative.Hypothesis  = FALSE}(default),
#'  then fit the \emph{alternative model} to \code{dataList} (for details of models, see   \href{https://cran.r-project.org/package=BayesianFROC}{ vignettes  }).
#' If \code{Null.or.Alternative.Hypothesis  = TRUE},
#'  then fit the \emph{null model} to \code{dataList}.(for details of models, see   \href{https://cran.r-project.org/package=BayesianFROC}{ vignettes  }).
#'  Note that the null model is constructed under the null hypothesis that
#'  all modality are same observer performance ability.
#'  The alternative model is made under the assumption that all modality are not same.
#' The reason why author creates this parameter is to test the null hypothesis by the Bayes factor.
#' But the result of test is not desired one for me. Thus the test is under construction.



#'@param ModifiedPoisson  Logical, that is \code{TRUE} or \code{FALSE}.
#'
#'If \code{ModifiedPoisson = TRUE},
#' then Poisson rate of false alarm is calculated \strong{\emph{per lesion}},
#' and model is fitted
#' so that the FROC curve is an expected curve
#'  of points consisting of the pairs of TPF per lesion and FPF  \strong{\emph{per lesion}}.
#'
#'Similarly,
#'
#'If \code{ModifiedPoisson = TRUE},
#' then Poisson rate of false alarm is calculated \strong{\emph{per image}},
#' and model is fitted
#' so that the FROC curve is an expected curve
#'  of points consisting of the pair of TPF per lesion and FPF  \strong{\emph{per image}}.
#'
#'
#'
#'For more details, see the author's paper in which I explained \emph{per image} and \emph{per lesion}.
#'(for details of models, see   \href{https://cran.r-project.org/package=BayesianFROC}{ vignettes  }, now, it is omiited from this package, because the size of vignettes are large.)
#'
#' If \code{ModifiedPoisson = TRUE},
#'  then the \emph{False Positive Fraction (FPF)} is defined as follows
#'  (\eqn{F_c} denotes the number of false alarms with confidence level \eqn{c} )
#'
#'
#' \deqn{ \frac{F_1+F_2+F_3+F_4+F_5}{N_L}, }
#'
#' \deqn{ \frac{F_2+F_3+F_4+F_5}{N_L}, }
#'
#'  \deqn{ \frac{F_3+F_4+F_5}{N_L}, }
#'
#'   \deqn{ \frac{F_4+F_5}{N_L}, }
#'
#'    \deqn{ \frac{F_5}{N_L}, }
#'
#'where \eqn{N_L} is a number of lesions (signal).
#'To emphasize its denominator  \eqn{N_L},
#'we also call it the \emph{False Positive Fraction (FPF)} \strong{per lesion}.
#'
#'
#'On the other hand,
#'
#'
#'if \code{ModifiedPoisson = FALSE} (Default), then
#'\emph{False Positive Fraction (FPF)} is given by
#'
#' \deqn{ \frac{F_1+F_2+F_3+F_4+F_5}{N_I}, }
#'
#' \deqn{ \frac{F_2+F_3+F_4+F_5}{N_I}, }
#'
#'  \deqn{ \frac{F_3+F_4+F_5}{N_I}, }
#'
#'   \deqn{ \frac{F_4+F_5}{N_I}, }
#'
#'    \deqn{ \frac{F_5}{N_I}, }
#'
#'where \eqn{N_I} is the number of images (trial).
#'To emphasize its denominator \eqn{N_I},
#' we also call it the \emph{False Positive Fraction (FPF)} \strong{per image}.

#'
#'
#' The model is fitted so that
#' the estimated FROC curve can be ragraded
#'  as the expected pairs of   FPF per image and TPF per lesion (\code{ModifiedPoisson = FALSE })
#'
#'  or as the expected pairs of   FPF per image and TPF per lesion  (\code{ModifiedPoisson = TRUE})
#'
#' If \code{ModifiedPoisson = TRUE}, then FROC curve means the expected pair of FPF \strong{per lesion} and TPF.
#'
#' On the other hand, if  \code{ModifiedPoisson = FALSE}, then FROC curve means the expected pair of \strong{FPF per image} and TPF.
#'
#'
#'
#'
#'So,data of FPF and TPF are changed thus, a fitted model is also changed whether  \code{ModifiedPoisson = TRUE} or \code{FALSE}.
#'In traditional FROC analysis, it uses only per images (trial). Since we can divide one image into two images or more images, number of
#'trial is not important. And more important is per signal. So, the author also developed FROC theory to consider FROC analysis under per signal.
#'One can see that the FROC curve is rigid with respect to change of a number of images, so, it does not matter whether \code{ModifiedPoisson = TRUE} or \code{FALSE}.
#'This rigidity of curves means that the number of images is redundant parameter for the FROC trial and
#'thus the author try to exclude it.
#'
#'
#'Revised 2019 Dec 8
#'Revised 2019 Nov 25
#'Revised 2019 August 28
#'
#'@param mesh.for.drawing.curve A positive large integer, indicating number of dots drawing the curves, Default =10000.
#'
#'
#'
#'
#'
#'
#'
#'
#'

#'@param DrawCurve Logical: \code{TRUE} of \code{FALSE}. Whether the curve is to be drawn. TRUE or FALSE. If you want to draw the FROC and AFROC curves, then you set \code{DrawCurve =TRUE}, if not then \code{DrawCurve =FALSE}.
#' The reason why the author make this variable \code{DrawCurve} is that it takes long time in MRMC case to draw curves, and thus Default value is \code{FALSE} in the case of MRMC data.
#'@param Drawcol Logical: \code{TRUE} of \code{FALSE}. Whether the (A)FROC curve is to be drawn  by using color of dark theme. The Default value is a \code{TRUE}.

#'@param significantLevel This is a number between 0 and 1. The results are shown if posterior probabilities are greater than this quantity.
#'@param DrawFROCcurve Logical: \code{TRUE} of \code{FALSE}. Whether  the FROC curve  is to be drawn.
#'@param DrawAFROCcurve Logical: \code{TRUE} of \code{FALSE}. Whether the  AFROC curve is to be drawn.
#'@param DrawCFPCTP Logical: \code{TRUE} of \code{FALSE}. Whether  the CFP and CTP points are to be drawn. CFP: Cumulative false positive per lesion (or image) which is also called False Positive Fraction (FPF). CTP Cumulative True Positive per lesion  which is also called True Positive Fraction (TPF)..

#'@param prior positive integer, to select the prior
#'
#'@param new.imaging.device Logical: \code{TRUE} of \code{FALSE}. If TRUE (default), then open a new device to draw curve.
#'  Using this we can draw curves in same plain by new.imaging.device=FALSE.

#'
#'@param  summary Logical: \code{TRUE} of \code{FALSE}. Whether to print the verbose summary. If \code{TRUE} then verbose summary is printed in the \R console. If \code{FALSE}, the output is minimal. I regret, this variable name should be verbose.
#'
#'@param  make.csv.file.to.draw.curve Logical: \code{TRUE} of \code{FALSE}. Whether  to create a csv file. If \code{TRUE} then csv file is created in your desktop to draw an FROC curve and cumulative hits and false alarms by scatter plot. Default is  \code{FALSE} since it took times to create csv files.
#'
#'
#'@param prototype A logical, if \code{TRUE} then the model is no longer
#' a generative model. Namely, in generally speaking,
#'  a dataset drawn from the model
#'  cannot satisfy the condition that
#'  the sum of the numbers of hits over all confidence levels
#'   is bounded from the above by the number of lesions, namely,
#'
#' \deqn{ \Sigma_c H_c \le N_L }
#'
#' However, this model (\code{TRUE} )
#'  is good in the sense that it admits various initial values of MCMC sampling.
#'
#'  if \code{FALSE}, then the model is precisely
#'   statistical model in the sense that
#'    any dataset drawn from the model
#'    satisfies that the sum of the number of hits is
#'  not greater than the number of lesions, namely,
#'
#'   \deqn{ \Sigma_c H_c \le N_L. }

#'   This model is theoretically perfect.
#'    However, in the practically,
#'     the calculation will generates
#'      some undesired results
#'       which caused by the so-called floo .... I forget English :'-D.
#'  The flood point??? I forgeeeeeeeeeeeeet!!
#'   Ha. So, prior synthesizes very small hit rates such as 0.0000000000000001234 and it cause the non accurate calculation such as 0.00000,,,00000123/0.000.....000012345= 0.0012 which becomes hit rate and thus OH No!.
#'  Then it synthesizes Bernoulli success rate which is not less than 1 !!
#'  To avoid this, the author should develop the theory of prior to avoid this very small numbers, however the author has idea but now it does not success.
#'
#'
#'
#'
#' If \code{prototype = TRUE},
#' then the model for hits is the following:
#'
#' \deqn{H_5 \sim Binomial(p_5,N_L)}
#' \deqn{H_4 \sim Binomial(p_4,N_L)}
#' \deqn{H_3 \sim Binomial(p_3,N_L)}
#' \deqn{H_2 \sim Binomial(p_2,N_L)}
#' \deqn{H_1 \sim Binomial(p_1,N_L)}
#'
#'
#'On the other hand,
#'if \code{prototype = FALSE},
#'then the model for hits is the following:
#'
#' \deqn{H_5 \sim Binomial(               p_5,N_L      )                        }
#' \deqn{H_4 \sim Binomial( \frac{p_4}{1-p_5},N_L - H_5)                        }
#' \deqn{H_3 \sim Binomial( \frac{p_3}{1-p_5-p_4},N_L - H_5-H_4)                }
#' \deqn{H_2 \sim Binomial( \frac{p_2}{1-p_5-p_4-p_3},N_L - H_5-H_4-H_3)        }
#' \deqn{H_1 \sim Binomial( \frac{p_1}{1-p_5-p_4-p_3-p_2},N_L - H_5-H_4-H_3-H_2)}
#'
#'
#'Each number of lesions is adjusted
#'so that the sum of hits \eqn{\Sigma_c H_c} is less than
#'the number of lesions (signals, targets) \eqn{N_L}.
#'And hence the model in case of \code{prototype = FALSE}
#'is a generative model in the sense that
#'it can replicate datasets of FROC  arises.
#'Note that the adjustment of the number of lesions
#' in the above manner leads us the adjustment of hit rates.
#'The reason why we use the hit rates such as
#' \eqn{\frac{p_2}{1-p_5-p_4-p_3}} instead of \eqn{p_c} is that
#' it ensures the equality \eqn{ E[H_c/N_L] = p_c}.
#'  This equality is very important.
#' To establish Bayesian FROC theory so that it is
#'  compatible to the classical FROC theory, we need the following two equations,
#'
#'   \deqn{ E[H_c/N_L] = p_c,}
#'   \deqn{ E[F_c/N_X] = q_c,}
#'
#'where  \eqn{E} denotes the expectation and \eqn{N_X} is the number of lesion or the number of images and
#' \eqn{q_c} is a false alarm rate, namely, \eqn{ F_c \sim Poisson( q_c N_X)}.
#'
#' Using the above two equations, we can establish the alternative Bayesian FROC theory preserving classical notions and
#' formulas. For the details, please see the author's pre print:
#'
#' Bayesian Models for ,,, for?? I forget my paper title .... :'-D.
#' What the hell!? I forget,... My health is so bad to forget , .... I forget.
#'
#'
#'
#'
#'The author did not notice that the prototype is not a generative model. And hence
#'the author revised the model so that the model is exactly generative model.
#'
#'But the reason why the author remains the prototype model(\code{prototype = TRUE})
#'is that the convergence of MCMC sampling in case of MRMC is not good in the current model (\code{prototype = FALSE}) .
#'Because it uses fractions \eqn{\frac{p_1}{1-p_5-p_4-p_3-p_2}} and which is very dangerous to numerical perspective.
#'For example, if \eqn{p_1} is very small, then the numerator and denominator of \eqn{\frac{p_1}{1-p_5-p_4-p_3-p_2}}  is very small.
#'Both of them is like 0.000000000000000123.... and such small number causes the non accurate results.
#'So, sometimes, it occurs that \eqn{\frac{p_1}{1-p_5-p_4-p_3-p_2} >1} which never occur in the theoretical perspective but
#' unfortunately, in numerically occurs.
#'
#' SO, now, the author try to avoid such phenomenon by using priors but it now does not success.
#'
#'
#'
#'Here of course we interpret the terms
#'such as \eqn{N_L - H_5-H_4-H_3} as
#'the remained targets after
#'reader get hits. The author thinks it is another manner to do so like \eqn{N_L -H_1-H2-H_3}, but it does not be employed.
#'Since the author thinks that the reader will assign his suspicious lesion location from high confidence level and in this view point
#'the author thinks it should be considered that targets are found from the highest confidence suspicious location.
#'
#'
#'
#'
#'
#'
# return -------
#'@return  An object of class \code{ \link{stanfitExtended}} which
#'is an inherited S4 class
#' from the S4 class  \strong{\emph{\code{\link[rstan]{stanfit}}}}
#'   By  \code{rstan::sampling},
#'   the function fit the author's
#'    FROC Bayesian models to user data.

#'
#' Use this fitted model  object for
#'  sequential analysis,
#'  such as drawing the FROC
#'   curve and alternative
#'    FROC (AFROC) curves.
#'
#'
#'
#'
#'@return  ------------------------------------------------------------------------------------------------------------
#'@return     Notations and symbols for the    \strong{Outputs of a single reader and a single modality case }
#'@return  ----------------------------------------------------------------------------------------------------------------
#'
#'@return In the following, the notations for estimated parameters are shown.
#'
#'@return \code{ w  }   A real number representing \strong{ \emph{ the lowest threshold}} of the Gaussian assumption (bi-normal assumption). so \code{w}=\code{z[1]}.
#'@return \code{dz[1]  } A real number representing \strong{ \emph{the difference of the first and second threshold}} of the Gaussian assumption: \code{dz[1] := z[2] - z[1]}.
#'@return \code{dz[2]  } A real number representing the difference of the second and third threshold of the Gaussian assumption: \code{dz[2] :=  z[3] - z[2]}.
#'@return \code{dz[3]  } A real number representing the difference of the  third and fourth threshold of the Gaussian assumption: \code{dz[3] :=  z[4] - z[3]}.
#'@return \code{...}


#'@return \code{m  }A real number representing the The \strong{ \emph{  mean }}of the Latent Gaussian distribution for diseased images. In TeX, it denoted by \eqn{\mu}
#'@return \code{v  }A positive real number representing the \strong{ \emph{  standard deviation }}of the Latent Gaussian distribution for diseased images.In TeX, it will be denoted by \eqn{\sigma}, not the square of \eqn{\sigma}.
#'@return \code{p[1]} A real number representing the Hit rate with confidence level 1.
#'@return \code{p[2]}A real number representing the Hit rate with confidence level 2.
#'@return \code{p[3]}A real number representing the Hit rate with confidence level 3.
#'@return \code{...}

#'@return \code{l[1]}A positive real number representing the (Cumulative) False positive rate with confidence level 1. In TeX, it will be denoted by \eqn{\lambda_1}.
#'@return \code{l[2]}A positive real number representing the (Cumulative) False positive rate with confidence level 2. In TeX, it will be denoted by \eqn{\lambda_2}.
#'@return \code{l[3]}A positive real number representing the (Cumulative) False positive rate with confidence level 3. In TeX, it will be denoted by \eqn{\lambda_3}.
#'@return \code{l[4]}A positive real number representing the (Cumulative) False positive rate with confidence level 4. In TeX, it will be denoted by \eqn{\lambda_4}.
#'@return \code{...}
#'@return \code{dl[1]}A positive real number representing   the difference \code{ l[1] - l[2]}.
#'@return \code{dl[2]}A positive real number representing   the difference \code{ l[2] - l[3]}.
#'@return \code{dl[3]}A positive real number representing   the difference \code{ l[3] - l[4]}.
#'@return \code{...}
#'@return \code{z[1]} A real number representing the lowest threshold of the (Gaussian) bi-normal assumption.
#'@return \code{z[2]}  A real number representing the 2nd threshold of the (Gaussian) bi normal assumption.
#'@return \code{z[3]} A real number representing the 3rd threshold of the (Gaussian) bi normal assumption.
#'@return \code{z[4]} A real number representing the fourth threshold of the (Gaussian) bi-normal assumption.
#'@return \code{a} A real number   defined by \code{m/v}, please contact the author's paper for detail.
#'@return \code{b} A real number representing   defined by \code{1/v}, please contact the author's paper for detail.
#'@return \code{A} A positive real number  between 0 and 1, representing AUC, i.e., the area under the alternative ROC curve.
#'@return \code{lp__} The logarithmic likelihood of our model for your data.
#'
#'

#'@return  ---------------------------------------------------------------------------------------------------------------
#'@return  ---- \strong{Notations and symbols:  }Outputs of Multiple Reader and Multiple Modality case       ------
#'@return  ------------------------------------------------------------------------------------------------------------------

#'@return \code{ w  }    The lowest threshold of the Gaussian assumption (bi-normal assumption). so \code{w}=\code{z[1]}.
#'@return \code{dz[1]  } The difference of the first and second threshold of the Gaussian assumption.
#'@return \code{dz[2]  } The difference of the second and third threshold of the Gaussian assumption.
#'@return \code{dz[3]  } The difference of the  third and fourth threshold of the Gaussian assumption.
#'@return \code{...}
#'@return \code{mu  }The mean of the Latent Gaussian distribution for diseased images.
#'@return \code{v  }The variance of the Latent Gaussian distribution for diseased images.
#'@return \code{ppp[1,1,1]} Hit rate with confidence level 1, modality 1, reader 1.
#'@return \code{ppp[2,1,1]} Hit rate with confidence level 2,  modality  1, reader 1.
#'@return \code{ppp[3,1,1]} Hit rate with confidence level 3,  modality  1, reader 1.
#'@return \code{...}
#'@return \code{l[1]} (Cumulative) False positive rate with confidence level 1.
#'@return \code{l[2]} (Cumulative) False positive rate with confidence level 2.
#'@return \code{l[3]} (Cumulative) False positive rate with confidence level 3.
#'@return \code{l[4]} (Cumulative) False positive rate with confidence level 4.
#'@return \code{...}

#'@return \code{dl[1]} This is defined by the difference \code{ l[1] - l[2]}.
#'@return \code{dl[2]} This is defined by the difference \code{ l[2] - l[3]}.
#'@return \code{dl[3]} This is defined by the difference \code{ l[3] - l[4]}.
#'@return \code{...}

#'@return \code{z[1]} The lowest threshold of the (Gaussian) bi-normal assumption.
#'@return \code{z[2]}  The 2nd threshold of the (Gaussian) bi normal assumption.
#'@return \code{z[3]} The 3rd threshold of the (Gaussian) bi normal assumption.
#'@return \code{z[4]} The fourth threshold of the (Gaussian) bi-normal assumption.
#'@return \code{aa} This is defined by \code{m/v}, please see the author's paper for more detail.
#'@return \code{bb} This is defined by \code{1/v}, please see the author's paper for more detail.
#'@return \code{AA} The area under alternative FROC curve associated to reader and modality.
#'@return \code{A} The area under alternative FROC curve associated to modality.
#'@return \code{hyper_v} Standard deviation of \code{AA} around \code{A}.

#'@return \code{lp__} The logarithmic likelihood of our model for your data.
#'
#'
#'@seealso
#'\strong{---------  Before fitting:} \emph{ create a dataset}

#' \describe{
#'\item{ \code{ \link{convertFromJafroc}}            }{ Convert from JAFROC format xlsx file to the author's format}
#'\item{ \code{ \link{dataset_creator_new_version}}  }{Create an \R object which represent user data.}
#'\item{ \code{ \link{create_dataset}}               }{Create an \R object which represent user data.}
#'\item{ \strong{---------  Further sequential analysis: Plot curves}  }{Using the result of fitting a Bayesian FROC model, we can go sequential analysis.}
#'\item{ \code{ \link{DrawCurves}}                   }{ for drawing free response ROC curves.}
#'\item{ \strong{---------  Further sequential analysis: Validation of the Model} }{}
#'\item{ \code{ \link{ppp}}  }{  Calculation of a p-value in the Bayesian paradigm.}
#'\item{   \strong{---------  \R objects of example datasets from real world or fictitious:} }{}
#'\item{ \code{\link{dataList.Chakra.1}}  }{A \code{list} for an example dataset of a single reader and a single modality data. The word Chakra in the dataset name means that it appears  in the paper of Chakraborty.  }
#'\item{ \code{\link{dataList.Chakra.2}}  }{A \code{list}  for an example dataset of a single reader and a single modality data. The word Chakra in the dataset name means that it appears  in the paper of Chakraborty.  }
#'\item{ \code{\link{dataList.Chakra.3}}  }{A \code{list}  for an example dataset of a single reader and a single modality data. The word Chakra in the dataset name means that it appears  in the paper of Chakraborty.  }
#'\item{ \code{\link{dataList.Chakra.4}}  }{A \code{list}  for an example dataset of a single reader and a single modality data. The word Chakra in the dataset name means that it appears  in the paper of Chakraborty.  }
#'\item{ \code{\link{dataList.high.ability}}  }{A \code{list}  for an example dataset of a single reader and a single modality data  }
#'\item{ \code{\link{dataList.low.ability}}  }{A \code{list}  for an example dataset of a single reader and a single modality data  }
#'\item{ \code{\link{dataList.Chakra.Web}}  }{A \code{list}  for an example dataset of multiple readers and  multiple modalities data. The word Chakra in the dataset name means that it appears  in the paper of Chakraborty. }
#'\item{ \code{\link{data.hier.ficitious}}  }{A \code{list}  for an example dataset of  multiple readers and  multiple modalities data }
#'\item{ \code{\link{dataList.High}}  }{A \code{list}  for an example dataset of a single reader and a single modality data   whose AUC is  high.}
#'\item{ \code{\link{dataList.Low}}  }{A \code{list}  for an example dataset of a single reader and a single modality data   whose AUC is  low.}
#'\item{ \code{\link{data.bad.fit}}  }{A \code{list}  for an example dataset of a single reader and a single modality data   whose fitting is bad, that is chi square is very large. However the MCMC convergence criterion is satisfied with very high quality. Thus the good MCMC convergence does not mean the model is correct. So, to fit a model to this data, we should change the latent Gaussian and differential logarithmic Gaussian to more appropriate distributions for hit and false alarm rate. In theoretically perspective, there is no a a prior distribution for hit and false alarm rate. So, if we encounter not good fitting data, then we should change the model, and such change will occur in the latent distributions. The reason why the author saved this data is to show that our model is not unique  nor good and gives a future research directions. To tell the truth the author is not interested the FROC theory. My background is mathematics, geometry, pure mathematics. So, I want to go back to my home ground. This program are made to show my skill for programming or my ability. But, now, I do not think to get job. I want to go back mathematics. Soon, my paper is published which is related Gromov Hausdorff topology. Of course, I will publish this package's theory soon. Please wait.  }
#'\item{ \code{\link{d}},\code{\link{dd}} ,\code{\link{ddd}} ,\code{\link{dddd}} ,\code{\link{ddddd}},\code{\link{dddddd}},\code{\link{ddddddd}}     }{ The other datasets, the author like these datasets because name is very simple.  }
#'}
#'
#'@references
#' Bayesian Models for Free-response Receiver Operating Characteristic Analysis; Pre-print
#' See \href{https://cran.r-project.org/package=BayesianFROC}{ vignettes  }
#'
#'
#'@examples
#' \dontrun{

#'\dontrun{
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                               The 1-st example
#'#========================================================================================
#'#
#'#
#'#                  Making FROC Data and Fitting a Model to the data
#'#
#'#                                Notations
#'#
#'#            h = hits = TP = True Positives
#'#            f = False alarms = FP = False Positives
#'#
#'#
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#            1)             Build a data-set
#'#========================================================================================

#'
#'                 BayesianFROC:::clearWorkspace()
#'
#'# For a single reader and a single modality  case.
#'
#'     dat <- list(c=c(3,2,1),    #     Confidence level. Note that c is ignored.
#'             h=c(97,32,31), #     Number of hits for each confidence level
#'             f=c(1,14,74),  #     Number of false alarms for each confidence level
#'
#'             NL=259,        #     Number of lesions
#'             NI=57,         #     Number of images
#'             C=3)           #     Number of confidence level
#'
#'
#'       if (interactive()){   viewdata(dat)}
#'
#'#  where,
#'#      c denotes confidence level, i.e., rating of reader.
#'#                3 = Definitely diseased,
#'#                2 = subtle,.. diseased
#'#                1 = very subtle
#'#      h denotes number of hits (True Positives: TP) for each confidence level,
#'#      f denotes number of false alarms (False Positives: FP) for each confidence level,
#'#      NL denotes number of lesions,
#'#      NI denotes number of images,

#'
#'
#'# For example, in the above example data,
#'#  the number of hits with confidence level 3 is 97,
#'#  the number of hits with confidence level 2 is 32,
#'#  the number of hits with confidence level 1 is 31,
#'
#'#  the number of false alarms with confidence level 3 is 1,
#'#  the number of false alarms with confidence level 2 is 14,
#'#  the number of false alarms with confidence level 1 is 74,
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                         2)       Fit an FROC model to the above dataset.
#'#========================================================================================
#'
#'
#'
#'
#'
#'
#'           fit <-   fit_Bayesian_FROC(
#'                            dat,       # dataset
#'                            ite = 1111,  #To run in time <5s.
#'                            cha = 1,      # number of chains, it is better more large.
#'                            summary = FALSE
#'                                )
#'
#'
#'
#'# The return value "fit" is an S4 object of class "stanfitExtended" which is inherited
#'# from the S4 class "stanfit".
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#             3)  Change the S4 class of fitted model object
#'# Change the S4 class from "stanfitExtended" to "stanfit" to apply other packages.
#'# The fitted model object of class "stanfit" is  available for the package ggmcmc, rstan
#'# Thus, to use such package, we coerce the class into "stanfit" as follows:
#'
#'# Changing the class from stanfitExtended to stanfit,
#'# we can apply other pakcage's functions to the resulting object.
#'
#'#========================================================================================
#'
#'
#'
#'
#'                    fit.stan   <-   methods::as(fit,"stanfit")
#'
#'
#'
#'# Then, return value "fit.stan" is no longer an S4 object of class "stanfitExtended" but
#'# the S4 object of class "stanfit".
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#             3.1)  Apply the functions for the class stanfit
#'#========================================================================================

#'
#'grDevices::dev.new();rstan::stan_hist(fit.stan, bins=33,pars = c("A"))
#'grDevices::dev.new();rstan::stan_hist(fit.stan, bins=22,pars = c("A"))
#'grDevices::dev.new();rstan::stan_hist(fit.stan, bins=11,pars = c("A"))
#'
#'grDevices::dev.off()
#'
#'# I am not sure why the above stan_hist also works for the new S4 class "stanfitExtended"
#'
#'# Get pipe operator
#'
#'
#'                  `%>%`    <-    utils::getFromNamespace("%>%", "magrittr")
#'
#'
#'
#'# Plot about MCMC samples of parameter name "A", representing AUC
#'
#'
#'
#'# Trace-plot density for parameter "A"
#'grDevices::dev.new()
#'      ggmcmc::ggs(fit.stan)   %>%   ggmcmc::ggs_traceplot(family	= "A")
#'grDevices::dev.off()
#'# Posterior density for parameter "A"
#'grDevices::dev.new()
#'      ggmcmc::ggs(fit.stan)   %>%   ggmcmc::ggs_density(family	= "A")
#'grDevices::dev.off()
#'
#'# Auto-correlation for parameter "A"
#'grDevices::dev.new()
#'      ggmcmc::ggs(fit.stan)   %>%   ggmcmc::ggs_autocorrelation(family	= "A")
#'grDevices::dev.off()
#'
#'
#'
#'
#'# The author does not think the inherited class "stanfitExtended" is good,
#'# Since the size of object is very redundant and large,
#'# which caused by the fact that inherited class contains plot data for FROC curve.
#'# To show the difference of size for the fitted model object of class
#'# stanfitExtended and stanfit, we execute the following code;
#'
#'
#'    size_of_return_value(fit) - size_of_return_value(methods::as(fit,"stanfit"))
#'
#'
#'
#'
#'
#'
#'
#' #4) Using the S4 object fit, we can go further step, such as calculation of the
#' # Chisquare and the p value of the Bayesian version for testing the goodness of fit.
#' # I think p value has problems that it relies on the sample size with monotonicity.
#' # But it is well used, thus I hate but I implement the p value.
#'
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                                   REMARK
#'#========================================================================================
#'
#'#
#'# Should not write the above data as follows:
#'
#'# MANNER (A)   dat <- list(c=c(1,2,3),h=c(31,32,97),f=c(74,14,1),NL=259,NI=57,C=3)
#'
#'
#'# Even if user writes data in the above MANNER (A),
#'# the program interprets it as the following MANNER (B);
#'
#'# MANNER (B)   dat <- list(c=c(3,2,1),h=c(31,32,97),f=c(74,14,1),NL=259,NI=57,C=3)
#'
#'# Because the vector c is ignored in the program,
#'# and it is generated by the code rep(C:1) automatically  in the internal of the function.
#'# So, we can omit the vector c from the list.
#'
#'

#'
#'#This package is very rigid format, so please be sure that your format is
#'#exactly same to the data in this package.
#'#More precisely, the confidence level vector should be denoted rep(C:1) (Not rep(1:C)).
#'#  Note that confidence level vector c  should not be specified.
#'#   If specified, will be ignored ,
#'#  since it is created by   c <-c(rep(C:1)) in the program and
#'#  do not refer from user input confidence level vector,
#'#  where C is the highest number of confidence levels.
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####

# devtools::document();help("fit_Bayesian_FROC")

#'
#'
#'
#'
#'
#'
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####

# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                               The 2-nd example
#'#========================================================================================
#'#

#'
#'#    (1)First, we prepare the data from this package.
#'
#'
#'                  dat  <- BayesianFROC::dataList.Chakra.1
#'
#'
#'#    (2)Second, we run fit_Bayesian_FROC() in which the rstan::stan() is implemented.
#'#    with data named "dat"  and the author's Bayesian model.
#'
#'
#'                  fit <-  fit_Bayesian_FROC(dat,
#'                            ite = 1111  #To run in time <5s.
#'                            )
#'
#'
#'
#'
#'
#'
#' #   Now, we get the stan's out put, i.e.,  an S4 class object named "fit".
#'
#'# << Minor Comments>>
#'#  More precisely, this is an S4 object of some inherited class (named stanfitExtended)
#'#  which is extended using stan's S4 class named "stanfit".
#'
#'
#'  fit.stan <- methods::as(fit,"stanfit")

#'#  Using the output "fit.stan",
#'
#'#  we can use the functions in the "rstan" package, for example, as follows;
#'
#'   grDevices::dev.new();
#'          rstan::stan_trace(fit.stan, pars = c("A"))# stochastic process of a posterior estimate
#'          rstan::stan_hist(fit.stan, pars = c("A")) # Histogram of a posterior estimate
#'          rstan::stan_rhat(fit.stan, pars = c("A")) # Histogram of rhat for all parameters
#'          rstan::summary(fit.stan, pars = c("A"))   # summary of fit.stan by rstan
#'  grDevices::dev.off()
#'
#'
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####

# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                               The 3-rd example
#'#========================================================================================
#'
#' #    Fit a model to a hand made data
#'
#' #     1) Build the data for a single reader and a single modality  case.
#'
#'    dat <- list(
#'             c=c(3,2,1),    #  Confidence level, which is ignored.
#'             h=c(97,32,31), #  Number of hits for each confidence level
#'             f=c(1,14,74),  #  Number of false alarms for each confidence level
#'
#'             NL=259,       #   Number of lesions
#'             NI=57,        #   Number of images
#'             C=3)          #   Number of confidence level
#'
#'
#'
#'
#'#  where,
#'#        c denotes confidence level, , each components indicates that
#'#                3 = Definitely lesion,
#'#                2 = subtle,
#'#                1 = very subtle
#'#          That is the high number indicates the high confidence level.
#'#        h denotes number of hits
#'#          (True Positives: TP) for each confidence level,
#'#        f denotes number of false alarms
#'#          (False Positives: FP) for each confidence level,
#'#        NL denotes number of lesions,
#'#        NI denotes number of images,

#'
#'
#'#     2) Fit  and draw FROC and AFROC curves.
#'
#'
#'
#'
#'            fit <-   fit_Bayesian_FROC(dat, DrawCurve = TRUE)
#'
#'
#'
#'# (( REMARK ))
#'#           Changing the hits and false alarms denoted by h and  f
#'#           in the above dataset denoted by dat,
#'#           user can fit a model to various datasets and draw corresponding FROC curves.
#'#           Enjoy drawing the curves for various datasets in case of
#'#           a single reader and a single modality data
#'
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#  For Prior and Bayesian Update:
#'
#'#            Calculates a posterior mean  and  variance
#'
#'#                                                         for each parameter
#'#========================================================================================
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####

#'
#'
#'# Mean values of posterior samples are used as a  point estimates,  and
#'# Although the variance of posteriors receives less attention,
#'# but to make a prior, we will need the it.
#'# For, example, if we assume that model parameter m has prior distributed by
#'# Gaussian, then we have to know the mean and variance to characterize prior.
#'
#'
#'                 e <- rstan::extract(fit)
#'
#'
#'
#'#  model parameter m and v is a number,
#'#  indicating the mean  and variance of signal distribution, respectively.
#'
#'                 stats::var(e$m)
#'
#'                 mean(e$m)
#'
#'
#'
#'
#'                 stats::var(e$v)
#'
#'                 mean(e$v)
#'
#'
#'
#'# The model parameter z or dz is a vector, and thus we execute the following;
#'
#'#   z = (   z[1],  z[2],  z[3]  )
#'
#'#  dz = (   z[2]-z[1],     z[3]-z[2]   )
#'
#'
#'
#'# `Posterior mean of posterior MCMC samples for parameter z and dz
#'
#'
#'               apply(e$dz, 2, mean)
#'
#'               apply(e$z, 2, mean)
#'
#'
#'
#'
#'
#'
#'# `Posterior variance of posterior MCMC samples for parameter z and dz
#'
#'
#'
#'               apply(e$dz, 2, var)
#'
#'               apply(e$z, 2, var)
#'
#'
#'
#'
#'
#'
#'               apply(e$dl, 2, mean)
#'
#'               apply(e$l, 2, mean)
#'
#'               apply(e$p, 2, mean)
#'
#'               apply(e$p, 2, var)
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'# Revised 2019 Sept 6
#'
#'
#'
#'
# ----1---- ----2---- ----3---- ----4---- ----5---- ----6---- ----7---- ----8---- ----9----
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                               The 4-th example
#'#========================================================================================
#'#
#'
#'
#'## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#'#         1) Build the data interactively,
#'
#'                       dataList <-  create_dataset()
#'
#'#Now, as as a return value of create_dataset(), we get the FROC data (list) named dataList.
#'
#'#        2) Fit an MRMC or srsc FROC model.
#'
#'                       fit <-  fit_Bayesian_FROC(dataList)
#'
#'
#'}## Only run examples in interactive R sessions
#'
#'
# ####1**** ****2**** ****3**** ****4**** ****5**** ****6**** ****7**** ****8**** ****9****
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                               The 5-th example
#'#========================================================================================
#'# Comparison of the posterior probability for AUC
#'
#'
#'# In the following, we calculate the probability of the events that
#'# the AUC of some modality is greater than the AUC of another modality.
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#     Posterior Probability for some events of AUCs by using posterior MCMC samples
#'#========================================================================================

#'
#'
#'# This example shows how to use the stanfit (stanfit.Extended) object.
#'# Using stanfit object, we can extract posterior samples and using these samples,
#'# we can calculate the posterior probability of research questions.
#'
#'
#'
#'     fit <- fit_Bayesian_FROC(dataList.Chakra.Web.orderd,ite = 1111,summary =FALSE)
#'
#'
#'
#'#    For example, we shall show the code to compute the posterior probability of the ever
#'#    that  the AUC of modality 1 is larger than that of modality 2:
#'
#'
#'
#'                               e <- extract(fit)
#'
#'
#'# This code means that the MCMC samples are retained in the object e for all parameters.
#'# For example, the AUC is extracted by the code e$A and it is a two dimensional array.
#'# The first component indicates the MCMC samples and
#'# the second component indicate the modality ID.
#'
#'# For example, the code e$A[,1] means the vector of MCMC samples of the 1 st modality.
#'# For example, the code e$A[,2] means the vector of MCMC samples of the 2 nd modality.
#'# For example, the code e$A[,3] means the vector of MCMC samples of the 3 rd modality.

#'#    To calculate the posterior probability of the ever
#'#    that the AUC of modality 1 is larger than that of modality 2,
#'#    we execute the following R script:
#'
#'                         mean(e$A[,1] > e$A[,2])
#'
#'
#'#    Similarly, to compute the posterior probability that
#'#     the AUC of modality 1 is larger  than  that of modality 3:
#'
#'                         mean(e$A[,1] > e$A[,3])
#'
#'
#'#    Similarly, to compute the posterior probability that
#'#     the AUC of modality 1 is larger  than  that of modality 4:
#'
#'                         mean(e$A[,1] > e$A[,4])
#'
#'
#'#    Similarly, to compute the posterior probability that
#'#     the AUC of modality 1 is larger  than  that of modality 5:
#'
#'                         mean(e$A[,1] > e$A[,5])
#'
#'
#'#    Similarly, to compute the posterior probability that
#'#     the AUC of modality 1 is larger  than  that of modality 5 at least 0.01
#'
#'
#'                         mean(e$A[,1] > e$A[,5]+0.01)
#'
#'
#'#      Similarly,
#'
#'                  mean( e$A[,1] > e$A[,5] + 0.01 )
#'                  mean( e$A[,1] > e$A[,5] + 0.02 )
#'                  mean( e$A[,1] > e$A[,5] + 0.03 )
#'                  mean( e$A[,1] > e$A[,5] + 0.04 )
#'                  mean( e$A[,1] > e$A[,5] + 0.05 )
#'                  mean( e$A[,1] > e$A[,5] + 0.06 )
#'                  mean( e$A[,1] > e$A[,5] + 0.07 )
#'                  mean( e$A[,1] > e$A[,5] + 0.08 )
#'
#'
#'
#' # Since any posterior distribution tends to the Dirac measure whose center is
#' # true parameter under the assumption that the model is correct in the sense that the
#' # true distribution is belongs to a family of models.
#' # Thus using this procedure, we will get
#' # the true parameter if any more large sample size we can take.
#'
#'
#' #      Close the graphic device to avoid errors in R CMD check.
#'
#'                       Close_all_graphic_devices()
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                               The 6-th Example for MRMC data
#'#========================================================================================
#'
#'
#'
#'# To draw FROC curves for each modality and each reader, the author provides codes.
#'# First, we make a fitted object of class stanfitExtended as following manner.
#'
#'
#'      fit <- fit_Bayesian_FROC( ite  = 1111,
#'                                   cha = 1,
#'                               summary = FALSE,
#'                       Null.Hypothesis  = FALSE,
#'                              dataList = dd # This is a MRMC dataset.
#'                               )
#'
#'# Using this fitted model object called fit, we can draw FROC curves for the
#'# 1-st modality as following manner:
#'
#'
#' DrawCurves(
#'# This is a fitted model object
#'            fit,
#'# Here, the modality is specified
#'            modalityID = 1,
#'# Reader is specified 1,2,3,4
#'            readerID = 1:4,
#'# If TRUE, the new imaging device is created and curves are drawn in it.
#'             new.imaging.device = TRUE
#'             )
#'
#'
#'
#'# The next codes are quite same, except modality ID and new.imaging.device
#'# The code that "new.imaging.device = F" means that the curves are drawn using
#'# the previous imaging device to plot the 1-st and 2-nd modality curves draw in the same
#'# Plot plain. Drawing in different curves in same plain, we can compare the curve
#'# of modality. Of course, the interpretation of FROC curve is the ordinal ROC curve,
#'# that is,
#'# if curve is upper then the observer performance with its modality is more greater.
#'# So, please enjoy drawing curves.
#'
#'            DrawCurves(fit,modalityID = 2,readerID = 1:4, new.imaging.device = FALSE)
#'            DrawCurves(fit,modalityID = 3,readerID = 1:4, new.imaging.device = FALSE)
#'            DrawCurves(fit,modalityID = 4,readerID = 1:4, new.imaging.device = FALSE)
#'            DrawCurves(fit,modalityID = 5,readerID = 1:4, new.imaging.device = FALSE)
#'
#'
#'                       Close_all_graphic_devices()

#'
# 111111111 222222222 333333333 444444444 555555555 666666666 777777777 888888888 999999999
# ----1---- ----2---- ----3---- ----4---- ----5---- ----6---- ----7---- ----8---- ----9----
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                               The 7-th example NON-CONVERGENT CASE 2019 OCT.
#'#========================================================================================
#'
#'
#'
#'
#'
#'
#'# ff <- fit_Bayesian_FROC( ite  = 1111,  cha = 1, summary = TRUE, dataList = ddd )
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' dat <- list(
#'   c=c(3,2,1),    #Confidence level
#'   h=c(73703933,15661264,12360003), #Number of hits for each confidence level
#'   f=c(1738825,53666125 , 254965774),  #Number of false alarms for each confidence level
#'
#'   NL=100000000,       #Number of lesions
#'   NI=200000000,        #Number of images
#'   C=3)          #Number of confidence level
#'
#'
#'
#'
#'

#'# From the examples of the function mu_truth_creator_for_many_readers_MRMC_data()
#'#========================================================================================
#'#                  Large number of readers cause non-convergence
#'#========================================================================================
#'
#'
#'   v <- v_truth_creator_for_many_readers_MRMC_data(M=4,Q=6)
#' m <- mu_truth_creator_for_many_readers_MRMC_data(M=4,Q=6)
#' d <-create_dataList_MRMC(mu.truth = m,v.truth = v)
#' # fit <- fit_Bayesian_FROC( ite  = 111,  cha = 1, summary = TRUE, dataList = d )
#'
#' plot_FPF_and_TPF_from_a_dataset(d)
#'
#'
#'
#'
#'#========================================================================================
#'#                             convergence
#'#========================================================================================
#'
#'
#'
#'  v  <- v_truth_creator_for_many_readers_MRMC_data(M=2,Q=21)
#'  m  <- mu_truth_creator_for_many_readers_MRMC_data(M=2,Q=21)
#'  d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#'# fit <- fit_Bayesian_FROC( ite  = 200,  cha = 1, summary = TRUE, dataList = d)
#'
#'
#'#========================================================================================
#'#                            non-convergence
#'#========================================================================================
#'
#'
#'
#' v  <- v_truth_creator_for_many_readers_MRMC_data(M=5,Q=6)
#'  m  <- mu_truth_creator_for_many_readers_MRMC_data(M=5,Q=6)
#'  d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#' # fit <- fit_Bayesian_FROC( ite  = 111,  cha = 1, summary = TRUE, dataList = d)
#'
#'
#'
#'#========================================================================================
#'#                           convergence
#'#========================================================================================
#'
#'
#' v  <- v_truth_creator_for_many_readers_MRMC_data(M=1,Q=36)
#' m  <- mu_truth_creator_for_many_readers_MRMC_data(M=1,Q=36)
#' d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#'# fit <- fit_Bayesian_FROC( ite  = 1111,  cha = 1, summary = TRUE, dataList = d)
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'#========================================================================================
#'#                            non-convergence
#'#========================================================================================
#'
#'
#' v  <- v_truth_creator_for_many_readers_MRMC_data(M=1,Q=37)
#' m  <- mu_truth_creator_for_many_readers_MRMC_data(M=1,Q=37)
#' d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#' # fit <- fit_Bayesian_FROC( ite  = 111,  cha = 1, summary = TRUE, dataList = d)
#'
#'
#'
#'
#'
#'#========================================================================================
#'#                            convergence A single modality and 11 readers
#'#========================================================================================
#'
#' v <- v_truth_creator_for_many_readers_MRMC_data(M=1,Q=11)
#' m <- mu_truth_creator_for_many_readers_MRMC_data(M=1,Q=11)
#' d <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#'#  fit <- fit_Bayesian_FROC( ite = 1111,
#'#                           cha = 1,
#'#                       summary = TRUE,
#'#                      dataList = d,
#'#                           see = 123455)
#'#
#'## f <-fit
#'# DrawCurves( summary = FALSE,
#'#          modalityID = c(1:f@dataList$M),
#'#             readerID = c(1:f@dataList$Q),
#'#             StanS4class = f  )
#'#
#'#
#'#
#'#
#'#========================================================================================
#'#                            convergence A single modality and 17 readers
#'#========================================================================================
#'
#'
#'
#' v <- v_truth_creator_for_many_readers_MRMC_data(M=1,Q=17)
#' m <- mu_truth_creator_for_many_readers_MRMC_data(M=1,Q=17)
#' d <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#'# fit <- fit_Bayesian_FROC( ite = 1111, cha = 1, summary = TRUE, dataList = d,see = 123455)
#'#
#'# f <-fit
#'# DrawCurves( summary = FALSE,   modalityID = c(1:f@dataList$M),
#'#             readerID = c(1:f@dataList$Q),f  )
#'#
#'#
#'# DrawCurves( summary = FALSE,   modalityID = 1,
#'#             readerID = c(8,9),f  )
#'#
#'## For readerID 8,9, this model is bad
#'#
#'Close_all_graphic_devices()
#'
#'
#'
#'
#'
#'#========================================================================================
#'#                            convergence 37 readers, 1 modality
#'#========================================================================================
#'
#'
#'
#' v  <- v_truth_creator_for_many_readers_MRMC_data(M=1,Q=37)
#' m  <- mu_truth_creator_for_many_readers_MRMC_data(M=1,Q=37)
#' d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#'# fit <- fit_Bayesian_FROC(see = 2345678, ite  = 2000,  cha = 1, summary = TRUE, dataList = d)
#'#
#'#
#'# f <-fit
#'# DrawCurves( summary = FALSE,   modalityID = c(1:f@dataList$M),
#'#             readerID = c(1:f@dataList$Q),f  )
#'#
#'#
#'# DrawCurves( summary = FALSE,   modalityID = 1,
#'#             readerID = c(8,9),f  )

#'
#' # In the following, consider two readers whose ID are 8 and 15, respectively.
#' # Obviously, one of them will have high performamce than the other,
#' # however,
#' # Sometimes, the FROC curve dose not reflect it,
#' # Namely, one of the FROC curve is upper than the other
#' # even if the FPF and TPF are not.... WHY???
#'
#'
#'
#'# DrawCurves( summary = FALSE,   modalityID = 1,
#'#             readerID = c(8,15),f  )
#'#
#'Close_all_graphic_devices()
#'
#'
#'
#'Close_all_graphic_devices()
#'}#dontrun

#'}# dontrun

# devtools::document();help("fit_Bayesian_FROC")
#'@inheritParams extractAUC

#' @export fit_Bayesian_FROC
#'@importFrom Rcpp evalCpp cpp_object_initializer
#'
# #devtools::use_package("base") #This cause the error, so not run.
#' @import Rcpp

#' @import rstan
#'
# ____________________------
# This  @import rstan is very important

fit_Bayesian_FROC <- function(
                dataList,
                ModifiedPoisson = FALSE,
                prior=-1,# Proper, Non-informative
                zz=1,
                verbose = TRUE,
                print_CI_of_AUC = TRUE,

                model_reparametrized = FALSE,
                Model_MRMC_non_hierarchical = TRUE,

                prototype = FALSE,
                PreciseLogLikelihood = TRUE,
                DrawCurve = length(dataList$m)==0,
                Drawcol = TRUE,
                summary=TRUE,
                make.csv.file.to.draw.curve=FALSE,
                mesh.for.drawing.curve=10000,
                significantLevel = 0.7,
                new.imaging.device=TRUE,
                cha = 1,
                ite = 10000,
                DrawFROCcurve=TRUE,
                DrawAFROCcurve=FALSE,
                DrawCFPCTP=TRUE,
                dig = 5,
                war = floor(ite/5),
                see = 1234567,
                Null.Hypothesis=FALSE,
                ...
){

  # options(mc.cores = parallel::detectCores())
  # if(requireNamespace("rstan" ,quietly = TRUE))rstan::rstan_options(auto_write = TRUE)
  # Sys.setenv(LOCAL_CPPFLAGS = '-march=native')


  #  message("Data:", substitute(dataList))
  dataList.Name <-  deparse(substitute(dataList))

  # message(crayon::silver$bold("Data: "),crayon::green$bold( substitute(dataList)  ),"\n")
  if(summary==TRUE&&ModifiedPoisson==FALSE) cat(crayon::silver$bold("False Positive Fraction per image"))
  if(summary==TRUE&&ModifiedPoisson==T) cat(crayon::silver$bold("False Positive Fraction per lesion"))






  if ( length(dataList[["m"]])==0  ) {# the reader =0 occur even the case of MRMC


    if(summary==TRUE) prior_print_srsc(prior = prior)


    if(summary==TRUE)    message(crayon::silver$bold("Study Design: "),crayon::green$bold("srsc case"),"\n")


    # fit_Srsc ------
    fit_srsc(
      verbose = verbose,

      dataList =dataList ,
      zz=zz,
      new.imaging.device=new.imaging.device,
      dataList.Name = dataList.Name,
      prior=prior,
      prototype = prototype,
      model_reparametrized =model_reparametrized,
      mesh.for.drawing.curve=mesh.for.drawing.curve,
      DrawCurve = DrawCurve ,
      ModifiedPoisson = ModifiedPoisson,
      PreciseLogLikelihood = PreciseLogLikelihood,
      Drawcol = Drawcol,
      make.csv.file.to.draw.curve=make.csv.file.to.draw.curve,
      summary=summary,
      DrawFROCcurve=DrawFROCcurve,
      DrawAFROCcurve=DrawAFROCcurve,
      DrawCFPCTP=DrawCFPCTP,
      cha = cha ,
      ite = ite ,
      dig = dig ,
      war = war ,
      see = see,
      ...

    )







  } else if (length(dataList[["m"]]) >= 1) {

    # message("\n* MRMC case. \n")
    message(crayon::silver$bold("Study Design: "),crayon::green$bold("MRMC case"),"\n")
    cat(crayon::silver$bold("False Positive Fraction is calculated"))

    message(crayon::silver("We calculate false alarm rate by per lesion in MRMC cases.  Per image are not availble in MRMC case."))

    if (!ite>250) {
      message(crayon::silver("\n* Your iteration is very small. You should raise the number of ite"))
      warn <- crayon::red $ bold
      message(warn("\n* Warning: The number of iteration is very small. Please raise the variable, (e.g. ite = 30000) to draw more large samples in the Hamiltonian Monte Carlo Simulation.\n"))
    }



    if (Null.Hypothesis == FALSE) {
      message(crayon::silver$bold( "\n* An Alaternative model is fitted, whose null hypothesis is that all modality are same observer performance abilities. \n") )

      if(summary==TRUE)  prior_print_MRMC(prior = prior)
      # fit_MRMC ------

      fit_MRMC(
        dataList = dataList,
        DrawCurve =  DrawCurve,
        dataList.Name = dataList.Name,
        prior=prior,
        zz=zz,
        prototype = prototype,
        model_reparametrized =model_reparametrized,
        Model_MRMC_non_hierarchical =Model_MRMC_non_hierarchical,

        verbose = verbose,
        print_CI_of_AUC = print_CI_of_AUC,

        # PreciseLogLikelihood = PreciseLogLikelihood,
        PreciseLogLikelihood = TRUE,
        # Null.Hypothesis =Null.Hypothesis,

        mesh.for.drawing.curve=mesh.for.drawing.curve  ,
        significantLevel =significantLevel,
        summary=summary,
        ModifiedPoisson=ModifiedPoisson,
        cha  = cha,
        war  = war,
        ite  = ite,
        dig  = dig,
        see  = see,
        ...


      )

    }else if (Null.Hypothesis == TRUE) {
      message("\n* Fit the Null hypothesis model to data. Null hypothesis all modality are same observer performance.")

      fit_Null_hypothesis_model_to_(
        dataList = dataList,
        DrawCurve =  DrawCurve,
        # PreciseLogLikelihood = PreciseLogLikelihood,
        PreciseLogLikelihood = TRUE,
        # Null.Hypothesis =Null.Hypothesis,
        dataList.Name = dataList.Name,

        mesh.for.drawing.curve=mesh.for.drawing.curve  ,
        significantLevel =significantLevel,
        summary=summary,

        cha  = cha,
        war  = war,
        ite  = ite,
        dig  = dig,
        see  = see,
        ...


      )
    }# Null.Hypothesis

















  } else

    message("Confirm your data's column of modality ID is named by exactly m.")



}


























#' @title  Fit and Draw the FROC models (curves)
#'@inheritParams fit_Bayesian_FROC
#'@description  Fit and Draw the FROC models (curves).
#'@inheritParams fit_srsc

# devtools::use_package("base")# this will cause error, do not run!!
# devtools::use_package("rstan")
# devtools::use_package("knitr")
# devtools::use_package("readxl")
# devtools::use_package("openxlsx")
# devtools::use_package("xlsx")
# @importFrom base system.file
# devtools::document();help("fit_MRMC") # Confirm reflection
#' @export fit_MRMC
#'
# ________________ ----------------

fit_MRMC<- function(
  dataList,
  DrawCurve = FALSE,
  verbose = TRUE,
  print_CI_of_AUC = TRUE,
  PreciseLogLikelihood = FALSE,
  summary =TRUE,
  dataList.Name = "",
  prior=1,
  ModifiedPoisson=TRUE,
  mesh.for.drawing.curve=10000,
  significantLevel = 0.7,
  cha = 1,
  war = floor(ite/5),
  ite = 10000,
  dig = 3,
  see = 1234569,
  Null.Hypothesis=FALSE,
  prototype = FALSE,
  model_reparametrized =FALSE,
  Model_MRMC_non_hierarchical = TRUE,

  zz=1,
  ...

){

  M <- dataList$M

  if ( is.null(dataList$m)||is.null(dataList$q)||is.null(dataList$c)) {
    return(message("Dataformat error.  Your data may be not multiple reader and multiple case,
                   but a single reader and a singlr modality case."))
  }


  if(summary)viewdata(dataList ) # I do not know, but this code is the availble only in the last part.


  # model -----
  if(Null.Hypothesis==FALSE){
    if(PreciseLogLikelihood == FALSE  ){
      # scr <- "Model_MRMC.stan";
      if(!M==1)  scr <- system.file("extdata", "Model_MRMC.stan", package="BayesianFROC")
      if (prototype) scr <- system.file("extdata", "Model_MRMC_prototype.stan", package="BayesianFROC")
    }else{
      if(PreciseLogLikelihood == TRUE  ){
        # scr <- "Model_Hiera_TargetFormulation.stan";
        if(!M==1)  scr <- system.file("extdata", "Model_MRMC.stan", package="BayesianFROC")
        if (prototype) scr <- system.file("extdata", "Model_MRMC_prototype.stan", package="BayesianFROC")
      } else{
        print("PreciseLogLikelihood is allowed only two choice; TRUE or FALSE.")
      }}
  }#Null.Hypothesis

  if(Null.Hypothesis==TRUE)   scr <- system.file("extdata", "null_hier.stan", package="BayesianFROC")
  if (model_reparametrized)  scr <-  system.file("extdata", "Model_MRMC_reparametrized.stan", package="BayesianFROC")
  if (Model_MRMC_non_hierarchical)  scr <-  system.file("extdata", "Model_MRMC_non_hierarchical.stan", package="BayesianFROC")


  if(M==1)  scr <- system.file("extdata", "Model_Hiera_OneModalityMultipleReader_TargetFormulation.stan", package="BayesianFROC")

  data <-metadata_to_fit_MRMC(dataList,ModifiedPoisson)


  # data ----
  data <- c(data,
            prior=prior,
            PreciseLogLikelihood=PreciseLogLikelihood,
            zz=zz,
            prototype=prototype
  )


  m<-data$m   ;S<-data$S;  NL<-data$NL;NI<-data$NI;c<-data$c;q<-data$q;
  h<-data$h; f<-data$f;
  hh<-data$hh; hhN<-data$hhN;
  ff<-data$ff;ffN<-data$ffN;
  harray<-data$harray;    farray<-data$farray;
  hharray<-data$hharray;    ffarray<-data$ffarray;
  hharrayN<-data$hharrayN;    ffarrayN<-data$ffarrayN;
  C<-as.integer(data$C)
  M<-as.integer(data$M)
  N<-as.integer(data$N)
  Q<-as.integer(data$Q)


  if (summary==FALSE)message("\n* Now, the Hamiltonian Monte Carlo simulation is running...")
  if (summary==FALSE)message(crayon::silver( "\n* To print the procedure, set  summary = TRUE. "))


  rstan_options(auto_write = TRUE)

  scr <- rstan::stan_model(scr)# add

  # initial <- initial_values_specification_for_stan_in_case_of_MRMC(dataList)

  # fit -----
  if (summary==FALSE) {


    invisible(utils::capture.output(
      fit  <-  rstan::sampling(
        object= scr, data=data,  verbose = FALSE,
        seed=see, chains=cha, warmup=war, iter=ite
        , control = list(adapt_delta = 0.9999999,
                         max_treedepth = 15)
        # ,init = initial
      )
    ))
  }#if


  if (summary==TRUE) {
# fit -----
    fit  <-  rstan::sampling(
      object= scr, data=data,  verbose = FALSE,
      seed=see, chains=cha, warmup=war, iter=ite
      , control = list(adapt_delta = 0.9999999,
                       max_treedepth = 15)#,init = initial
    )

  }#if


  #   fit  <- stan(file=scr, model_name=scr, data=data, verbose = FALSE,
  #                seed=see, chains=cha, warmup=war,
  #                iter=ite, control = list(adapt_delta = 0.9999999,
  #                                         max_treedepth = 15)
  # )

  # rstan::check_hmc_diagnostics(fit)

  convergence <- ConfirmConvergence(fit)
  #   if(convergence ==FALSE){
  #
  #     message("\n
  # * So, model has no mean, we have to finish a calculation !!
  # * Changing seed may help, that is, variable [see = 1] or [see = 123] or [see = 12345] or ...
  #
  #             \n")
  #     return(fit)}
  #   if(convergence ==TRUE){ if(summary==TRUE)         message(crayon::silver( "\n* We do not stop, since model cannot be said not converged.\n"))
  #                          }

  # if(summary==TRUE) {
  #   # message("---------- Useage of the return value-------------------------  \n")
  #   message(crayon::silver( "\n * Using the return value which is S4 class generated by rstan::stan, you can draw FROC and AFROC curves.   \n"))
  #   message(crayon::silver( "\n * Using this return value, you can apply functions in the package rstan, e.g., rstan::traceplot().   \n"))
  #   if(PreciseLogLikelihood == FALSE  ){
  #     message(crayon::silver( "\n* WAIC did not caluculated, since log likelihood is not a precise value.\n"))
  #   }else{
  #     if(PreciseLogLikelihood == TRUE  ){
  #       message(crayon::silver( "\n* WAIC was caluculated, since log likelihoodis is a precise value, i.e., the traget += statement are used in the stan file.\n"))
  #       waic(fit)
  #     } else{
  #       print("PreciseLogLikelihood is allowed only two choice; TRUE or FALSE.")
  #     }}
  # }# if summary ==TRUE


  fit.new.class <- methods::as(fit,"stanfitExtended")
  fit.new.class@metadata <-data
  fit.new.class@dataList <-dataList
  fit.new.class@studyDesign <-  "MRMC"
  fit.new.class@PreciseLogLikelihood    <-  PreciseLogLikelihood
  fit.new.class@ModifiedPoisson    <- ModifiedPoisson
  if(PreciseLogLikelihood==TRUE) {fit.new.class@WAIC <- waic(fit,dig,summary=FALSE)}
  fit.new.class@convergence    <-  convergence
  fit.new.class@plotdataMRMC  <-  metadata_to_DrawCurve_MRMC(fit.new.class,mesh.for.drawing.curve = mesh.for.drawing.curve)
  fit.new.class@prototype    <-  prototype

  if ( dataList.Name==""   ) dataList.Name <-  deparse(substitute(dataList))
  fit.new.class@dataList.Name <- dataList.Name








  # is(fit,"stanfit")
  # getClass("stanfit")


  #Change the S4 object (fit) from old class to new class


# AUC -----
  # if(!M==1) {
  extractAUC(
    StanS4class=fit.new.class,
    summary=summary,
    dig=dig,
    print_CI_of_AUC = print_CI_of_AUC
  )
  # }
  if(summary ==FALSE){ message(crayon::silver( "\n* To see results, summary=TRUE \n"))}


  check_rhat(fit)


  rstan::check_hmc_diagnostics(fit)

  if(summary){size_of_return_value(summary=summary,object =  fit.new.class)}
  if(summary&&(!M==1)&&!model_reparametrized)summarize_MRMC(fit.new.class,dig=dig)#fit@ModifiedPoisson is used in this function
  if(summary&&(!M==1)) sortAUC(fit.new.class)


  if(DrawCurve == TRUE  ){

    message(crayon::silver( "* Now, we draw the FROC and AFROC curves. \n"))
    message(crayon::silver( "* Please wait ... . \n"))


    DrawCurves(    modalityID  = 1:fit.new.class@dataList$M,
                   readerID    = 1:fit.new.class@dataList$Q,
                   StanS4class =   fit.new.class,
                       summary = FALSE)

  }
  invisible(fit.new.class)




}








































#' @title  fit a model to data in the case of
#' A Single reader and A Single modality (srsc).
#'@description  Build a \emph{fitted model object}  in case of  \strong{single reader
#'and single modality} data \code{dataList}. FPF is \strong{per image}.
#'@details Revised 2019.Jun. 17
#'@param dataList.Name This is not for user, but the author for this package development.
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves_MRMC_pairwise
#'@param dataList A list, to be fitted a model.
#'For example, in case of a single reader and a single modality,
#'it consists of  \code{f, h, NL, NI, C}.
#'The detail of these dataset,
#' see the example data-sets.
#'Note that the maximal number of confidence level,
#' denoted by  \code{C}, are included,
#' however,
#' should not include its each confidence level in \code{dataList}
#'@return An S4 object of class \code{stanfitExtended},
#'which is an inherited S4 class from \code{stanfit}.
#'
#'To change the S4 class, use
#'
#'@examples
#' \dontrun{
#'#First, prepare the example data from this package.
#'
#'
#'
#'           dat  <- get(data("dataList.Chakra.1"))
#'
#'
#'
#'
#'#Second, fit a model to data named "dat"
#'
#'
#'
#'
#'
#'            fit <-  fit_srsc(dat)
#'
#'
#'
#'
#'
#'
#'
#' #      Close the graphic device to avoid errors in R CMD check.
#'
#'          Close_all_graphic_devices()
#'
#'
#'
#'}# dottest
#' @export fit_srsc
#'
#'
#___________________------

fit_srsc <- function(
  dataList,
  prior = -1,
  new.imaging.device=TRUE,
  dataList.Name = "",
  ModifiedPoisson = FALSE,
  model_reparametrized =FALSE,
  verbose = TRUE,

  DrawCurve = TRUE,
  PreciseLogLikelihood = TRUE,
  Drawcol = TRUE,
  make.csv.file.to.draw.curve=FALSE,
  mesh.for.drawing.curve=10000,
  summary =TRUE,
  DrawFROCcurve=TRUE,
  DrawAFROCcurve=FALSE,
  DrawCFPCTP=TRUE,
  cha = 4,
  ite = 3000,
  dig = 5,
  war = floor(ite/5),
  see = 1234,

  prototype = FALSE,

  # ww=-0.81,
  # www =0.001,
  # mm=0.65,
  # mmm=0.001,
  # vv=5.31,
  # vvv=0.001,
  # zzz=0.001
  zz= 2.55,
  ...

){

  if(ModifiedPoisson==TRUE) NX <- dataList$NL
  if(ModifiedPoisson==FALSE)NX <-dataList$NI
  if(summary==TRUE) {  viewdata(dataList )}

  data <- metadata_srsc_per_image(dataList,ModifiedPoisson)




  ww=-0.81;
  www =0.001;
  mm=0.65;
  mmm=0.001;
  vv=5.31;
  vvv=0.001;
  # zz= 1.55;
  zzz=0.001;



  # data ----

  data <- c(data,
            prior=prior,
            PreciseLogLikelihood=PreciseLogLikelihood,
            ModifiedPoisson=ModifiedPoisson,
            zz=zz
  )
  C <- as.integer(data$C)
  f <- data$f
  h <- data$h
  NI <- data$NI
  NL <- data$NL

  ff <- data$ff

  hh <- data$hh
  # scr <- "Model_srsc_per_image.stan";
  #base::system.file is not go well
  # model ----

                scr <-  system.file("extdata", "Model_srsc.stan", package="BayesianFROC")
  if(prototype) scr <-  system.file("extdata", "Model_srsc_prototype.stan", package="BayesianFROC")
if (model_reparametrized)  scr <-  system.file("extdata", "Model_srsc_reparametrized.stan", package="BayesianFROC")

  initial <-c("m"=1,"v"=5,"w"=0,"dz"=1/2)

  rstan::rstan_options(auto_write = TRUE)
  scr <- rstan::stan_model(scr)# add

  # fit ----

  if (summary==FALSE) {


    invisible(utils::capture.output(
      fit  <-  rstan::sampling(
        object= scr, data=data,  verbose = FALSE,
        seed=see, chains=cha, warmup=war, iter=ite
        , control = list(adapt_delta = 0.9999999,
                         max_treedepth = 15),init = initial
      )
    ))
  }#if


  if (summary==TRUE) {

    fit  <-  rstan::sampling(
      object= scr, data=data,  verbose=FALSE,
      seed=see, chains=cha, warmup=war, iter=ite
      , control = list(adapt_delta = 0.9999999,
                       max_treedepth = 15),init = initial
    )

  }#if




  rstan::check_hmc_diagnostics(fit)
  check_rhat(fit)


  convergence <- ConfirmConvergence(fit,summary = summary)
  if(convergence ==FALSE)message("\n* So, model is unreliable!!\n")

    # fit.new.class <- methods::as(fit,"stanfitExtended")
    # fit.new.class@metadata <-data
    # fit.new.class@dataList <-dataList
    # # slot -------------------
    # if(!ModifiedPoisson)  fit.new.class@studyDesign <-  "srsc.per.image"
    # if(ModifiedPoisson)  fit.new.class@studyDesign <-  "srsc.per.lesion"
    #
    # # if(PreciseLogLikelihood==TRUE) {fit.new.class@WAIC <- waic(fit,dig,summary=FALSE)}
    # fit.new.class@convergence    <-  convergence
    # # fit.new.class@chisquare <- chisquare
    # fit.new.class@PreciseLogLikelihood    <-  PreciseLogLikelihood
    # fit.new.class@prototype    <-  prototype



    # return(fit.new.class)
  # }
  # if(convergence ==TRUE){   if(summary==TRUE) message(crayon::silver("\n* We do not stop, since model converged.\n"))}

  if(summary==TRUE) {print(fit )}
  if(summary==FALSE) {  message(crayon::silver("\n* summary = TRUE for more details.\n"))}



  if(PreciseLogLikelihood == FALSE  ){
    if(summary==TRUE) message(crayon::silver("\n* WAIC was not caluculated, since log likelihood is not a precise value."))
  }else{
    if(PreciseLogLikelihood == TRUE  ){
      if(summary==TRUE)  message(crayon::silver("\n* WAIC was caluculated,\n since log likelihoodis is a precise value, i.e., the traget += statement are used in the stan file."))
      waic <-waic(fit,dig,summary=verbose)
    } else{
      print("* PreciseLogLikelihood is allowed only two choice; TRUE or FALSE.")
    }}

  MCMC=(ite-war)*cha
  # EAP_a <-  array(0, dim=c(  MCMC))
  # EAP_b <-  array(0, dim=c(  MCMC))
  # EAP_a <- 0
  # EAP_b <- 0
  # s<-0
  # t<-0
  # for(mc in 1:MCMC){
  #   s<-  EAP_a
  #   EAP_a <-  s+ a[mc]
  #   t<-  EAP_b
  #   EAP_b <-  t+ b[mc]
  # }
  # EAP_a<-EAP_a/MCMC
  # EAP_b<-EAP_b/MCMC

  # MCMC=(ite-war)*cha
  # #--------- chi ^2 -----------Start
  # p<-rstan::extract(fit)$p
  # lchi<-rstan::extract(fit)$l
  # EAP_p <-  array(0, dim=c(   C))
  # EAP_l <-  array(0, dim=c(   C+1))
  #
  # s <-  array(0, dim=c(   C))
  # t <-  array(0, dim=c(   C+1))
  # for(mc in 1:MCMC){
  #   for(cd in 1:C){
  #     s[ cd]<-  EAP_p[ cd]
  #
  #     EAP_p[ cd] <-  s[ cd]+ p[mc,cd]
  #   }
  #   for(cd in 0:C){
  #     t[ cd]<-  EAP_l[ cd]
  #     EAP_l[ cd] <-  t[ cd]+ lchi[mc,cd]
  #   }
  # }
  # EAP_p<-EAP_p/MCMC
  # EAP_l<-EAP_l/MCMC
  #
  # ss<-vector()
  # tt<-vector()
  # for(cd in 1:C){
  #
  #   ss[cd]<-(h[C+1-cd]-NL*EAP_p[cd])^2/(NL*EAP_p[cd])
  #   tt[cd]<-(f[C+1-cd]-NI*(EAP_l[cd]-EAP_l[cd+1]))^2/(NI*(EAP_l[cd]-EAP_l[cd+1]))
  #
  # }
  # chisquare <- sum(ss)+sum(tt)
  ###

  e <-extract_EAP_CI(fit,"l",dataList$C ,summary = FALSE)
  lambda <- e$l.EAP

  e <-extract_EAP_CI(fit,"p",dataList$C ,summary = FALSE)
  p <- e$p.EAP
  chisquare <-   chi_square_goodness_of_fit_from_input_all_param(

    h   =   h,
    f   =   f,
    p   =   p,
    lambda  =   lambda,
    NL  =   NL,
    NI  =   NI,
    ModifiedPoisson =  ModifiedPoisson
  )
  chisquare <- signif(chisquare,digits = dig)
  #Draw the  AFROC curve-----
  set.seed(1);ll<- stats::rchisq(mesh.for.drawing.curve, 1)
  lll<- 0.99+ll

  # for near 0 and 1, FROC cure are parse, if usual points, so we generate points with very large weights for 0 and 1
  l0<-pracma::logspace(-0.5, -222, mesh.for.drawing.curve)
  l2<-pracma::linspace(0, 1.5, mesh.for.drawing.curve)
  l3<-pracma::logspace(0,3, mesh.for.drawing.curve)


  l4<-append(l0,l2)
  la<-append(l4,l3)

  lb<-append(ll,lll)

  l <- append(la,lb)

  x<- 1-exp(-l) #AFROC
  y <-  array(0, dim=c(length(x)))


  # here aaaaaaaaaaaaaaaaaaaaaaa -----

  a<-rstan::extract(fit)$a
  b<-rstan::extract(fit)$b
  EAP_a<-mean(a)
  EAP_b<-mean(b)


  for(i in 1:length(x)) y[i]<-1-stats::pnorm(EAP_b*stats::qnorm(exp(-l[i]))-EAP_a)


  # if(DrawCurve == FALSE  ||DrawCurve == FALSE){
  #   if(summary==TRUE)  message(crayon::silver(" \n We de not draw the FROC and AFROC curves. \n"))
  # }






  #     if(summary ==TRUE){
  #   message("\n--------------------------------------------------  \n")
  #   message("* The goodness of fit chi-square statistic is equal to ",  signif(chisquare,digits = dig),". \n")
  #   message("\n* The representation of goodness of fit chi-square statistic is given in the Chakraborty's paper; Med Phys. 1989 Jul-Aug;16(4):561-8. Maximum likelihood analysis of free-response receiver operating characteristic (FROC) data. Chakraborty DP. It is also given in the author's paper.\n")
  #   message("--------------------------------------------------  \n")
  #   message("\n The expected a posterior estimate of the area under the FROC curve: \n \n ")
  #   pasteAUC <- paste("AUC =  ",  signif(summary(fit)$summary["A","mean"],digits = dig), " \n" )
  #   message(pasteAUC)
  #   message("\n The 95% Credible Interval of AUC := [ lower bound, upper bound] is the following:\n \n ")
  #   message("The 95%CI = [",signif( summary(fit)$summary["A","2.5%"],digits = dig), ",",signif(summary(fit)$summary["A","97.5%"],digits = dig), "]." )
  #   message("\n--------------------------------------------------  \n")
  # }




  fit.new.class <- methods::as(fit,"stanfitExtended")
  fit.new.class@metadata <-data
  fit.new.class@dataList <-dataList
  # slot -------------------
  if(!ModifiedPoisson)  fit.new.class@studyDesign <-  "srsc.per.image"
  if( ModifiedPoisson)  fit.new.class@studyDesign <-  "srsc.per.lesion"
  if(PreciseLogLikelihood==TRUE) {fit.new.class@WAIC <- waic(fit,dig,summary=FALSE)}
  fit.new.class@convergence    <-  convergence
  fit.new.class@chisquare <- chisquare

  fit.new.class@PreciseLogLikelihood    <-  PreciseLogLikelihood
  fit.new.class@ModifiedPoisson   <- ModifiedPoisson
  fit.new.class@prototype    <-  prototype



  if(verbose){
    summary_EAP_CI_srsc(
      StanS4class=fit.new.class,
      dig=dig
    )
  }#if
  extractAUC(
    StanS4class=fit.new.class,
    # summary=summary,
    summary =   verbose,
    dig=dig
  )


  drawdata <- data.frame(x.AFROC =x,
                         y.AFROC=y,
                         x.FROC= l,
                         y.FROC=y )
  if(make.csv.file.to.draw.curve==TRUE){
    message("\n\n* Please wait ... now we launch two scv files to draw your FROC curve and cumulative hits and false alarms")
    #Launch the Draw data---START
    xlsx::write.xlsx (drawdata, paste(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/DrawData.xlsx", sep = ""),  col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE)
    message("* A DrawData.csv are created in your desktop. \n* Using this csv file, you can draw the FROC and AFROC curves by scatter plot.")
    drawTPFP <- data.frame(NumberOfCumulativeFalsePositives =ff,
                           NumberOfCumulativeTurePositives=hh)
    xlsx::write.xlsx (drawTPFP, paste(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/DrawPoints.xlsx",  sep = ""),col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE)
    message("\n* A DrawPoints.csv are created in your desktop. \n")
    message("\n* Using this csv file you can plot cumlative false positives and cumulative true positives by scatter plot.")
  }
  #Launch the Draw data---STOP
  fit.new.class@plotdata <-drawdata



  if(!sum(rstan::get_divergent_iterations(fit))==0){

    message("\n* Divergence:",    sum(rstan::get_divergent_iterations(fit)) )
    message("\n* Iterations:",    length(rstan::get_divergent_iterations(fit)) )# = cha*(ite-war)
    message("\n* Rate: ", 100*sum(rstan::get_divergent_iterations(fit))/length(rstan::get_divergent_iterations(fit)),"% \n")
  }


  fit.new.class@Divergences      <- sum(rstan::get_divergent_iterations(fit))
  fit.new.class@MCMC.Iterations       <- length(rstan::get_divergent_iterations(fit))
  fit.new.class@Divergence.rate  <- 100*sum(rstan::get_divergent_iterations(fit))/length(rstan::get_divergent_iterations(fit))
  if ( dataList.Name==""   ) dataList.Name <-  deparse(substitute(dataList))
  fit.new.class@dataList.Name <- dataList.Name

  # here ----

  if(DrawCurve   )DrawCurves(fit.new.class, Colour = TRUE, new.imaging.device = T)
  if(summary  ){size_of_return_value(summary=summary,object =  fit.new.class)}
  invisible(fit.new.class)

}



#
# ll<- stats::rchisq(100, 1)
# lll<- 0.99+ll
# l<-append(ll,lll)
#
# x<- 1-exp(-l)
# y <- 1-stats::pnorm(0.3*stats::qnorm( exp(-l ) )-0.5)
#
# x <- c(0, x, 1)
# y <- c(0, y, 1)
#
# plot(x,y,xlim=c(0,1),ylim=c(0,1))
































#' @title  Fit the null model
#'@description  Fit the null model, representing the null hypothesis that all modalities are same.
#'@inheritParams fit_srsc
#'@inheritParams fit_Bayesian_FROC
#' @export fit_Null_hypothesis_model_to_
#'
#'
#______________-------
fit_Null_hypothesis_model_to_<- function(
  dataList,
  DrawCurve = FALSE,
  PreciseLogLikelihood = FALSE,
  dataList.Name = "",
  ModifiedPoisson = FALSE,
  verbose = TRUE,
  summary =TRUE,
  mesh.for.drawing.curve=10000,
  significantLevel = 0.7,
  cha = 1,
  war = floor(ite/5),
  ite = 10000,
  dig = 3,
  see = 1234569,
  ...


){

  if ( is.null(dataList$m)||is.null(dataList$q)||is.null(dataList$c)) {
    return(message("Dataformat error.  Your data may be not multiple reader and multiple case,
                   but a single reader and a singlr modality case."))
  }


  if(summary==TRUE){ viewdata(dataList )} # I do not know, but this code is the availble only in the last part.

  # war <- 5000 ;ite <- 10000;see <- 1234;dig <- 3;cha <- 4;
  # scr <- "Model_Hiera.stan";
  # source("Data_Hiera.R");

  scr <- system.file("extdata", "null_hier.stan", package="BayesianFROC")



  data <-metadata_to_fit_MRMC(dataList,ModifiedPoisson)

  m<-data$m   ;S<-data$S;  NL<-data$NL;c<-data$c;q<-data$q;
  h<-data$h; f<-data$f;
  hh<-data$hh; hhN<-data$hhN;
  ff<-data$ff;ffN<-data$ffN;
  harray<-data$harray;    farray<-data$farray;
  hharray<-data$hharray;    ffarray<-data$ffarray;
  hharrayN<-data$hharrayN;    ffarrayN<-data$ffarrayN;

  C<-as.integer(data$C)
  M<-as.integer(data$M)
  N<-as.integer(data$N)
  Q<-as.integer(data$Q)


  if (summary==FALSE)message("\n* Now, the Hamiltonian Monte Carlo simulation is running...")


  rstan_options(auto_write = TRUE)

  scr <- rstan::stan_model(scr)# add

  # initial <- initial_values_specification_for_stan_in_case_of_MRMC(dataList)
  if (summary==FALSE) {


    invisible(utils::capture.output(
      fit  <-  rstan::sampling(
        object= scr, data=data,  verbose = FALSE,
        seed=see, chains=cha, warmup=war, iter=ite
        , control = list(adapt_delta = 0.9999999,
                         max_treedepth = 15)
        # ,init = initial
      )
    ))
  }#if


  if (summary==TRUE) {

    fit  <-  rstan::sampling(
      object= scr, data=data,  verbose = FALSE,
      seed=see, chains=cha, warmup=war, iter=ite
      , control = list(adapt_delta = 0.9999999,
                       max_treedepth = 15)#,init = initial
    )

  }#if


  #   fit  <- stan(file=scr, model_name=scr, data=data, verbose = FALSE,
  #                seed=see, chains=cha, warmup=war,
  #                iter=ite, control = list(adapt_delta = 0.9999999,
  #                                         max_treedepth = 15)
  # )

  convergence <- ConfirmConvergence(fit)
  if(convergence ==FALSE){message("\n* So, model has no mean, we have to finish a calculation !!\n")
    return(fit)}
  if(convergence ==TRUE){ if(summary==TRUE)         message(crayon::silver( "\n* We do not stop, since model cannot be said not converged.\n"))
  }

  if(summary==TRUE) {
    # message("---------- Useage of the return value-------------------------  \n")
    message(crayon::silver( "\n * Using this return value which is S4 class generated by rstan::stan and another function in this package, you can draw FROC and AFROC curves.   \n"))
    message(crayon::silver( "\n * Using this return value, you can apply functions in the package rstan, e.g., rstan::traceplot().   \n"))
    if(PreciseLogLikelihood == FALSE  ){
      message(crayon::silver( "\n* WAIC did not caluculated, since log likelihood is not a precise value.\n"))
    }else{
      if(PreciseLogLikelihood == TRUE  ){
        message(crayon::silver( "\n* WAIC was caluculated, since log likelihoodis is a precise value, i.e., the traget += statement are used in the stan file.\n"))
        waic(fit,summary = verbose)
      } else{
        print("PreciseLogLikelihood is allowed only two choice; TRUE or FALSE.")
      }}
  }# if summary ==TRUE


  fit.new.class <- methods::as(fit,"stanfitExtended")
  fit.new.class@metadata <-data
  fit.new.class@dataList <-dataList
  fit.new.class@studyDesign <-  "MRMC"
  fit.new.class@PreciseLogLikelihood    <-  PreciseLogLikelihood


  if(DrawCurve == TRUE  ){

    message(crayon::silver( "* Now, we draw the FROC and AFROC curves. \n"))
    message(crayon::silver( "* Please wait ... . \n"))
# hhhhh-----------
    DrawCurves_MRMC(
      StanS4class = fit.new.class
    )


  }else{
    if(DrawCurve == FALSE  ){
      message(crayon::silver( "\n* We de not draw the FROC and AFROC curves. \n"))
    } else{
      message(crayon::silver( "\n* DrawCurve is allowed only two choice; TRUE or FALSE. \n"))
    }
  }



  # is(fit,"stanfit")
  # getClass("stanfit")


  #Change the S4 object (fit) from old class to new class

  if(PreciseLogLikelihood==TRUE) {fit.new.class@WAIC <- waic(fit,dig,summary=FALSE)}
  fit.new.class@convergence    <-  convergence
  fit.new.class@plotdataMRMC  <-  metadata_to_DrawCurve_MRMC(fit.new.class,mesh.for.drawing.curve = mesh.for.drawing.curve)



  if(!summary) message(crayon::silver( "\n* To see results, summary=TRUE \n"))
  check_rhat(fit)
  rstan::check_hmc_diagnostics(fit)
  if(summary)size_of_return_value(summary=summary,object =  fit.new.class)
  if ( dataList.Name==""   ) dataList.Name <-  deparse(substitute(dataList))
  fit.new.class@dataList.Name <- dataList.Name
  fit.new.class@ModifiedPoisson <- ModifiedPoisson

  invisible(fit.new.class)




}

