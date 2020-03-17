#' @title Theory of FROC Analysis via Bayesian Approaches
#' @description
#'
#' The crucial difference of
#'  the author's model and the classical Chakraborty's model is that
#' the former is a \emph{\sQuote{generative} model} but the later is \dQuote{not.}
#' In the  general theory of Statistic,
#'  the fake data synthesized from models are important.
#' Thus, the author made such  generative models
#' and implements these models in the package.
#'
# description -----

#' The following \pkg{Shiny} based GUI
#'  available to help  user executes
#'  this package and the
#'   author's techniques with relaxation
#'   without feeling stuck.
#'
#' \describe{
#' \item{  \code{ \link{fit_GUI_Shiny}()}         }{Interactive GUI for a single reader and single modality}
#' \item{  \code{ \link{fit_GUI_Shiny_MRMC}()}    }{Interactive GUI for  multiple readers and multiple modalities}
#' }
#'
#'
#'
#'
#' The aim of FROC analysis is to compare imaging \emph{modalities},
#'  which are imaging methods such as  MRI, CT, PET, etc.
#'  We want to find a imaging method with
#'   which we can find more many lesions in radiographs.
#'
#' To investigate this issue of modality comparison,
#'  we have to do a trial
#'  in order to obtain dataset consisting of \emph{TP} and \emph{FP}.
#'
#'
#details-----
#' @details
#'
#'
# ###### @keywords internal This cause heavy warning, never use.
#'
#' Here is what this package implements.
#'
#'To compare imaging modalities such as
#' MRI, CT, PET, etc, we do a  \strong{trial} from which
#'  \strong{Data} arise and we fit a \strong{model} to the data.
#'  Using the resulting model, we can compare modalities or
#'  evaluate the observer performance based on AUC.

#'
#'
#'
#'In the sequel,
#' we give a complete description
#'  about the following three terms.
#'
#'
#' \describe{
#' \item{ \strong{\emph{ Trial    }}    }{ from which  data arise.                         }
#' \item{ \strong{\emph{ Data     }}    }{ consist of the number of TPs and FPs.  }
#' \item{ \strong{\emph{ Modeling }}    }{ calculates the probability law in which  data (TPs and FPs) arise   }
#' }
#'
#'
#' \strong{\emph{ Trial    }}.
#'
#' To introduce FROC trial,
#'  let us consider the following terms.
#'
#'
#'
#'
#'
#' \describe{
#' \item{ \strong{\emph{ A reader (in other words, player)        }}     }{ who is a physician or radiologist challenges to find lesions (in other words, it is called signals, targets, nodules, ...) from radiographs.}
#' \item{ \strong{\emph{  images  (in other words, radiographs, x-ray films such as CT, MRI, PET,etc.)                           }}     }{ containing shadows (not necessarily caused by lesions). We assume that \eqn{N_L} lesions make shadows as targets. (Note that each image can contain one more lesions and this multiple signals for a single image distinct FROC trial from the ordinal ROC trial). The number of images are denoted by \eqn{N_I}.}
#' \item{ \strong{\emph{ A researcher (in other words, data-analyst)         }}     }{ knows true lesion locations (signal) and she can count readers True Positives and False Positives after  his lesion finding task. }
#' }
#'
#' For the sake of simplicity, we consider a single reader.
#'
#' Throughout this explanation,
#' we follow the convention that readers are male
#' and the researcher is female. So, "he" means the reader, and "she" means a data-analyst.
#'
#'
#'
#'  \emph{\strong{FROC trial and data}}
#'
#'
#' \describe{
#' \item{1. First trial start                                                                }{The researcher gives the reader the \strong{\emph{first}} image which contains  suspicious shadows, each of which is noise or lesion. }
#' \item{2. \strong{\emph{LESION FINDING TASK}} for the first image (trial)                  }{The reader marks (localizes) his suspicious locations of shadow (multiple answer is allowed) each of which is also assigned a \strong{\emph{integer}} indicating his \strong{\emph{confidence}} levels (if he thinks some shadow is obviously a lesion, then he gives a higher integer with respect to the shadow). So, reader marks two things: location and confidence for each suspicious shadow.}
#' \item{3. Second trial and \strong{\emph{LESION FINDING TASK}} for the second image (trial)}{The researcher gives the reader the second image and reader does the above LESION FINDING TASK for the second image.}
#' \item{4. repeat this trial for all images.                                                }{The reader do the \strong{\emph{LESION FINDING TASK}}   for all images}
#' \item{5. evaluation of TP and FP                                                          }{The researcher count the number of their true marking positions (\emph{hit}) and false making positions (\emph{false alarm}).}
#' }
#'
#' Consequently, we obtain the following table.
#'
#'
#'
#' \strong{\emph{ Example data and its Format:}}
#'
#'
#'
#'   \emph{ A single reader and a single modality case }
#'
#'
#'
#'---------------------------------------------------------------------------------------------------
#' \tabular{llll}{
#' \code{NI=63,NL=124}   \tab \strong{ confidence level } \tab \strong{ No. of false alarms} \tab \strong{No. of hits}  \cr
#'  In R console ->      \tab \code{ c} \tab   \code{f }  \tab   \code{h}  \cr
#'   -----------------------\tab ----------------------- \tab ----------------------------- \tab ------------- \cr
#' \emph{definitely} present  \tab  \code{c[1] = }5 \tab \code{f[1] = }\eqn{F_5} = 1 \tab  \code{h[1] = }\eqn{H_5} = 41 \cr
#'  \emph{probably} present   \tab  \code{c[2] = }4 \tab \code{f[2] = }\eqn{F_4} = 2 \tab  \code{h[2] = }\eqn{H_4} = 22 \cr
#'  equivocal                 \tab  \code{c[3] = }3 \tab \code{f[3] = }\eqn{F_3} = 5 \tab  \code{h[3] = }\eqn{H_3} = 14  \cr
#'  subtle                    \tab  \code{c[4] = }2 \tab \code{f[4] = }\eqn{F_2} = 11 \tab \code{h[4] = }\eqn{H_2} = 8  \cr
#'  \emph{very} subtle        \tab  \code{c[5] = }1 \tab \code{f[5] = }\eqn{F_1} = 13 \tab \code{h[5] = }\eqn{H_1} = 1  \cr
#'  }
#'---------------------------------------------------------------------------------------------------

#' We use two notations for the same number of FPs, e.g.,
#'  one is \code{f[1]} and the other is \eqn{F_5}.
#'  We use the former \code{f[1]}
#'  for programming and the later \eqn{F_5} is used for
#'  descriptions of the theory.
#'
#'  By the R code \code{BayesianFROC::viewdata(BayesianFROC::dataList.Chakra.1.with.explanation)}, we can see example data named \code{"dataList.Chakra.1.with.explanation"}.
#'
#'
#'
#'
#'
#'
#'  \emph{\strong{Modeling}}
#'
#' Our goal now is to define
#'  a model of the random variables  \eqn{H_c,F_c},
#' namely, give a probability law of   \eqn{H_c,F_c}.
#'
#'
#' First, we shall discuss our model  \strong{\emph{rigorously}} (ignore the confidence).
#' First, to simplify our argument,
#' first we reduce the FP and TP
#'  dataset from \eqn{H_c,F_c} to \eqn{H,F}
#'   by ignoring the confidence level.
#' Suppose that there are \eqn{N_L} targets (signal),
#' and radiological context, target is lesion.
#' Suppose that a radiologist try to
#'  find these lesions from radiographs.
#' Suppose that now,
#'   \strong{\emph{the reader fined \eqn{H} lesions}}
#' from radiographs which contains \eqn{N_L} lesions,
#' then it is natural to assume that
#'
#'  \deqn{ H \sim Binomial(\theta_, N_L)}
#'
#' where, the Bernoulli success rate \eqn{\theta_H} is
#'  one of parameter for our model, which should be estimated.
#'
#' In addition, suppose that
#'  \strong{\emph{the reader fails \eqn{F} times}},
#'  namely, the reader marked \eqn{F} locations
#'   in radiographs each of which is not a true lesion location.
#'   In other words, the reader marked \eqn{F} false positives.
#'  Then it is natural to assume that
#'
#'
#'  \deqn{ F \sim Poisson(\theta_F)}
#'
#'
#' where, \eqn{\theta_F} is also an another parameter of model, which should be estimated from given data.
#'So, our model has a vector  \eqn{\theta_H, \theta_F} as a model parameter.
#'
#'  The above two is very simple, since data is only \eqn{H,F},
#'  indicating the number of TP and the number of FP.
#'
#'  \strong{\emph{Unfortunately}}, the FROC data is more complex than above,
#'  namely, we have to make a model for data  \eqn{F_c} \eqn{H_c},\eqn{c=1,...,5} instead of the above simplified data \eqn{H,F}.
#'  That is, reader answers with his confidence level for each suspicious location,
#'   which is usually an integer such as  \eqn{1,2,3,4,5}.
#'
#'
#'
#'

#'
#'
#' We give a probability law for the random variables \eqn{F_c} and \eqn{H_c} for \eqn{c=1,...,5}.
#'
#' Suppose that there are \eqn{N_L} targets,
#' and radiological context,
#'  each target is a lesion contained in \eqn{N_I} Radiographs.
#' Suppose that a radiologist try to find lesions.
#' Suppose that now, he found \eqn{H_c} lesions
#' with his \eqn{c}-th confidence,
#' then we assume that each random variable \eqn{H_c} is distributed by the following law.
#'
#'
# model -----
#'  \deqn{ H_5 \sim Binomial(p_5(\theta), N_L) }
#'  \deqn{ H_4 \sim Binomial(\frac{p_4(\theta)}{1-p_5(\theta) }, N_L - H_5)}
#'  \deqn{ H_3 \sim Binomial(\frac{p_3(\theta)}{1-p_5(\theta)-p_4(\theta) }, N_L - H_5 - H_4)}
#'  \deqn{ H_2 \sim Binomial(\frac{p_2(\theta)}{1-p_5(\theta)-p_4(\theta)-p_3(\theta)  }, N_L - H_5 - H_4 -H_3)}
#'  \deqn{ H_1 \sim Binomial(\frac{p_1(\theta)}{1-p_5(\theta)-p_4(\theta)-p_3(\theta)-p_2(\theta)  }, N_L - H_5 - H_4 -H_3 -H_2)}
#'
#'  where, hit rates \eqn{p_1(\theta)},
#'                   \eqn{p_2(\theta)},
#'                   \eqn{p_3(\theta)},
#'                   \eqn{p_4(\theta)} and
#'                   \eqn{p_5(\theta)} are
#'                    some functions of
#'                    a model parameter \eqn{\theta}.
#'                    We also denote them simply by \eqn{p_c} for \eqn{p_c(\theta),c=1,2,3,4,5}.
#' In addition,
#'  suppose that the reader fails \eqn{F_c} times
#'   with his \eqn{c}-th confidence,
#'   that is,
#' the reader marked \eqn{F_c} false locations in radiographs
#' with his \eqn{c}-th confidence.

#'  Then it natural to assume that
#'
#'
#'  \deqn{ F_5 \sim Poisson(q_5(\theta)N_X)}
#'  \deqn{ F_4 \sim Poisson(q_4(\theta)N_X)}
#'  \deqn{ F_3 \sim Poisson(q_3(\theta)N_X)}
#'  \deqn{ F_2 \sim Poisson(q_2(\theta)N_X)}
#'  \deqn{ F_1 \sim Poisson(q_1(\theta)N_X)}

#'
#' where, \eqn{N_X = N_I} or \eqn{N_L}.
#'
#' The false rates
#' \eqn{q_1(\theta)},
#' \eqn{q_2(\theta)},
#' \eqn{q_3(\theta)},
#' \eqn{q_4(\theta)} and
#' \eqn{q_5(\theta)} are
#' functions  of  a parameter of model.
#'
#'
#'  The above model gives the probability law for the
#'  the random variables  \eqn{H_c,F_c, c=1,2,..,C},
#'  indicating the number of TP and the number of FP
#'   for each confidence level \eqn{c=1,2,..,C}.
#'
#'
#'
#' We define \eqn{p_c(\theta)}
#'  and \eqn{q_c(\theta)}
#'  in terms of
#'  the model parameter \eqn{\mu, \sigma, \theta_c, c=1,2,..,C}.
#'
#'
#'
#'
#' \deqn{p_c(\theta)= \int ^{\theta_{c+1}}_{\theta_c} Gaussian(z|\mu, \sigma) dz        }
#'
#'
#' \deqn{q_c(\theta)= \int ^{\theta_{c+1}}_{\theta_c} \frac{d}{dz} \log \Phi(z) dz        }
#'
#'
#' For any given dataset, we will estimate
#'  the model parameter vector \eqn{\theta};
#'
#'  \deqn{ \theta = (\theta_1,\theta_2,...,\theta_C; \mu,\sigma).}
#'
#'
#'
#'
#'
#'Intuitively, the reason why we choose such functions
#' for \eqn{p_c(\theta)} is the assumption that
#'each lesion is equipped with i.i.d. latent variable,
#' \eqn{X} distributed by \eqn{ Gaussian(z|\mu, \sigma) }, and
#'if \eqn{X} associated to some lesion falls
#'into the interval \eqn{ \theta_c <X< \theta_{c+1}},
#'then we consider that
#'the reader marks this lesion with his \eqn{c}-th confidence level.
#' In order to emphasize that each \eqn{X} is
#' associated to some  \eqn{l}-th lesion,
#'  \eqn{l=1,2,...,N_L} we denote the latent
#'  variable by \eqn{X_l} for the \eqn{l}-th lesion
#'   instead the latent decision variable \eqn{X}.
#'Here, we uses \emph{latent} to means that the variable \eqn{X}
#'cannot be observed. Since the latent variable relates decision of reader,
#' and thus, in this context
#'the latent variable is called a \emph{decision} variable.
#'
#'Similarly, suppose that each image (radiograph)
#'is associated some latent variable \eqn{Y}
#' distributed by \eqn{ N_I \frac{d}{dz} \Phi(z)}
#'  and if the \eqn{Y} associated to some image
#'   falls into interval the interval \eqn{ \theta_c <Y<\theta_{c+1}},
#'   then we consider that
#'the reader will false decision with
#'his \eqn{c}-th confidence level for the image.
#'
#'
#'
#'
#'
#  Fundamental equations -----
#'  \strong{Fundamental equations}
#'
#'
#'The reason why we use the hit rates such as \eqn{\frac{p_2}{1-p_5-p_4-p_3}} instead of \eqn{p_c} is that
#' it ensures the equality \eqn{ E[H_c/N_L] = p_c}.
#' This equality is very important
#' to establish Bayesian FROC theory
#'  so that it is compatible
#'  with the classical FROC theory.
#'  It is easy to see that the following two equality holds,
#'
#'   \deqn{ E[H_c/N_L] = p_c,}
#'   \deqn{ E[F_c/N_X] = q_c,}
#'
#'where  \eqn{E} denotes the expectation and \eqn{N_X} is the number of lesion or the number of images and
#' \eqn{q_c} is a false alarm rate, namely, \eqn{ F_c ~ Poisson(N_X q_c)}.
#'
#'
#'
#'
#'
#'
#' More precisely or to express
#' the above with model parameter explicitly,
#'  we should rewrite it as follows.
#'
#'   \deqn{ E_{\theta}[H_c/N_L] = p_c(\theta),}
#'   \deqn{ E_{\theta}[F_c/N_X] = q_c(\theta),}
#'
#'where  \eqn{E_{\theta}[X]} denotes the expectation
#' of a random variable \eqn{X}
#'  with the likelihood  \eqn{f(\omega|\theta)} of parameter \eqn{\theta}, namely,
#'
#'   \deqn{ E_{\theta}[X] :=  \int X(\omega) f(\omega|\theta)d\omega.}
#'
#'These two family of equations are most important one, and the author
#'made this model to satisfy this. Using these equations, we can define
#'the FROC curve such that
#'the curve can be interpreted as the points of expectations.
#'
#'
#' We call these equations \strong{\emph{fundamental equations}} of FROC analysis.
#' Using this, we can calculates the expectations of FPF and TPF in the later.
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
#' Some bitch will ask
#'  the author what
#'   is the original or new?
#'   So,..., for such a bitch I remark the following.
#'
#' \describe{
#' \item{ \strong{\emph{ The new model by the author is a  generative model            }}     }{ The classical model can not synthesize dataset so that the total number of hits is bounded from above by the number of lesions.  }
#' \item{ \strong{\emph{ Love         }}     }{ The new model is made with great love of the author and poor condition and poor books (to tell the truth, I did not read any books when I made a prototype) without any support of money.}
#' \item{ \strong{\emph{ A details of model           }}     }{ The formulation of hit rate differs from the classical theory. }
#' \item{ \strong{\emph{ The new model excludes the number of images           }}     }{ The formulation of false rate differs from the classical theory and it allows us to exclude the number of images from modeling. }
#' \item{ \strong{\emph{ A multiple chemical sensitivity           }}     }{  The author diseased the serious , so,,,, the author is a patient of  the chemical sensitivity, which make his life of quality much lower. }
#' \item{ \strong{\emph{ A multiple chemical sensitivity           }}     }{  The author diseased the serious , so,,,, the author is a patient of  the chemical sensitivity, which make his life of quality much lower. }
#'  }
#'#'
#' Using the above two equations, we can establish the alternative Bayesian FROC theory preserving classical notions and
#' formulas.
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
#' To fit a model to any dataset, we use the code:
#'
#' \describe{
#' \item{  \code{ \link{fit_Bayesian_FROC}()}   }{ Fit a model to data                  }
#' \item{  \code{ \link{dataList.Chakra.2}  }     }{Example data in Chakraborty 1989 paper}
#' \item{  \code{ \link{dataList.Chakra.3}  }     }{Example data in Chakraborty 1989 paper}
#' \item{  \code{ \link{dataList.Chakra.4}  }     }{Example data in Chakraborty 1989 paper}
#' }
#'
#'
# prior ----
#'  \emph{\strong{Priors on the Model Parameter.}}
#'
#'Recall that our model has the following parameter.
#'
#'  \deqn{ \theta = (\theta_1,\theta_2,...,\theta_C; \mu,\sigma).}
#'
#'  In this section, we give priors on this parameter.
#'  Only one necessarily prior is to ensure the monotonicity on the thresholds parameters.
#'  \deqn{  \theta_1 < \theta_2 < ... < \theta_C.}
#'
#'  To give this monotonicity, we have to assume ....
#'  UNDER CONSTRUCTION
#'
#'  \emph{\strong{Visualization of TP, FP by FPF, TPF}}
#'
#'  How to visualize our data constructed by hit and false alarms, that is, TP and FP?
#'  Traditionally, the so-called FPF;\emph{False Positive Fraction} and TPT:\emph{True Positive Fraction} are used.
#'  Recall that our data format:
#'
#'
#'
#'
#'  \emph{            A single reader and a single modality case   }
#' auxiliary: number of images and lesions \code{NI, NL }
#'------------------------------------------------------------------------------------------------------

#' \tabular{llll}{
#'   \tab \strong{ confidence level } \tab \strong{ No. of false alarms} \tab \strong{No. of hits}  \cr
#'            \tab    \tab   (FP:False Positive)  \tab    (TP:True Positive) \cr
#'     -----------------------\tab ----------------------- \tab ----------------------------- \tab ------------- \cr
#' \emph{definitely} present  \tab   5 \tab  \eqn{F_5}   \tab   \eqn{H_5}  \cr
#'  \emph{probably} present   \tab   4 \tab  \eqn{F_4}  \tab   \eqn{H_4}   \cr
#'  equivocal                 \tab   3 \tab  \eqn{F_3}  \tab   \eqn{H_3}    \cr
#'  subtle                    \tab   2 \tab  \eqn{F_2}  \tab  \eqn{H_2}    \cr
#'  \emph{very} subtle        \tab   1 \tab  \eqn{F_1}  \tab \eqn{H_1}    \cr
#'  }
#'
#'---------------------------------------------------------------------------------------------------
#'
#' In the above table,
#'  we introduce two kinds of random variables \eqn{F_c}  \eqn{H_c} \eqn{c=1,2,3,4,5} which are non-negative integers
#'  and please keep
#'   in mind the notations because, from now on, we use them frequently throughout this paper.
#'
#'
#'
# FPF ----
#' Recall that \emph{FPF} (\emph{ False Positive Fraction}) is defined as follows;
#'
#'
#' \deqn{FPF(5):= \frac{F_5}{N_I},}
#' \deqn{FPF(4):= \frac{F_4+F_5}{N_I},}
#' \deqn{FPF(3):= \frac{F_3+F_4+F_5}{N_I},}
#' \deqn{FPF(2):= \frac{F_2+F_3+F_4+F_5}{N_I},}
#' \deqn{FPF(1):= \frac{F_1+F_2+F_3+F_4+F_5}{N_I}.}
#'
#'
#' Similarly, \emph{TPF} (\emph{ True Positive Fraction}) is defined as follows;
#'
#'
#' \deqn{TPF(5):= \frac{H_5}{N_L},}
#' \deqn{TPF(4):= \frac{H_4+H_5}{N_L},}
#' \deqn{TPF(3):= \frac{H_3+H_4+H_5}{N_L},}
#' \deqn{TPF(2):= \frac{H_2+H_3+H_4+H_5}{N_L},}
#' \deqn{TPF(1):= \frac{H_1+H_2+H_3+H_4+H_5}{N_L}.}
#'
#'
#' Combining TPF and FPF, we obtain the pairs.
#'
#'  \deqn{(   FPF(1), TPF(1)   ),}
#'  \deqn{(   FPF(2), TPF(2)   ),}
#'  \deqn{(   FPF(3), TPF(3)   ),}
#'  \deqn{(   FPF(4), TPF(4)   ),}
#'  \deqn{(   FPF(5), TPF(5)   ).}
#'
#'
#' Plotting these five points in a two-dimensional plain,
#' we can visualize our dataset..
#'
#' In addition,
#'  connecting these points by lines,
#'  we obtain the so-called \emph{empirical FROC curve.}
#'
#'
#interpretation of the empirical FROC curve -----
#'   \emph{\strong{ interpretation of the empirical FROC curve}}
#'
#'    In fact,
#'  If a reader (physician) has a high signal detection ability,
#'   namely,
#'  he can find more lesions in Radiographs (image),
#'  then the number of TPs denoted by \eqn{H_1,H_2,H_3,H_4,H_5}
#'   will be more and more greater.
#'  Thus, the
#'
#'  \eqn{TPF(1),TPF(2),TPF(3),TPF(4),TPF(5) }
#'
#'    is also greater.
#'  Consequently, the points
#'
#'     \eqn{(   FPF(1), TPF(1)   ),(   FPF(2), TPF(2)   ),(   FPF(3), TPF(3)   ),(   FPF(4), TPF(4)   ),(   FPF(5), TPF(5)   ) }
#'
#'    are located in the more upper positions.
#'    \emph{This indicates that the high observer performance leads the empirical FROC curve more to be more upper positions.}

#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
# curve  Visualization ------
#'   \emph{\strong{Visualization of our model by curve}}
#'
#'
#'  In this section, we provides the so-called \emph{FROC curve} which is our desired
#'  visualization of estimated model.
#'  Roughly speaking, \strong{an FROC curve is expected pairs of FPF and TPF.}
#'  Namely, the points of FPF and TPF will be on FROC curve if model is  well fitting to data.
#'  So, comparing the FROC curve and the FPF and TPF, we can evaluate our goodness of fit.
#'
#'
#'  In the above, ha,... I want to die.
#'
#'
#'
#'
#'  Define  \eqn{x(c), y(c), c = 1,2,3,4,5} by
#'  the expectations of FPF and TPF, respectively, namely,
#'
#'   \deqn{x(c):= E[ FPF(c) ], }
#'
#'   \deqn{y(c):= E[ TPF(c) ]. }
#'
#' for \eqn{c = 1,2,3,4,5}.
#'
#' Using the fundamental equations \eqn{ E_{\theta}[H_c/N_L] = p_c(\theta), E_{\theta}[F_c/N_X] = q_c(\theta),},
#'  we can rewrite them
#'  in terms of the parameters \eqn{\mu, \sigma} of the latent Gaussian, as follows.
#'
#'
# I am not sure \log is available or not 2020 Jan
#'   \deqn{x(c) = E[ FPF(c) ] =\int^\infty_{\theta_c}\frac{d}{dz} \log \Phi(z) = - \log \Phi(\theta_c),}
#'
#'   \deqn{y(c) = E[ TPF(c) ] =\int^\infty_{\theta_c} Gaussian(z|\mu, \sigma) dz = \Phi(\frac{\theta_c - \mu}{\sigma}).}
#'
#'
#' From the first equation, we obtain
#' that \eqn{ \theta_c = \Phi^{-1}(\exp(-x(c)))}.
#' Substituting this into the second equation, we obtain that
#'
#'
#'   \deqn{y(c) = \Phi(\frac{ \Phi^{-1}(\exp(-x(c))) - \mu}{\sigma}).}
#'
#'
#' This implies that the set of points \eqn{(x(c), y(c)), c = 1,2,3,4,5}
#' consisting of all expectations for the pair of FPF and TPF is contained
#' in the following set:
#'
#'
#'   \deqn{  \{(x,y) |     y= \Phi(\frac{ \Phi^{-1}(\exp(-x) - \mu}{\sigma})\}.  }
#'
#'
#' We can regard this set as an image of smooth curves, Namely,
#' here we define the so-called FROC curve as a map from 1-dimensional Euclidean space to
#' 2-dimensional Euclidean space, mapping each \eqn{t>0} to
#'
#' \deqn{  (x(t),y(t) ) =(t, \Phi(\frac{ \Phi^{-1}(\exp(-t)) - \mu}{\sigma})   ) }
#'
#'
#' Because \eqn{x(t)=t,t>0} is not bounded, the area under the FROC curve is infinity.
#'
#' To calculates alternative notion of AUC in the ordinal ROC theory, we define the so-called
#' AFROC curve:
#'
#' \deqn{  (\xi(t),\eta(t) ) =(1-e^{-t}, \Phi(\frac{ \Phi^{-1}(\exp(-t)) - \mu}{\sigma})   ) }
#'
#'
#' which contained in the rectangular space \eqn{[0,1]^2}.

#' The area Under the (AFROC) curve  (briefly, we call it AUC) represents the observer performance.
#' For example, if radiologist detects more lesions with small False Positives (FPs), then AUC would be high.
#'
#'Using the parameter of the signal distribution, we express AUC as follows,
#'
#' \deqn{  AUC = \int \eta d \xi = \frac{ \mu / \sigma}{  \sqrt{1+  1/\sigma^2}  }.}
#'
#' Introducing new parameter \eqn{a:= \mu / \sigma}
#'  and \eqn{b:= 1 / \sigma}, we can also write
#'
#'
#' \deqn{  AUC = \frac{ a }{ \sqrt{1+  b^2} }. }
#'
#'
#'
# Generalized Model ------
#'  \emph{\strong{ Generalized Model}}
#'
#'
#'
#'
#' Until now, we use the following two
#'
#' \deqn{p_c(\theta)= \int ^{\theta_{c+1}}_{\theta_c} Gaussian(z|\mu, \sigma) dz        }
#'
#'
#' \deqn{q_c(\theta)= \int ^{\theta_{c+1}}_{\theta_c}  \frac{d}{dz} \Phi(z) dz        }
#'
#'for  hit rates and false alarm rates.
#'
#' However, the  explicit representations of these integrands of  \eqn{p_c(\theta),q_c(\theta) } are not determined in a prior manner.
#' So,  such   explicit representations are  redundant for a general theory.
#'   So, to simplify our argument in the following,
#'   we use general notations \eqn{ P(z|\theta_P), Q(z|\theta_Q)  }
#'    instead of the above two integrands
#'    \eqn{Gaussian(z|\mu, \sigma)} and  \eqn{ \frac{d}{dz} \log \Phi(z)},
#'    and rewrite them as follows,
#'
#' \deqn{p_c(\theta)= \int ^{\theta_{c+1}}_{\theta_c} P(z|\theta_P) dz,        }
#'
#'
#' \deqn{q_c(\theta)= \int ^{\theta_{c+1}}_{\theta_c} Q(z|\theta_Q ) dz.        }
#'
#'
#' In the sequel, we assume that
#'  \eqn{P(z|\theta_P)} is a \strong{probability density}  function (namely, its total integral is one)
#'  and \eqn{Q(z|\theta_Q)} is a \strong{positive} function (not necessarily to be a probability function).
#'   Namely,
#'
#' \deqn{  \int  P(z|\theta_P) dz  = 1,      }
#' for all \eqn{\theta_P} and
#'
#' \deqn{ Q(z|\theta_Q )  > 0,       }
#'
#' for all \eqn{z} and \eqn{\theta_Q}.
#'
#' \emph{ A single reader and a single modality   }
#'
# data table -----
#'
#'---------------------------------------------------------------------------------------------------
#' \tabular{llll}{
#' \code{NI=63,NL=124}   \tab \strong{ confidence level } \tab \strong{ No. of false alarms} \tab \strong{No. of hits}  \cr
#'  In R console ->      \tab \code{ c} \tab   \code{f }  \tab   \code{h}  \cr
#'   -----------------------\tab ----------------------- \tab ----------------------------- \tab ------------- \cr
#' \emph{definitely} present  \tab  \code{c[1] = }5 \tab \code{f[1] = }\eqn{F_5} = 1 \tab  \code{h[1] = }\eqn{H_5} = 41 \cr
#'  \emph{probably} present   \tab  \code{c[2] = }4 \tab \code{f[2] = }\eqn{F_4} = 2 \tab  \code{h[2] = }\eqn{H_4} = 22 \cr
#'  equivocal                 \tab  \code{c[3] = }3 \tab \code{f[3] = }\eqn{F_3} = 5 \tab  \code{h[3] = }\eqn{H_3} = 14  \cr
#'  subtle                    \tab  \code{c[4] = }2 \tab \code{f[4] = }\eqn{F_2} = 11 \tab \code{h[4] = }\eqn{H_2} = 8  \cr
#'  \emph{very} subtle        \tab  \code{c[5] = }1 \tab \code{f[5] = }\eqn{F_1} = 13 \tab \code{h[5] = }\eqn{H_1} = 1  \cr
#'  }
#'---------------------------------------------------------------------------------------------------

#'
#'
#'
#'
#'
#'
#'
#'
#'
#' We give a probability law for the random variables \eqn{F_c} \eqn{H_c},\eqn{c=1,...,5}.
#'
#' Suppose that there are \eqn{N_L} targets,
#' and radiological context,
#'  each target is a lesion
#'  contained in some Radiograph as a shadow.
#' Suppose that a radiologist try to find lesions for \eqn{N_I} radiographs.
#' Suppose that now, the radiologist fined \eqn{H_c} lesions
#' with his \eqn{c}-th confidence,
#' then we assume that
#'
#'
# generalized model -----
#'  \deqn{ H_5 \sim Binomial(p_5(\theta), N_L) }
#'  \deqn{ H_4 \sim Binomial(\frac{p_4(\theta)}{1-p_5(\theta) }, N_L - H_5)}
#'  \deqn{ H_3 \sim Binomial(\frac{p_3(\theta)}{1-p_5(\theta)-p_4(\theta) }, N_L - H_5 - H_4)}
#'  \deqn{ H_2 \sim Binomial(\frac{p_2(\theta)}{1-p_5(\theta)-p_4(\theta)-p_3(\theta)  }, N_L - H_5 - H_4 -H_3)}
#'  \deqn{ H_1 \sim Binomial(\frac{p_1(\theta)}{1-p_5(\theta)-p_4(\theta)-p_3(\theta)-p_2(\theta)  }, N_L - H_5 - H_4 -H_3 -H_2)}
#'
#'  where, hit rates \eqn{p_1(\theta)},
#'                   \eqn{p_2(\theta)},
#'                   \eqn{p_3(\theta)},
#'                   \eqn{p_4(\theta)} and
#'                   \eqn{p_5(\theta)} are
#'                    functions of
#'                    a model parameter \eqn{\theta}.
#' In addition,
#'  suppose that the reader fails \eqn{F_c} times
#'   with his \eqn{c}-th confidence,
#'   that is,
#' the reader marked \eqn{F_c} false positives.
#'  Then it natural to assume that
#'
#'
#'  \deqn{ F_5 \sim Poisson(q_5(\theta)N_X)}
#'  \deqn{ F_4 \sim Poisson(q_4(\theta)N_X)}
#'  \deqn{ F_3 \sim Poisson(q_3(\theta)N_X)}
#'  \deqn{ F_2 \sim Poisson(q_2(\theta)N_X)}
#'  \deqn{ F_1 \sim Poisson(q_1(\theta)N_X)}

#'
#' where, \eqn{N_X = N_I} or \eqn{N_L} false rates
#' \eqn{q_1(\theta)},
#' \eqn{q_2(\theta)},
#' \eqn{q_3(\theta)},
#' \eqn{q_4(\theta)} and
#' \eqn{q_5(\theta)} are
#' functions  of  a parameter of model.
#'
#'
#'  The above model calculates the event of the data  \eqn{H_c,F_c, c=1,2,..,C} arises,
#'  indicating the number of TP and the number of FP.
#'
#'
#'
#' We use Gaussian distributions for
#'  the  functions \eqn{p_c(\theta)}
#'     and \eqn{q_c(\theta)} as follows.
#'
#'
#' \deqn{p_c(\theta)= \int ^{\theta_{c+1}}_{\theta_c} P(z|\theta_P) dz        }
#'
#'
#' \deqn{q_c(\theta)= \int ^{\theta_{c+1}}_{\theta_c} Q(z|\theta_Q ) dz        }
#'
#'
#' where the model parameter vector is
#'
#'  \deqn{ \theta = (\theta_1,\theta_2,...,\theta_C; \theta_P,\theta_Q).}
#'
#'
#'
#' Recall that \emph{FPF} is defined as follows;
#'
#'
#' \deqn{FPF(5):= \frac{F_5                }{N_I},}
#' \deqn{FPF(4):= \frac{F_4+F_5            }{N_I},}
#' \deqn{FPF(3):= \frac{F_3+F_4+F_5        }{N_I},}
#' \deqn{FPF(2):= \frac{F_2+F_3+F_4+F_5    }{N_I},}
#' \deqn{FPF(1):= \frac{F_1+F_2+F_3+F_4+F_5}{N_I}.}
#'
#'
#' Similarly, \emph{TPF} is defined as follows;
#'
#'
#' \deqn{TPF(5):= \frac{H_5                }{N_L},}
#' \deqn{TPF(4):= \frac{H_4+H_5            }{N_L},}
#' \deqn{TPF(3):= \frac{H_3+H_4+H_5        }{N_L},}
#' \deqn{TPF(2):= \frac{H_2+H_3+H_4+H_5    }{N_L},}
#' \deqn{TPF(1):= \frac{H_1+H_2+H_3+H_4+H_5}{N_L}.}
#'
#'
#' Combining TPF and FPF, we obtain the pairs.
#'
#'  \deqn{(   FPF(1), TPF(1)   ),}
#'  \deqn{(   FPF(2), TPF(2)   ),}
#'  \deqn{(   FPF(3), TPF(3)   ),}
#'  \deqn{(   FPF(4), TPF(4)   ),}
#'  \deqn{(   FPF(5), TPF(5)   ).}
#'
#'
#' Plotting these five points in a 2-dimensional plain,
#' we can visualize our dataset.
#'
# curve  Visualization ------
#'   \emph{\strong{Visualization of a generalized model by curve}}
#'
#'
#'  In this section, we provide the so-called \emph{FROC curve} which is our desired
#'  visualization of estimated model.
#'  Roughly speaking, \strong{an FROC curve is expected pairs of FPF and TPF.}
#'  Namely, the points of FPF and TPF will be on FROC curve if model is  well fitting to data.
#'  So, comparing the FROC curve and the FPF and TPF, we can evaluate our goodness of fit.
#'
#'
#'
#'  Let \eqn{c = 1,2,3,4,5}.
#'
#'  Define
#'
#'   \deqn{x(c):= E[ FPF(c) ], }
#'
#'   \deqn{y(c):= E[ TPF(c) ]. }
#'
#'
#'
#'
#' Using the fundamental equations \eqn{ E_{\theta}[H_c/N_L] = p_c(\theta), E_{\theta}[F_c/N_X] = q_c(\theta)},
#'
#'
#'
#'   \deqn{y(c) = E[ TPF(c) ] =\int^\infty_{\theta_c} P(x|\theta_P)dx =: \Psi_P( \theta_c ),}
#'   \deqn{x(c) = E[ FPF(c) ] =\int^\infty_{\theta_c} Q(x|\theta_Q)dx =: \Psi_Q( \theta_c ),}
#'
#'
#' where \eqn{\Psi_P} and \eqn{\Psi_Q}
#' denote the cumulative distribution functions
#' of the functions \eqn{P} and \eqn{Q}, respectively. ( That is,
#' \eqn{\Psi_P(x):=\int_x^\infty P(t)dt} and
#' \eqn{\Psi_Q(x):=\int_x^\infty Q(t)dt}.)
#'
#'
#'
#' This implies that
#'  all expectations for the pair of FPF and TPF, namely
#'   \eqn{(x(c),y(c)) = (E[ FPF(c) ] , E[ TPF(c) ]) },
#'     is on the following set:
#'
#'
#'   \deqn{  \{(x(t),y(t)) |    x(t)= \Psi_Q(t),  y(t)= \Psi_P(t), t >0  \}.  }
#'
#'
#' We can regard this set as the image of
#'  the smooth curve which is called
#' \emph{the generalized FROC curve} in this manuscript.
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
#'
#'
#'
#'
#' From the first equation, we obtain
#' that \eqn{ \theta_c = \Psi_Q^{-1}( x(c) )}.
#' Substituting this into the second equation, we obtain that
#'
#'
#'   \deqn{y(c) = \Psi_P(\Psi_Q^{-1}( x(c) ) ).}
#'
#'
#' This implies that all exceptions for the pair of FPF and TPF is on the set:
#'
#'
#'   \deqn{  \{(x,y) |     y= \Psi_P(\Psi_Q^{-1}( x )).  \}.  }
#'
#'
#' We can regard this set as an image of smooth curves.
#'
#' \deqn{  (x(t),y(t) ) =(t, \Psi_P(\Psi_Q^{-1}( t )) )}
#'
#'
#' Sine \eqn{x(t)=t,t>0} is not bounded, the area under the FROC curve is infinity.
#'
#' To calculates alternative notion of AUC in the ordinal ROC theory, we define the so-called
#' AFROC curve:
#'
#' \deqn{  (\xi(t),\eta(t) ) =(1-e^{-t},   \Psi_P(\Psi_Q^{-1}( x )) )}
#'
#'
#'
#'
# model generalization fin -------
#'
#'

#'
#Hierarchical-------
#'  \emph{\strong{MRMC Model for Multiple Readers and Multiple Modalities (MRMC)}}
#'
#'
#'
#   111111111 222222222 333333333 444444444 555555555 666666666 777777777 888888888 999999999

# data table -----
#'
#'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' \tabular{cccccc}{
#' \code{NI=63,NL=124}  \tab \strong{ modality ID}  \tab \strong{ reader ID}  \tab \strong{ confidence } \tab \strong{ No. of FPs} \tab \strong{No. of TP}  \cr
#'  In R console ->      \tab \code{ m}  \tab \code{ q}  \tab \code{ c} \tab   \code{f }  \tab   \code{h}  \cr
#'   ----------------------\tab ----------------------- \tab    --------------\tab ----------------------- \tab ------------ \tab -------------------- \cr
#' \emph{definitely} present  \tab 1  \tab 1  \tab  \code{c[1] = }5 \tab \code{f[1]  = }\eqn{F_{1,1,5}} \tab  \code{h[1] = }\eqn{H_{1,1,5}} \cr
#'  \emph{probably} present   \tab 1  \tab 1  \tab  \code{c[2] = }4 \tab \code{f[2]  = }\eqn{F_{1,1,4}} \tab  \code{h[2] = }\eqn{H_{1,1,4}} \cr
#'  equivocal                 \tab 1  \tab 1  \tab  \code{c[3] = }3 \tab \code{f[3]  = }\eqn{F_{1,1,3}} \tab  \code{h[3] = }\eqn{H_{1,1,3}} \cr
#'  subtle                    \tab 1  \tab 1  \tab  \code{c[4] = }2 \tab \code{f[4]  = }\eqn{F_{1,1,2}} \tab  \code{h[4] = }\eqn{H_{1,1,2}} \cr
#'  \emph{very} subtle        \tab 1  \tab 1  \tab  \code{c[5] = }1 \tab \code{f[5]  = }\eqn{F_{1,1,1}} \tab  \code{h[5] = }\eqn{H_{1,1,1}} \cr
#'
#' \emph{definitely} present  \tab 1  \tab 2  \tab  \code{c[6] =  }5 \tab \code{f[6]  = }\eqn{F_{1,2,5}} \tab  \code{h[6] = }\eqn{H_{1,2,5}} \cr
#'  \emph{probably} present   \tab 1  \tab 2  \tab  \code{c[7] =  }4 \tab \code{f[7]  = }\eqn{F_{1,2,4}} \tab  \code{h[7] = }\eqn{H_{1,2,4}} \cr
#'  equivocal                 \tab 1  \tab 2  \tab  \code{c[8] =  }3 \tab \code{f[8]  = }\eqn{F_{1,2,3}} \tab  \code{h[8] = }\eqn{H_{1,2,3}} \cr
#'  subtle                    \tab 1  \tab 2  \tab  \code{c[9] =  }2 \tab \code{f[9]  = }\eqn{F_{1,2,2}} \tab  \code{h[9] = }\eqn{H_{1,2,2}} \cr
#'  \emph{very} subtle        \tab 1  \tab 2  \tab  \code{c[10] = }1 \tab \code{f[10] = }\eqn{F_{1,2,1}} \tab  \code{h[10] = }\eqn{H_{1,2,1}} \cr
#'
#' \emph{definitely} present  \tab 2  \tab 1  \tab  \code{c[11] = }5 \tab \code{f[11]  = }\eqn{F_{2,1,5}} \tab  \code{h[11] = }\eqn{H_{2,1,5}} \cr
#'  \emph{probably} present   \tab 2  \tab 1  \tab  \code{c[12] = }4 \tab \code{f[12]  = }\eqn{F_{2,1,4}} \tab  \code{h[12] = }\eqn{H_{2,1,4}} \cr
#'  equivocal                 \tab 2  \tab 1  \tab  \code{c[13] = }3 \tab \code{f[13]  = }\eqn{F_{2,1,3}} \tab  \code{h[13] = }\eqn{H_{2,1,3}} \cr
#'  subtle                    \tab 2  \tab 1  \tab  \code{c[14] = }2 \tab \code{f[14]  = }\eqn{F_{2,1,2}} \tab  \code{h[14] = }\eqn{H_{2,1,2}} \cr
#'  \emph{very} subtle        \tab 2  \tab 1  \tab  \code{c[15] = }1 \tab \code{f[15]  = }\eqn{F_{2,1,1}} \tab  \code{h[15] = }\eqn{H_{2,1,1}} \cr
#'
#' \emph{definitely} present  \tab 2  \tab 2  \tab  \code{c[16] =  }5 \tab \code{f[16]  = }\eqn{F_{2,2,5}} \tab  \code{h[16] = }\eqn{H_{2,2,5}} \cr
#'  \emph{probably} present   \tab 2  \tab 2  \tab  \code{c[17] =  }4 \tab \code{f[17]  = }\eqn{F_{2,2,4}} \tab  \code{h[17] = }\eqn{H_{2,2,4}} \cr
#'  equivocal                 \tab 2  \tab 2  \tab  \code{c[18] =  }3 \tab \code{f[18]  = }\eqn{F_{2,2,3}} \tab  \code{h[18] = }\eqn{H_{2,2,3}} \cr
#'  subtle                    \tab 2  \tab 2  \tab  \code{c[19] =  }2 \tab \code{f[19]  = }\eqn{F_{2,2,2}} \tab  \code{h[19] = }\eqn{H_{2,2,2}} \cr
#'  \emph{very} subtle        \tab 2  \tab 2  \tab  \code{c[20] =  }1 \tab \code{f[20] =  }\eqn{F_{2,2,1}} \tab  \code{h[20] = }\eqn{H_{2,2,1}} \cr
#'
#'      }
#'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#'
#'  An example data in this package
#'
#'  \R codes
#'
#'  \R object named \code{dd}
#'   is an example data,
#'    and to show the above
#'     table format,
#'     execute the following codes
#'
#'  \code{library(BayesianFROC);viewdata(dd)}
#'
#'
#'
#'  In this section we use
#'  the abbreviation \emph{MRMC} which
#'  means \emph{Multiple Readers and Multiple Modalities}.
#'
#'  Observer performance ability has INDIVIDUALITIES caused by  readers and modalities.
#'  Once we includes these individual differences in our Bayesian model, such model
#'  will give us  an answer for the modality comparison issues.
#'
#'
#'
#'
#' The author implements several models for MRMC.
#'
#' 1) Non hierarchical MRMC model
#'
#' 2)     hierarchical MRMC model
#'
#' 3)   A  Single reader and multiple modalities model
#'
#'
#'
#' I am a patient of Multiple Chemical Sensitivity (CS) which
#'  cause inflammations in the brain and it makes me
#'  hard to write this. I know there are many mistakes.
#'  When I read my writing, I always find and fix.
#'  Please forgive me, because CS makes me foolish.
#'

#'  \emph{\strong{MRMC model Without hyper parameter }}
#'
#'  To include heterogeneity caused by readers and modalities,
#'  the author first made a hierarchical model.
#'   However, the model has
#'  divergent transitions in MCMC iterations.
#'   Thus the author also made a
#'   non-hierarchical model in which the
#'   author removed the hyper parameters
#'    to get more stable MCMC simulation
#'  and he confirmed that the new model
#'   is divergent free with my fake data.
#'
#'
#'
#
#
# Specifying model parameters, we can replicates fake datasets.
# Different \code{seed} gives different fake data.
# Model parameters are the following.
#
#      \code{z.truth}
#
#      \code{mu.truth}
#
#      \code{v.truth}.
#
# model param -----


#'
#'In MRMC models, the model parameter is a vector denoted by
#'
#'  \deqn{ \theta = (\theta_1,\theta_2,...,\theta_C; \mu,\sigma),}
#'
#'where  each \eqn{\theta_i (i= 1,2,...,C)} is a real number and  \eqn{\mu,\sigma} are \eqn{(M, R)}-matrices whose components are denoted by
#'
#'
#'
#'\deqn{\mu_{1,1},\mu_{1,2},\mu_{1,3},...,\mu_{1,r},....,\mu_{1,R},   }
#'\deqn{\mu_{2,1},\mu_{2,2},\mu_{2,3},...,\mu_{2,r},....,\mu_{2,R},  }
#'\deqn{\mu_{3,1},\mu_{3,2},\mu_{3,3},...,\mu_{3,r},....,\mu_{3,R},   }
#'\deqn{...,}
#'\deqn{\mu_{m,1},\mu_{m,2},\mu_{m,3},...,\mu_{m,r},....,\mu_{m,R},  }
#'\deqn{...,}
#'\deqn{\mu_{M,1},\mu_{M,2},\mu_{M,3},...,\mu_{M,r},....,\mu_{M,R},  }
#'
#'
#' and
#'
#'
#'\deqn{\sigma_{1,1},\sigma_{1,2},\sigma_{1,3},...,\sigma_{1,r},....,\sigma_{1,R},   }
#'\deqn{\sigma_{2,1},\sigma_{2,2},\sigma_{2,3},...,\sigma_{2,r},....,\sigma_{2,R},  }
#'\deqn{\sigma_{3,1},\sigma_{3,2},\sigma_{3,3},...,\sigma_{3,r},....,\sigma_{3,R},   }
#'\deqn{...,}
#'\deqn{\sigma_{m,1},\sigma_{m,2},\sigma_{m,3},...,\sigma_{m,r},....,\sigma_{m,R},  }
#'\deqn{...,}
#'\deqn{\sigma_{M,1},\sigma_{M,2},\sigma_{M,3},...,\sigma_{M,r},....,\sigma_{M,R},  }
#'
#' where the subscripts \eqn{m,r} corresponds
#' the \eqn{m}-th modality
#'  and the \eqn{r}-th reader, respectively.
#'
#' Note that we use the notation \eqn{\theta} for
#'  \deqn{ \theta = (\theta_1,\theta_2,...,\theta_C; \mu,\sigma),}
#'
#'  and do not confuse it with
#'
#'  \deqn{ (\theta_1,\theta_2,...,\theta_C).}
#'
#'
#' Using the model parameter \eqn{\theta} , we can define AUC associated with
#' each pair of reader and modality as follows.
#'
#' \deqn{  AUC_{m,r}  = \frac{ \mu_{m,r} / \sigma_{m,r}}{  \sqrt{1+  1/\sigma_{m,r}^2}  }.}
#'
#' Furthermore, we can extract the efficacy of modality.
#'
#' \deqn{  AUC_{m}  = \frac{1}{R} \sum_{r=1}^R AUC_{m,r},}
#'
#' which is also denoted by \code{A[m],m=1,2,...,M} in the R console (or R studio console)
#'  and retained in the \R object of the S4 class (the so-called \emph{stanfit} or its extended class).
#'
#'  Using \code{A[m],m=1,2,...,M}, we can compare modalities such as MRI, CT, PET, etc.
#' Note that if our trial use x-ray films taken by MRI and CT, then \code{M=2}.
#' If images are taken by MRI, CT, PET, then \code{M=3}.
#' So, \code{A[m],m=1,2,...,M} is a function of the model parameter.
#' In Bayesian sense, the estimates are posterior samples and thus,
#'  \code{A[m],m=1,2,...,M} are obtained as MCMC samples.
#'   Using these,
#'    we can calculate
#'     posterior probabilities of any events.
#'     This is the author's main scheme. Ha,,, I want to
#'
#'
#'
# Probability law of hits -----
#' \strong{Probability law of hits}
#'
#' In the sequel, the subscripts \eqn{m,r} mean the \eqn{m}-th modality and the \eqn{r}-th reader, respectively.
#'
#'  Random variables of hits are distributed as follows.

#'                  \deqn{H_{5,m,r} \sim Binomial (p_{5,m,r}(\theta), N_L ),}
#'
#'  where the notation   \eqn{ H_{5,m,r} }
#'  denotes the number of hits (TPs) with confidence level \eqn{5} of  the \eqn{m}-th modality for the \eqn{r} th reader.
#'
#'
#' Now, the \eqn{ H_{5,m,r} } targets (signals, lesions) are found by the reader (radiologist),
#' and the number of remaining targets is \eqn{N_L - H_{5,m,r}}.
#'
#' Thus, the number of hits with the 4-th confidence
#'  level \eqn{H_{4,m,r}} should be drawn from
#'   the binomial distribution with remaining targets
#'    whose number is \eqn{N_L - H_{5,m,r}} and thus
#'
#'      \deqn{H_{4,m,r} \sim Binomial (\frac{p_{4,m,r}(\theta)}{1-p_{5,m,r}(\theta)}, N_L - H_{5,m,r}).}
#'
#' Similarly,
#'
#'      \deqn{H_{3,m,r} \sim Binomial (\frac{p_{3,m,r}(\theta)}{1-p_{5,m,r}(\theta)-p_{4,m,r}(\theta)}, N_L - H_{5,m,r} -H_{4,m,r}).}
#'
#'      \deqn{H_{2,m,r} \sim Binomial (\frac{p_{2,m,r}(\theta)}{1-p_{5,m,r}(\theta)-p_{4,m,r}(\theta)-p_{3,m,r}(\theta)}, N_L - H_{5,m,r} -H_{4,m,r}-H_{3,m,r}).}
#'
#'      \deqn{H_{1,m,r} \sim Binomial (\frac{p_{1,m,r}(\theta)}{1-p_{5,m,r}(\theta)-p_{4,m,r}(\theta)-p_{3,m,r}(\theta)-p_{2,m,r}(\theta)}, N_L - H_{5,m,r} -H_{4,m,r}-H_{3,m,r}-H_{2,m,r}).}
#'
#'
#'
#'
#'

#'
#'
#' \strong{Probability law of false alarms}
#'
#'Let \eqn{N_X} be the one of the followings.
#'
#'
#'
#' 1)  \eqn{N_X} = \eqn{N_L} (The number of lesions), if \code{  ModifiedPoisson = TRUE}.
#'
#' 2)  \eqn{N_X} = \eqn{N_I} (The number of images),  if \code{  ModifiedPoisson = FALSE}.
#'
#'Using \eqn{N_X}, we assume the following,
#'
#'
#'      \deqn{F_{5,m,r} \sim Poisson(q_{5}(\theta) N_X ),}
#'
#'  where subscripts \eqn{m,r} mean the \eqn{m}-th modality and the \eqn{r}-th reader, respectively.

#'      \deqn{F_{4,m,r} \sim Poisson( q_{4}(\theta) N_X ),}
#'
#' Similarly,
#'
#'      \deqn{F_{3,m,r} \sim Poisson( q_{3}(\theta) N_X ),}
#'
#'      \deqn{F_{2,m,r} \sim Poisson( q_{2}(\theta) N_X ),}
#'
#'      \deqn{F_{1,m,r} \sim Poisson( q_{1}(\theta) N_X ),}
#'

#'
#'
# here ----
#'     The rate \eqn{p_{c,m,r}(\theta)} and \eqn{q_{c}(\theta)} are calculated from the model parameter \eqn{\theta}.
#'
#'
#' We use a Gaussian distribution and the cumulative distribution function  \eqn{\Phi()} of the standard Gaussian for
#'  the  functions \eqn{p_{c,m,r}(\theta)}
#'     and \eqn{q_c(\theta)} as following manner.
#'
#'
#' \deqn{p_{c,m,r}(\theta)= \int ^{\theta_{c+1}}_{\theta_c} Normal(z|\mu_{c,m,r},v_{c,m,r}) dz        }
#'
#'
#' \deqn{q_{c}(\theta)= \int ^{\theta_{c+1}}_{\theta_c}  \frac{d}{dz} \log \Phi(z) dz        }
#'
#'
#' where the model parameter vector is
#'
#'  \deqn{ \theta = (\theta_1,\theta_2,...,\theta_C; \theta_P,\theta_Q).}
#'
#'
#'      By specifying a model parameter \eqn{ \theta = (\theta_1,\theta_2,...,\theta_C; \theta_P,\theta_Q).}
#'      we can make a fake dataset consisting of
#'      hit data \eqn{H_{c,m,r}}
#'      false alarm data \eqn{F_{c,m,r}}
#'      for each \eqn{c,m,r}. So, our model is a generative model and this is a crucial difference between our model and the classical one.
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
#'
#'
#'
#'
#'  \emph{\strong{Without hyper parameter MRMC model}}
#'
#'
#'
#'
# Non centered version ------

#'  \emph{\strong{A Non-Centered Implementation}}
#'
#' \code{AA[md,qd] ~ Normal(A[md],hyper_v[qd])}
#'
#'Non centered version is the following:
#'
#' \code{AA_tilde[md,qd] ~ Normal(0,1)}
#'
#' \code{AA[md,qd] = A[md]+hyper_v[qd]*AA_tilde}
#'
#' But, the \code{AA[md,qd]} is already defined as follows.
#'
#' \code{ AA[md,qd]=Phi(  (mu[md,qd]/v[md,qd])/sqrt((1/v[md,qd])^2+1)  ); }
#'
#'Thus usual non centered model \strong{cannot be implemented.}
#'
#'The assumption
#'
#'  \code{AA[md,qd] ~ Normal(A[md],hyper_v[qd])}
#'
#'  is an approximation. So, this model is not correct.
#'  I am not sure whether the approximation worsen my model.
#'
#' The hyper parameters have been in use for more than 2 years in this package.
#' However it caused divergent transitions. Thus the author made a new model
#' without these hyper parameters.
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
#'
#'
#'
#'
#'
#'
#'  Example dataset is  \code{\link{dd}} and \code{\link{ddd}} and \code{\link{dddd}} and \code{\link{ddddd}} and ...etc.
#'---------------------------------------------------------------------------------------------
#SBC -------
#'
#'
#'      \emph{\strong{Validation of model via SBC          }}
#'
#'  SBC tests the Null hypothesis that the MCMC sampling
#'   is correct by using some
#'  rank statistic which synthesizes a histogram.
#' If this hits gram is not uniformly distributed, then
#' we reject the null hypothesis, and we conclude that our MCMC sampling
#' contains bias.
#'
#'
#' \emph{Talts, S., Betancourt, M., Simpson, D., Vehtari, A., and Gelman, A. (2018). Validating Bayesian Inference Algorithms with Simulation-Based Calibration. arXiv preprint arXiv:1804.06788. https://arxiv.org/abs/1804.06788}
#'
#'
#'  \emph{\strong{Validation of model  via  Posterior Predictive p value        }}
#'
#'  See \code{\link{ppp}()}.
#'
#'
#'
#' Let \eqn{ \theta_1,\theta_2,...,\theta_n} be  MCMC samples from a posterior distribution \eqn{\pi(.| D)} for a given dataset \eqn{D}.
#' Let \eqn{L(y| \theta_i)} be a likelihood function for a dataset \eqn{y} and model parameter \eqn{\theta}.
#' Let
#'
#'  \deqn{ y^i_j \sim L(| \theta_i).}
#'
#' For any real-valued function \eqn{\phi=\phi(y,\theta)},
#' we can calculates its integral
#'  with the posterior predictive measure
#'  as  the approximation of two steps Monte Carlo integral
#'   as follows.
#'
#' \deqn{ \int \int \phi(y,\theta) L(y| \theta) \pi(\theta|y)dy d\theta}
#' \deqn{ =\int \Sigma_i \phi(y,\theta_i) L(y| \theta_i) dy }
#' \deqn{ =\Sigma_j \Sigma_i \phi( y^i_j,\theta_i) L( y^i_j| \theta_i).  }
#'  Using \eqn{\phi= 1(T(y,\theta)> T(y,\theta_{observed}))}, we obtain the so-called
#'  \emph{posterior predictive p value.}
#'   (The author hates this notion.)

#'
#'  In my opinion, this criteria is
#'  not clear whether it is reliable quantities
#'   for evaluations.
#'
#Validation ----------
#'  \emph{\strong{Validation of model; Comparison between truth and  estimates of fake data-sets which are drawn using the truth.        }}
#'
#'
#'
#'
#'
#'
#'
#'  I think this is the most fundamental
#'  and intuitive validation.
#'
#'
#'  Under Construction
#'
#'----------------------------------------------------
#  Terminology   -----
#'  \emph{\strong{Appendix:  -----      Terminology    -------}}
#'
#'\describe{
#' \item{  \emph{hit                                } }{ which is also called True Positive: TP, which is denoted with each confidence level, \eqn{c=1,2,3,...,C} as follows: \eqn{H_1,H,2,...,H_C} or \code{h=c(h[1],h[2],...,h[C])},   where \code{h[1]}=\eqn{H_C} corresponds a number of hit with most high confidence level}
#' \item{  \emph{False alarm                        } }{ which is also called False Positive: FP , which is denoted with each confidence, \eqn{c=1,2,3,...,C} levels as follows: \eqn{F_1,F,2,...,F_C} or \code{f=c(f[1],f[2],...,f[C])}, where \code{f[1]}=\eqn{F_C} corresponds a number of false alarms with most high confidence level}
#' \item{  \emph{Modality                           } }{ Imaging methods, such as MRI, CT, PET,...etc. In another context, it means efficacy of treatment.}
#' \item{  \emph{Reader                             } }{ is a radiologist, physician, who try to detect lesions from radiographs. For a single image, reader can answer multiple suspicious shadows and he assigns to each suspicious shadows his or her confidence level. So, the reader localizes and rates for each suspicious shadows. A data analyst evaluates whether each reader's localization of lesion is true or false. Note that a single image can synthesize multiple-false positives or multiple true positives. Such a multiplicity distinct FROC analysis with ordinal ROC analysis.         }
#' \item{  \emph{Image                              } }{ is a radiograph taken by MRI, CT, PET, etc.       }
#' \item{  \emph{Modality comparison                } }{ The question that which modality (MRI, CT, PET, ... etc) is best to detect lesions in radiographs? In order to answer this question, the FROC analysis exists.}
#' \item{  \emph{hit  rate                          } }{ Each lesion can synthesize a hit of confidence level \eqn{c} according to Bernoulli distribution with probability of \eqn{p_c}, which call hit rate (of \eqn{c})}
#' \item{  \emph{false alarm  rate                  } }{ Each image synthesize a false alarm (False Positive: FP) of confidence level \eqn{c} according to Poisson distribution with probability of \eqn{lambda_c}, which call \emph{false alarm rate (of \eqn{c})} or \emph{simply false} rate.}
#' \item{  \emph{Number of images                   } }{ which is denoted by  \eqn{N_I}. An image means a radiograph or an X ray film, including shadows, each of which is caused by lesions or noise. Namely, each radiograph does not necessarily includes lesions. }
#' \item{  \emph{Number of lesions                  } }{ Suppose that there are \eqn{N_I} radiographs. Then by summing the number of lesions over all radiographs, we obtain the number of lesion \eqn{N_L}. }
#' \item{  \emph{FROC curve                         } }{ alternative notion of ROC curve in FROC context. }
#' \item{  \emph{AFROC curve                        } }{ Alternative-FROC curve, whose area under the curve indicates observer performance. Since area under the FROC curve is infinity, we use this area under the AFROC curve instead of the area under the FROC curve.}
#' \item{  \emph{AUC                                } }{ A real number between 0 and 1, indicating how many lesions radiologist can detect from radiographs. It is the area under the AFROC curve. In ROC context, AUC should be greater than 0.5,  but in  FROC context, the interpretation of AUC is not same as that in ROC context. For example, AUC =0.5 does not means that it is sames as the most bad observer performance.}
#' \item{  \emph{Chi square                         } }{ The difference of expectation minus observation, namely it is estimates minus actual observed data. Smaller is better.}
#' \item{  \emph{Posterior Predictive P value (PPP) } }{  This is a posterior predictive probability of the event that a test statistic is greater than its observed value. The author implements the \eqn{\chi^2} goodness of fit as a test statistic and in this context, if the PPP is small then we reject the null hypothesis that the model is well fit to data. The author hates this traditional bitch. }
#' \item{  \emph{FPF:False Positive Fraction        } }{  Cumulative sum of false alarms (FPs) divided by the number of Images or the number of lesions. Using FPFs as x and TPFs as y, we can visualize FPs and TPs. }
#' \item{  \emph{TPF:True Positive Fraction         } }{  Cumulative sum of hits (TPs) decided by the number of  Lesions (signals, targets).  Using FPFs as x and TPFs as y, we can visualize FPs and TPs.}
#' }
#'
#'
#'
#'
#' Now, I am in very serious condition both money  and employment.
#' I cannot get any job, this package development cannot save my life.
#'
#' I am a chemical sensitivity patient. I cannot overcome this serious disease.
#'
#' When I made this package, I hoped this makes my life safe, but it cannot.
#'
#' I really Despair my life.
#'
#' I do not study Statistics, but geometry, differential geometry.
#'
#'
#'
#' @name  BayesianFROC
#' @docType package
NULL
