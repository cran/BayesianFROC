#' @title Theory of FROC Analysis via Bayesian Approaches
#' @description
#'
# description -----

#' The following \pkg{Shiny} based GUI would be helpful.
#'
#' \describe{
#' \item{  \code{ \link{fit_GUI_Shiny}()}         }{Interactive GUI for a single reader and single modality}
#' \item{  \code{ \link{fit_GUI_Shiny_MRMC}()}    }{Interactive GUI for  multiple readers and multiple modalities}
#' }
#'
#'
#'
#'
#' The aim of FROC analysis is to compare imageing \emph{modalities},
#'  which are imaging methods such as  MRI, CT, PET, etc.
#'  We want to find a imaing method with
#'   which we can find more many lesions in radiographs.
#'
#' To investigate this,
#'  we have to do a trial
#'  in oreder to obtain dataset consisting of \emph{TP} and \emph{FP}.
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
#'  \strong{trial} --> \strong{Data} --> \strong{modeling}

#'
#'
#'
#'In the sequal,
#' we give a complete desctiption
#'  about the following three terms.
#'
#'
#' \describe{
#' \item{ \strong{\emph{ Trial    }}    }{ from which  data arise.                         }
#' \item{ \strong{\emph{ Data     }}    }{ consist of the number of TPs and FPs.  }
#' \item{ \strong{\emph{ Modeling }}    }{ calculates the probablity law in which  data (TPs and FPs) arise   }
#' }
#'
#'
#' \strong{\emph{ Trial    }}.
#'
#' Suppose that there are followings.
#'
#' \describe{
#' \item{ \strong{\emph{ a reader              }}     }{ who is a physician or radiologist trys to find lesions (in other words, it is called signals, targets, nodules, ...) from radiographs.}
#' \item{ \strong{\emph{ \eqn{N_I} images      }}     }{ contains shadows caused by noises  or signals (each image can contains one more signals and this multi signal for a single image distinct FROC trial from ROC trial).}
#' \item{ \strong{\emph{ a researcher          }}     }{ knows true lesion locations (signal) and count readers True Positives and False Positives. }
#' }
#'
#'
#'  \emph{\strong{FROC trial and data}}
#'
#'
#' \describe{
#' \item{1. First trial start                                                                }{The researcher gives the reader the \strong{\emph{first}} image which contains  suspicous shadows, each of which is noise or lesion. }
#' \item{2. \strong{\emph{LESION FINDING TASK}} for the first image (trial)                  }{The reader marks his suspicious locations (muliple answer is allowed) with a \strong{\emph{integer}} indicating their \strong{\emph{confidence}} levels (if he or she thinks it is obviously lesion, then he or she gives a higher integer). So, reader marks two things: location and confidence for each suspicous shadow.}
#' \item{3. Second trial and \strong{\emph{LESION FINDING TASK}} for the second image (trial)}{The researcher gives the reader the second image and reader do the above LESION FINDING TASK for the second image.}
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

#'
#'
#'  By the R code \code{BayesianFROC::viewdata(BayesianFROC::dataList.Chakra.1.with.explantation)}, we can see example data named \code{"dataList.Chakra.1.with.explantation"}.
#'
#'
#'
#'
#'
#'
#'  \emph{\strong{Modeling}}
#'
#'
#'
#'
#' First, we shall discuss our model  \strong{\emph{rigorously}} (ignore the confidence).
#' First, to simplify our argument,
#' first we reduce the FP and TP
#'  dataset from \eqn{H_c,F_c} to \eqn{H,F}
#'   by ignoring the confidence level.
#' Suppose that there are \eqn{N_L} targets (signal),
#' and radiological context, target is lesion.
#' Suppoes that a radiologist try to
#'  find these lesions from radiographs.
#' Suppose that now,  \strong{\emph{the reader fined \eqn{H} lesions}}
#' from radiographs which contains \eqn{N_L} lesions,
#' then it is natrual to assume that
#'
#'  \deqn{ H \sim Binomial(\theta, N_L)}
#'
#' where, the Bernoulli success rate \eqn{\theta} is
#'  a parameter of our model, which should be estimated.
#'
#' In addition, suppose that
#'  \strong{\emph{the reader fails \eqn{F} times}}, that is,
#' reader marked \eqn{F} false positives. Then it natrual to assume that
#'
#'
#'  \deqn{ F \sim Poisson(\lambda)}
#'
#'
#' where, \eqn{\lambda} is a parameter of model, which should be estimated.
#'
#'
#'  The above two is very simple, since data is only \eqn{H,F},
#'  indicating the number of TP and the number of FP.
#'
#'  \strong{\emph{Unfortunatley}}, the FROC data is more complex than above.
#'  That is, reader answers with his confidence levels, which is usually number of 1,2,3,4,5:
#'
#'
#'
#'

#'
#'
#' We give a probability law for the random variables \eqn{F_c} \eqn{H_c},\eqn{c=1,...,5}.
#'
#' Suppose that there are \eqn{N_L} targets,
#' and radiological context, each target is a lesion in \eqn{N_I} Radiographs.
#' Suppoes that a radiologist try to find lesions.
#' Suppose that now, he or she fined \eqn{H_c} lesions
#' with his \eqn{c}-th confidence,
#' then we assume that
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
#'                    functions of
#'                    a model parameter \eqn{\theta}.
#' In addition,
#'  suppose that the reader fails \eqn{F_c} times
#'   with his \eqn{c}-th confidence,
#'   that is,
#' the reader marked \eqn{F_c} false positives.
#'  Then it natrual to assume that
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
#'  such functions \eqn{p_c(\theta)}
#'  and \eqn{q_c(\theta)}.
#'
#'
#' \deqn{p_c(\theta)= \int ^{\theta_{c+1}}_{\theta_c} Gaussian(z|\mu, \sigma) dz        }
#'
#'
#' \deqn{q_c(\theta)= \int ^{\theta_{c+1}}_{\theta_c} N_I \frac{d}{dz} \Phi(z) dz        }
#'
#'
#' where the model parameter vector which should be estimated is
#'
#'  \deqn{ \theta = (\theta_1,\theta_2,...,\theta_C; \mu,\sigma).}
#'
#'
#'
#'
#'Intuitively, the reason why we choose such function
#'forms for \eqn{p_c(\theta)} is the assumption that
#'each lesion is equipped with i.i.d. latent variable,
#' \eqn{X} distributed by \eqn{ Gaussian(z|\mu, \sigma) }, and
#'if the \eqn{X} associated to some lesion falls
#'into the interval \eqn{ \theta_c <X< \theta_{c+1}},
#'then we consider that
#'the reader marks this lesion with his \eqn{c}-th confidence level.
#' In oreder to emphasize that each \eqn{X} is
#' associated to some  \eqn{l}-th lesion,
#'  \eqn{l=1,2,...,N_L} we denote the latent
#'  variable \eqn{X_l} for \eqn{l}-th lesion
#'   insead the latent decision variable \eqn{X}.
#'Here, we uses \emph{latent} to means that the variable \eqn{X}
#'cannot observed. Since the latent variable relates decision of reader,
#' and thus, in this context
#'the latent variable is called a \emph{decision} variable.
#'
#'Similarily, suppose that each image (radiograph)
#'is associated some latent variable \eqn{Y}
#' distibuted by \eqn{ N_I \frac{d}{dz} \Phi(z)}
#'  and if the \eqn{Y} associated to some image
#'   falls into interval the interval \eqn{ \theta_c <Y<\theta_{c+1}},
#'   then we consider that
#'the reader will false decision with
#'his \eqn{c}-th confidence level for the image.
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
#'  \emph{\strong{Priors on the parameter.}}
#'
#'Recall that our model has the following parameter.
#'
#'  \deqn{ \theta = (\theta_1,\theta_2,...,\theta_C; \mu,\sigma).}
#'
#'  In this section, we give priors on this parameter.
#'  Only one necessarily prior is to ensure the monotonicity on the thresholds parameters.
#'  \deqn{  \theta_1 < \theta_2 < ... < \theta_C.}
#'
#'  To give this monotonicity, we have to assume
#'
#'  \emph{\strong{Visualization of TP, FP by FPF, TPF}}
#'
#'  How to visualize our data constructed by hit and false alarms, that is, TP and FP?
#'  The traditionaly, the so-called FPF;\emph{False Positive Fraction} and TPT:\emph{True Positive Fraction} are used.
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
#' Recall that \emph{FPF} is defined as follows;
#'
#'
#' \deqn{FPF(5):= \frac{F_5}{NI},}
#' \deqn{FPF(4):= \frac{F_4+F_5}{NI},}
#' \deqn{FPF(3):= \frac{F_3+F_4+F_5}{NI},}
#' \deqn{FPF(2):= \frac{F_2+F_3+F_4+F_5}{NI},}
#' \deqn{FPF(1):= \frac{F_1+F_2+F_3+F_4+F_5}{NI}.}
#'
#'
#' Similarly, \emph{TPF} is defined as follows;
#'
#'
#' \deqn{TPF(5):= \frac{H_5}{NL},}
#' \deqn{TPF(4):= \frac{H_4+H_5}{NL},}
#' \deqn{TPF(3):= \frac{H_3+H_4+H_5}{NL},}
#' \deqn{TPF(2):= \frac{H_2+H_3+H_4+H_5}{NL},}
#' \deqn{TPF(1):= \frac{H_1+H_2+H_3+H_4+H_5}{NL}.}
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
#'    \emph{This indicates that the high obserber performance leads the empirical FROC curve more to be more upper positions.}

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
#'  Define
#'
#'   \deqn{x(c):= E[ FPF(c) ], }
#'
#'   \deqn{y(c):= E[ TPF(c) ]. }
#'
#' for \eqn{c = 1,2,3,4,5}.
#'
#'
#' Then,
#'
#'
#'
#'   \deqn{x(c):= E[ FPF(c) ] =\int^\infty_{\theta_c}\frac{d}{dz} \Phi(z) = - log \Phi(\theta_c),}
#'
#'   \deqn{y(c):= E[ TPF(c) ] =\int^\infty_{\theta_c} Gaussian(z|\mu, \sigma) dz = \Phi(\frac{\theta_c - \mu}{\sigma}).}
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
#' This implies that all expections for the pair of FPF and TPF is on the set:
#'
#'
#'   \deqn{  \{(x,y) |     y= \Phi(\frac{ \Phi^{-1}(\exp(-x) - \mu}{\sigma})\}.  }
#'
#'
#' We can regrad this set as an image of smooth curves, Namely,
#' here we define the so-called FROC curve as a map from 1-dimensional Euclidean space to
#' 2-dimensional Euclidean space, mapping each \eqn{t>0} to
#'
#' \deqn{  (x(t),y(t) ) =(t, \Phi(\frac{ \Phi^{-1}(\exp(-t) - \mu}{\sigma})   ) )}
#'
#'
#' Sine \eqn{x(t)=t,t>0} is not bounded, the area under the FROC curve is infinity.
#'
#' To calculates aleternative notion of AUC in the ordinal ROC theory, we define the so-called
#' AFROC curve:
#'
#' \deqn{  (\xi(t),\eta(t) ) =(1-e^{-t}, \Phi(\frac{ \Phi^{-1}(\exp(-t) - \mu}{\sigma})   ) )}
#'
#'
#' which contained in the rectangular space \eqn{[0,1]^2}.

#' The area Under the (AFROC) curve  (breifly, we call it AUC) represents the observer performance.
#' For example, if radiologist detects more lesions with small False Positives (FPs), then AUC would be high.
#'
#'Using the parameter of the signal distribution, we express AUC as follows,
#'
#' \deqn{  AUC = \frac{ \mu / \sigma}{  \sqrt{1+  1/\sigma^2}  }.}
#'
#' Introducing new parameter \eqn{a:= \mu / \sigma} and \eqn{b:= 1 / \sigma}, we also write
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
#' So,  such   explicit representations are  redandunt for a general theory.
#'   So, to simplify our argument in the following,
#'   we use two functions \eqn{ P(z|\theta_P), Q(z|\theta_Q)  } instead of the above two integrands \eqn{Gaussian(z|\mu, \sigma), \frac{d}{dz} \Phi(z)}.
#'
#' \deqn{p_c(\theta)= \int ^{\theta_{c+1}}_{\theta_c} P(z|\theta_P) dz        }
#'
#'
#' \deqn{q_c(\theta)= \int ^{\theta_{c+1}}_{\theta_c} Q(z|\theta_Q ) dz        }
#'
#'
#' where \eqn{P(z|\theta_P)} is a probability  function
#'  and \eqn{Q(z|\theta_Q)} is a positive function (not nessarily to be a probability function). namely,
#'
#' \deqn{  \int  P(z|\theta_P) dz  = 1      }
#'
#'
#' \deqn{ Q(z|\theta_Q )  > 0       }
#'
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
#' and radiological context, each target is a lesion in \eqn{N_I} Radiographs.
#' Suppoes that a radiologist try to find lesions.
#' Suppose that now, he or she fined \eqn{H_c} lesions
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
#'  Then it natrual to assume that
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
#' \deqn{FPF(5):= \frac{F_5                }{NI},}
#' \deqn{FPF(4):= \frac{F_4+F_5            }{NI},}
#' \deqn{FPF(3):= \frac{F_3+F_4+F_5        }{NI},}
#' \deqn{FPF(2):= \frac{F_2+F_3+F_4+F_5    }{NI},}
#' \deqn{FPF(1):= \frac{F_1+F_2+F_3+F_4+F_5}{NI}.}
#'
#'
#' Similarly, \emph{TPF} is defined as follows;
#'
#'
#' \deqn{TPF(5):= \frac{H_5                }{NL},}
#' \deqn{TPF(4):= \frac{H_4+H_5            }{NL},}
#' \deqn{TPF(3):= \frac{H_3+H_4+H_5        }{NL},}
#' \deqn{TPF(2):= \frac{H_2+H_3+H_4+H_5    }{NL},}
#' \deqn{TPF(1):= \frac{H_1+H_2+H_3+H_4+H_5}{NL}.}
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
#'  In this section, we provides the so-called \emph{FROC curve} which is our desired
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
#' Then,
#'
#'
#'
#'
#'   \deqn{y(c):= E[ TPF(c) ] =\int^\infty_{\theta_c} P(x|\theta_P)dx =: \Psi_P( x(c) ),}
#'   \deqn{x(c):= E[ FPF(c) ] =\int^\infty_{\theta_c} Q(x|\theta_Q)dx =: \Psi_Q( x(c) ),}
#'
#'
#' where \eqn{\Psi_P} and \eqn{\Psi_Q} denote the cumulative distribution functions, respectively.
#'
#' This implies that all expections for the pair of FPF and TPF is on the set:
#'
#'
#'   \deqn{  \{(x(t),y(t)) |    x(t)= \Psi_Q(t),  y(t)= \Psi_P(t).  \}.  }
#'
#'
#' We can regrad this set as the image of the smooth curve which is called
#' the generalized FROC curve.
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
#' This implies that all expections for the pair of FPF and TPF is on the set:
#'
#'
#'   \deqn{  \{(x,y) |     y= \Psi_P(\Psi_Q^{-1}( x )).  \}.  }
#'
#'
#' We can regrad this set as an image of smooth curves.
#'
#' \deqn{  (x(t),y(t) ) =(t, \Psi_P(\Psi_Q^{-1}( t )) )}
#'
#'
#' Sine \eqn{x(t)=t,t>0} is not bounded, the area under the FROC curve is infinity.
#'
#' To calculates aleternative notion of AUC in the ordinal ROC theory, we define the so-called
#' AFROC curve:
#'
#' \deqn{  (\xi(t),\eta(t) ) =(1-e^{-t},   \Psi_P(\Psi_Q^{-1}( x )) )}
#'
#'
#'
#'
# model generaliation fin -------
#'
#'

#'
#Hierarchical-------
#'  \emph{\strong{Hierarchical Model for Multiple Readers and Mutiple Modalities (MRMC)}}
#'
#'
#'
#   111111111 222222222 333333333 444444444 555555555 666666666 777777777 888888888 999999999

# data table -----
#'
#'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' \tabular{cccccc}{
#' \code{NI=63,NL=124}  \tab \strong{ modaity ID}  \tab \strong{ reader ID}  \tab \strong{ confidence } \tab \strong{ No. of FPs} \tab \strong{No. of TP}  \cr
#'  In R console ->      \tab \code{ m}  \tab \code{ q}  \tab \code{ c} \tab   \code{f }  \tab   \code{h}  \cr
#'   -----------------------\tab ----------------------- \tab    -----------------------\tab ----------------------- \tab --------------------------------------------------- \tab -------------------------------------------- \cr
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
#'  means \emph{Multiple Readers and Mutiple Modalities}.
#'
#'  Observer performance ability has INDIVIDUALITIES caused by  readers and modalities.
#'  Once we includes these individual difference in our Bayesian moded, such model
#'  will give us  an answer for the modality comparison issues.
#'
#'
#'
#'
#' The author implements several hierarchical models.
#'
#' 1) Non hierarchical MRMC model
#'
#' 2)     hierarchical MRMC model
#'
#' 3)   A  Single reader and multiple modalities model
#'
#'

#'  \emph{\strong{Without hyper parameter MRMC model}}
#'
#'  To include heterogeneity caused by readers and modalities,
#'  the author first made a hierarchical model.
#'   However, the model has
#'  divergent transitions in MCMC iterations.
#'   Thus the author also made a
#'   non-hierarchial model in which the
#'   author removed the hyper parameters
#'    to get more stable MCMC samplings
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
# #
# Probablity law of hits -----
#' \strong{Probablity law of hits}
#'
#' In the sequel, subscripts \eqn{m,r} means the \eqn{m}-th modality and the \eqn{r}-th reader.
#'
#'  Random variables of hits are distributed as follows.

#'                  \deqn{H_{5,m,r} \sim Binomial (p_{5,m,r}, N_L ),}
#'
#'  where the subscripts   \eqn{ H_{5,m,r} }
#'  denotes the number of hits (TPs) with confidence level \eqn{5} of  the \eqn{m}-th modality for the \eqn{r} th reader.
#'
#'
#' Now, the number of targets (signals, lesions) are found,
#' and the number of remaining targets is \eqn{N_L - H_{5,m,r}}.
#'
#' Thus, the number of hits with the 4-th confidence level \eqn{H_{4,m,r}} should be drawn from the binomial distribution with remaining targets
#'
#'      \deqn{H_{4,m,r} \sim Binomial (\frac{p_{4,m,r}}{1-p_{5,m,r}}, N_L - H_{5,m,r}).}
#'
#' Similarly,
#'
#'      \deqn{H_{3,m,r} \sim Binomial (\frac{p_{3,m,r}}{1-p_{5,m,r}-p_{4,m,r}}, N_L - H_{5,m,r} -H_{4,m,r}).}
#'
#'      \deqn{H_{2,m,r} \sim Binomial (\frac{p_{2,m,r}}{1-p_{5,m,r}-p_{4,m,r}-p_{3,m,r}}, N_L - H_{5,m,r} -H_{4,m,r}-H_{3,m,r}).}
#'
#'      \deqn{H_{1,m,r} \sim Binomial (\frac{p_{1,m,r}}{1-p_{5,m,r}-p_{4,m,r}-p_{3,m,r}-p_{2,m,r}}, N_L - H_{5,m,r} -H_{4,m,r}-H_{3,m,r}-H_{2,m,r}).}
#'
#'
#'
#'
#'

#'
#'
#' \strong{Probablity law of false alarms}
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
#'      \deqn{F_{5,m,r} \sim Poisson(q_{5,m,r} N_X ),}
#'
#'  where subscripts \eqn{m,r} means the \eqn{m}-th modality and the \eqn{r}-th reader.

#'      \deqn{F_{4,m,r} \sim Poisson( q_{4,m,r} N_X ),}
#'
#' Similarly,
#'
#'      \deqn{F_{3,m,r} \sim Poisson( q_{3,m,r} N_X ),}
#'
#'      \deqn{F_{2,m,r} \sim Poisson( q_{2,m,r} N_X ),}
#'
#'      \deqn{F_{1,m,r} \sim Poisson( q_{1,m,r} N_X ),}
#'

#'
#'
# here ----
#'     The rate \eqn{p_{c,m,r}} and \eqn{q_{c,m,r}} are calculated from the model parameters.
#'
#'
#' We use Gaussian distributions for
#'  the  functions \eqn{p_c(\theta)}
#'     and \eqn{q_c(\theta)} as follows.
#'
#'
#' \deqn{p_{c,m,r}(\theta)= \int ^{\theta_{c+1}}_{\theta_c} Normal(z|\mu_{c,m,r},v_{c,m,r}) dz        }
#'
#'
#' \deqn{q_{c,m,r}(\theta)= \int ^{\theta_{c+1}}_{\theta_c}  \frac{d}{dz} \Phi(z) dz        }
#'
#'
#' where the model parameter vector is
#'
#'  \deqn{ \theta = (\theta_1,\theta_2,...,\theta_C; \theta_P,\theta_Q).}
#'
#'
#'      By specifying these model parameters
#'      we can make a fake dataset consisting of
#'      hit data \eqn{H_{c,m,r}}
#'      false alarm data \eqn{F_{c,m,r}}
#'      for each \eqn{c,m,r}.
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
#'  rank statistic which generates a hitstogram.
#' If this hitsgram is not uniformly distributed, then
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
#' Let \eqn{ \theta_1,\theta_2,...,\theta_n} be a sample from a posterior \eqn{\pi(.| D)} for a given dataset \eqn{D}.
#' Let \eqn{L(y| \theta_i)} be a likelihood functon for a dataset \eqn{y} and model parameter \eqn{\theta}.
#' Let \eqn{ y^i_j \sim L(| \theta_i)}.
#'
#' For any \eqn{\phi},
#'
#' \deqn{ \int \int \phi(y,\theta) L(y| \theta) \pi(\theta|y)dy d\theta}
#' \deqn{ =\int \Sigma \phi(y,\theta_i) L(y| \theta_i) dy }
#' \deqn{ =\Sigma \Sigma \phi( y^i_j,\theta_i) L( y^i_j| \theta_i).  }
#'  Using this  \eqn{\phi= 1(T(y,\theta)> T(y,\theta_{observed}))}, we obtain the so-called
#'  \emph{posterior predicitive p value.} (The author hatest this notion.)

#'
#'  In my opinion, this criteria is not clear whether it is reliable quantities for evaluations.
#'
#Validation ----------
#'  \emph{\strong{Validation of model  via  fitting models to replicated datasets drawn from know distribution       }}
#'
#'
#'  I think this is the most fundamental and intuitive validation.
#'
#'----------------------------------------------------
#  Terminology   -----
#'  \emph{\strong{Appendix:  -----      Terminology    -------}}
#'
#'\describe{
#' \item{  \emph{hit                          } }{ which is Also called True Positive: TP, which is denoted with each confidence level, \eqn{c=1,2,3,...,C} as follows: \eqn{H_1,H,2,...,H_C} or \code{h=c(h[1],h[2],...,h[C])},   where \code{h[1]}=\eqn{H_C} corresponds a number of hit with most high confidence level}
#' \item{  \emph{False alarm                  } }{ which is Also called False Positive: FP , which is denoted with each confidence, \eqn{c=1,2,3,...,C} levels as follows: \eqn{F_1,F,2,...,F_C} or \code{f=c(f[1],f[2],...,f[C])}, where \code{f[1]}=\eqn{F_C} corresponds a number of false alarms witf most high confidence level}
#' \item{  \emph{Modality                     } }{ Imaging methods, such as MRI, CT, PET,...etc. In another context, it means efficasy of treatment.}
#' \item{  \emph{Modality comparison          } }{ The question that which modality (MRI, CT, PET, ... etc) is best to detect lesions in radiographs? In order to answer this question, the FROC analysis exists.}
#' \item{  \emph{hit  rate                    } }{ Each lesion can generate a hit of confidence level \eqn{c} according to Bernoulli distribution with probaility of \eqn{p_c}, which call hit rate (of \eqn{c})}
#' \item{  \emph{false alarm  rate            } }{ Each image generate a false alarm (False Positive: FP) of confidence level \eqn{c} according to Poisson distribution with probaility of \eqn{lambda_c}, which call \emph{false alarm rate (of \eqn{c})} or \emph{simply false} rate.}
#' \item{  \emph{Number of images             } }{ which is denoted by  \eqn{N_I}. image means radiographs, including lesions or noise. Namely, each radiograph does not necessarily includes lesions. }
#' \item{  \emph{Number of lesions            } }{ Suppose there are \eqn{N_I} radiographs. Then by summing the number of lesions over all radiographs, we obtain the number of lesion \eqn{N_L}. }
#' \item{  \emph{FROC curve                   } }{ alternative notion of ROC curve in FROC context. }
#' \item{  \emph{AFROC curve                  } }{ Alternative-FROC curve, whose area under the curve is used for evaluation of observer performance. Since area under the FROC curve is infinity, we use this are under the AFROC curve instead of the are under the FROC curve.}
#' \item{  \emph{AUC                          } }{ A real number between 0 and 1, indicating A metic to evaluate how much radiologist can detect lesions from radiographs. Area under the AFROC curve. In ROC context, AUC might be greater than 0.5,  but in  FROC context, the interpreation of AUC is not same as ROC context. For example, AUC =0.5 does not means that it is sames as the most bad observer perfomance.}
#' \item{  \emph{Chi square                   } }{ Defined by the difference of expected values from estimates minus actual observered data. Smaller is better.}
#' \item{  \emph{Posterior Predictive P value } }{  This is a posterior predicitive probability of the event that the test statistic is greater than its observed value. The author implements this for the \eqn{\chi^2} goodness of fit and in this context, if the PPP is small then we reject the null hypothesis that the model is well fit to data. The author hates this traditional bitch. }
#' \item{  \emph{FPF:False Positive Fraction  } }{  Cumulative sum of false alarms (FPs) devided by the number of Images or the number of lesions. Using FPFs as x and TPFs as y, we can visualize FPs and TPs. }
#' \item{  \emph{TPF:True Positive Fraction   } }{  Cumulative sum of hits (TPs) devided by the number of  Lesions (signals, targets).  Using FPFs as x and TPFs as y, we can visualize FPs and TPs.}
#' }
#'
#'
#'
#'
#' Now, I am in very serious condition both money  and employment.
#' I cannot get any job, this package development cannot save my life.
#'
#' I am a chemical sensititity patient. I cannot overcome this serious disease.
#'
#' When I made this package, I hoped this makes my life safe, but it cannot.
#'
#' I really Despair my life.
#'
#' I am not study Statistics, but geometry, differential geometry.
#'
#'
#'
#' @name  BayesianFROC
#' @docType package
NULL
