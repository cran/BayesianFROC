


#' @title Simulation Based Calibration (SBC)
#'  for a single reader and a single modality case
#' @description Implements the SBC algorithm for
#'   a single reader and a single modality case.
#'

# prior _______________________----
#'
#'  \strong{Prior _________ Under Construction----------}
#'
#' I do not use the following prior, but instead the precise prior
#' is defeined in the file: sbcVer2.stan. I am tired and not want to write this.
#'
#'  For sufficinetly small \eqn{\epsilon},
#'
#' \deqn{ \epsilon < \widetilde{p}_c(\theta) < 1- \epsilon, }
#'\deqn{q_c(\theta) >c \epsilon,}
#'
#'namely
#' \deqn{ \epsilon < \log \frac{ \Phi(\theta_{c+1})  }{ \Phi(\theta_c)  },  }
#'  \deqn{ \epsilon <   \Phi( \frac{\theta_{c+1} - \mu }{\sigma} ) -\Phi( \frac{\theta_{c} - \mu }{\sigma} ).     <   1-\epsilon   }
#'
#'
#'  We have to consider this equation.
#'
#'  To satisfy the condition \eqn{q_c(\theta) >c \epsilon,}
#'  we propose the following priors.
#'
#' \deqn{\theta_1 \sim Unif(-111,\Phi^{-1}(\exp^{- 5\epsilon})),}
#' \deqn{\theta_2 \sim Unif(  \Phi^{-1}(\Phi(\theta_1) \exp^{\epsilon} ),\Phi^{-1}(\exp^{- 4\epsilon})),}
#' \deqn{\theta_3 \sim Unif(  \Phi^{-1}(\Phi(\theta_2) \exp^{\epsilon} ),\Phi^{-1}(\exp^{- 3\epsilon})),}
#' \deqn{\theta_4 \sim Unif(  \Phi^{-1}(\Phi(\theta_3) \exp^{\epsilon} ),\Phi^{-1}(\exp^{- 2\epsilon})),}
#' \deqn{\theta_5 \sim Unif(  \Phi^{-1}(\Phi(\theta_4) \exp^{\epsilon} ),\Phi^{-1}(\exp^{- 1\epsilon})).}
#'
#'
#'
# p_c -------
#'  To satisfy the condition
#'  \eqn{ \epsilon < p_c(\theta) < 1- \epsilon,}
#'  we propose the following priors
#'   for more general condition\eqn{ f < p_c(\theta) < g,}
#' where \eqn{f} and  \eqn{g} are
#'  function of \eqn{\epsilon, c}, e.g.,   \eqn{ f=\epsilon, g= 1- \epsilon.}
#'
#' \deqn{\theta_1 \sim Unif(\phi^{-1}(1-g),\phi^{-1}(1-f)    ),}
#' \deqn{\theta_2 \sim Unif(\phi^{-1}(\frac{\phi(\theta_1)}{1-f}),\phi^{-1}(\frac{1-g}{(1-f)^1})    ),}
#' \deqn{\theta_3 \sim Unif(\phi^{-1}(\frac{\phi(\theta_2)}{1-f}),\phi^{-1}(\frac{1-g}{(1-f)^1})    ),}
#' \deqn{\theta_4 \sim Unif(\phi^{-1}(\frac{\phi(\theta_3)}{1-f}),\phi^{-1}(\frac{1-g}{(1-f)^1})    ),}
#' \deqn{\theta_5 \sim Unif(\phi^{-1}(\frac{\phi(\theta_4)}{1-f}),\phi^{-1}(\frac{1-g}{(1-f)^1})    ),}
#'
#'
#' where \eqn{\phi(\theta):= \Phi(\frac{\theta - \mu}{\sigma}) }
#'  and  \eqn{\phi^{-1}(\tau):=   \mu + \sigma \Phi^{-1}(\tau). }
#'
#' To show that the above equations are well-definded,
#' we have to show
#'
#' (1) the support of the above uniform disribution is not empty
#'
#' (2) the condition  \eqn{q_c(\theta) >c \epsilon } holds.
#'
#'
#' To show (1), we have to verify
#'
#' \deqn{ \Phi^{-1}(\exp^{- c\epsilon}) - \Phi^{-1}(\Phi(\theta_c) \exp^{\epsilon} ) }
#'
#'Suppose that we obtain \eqn{\theta_1,\theta_2,\cdots,\theta_{c} } disributed by the above.
#'
#' \deqn{  \exp^{- (C+1-c)\epsilon}  -  \Phi(\theta_c) \exp^{\epsilon}   }
#' \deqn{  > \exp^{- (C+1-c-1)\epsilon}  - \exp^{(C+1-c)\epsilon}  \exp^{\epsilon}   }
#' \deqn{  > 0  }
#'
#'
#'
#' Recall that the number of false alarms is distributed by
#' Poisson with rate
#'
#' \deqn{q_c(\theta) = \log \frac{ \Phi(\theta_{c+1})  }{ \Phi(\theta_c)  }    }
#'
#'
#'Because  \eqn{q_c(\theta)} cannot be zero, but if we use non-informative priors for
#'the model parameter \eqn{\theta}, then some synthesized parameter
#'gives \eqn{q_c(\theta)=0} which causes undesired results in SBC.
#'
#'Thus, for sufficiently small fixed \eqn{\epsilon}, we should assume that
#'
#'\deqn{q_c(\theta) > c\epsilon,}
#'
#'namely,
#'
#' \deqn{ \epsilon < \log \frac{ \Phi(\theta_{c+1})  }{ \Phi(\theta_c)  },  }
#'
#'
#' from which
#'
#' \deqn{ \Phi^{-1}(\Phi(\theta_c) \exp^{\epsilon} ) < \theta_{c+1},  }
#'
#'where we assume \eqn{\Phi(\theta_c) \exp^{\epsilon} <1, }
#'namely, \eqn{ \theta_c <   \Phi^{-1}(\exp^{- \epsilon}).}
#'
#'
#'
#'
# for \eqn{\theta_1,\theta_2, \cdots, \theta_i, \cdots, \theta_C.}
#'
#'
#' \deqn{\theta_1 \sim Unif(-111,\Phi^{-1}(\exp^{- \epsilon})),}
#' \deqn{\theta_2 \sim Unif(- \Phi^{-1}(\Phi(\theta_1) \exp^{\epsilon} ),\Phi^{-1}(\exp^{- \epsilon})),}
#' \deqn{\theta_3 \sim Unif(- \Phi^{-1}(\Phi(\theta_2) \exp^{\epsilon} ),\Phi^{-1}(\exp^{- \epsilon})),}
#' \deqn{\theta_4 \sim Unif(- \Phi^{-1}(\Phi(\theta_3) \exp^{\epsilon} ),\Phi^{-1}(\exp^{- \epsilon})),}
#' \deqn{\theta_5 \sim Unif(- \Phi^{-1}(\Phi(\theta_4) \exp^{\epsilon} ),\Phi^{-1}(\exp^{- \epsilon})).}
#'
#' These assumptions are necessary restriction for the equation \eqn{q_c(\theta) > \epsilon}.
#'
#'Furthermore, we should consider the Bernoulli success rate for the number of hits.
#'Next, recall that the number of hits is distributed by the binomial distribution
#'of rate  \eqn{p_c(\theta)} which should be in between zero and one.
#'However, non-informative prior cannot holds this condition.
#'Thus, we should investigate the prior such that it restricts the hit rate to be in
#' the interval [0,1].
#'
#' Recall that
#'  \deqn{p_c(\theta) =   \Phi( \frac{\theta_{c+1} - \mu }{\sigma} ) -\Phi( \frac{\theta_{c} - \mu }{\sigma} ).         }
#'  We have to assume
#'
#'
#'
#'  \deqn{ \epsilon < p_c(\theta) < 1- \epsilon, }
#'
#'  from which, we obtain
#'
#'
#'  \deqn{ \epsilon <   \Phi( \frac{\theta_{c+1} - \mu }{\sigma} ) -\Phi( \frac{\theta_{c} - \mu }{\sigma} ).     <   1-\epsilon   }
#'  \deqn{ \epsilon + \Phi( \frac{\theta_{c} - \mu }{\sigma} ) <   \Phi( \frac{\theta_{c+1} - \mu }{\sigma} ) .     < 1-\epsilon +\Phi( \frac{\theta_{c} - \mu }{\sigma} )   }
#'
#'  To go further step, we assume that
#'  \deqn{  \Phi( \frac{\theta_{c} - \mu }{\sigma} )   <  \epsilon, }
#'  from which, we can apply \eqn{ \Phi^{-1}} to \eqn{  1-\epsilon +\Phi( \frac{\theta_{c} - \mu }{\sigma} )  }.
#'  So,
#'  \deqn{  \frac{\theta_{c} - \mu }{\sigma}    < \Phi^{-1}( \epsilon), }
#'  and thus
#'  \deqn{   \theta_{c}     <  \mu  + \sigma\Phi^{-1}( \epsilon). }
#'
#' \deqn{\Phi^{-1}( \epsilon + \Phi( \frac{\theta_{c} - \mu }{\sigma} ) )<   \frac{\theta_{c+1} - \mu }{\sigma}      < \Phi^{-1}(  1-\epsilon +\Phi( \frac{\theta_{c} - \mu }{\sigma} ) )  }
#'  \deqn{ \mu   + \sigma \Phi^{-1}( \epsilon + \Phi( \frac{\theta_{c} - \mu }{\sigma} ) )<   \theta_{c+1}    <   \mu   + \sigma \Phi^{-1}(  1-\epsilon +\Phi( \frac{\theta_{c} - \mu }{\sigma} ) )  }
#'
#'
#'  To accomplish the above, we shold assume that
#'
#'  \deqn{ \theta_{c+1}    \sim Uniform( \mu   + \sigma \Phi^{-1}( \epsilon + \Phi( \frac{\theta_{c} - \mu }{\sigma} ) ),    \mu   + \sigma \Phi^{-1}(  1-\epsilon +\Phi( \frac{\theta_{c} - \mu }{\sigma} ) )  ),}
#'  namely,
#'  \deqn{\theta_1 \sim Unif(-111,111),}
#'  \deqn{ \theta_{2}    \sim Uniform( \mu   + \sigma \Phi^{-1}( \epsilon + \Phi( \frac{\theta_{1} - \mu }{\sigma} ) ),    \mu   + \sigma \Phi^{-1}(  1-\epsilon +\Phi( \frac{\theta_{1} - \mu }{\sigma} ) )  ),}
#'  \deqn{ \theta_{3}    \sim Uniform( \mu   + \sigma \Phi^{-1}( \epsilon + \Phi( \frac{\theta_{2} - \mu }{\sigma} ) ),    \mu   + \sigma \Phi^{-1}(  1-\epsilon +\Phi( \frac{\theta_{2} - \mu }{\sigma} ) )  ),}
#'  \deqn{ \theta_{4}    \sim Uniform( \mu   + \sigma \Phi^{-1}( \epsilon + \Phi( \frac{\theta_{3} - \mu }{\sigma} ) ),    \mu   + \sigma \Phi^{-1}(  1-\epsilon +\Phi( \frac{\theta_{3} - \mu }{\sigma} ) )  ),}
#'  \deqn{ \theta_{5}    \sim Uniform( \mu   + \sigma \Phi^{-1}( \epsilon + \Phi( \frac{\theta_{4} - \mu }{\sigma} ) ),    \mu   + \sigma \Phi^{-1}(  1-\epsilon +\Phi( \frac{\theta_{4} - \mu }{\sigma} ) )  ),}
#'
#'  Combining the necessary conditions of hit rates and false alarm retes,
#'
#'  we should assume their intersections.
#'
#' Set
#'
#' \deqn{X_c := \Phi^{-1}(\Phi(\theta_c) \exp^{\epsilon} ),  }
#' \deqn{Y_c :=    \mu   + \sigma \Phi^{-1}( \epsilon + \Phi( \frac{\theta_{c} - \mu }{\sigma} ) )}
#' \deqn{Z_c :=    \mu   + \sigma \Phi^{-1}(  1-\epsilon +\Phi( \frac{\theta_{c} - \mu }{\sigma} ) )  ),}
#'
#' then,
#'
#'  \deqn{\theta_1 \sim Unif(-111,111),}
#'  \deqn{\theta_2 \sim Unif( max(X_1,Y_1),Z_1),}
#'  \deqn{\theta_3 \sim Unif( max(X_2,Y_2),Z_2),}
#'  \deqn{\theta_4 \sim Unif( max(X_3,Y_3),Z_3),}
#'  \deqn{\theta_5 \sim Unif( max(X_4,Y_4),Z_4).}
#'
#' To justify these priors, we have to implement the SBC algorithm.
#'
#'In the above unform distribution, the support of them
#' should not be empty. However it is not satisfied without any restriction.
#' So, we should require the inequality that
#'
#' \deqn{ \Phi^{-1}(\Phi(\theta_c) \exp^{\epsilon} )  < \mu   + \sigma \Phi^{-1}(  1-\epsilon +\Phi( \frac{\theta_{c} - \mu }{\sigma} ) )  ),}
#'
#'which is satisfied in sufficiently small \eqn{\theta_c} and the continuity of this equation
#'implies that the set of solutions of \eqn{\theta_c} satifying
#' the inequality is not empty. Thus we have to find the minimun of
#' parameter  \eqn{\theta_c^*} such that it saisfies the inequality.
#'
#'

#'
#'
#'
#'#'
#'
#' @details The implementation is
#'  done using the rstan::sbc. The stan file is \code{SBC.stan}
#'The implementation is done using the function \code{rstan::sbc}.
#' The stan file is \code{SBC.stan}
#' The variable in this function is a collection of parameters of priors
#'
#'
#'  If we use non-informative prior, then from the prior the odd model parameter are synthesized.
#' For example,
#' If two thresholds z[c] and z[c+1] agree for some c, then  the false alarm rate becomes zero with the following error from \code{rstan::sbc}:
#'
#'
#'
#'
#'\code{ failed to create the sampler; sampling not done}
#'
#'\code{  Error in new_CppObject_xp(fields$.module, fields$.pointer, ...) :}
#'
#'\code{  Exception: poisson_rng: Rate parameter is 0, but must be > 0!}
#'
#'
#'
#' \strong{Thus, we have to use very strong prior to avoid to synthesize  such odd parameters of model.}
#'
#'
#'
#'
#' SBC is a validation algorithm for models with respect to its prior.
#'
#' I cannot fined the prior in which we can fit a model to various datasets.
#'
#' \strong{What is SBC?}
#'
#' Aim of SBC is to evaluate \emph{how} the computed posteriors are incorrect.
#' To do so, SBC algorithm makes a histogram whose uniformity indicates MCMC samples contains bias.
#'
#' For example,
#'
#'
#' If histogram is concave, namely there are spikes at the boundaries of histogram,
#' then it indicates that MCMC samples is correlated.
#' If a histogram is convex ( \eqn{\cap}-shaped), then it indicates that
#' over-dispersed posteriors relative to the \strong{true} posterior.
#'
#' \describe{
#' \item{if histogram is concave, }{ namely there are spikes at the boundaries of histogram,then it indicates that MCMC samples is correlated.}
#' \item{ If a histogram is convex ( \eqn{\cap}-shaped), }{ then it indicates that over-dispersed posteriors relative to the \strong{true} posterior.}
#' \item{ If a histogram is  weighted to right or left,}{ then posterior moves opposite direction, namely left or right respectively.}
#' }
#'
#'
#'
#'
#' We may say that SBC is a statistical test of the null hypothesis \eqn{H_0}:
#'
#'  \deqn{H_0: MCMC sampling is correct. }
#'
#' If the histogram is far from uniformity, then we reject \eqn{H_0} and say that
#' MCMC sampling contains bias.
#'
#'
#'
#'
#'
#'
#'
#' \emph{\strong{Parameters of our model}}
#'
#' \describe{
#' \item{ \code{w} }{ The first threshold}
#' \item{  \code{dz} }{ The difference of thresholds, that is, dz[c]:= z[c+1]-z[c]}
#' \item{  \code{ m}  }{Mean of signal Gaussian}
#' \item{   \code{v } }{Standard deviation (Do not confuse it with Variance) of signal Gaussian}
#' }
#'
#'
#'
#'
#' @references Talts, S., Betancourt, M., Simpson, D., Vehtari, A., and Gelman, A. (2018). Validating Bayesian Inference Algorithms with Simulation-Based Calibration. arXiv preprint arXiv:1804.06788. https://arxiv.org/abs/1804.06788
#'
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
#' \emph{definitely} present  \tab  \code{c[1] = }5 \tab \code{f[1] = }\eqn{F_5} = 1 \tab  \code{h[1] = }\eqn{H_5} = 41 \cr
#'  \emph{probably} present   \tab  \code{c[2] = }4 \tab \code{f[2] = }\eqn{F_4} = 2 \tab  \code{h[2] = }\eqn{H_4} = 22 \cr
#'  equivocal                 \tab  \code{c[3] = }3 \tab \code{f[3] = }\eqn{F_3} = 5 \tab  \code{h[3] = }\eqn{H_3} = 14  \cr
#'  subtle                    \tab  \code{c[4] = }2 \tab \code{f[4] = }\eqn{F_2} = 11 \tab \code{h[4] = }\eqn{H_2} = 8  \cr
#'  \emph{very} subtle        \tab  \code{c[5] = }1 \tab \code{f[5] = }\eqn{F_1} = 13 \tab \code{h[5] = }\eqn{H_1} = 1  \cr
#'  }
#'
#'---------------------------------------------------------------------------------------------------
#'
#'
#' Recall our model for the above data format;
#'
#'
#'   \deqn{ H_5 \sim Binomial (p_5,N_L )}
#'   \deqn{ H_4 \sim Binomial (p_4,N_L )}
#'   \deqn{ H_3 \sim Binomial (p_3,N_L )}
#'   \deqn{ H_2 \sim Binomial (p_2,N_L )}
#'   \deqn{ H_1 \sim Poisson (p_1,N_L )}
#'   \deqn{ F_5 \sim Poisson (q_5 )}
#'   \deqn{ F_4 \sim Poisson (q_4 )}
#'   \deqn{ F_3 \sim Poisson (q_3 )}
#'   \deqn{ F_2 \sim Poisson (q_2 )}
#'   \deqn{ F_1 \sim Poisson (q_1 )}
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' where
# \deqn{ p_5= p_5(z_1,...z_C; \mu, \sigma) = \int_{z_{5}}^{\infty} Gaussian(z|\mu,\sigma)dz}
# \deqn{ p_4=p_4(z_1,...z_C; \mu, \sigma) = \int_{z_{4}}^{5} Gaussian(z|\mu,\sigma)dz}
# \deqn{ p_3=p_3(z_1,...z_C; \mu, \sigma) = \int_{z_{3}}^{4} Gaussian(z|\mu,\sigma)dz}
# \deqn{ p_2=p_2(z_1,...z_C; \mu, \sigma) = \int_{z_{2}}^{3} Gaussian(z|\mu,\sigma)dz}
# \deqn{ p_1=p_1(z_1,...z_C; \mu, \sigma) = \int_{z_{1}}^{2} Gaussian(z|\mu,\sigma)dz}
#
# \deqn{ q_5=q_5(z_1,...z_C) = \int_{z_{5}}^{\infty}  d \log \Phi (z)}
# \deqn{ q_4=q_4(z_1,...z_C) = \int_{z_{4}}^{5}  d \log \Phi (z)}
# \deqn{ q_3=q_3(z_1,...z_C) = \int_{z_{3}}^{4}  d \log \Phi (z)}
# \deqn{ q_2=q_2(z_1,...z_C) = \int_{z_{2}}^{3}  d \log \Phi (z)}
#  \deqn{ q_1=q_1(z_1,...z_C) = \int_{z_{1}}^{2}  d \log \Phi (z)}


#' \deqn{ p_5= p_5(z_1,...z_C; \mu, \sigma) = \int_{z5}^{\infty} Gaussian(z|\mu,\sigma)dz}
#' \deqn{ p_4=p_4(z_1,...z_C; \mu, \sigma) = \int_{z4}^{z5} Gaussian(z|\mu,\sigma)dz}
#' \deqn{ p_3=p_3(z_1,...z_C; \mu, \sigma) = \int_{z3}^{z4} Gaussian(z|\mu,\sigma)dz}
#' \deqn{ p_2=p_2(z_1,...z_C; \mu, \sigma) = \int_{z2}^{z3} Gaussian(z|\mu,\sigma)dz}
#' \deqn{ p_1=p_1(z_1,...z_C; \mu, \sigma) = \int_{z1}^{z2} Gaussian(z|\mu,\sigma)dz}
#'
#' \deqn{ q_5=q_5(z_1,...z_C) = \int_{z5}^{\infty}  d \log \Phi (z)}
#' \deqn{ q_4=q_4(z_1,...z_C) = \int_{z4}^{z5}  d \log \Phi (z)}
#' \deqn{ q_3=q_3(z_1,...z_C) = \int_{z3}^{z4}  d \log \Phi (z)}
#' \deqn{ q_2=q_2(z_1,...z_C) = \int_{z2}^{z3}  d \log \Phi (z)}
#' \deqn{ q_1=q_1(z_1,...z_C) = \int_{z1}^{z2}  d \log \Phi (z)}
#'

#prior ------
#' \strong{Priors}
#'
#'
#'\deqn{z[c] \sim ?}
#'\deqn{m \sim ?}
#'\deqn{v \sim ?}
#'
#'
#'
#' In SBC, we have to specify proper priors, thus, we use the above priors.
#' So, what reader should do is to specify the above parameters, that is,
#' \code{ww,www,zz,zzz,mm,mmm,vv,vvv} and further a number of images\code{NL}
#' and a number of lesion \code{NI} and a number of confidence levels should be specified.
#' In the above example data format, the number of confidence level is the number of rows,
#' and now it is 5, that is \code{C=5}.
#'
#'  Revised 2019 August 4
#'
#'
#'
#'
#'
#'
#'
#' I am not statistician nor researcher nor human. My leg is gotten by death who is prurigo nodularis.
#' Death is soon. I cannot understand, I hate statistics. I do not want to waste my time to this FROC analysis.
#' My program is volunteer, I am no money no supported. Completely my own support or my parents. Completely my own.
#' I am tired for this no end point running. I have not money to research or place or circumstance.
#' No healthy condition. This program is made with my blood and pain, great pain. I no longer want to live.
#' I hate all. Honesty.
#'
#'


#' @param NL number of lesions
#' @param NI number of images
#' @param C number of confidence levels
#' @param sbc_from_rstan A logical, wheter \code{rstan::sbc()} is used
#'
#' @param M  To be passed to the function \code{rstan::}\code{\link[rstan]{sbc}}() in \strong{rstan}.
#' @param BBB a real
#' @param AAA a real
#' @param vvv a real
#' @param vvvv a real
#' @param mmm a real
#' @param mmmm a real
#'
#'@param epsilon lower bound of Poisson for false positives.
#'@param stanModel An object of the class stanfit of sbc. This is for the package developer.
#'
#'@inheritParams fit_Bayesian_FROC

#' @return  A list of S3 class "sbc", which is an output of  the function \code{rstan::sbc()}  in \pkg{rstan}.
#' @export
#' @examples
#'
#'
#'
#' \dontrun{
#'#========================================================================================
#'#                             SBC via rstan::sbc          Default prior
#'#========================================================================================
#'#
#'
#' stanModel <- stan_model_of_sbc()
#'
#' Simulation_Based_Calibration_single_reader_single_modality_via_rstan_sbc(
#' NL = 11111,
#' NI = 11111,
#' stanModel = stanModel,
#' ite     = 323,
#' M       = 211,
#' epsilon = 0.04,BBB = 1.1,AAA =0.0091,sbc_from_rstan = TRUE)
#'
#'
#'
#'#========================================================================================
#'#                             SBC via rstan::sbc          Default prior
#'#========================================================================================
#'
#'
#' stanModel <- stan_model_of_sbc()
#'
#' Simulation_Based_Calibration_single_reader_single_modality_via_rstan_sbc(
#' NL = 11111,
#' NI = 11111,
#' stanModel = stanModel,
#' ite     = 323,
#' M       = 511,
#' epsilon = 0.04,BBB = 1.1,AAA =0.0091,sbc_from_rstan = TRUE)
#'
#'
#'
#'      Close_all_graphic_devices() # 2020 August
#'
#'
#'
#' }#dontrun







Simulation_Based_Calibration_single_reader_single_modality_via_rstan_sbc <- function(
                 epsilon=0.01,
                 ite=3333,
                 NL = 259,
                 NI = 57,
                 C  = 3,
                 M  = 500,
                 BBB = 0.3,
                 AAA = 0.0003,
                 vvv  = 0.3,
                 vvvv = 11,
                 mmm  = 0,
                 mmmm = 1,
                 war = ite/10,

                 stanModel,
                 sbc_from_rstan =TRUE
                 ){


if(missing(stanModel)){
  # if (old_ver == TRUE)  scr <-system.file("extdata", "SBCold.stan",  package = "BayesianFROC")
  # if (old_ver == F)  scr <-system.file("extdata", "SBC.stan",  package = "BayesianFROC")
  scr <-system.file("extdata", "SBCver2.stan",  package = "BayesianFROC")
  # if (model_reparametrized)  scr <-  system.file("extdata", "SBC_reparametrized.stan", package="BayesianFROC")

  stanModel <- rstan::stan_model(scr)
}
  # browser()

  if(sbc_from_rstan)  ffff <-  rstan::sbc
  if(!sbc_from_rstan) ffff <-  sbcc

        # fit <- rstan::sbc(stanModel,

  fit <- ffff(stanModel,
                    data = list(
                      epsilon=epsilon,
                      N = C,
                      NL = NL,
                      NI = NI,
                      C=C,
                      AAA=AAA,
                      BBB=BBB,
                      vvv=vvv,
                      vvvv=vvvv,
                      mmm=mmm,
                      mmmm=mmmm,
                      c=C:1,
                      warmup  = war),
                    M = M,
                    iter =ite,
                    refresh = 0)

  print(plot(fit, bins = 111)) # it is best to specify the bins argument yourself

  # ## Only run examples in interactive R sessions
  # if (interactive()) {
  #  tcltk::tkmessageBox(  message=" SBC finished.")
  #   }### Only run examples in interactive R sessions
  #
 message("TOtal iterations of MCMC = ", M * ite)

  return(fit)

}



#' @title Creates an object of class stanfit of SBC
#'
#' @return An object of class stanfit for SBC
#' @param model_ver An integer, indicating priors
#' @export
#' @seealso \code{\link{Simulation_Based_Calibration_single_reader_single_modality_via_rstan_sbc}()}
#'
#' @examples
#' \dontrun{
#' stan_model_of_sbc()
#' }
stan_model_of_sbc  <- function(model_ver = 2) {

  if(model_ver == 2) scr <-system.file("extdata", "SBCver2.stan",  package = "BayesianFROC")
  if(model_ver == 3) scr <-system.file("extdata", "SBCver3.stan",  package = "BayesianFROC")
  if(model_ver == 4) scr <-system.file("extdata", "SBCver4.stan",  package = "BayesianFROC")

   stanmodel <- rstan::stan_model(scr)
return(stanmodel)
  }


