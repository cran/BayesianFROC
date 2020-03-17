# This file name cannot be changed 2019 Oct 6


#'@title  \code{stanfitExtended}, an S4 class inherited
#'from the S4 class \strong{\emph{\code{\link[rstan]{stanfit}}}}
#'
#'
#'@description Inherits from the class \strong{\emph{\code{\link[rstan]{stanfit}}}} which
#' is an S4 class defined in  the package \pkg{rstan} :
#'
#'@details  Revised in 2019.Jun 5
#'          Revised in 2019 Oct 19
#'          Revised in 2019 Nov 25
#'
#'
#'    --------   To read the table of \R object of class \code{stanfit} in case of MRMC  ----------------------------
#'
#'
#'   * The  AUC denoted by \code{AA[modalityID , readerID]} are shown.
#'
#'     For example, \code{AA[2,3]} means the AUC of the 2 nd modality and the 3 rd reader.

#'
#'   * The column of \code{2.5\%} and \code{97.5\%} means the lower and upper bounds of the 95% Credible Interval (posterior interval) of AUCs.
#'
#'
#'
#'
#'
#'
#'
#'
#'
# \code{ S4@@slot} <- This code generate error ?
#'@slot plotdataMRMC Plot data for MRMC case.
#'@slot plotdata This is a data frame with four
#'components which is used to draw curves
#'such as FROC curves and AFROC curves.
#'So, this slot includes the component:
#'
#'
#' \code{ fit@@plotdata$x.AFROC, }
#'
#' \code{fit@@plotdata$y.AFROC, }
#'
#' \code{fit@@plotdata$x.FROC, }
#'
#' \code{fit@@plotdata$y.AFROC }
#'
#' where \code{fit} is an
#' object of class \code{stanfitExtended}.
#'
#'For example, we can use this slot
#' such as \code{plot(fit@@plotdata$x.AFROC, fit@@plotdata$y.AFROC ) },
#'  where \code{fit} is a fitted model object of class \code{stanfitExtended}.
#'The author think this slot is not so good since it increase the object size.
#'@slot dataList An FROC dataset.
#' Using the dataset, the fitting has done.
#'@slot studyDesign A character, e.g.,
#' "srsc.per.image",  "srsc.per.lesion",
#'  according to False Positive Fraction (FPF) is per image or per lesion.
#'@slot metadata An additional data
#' calculated from dataList,
#'  such as cumulative hits and false alarms,...,etc.
#'@slot WAIC A WAIC calculated
#'by the function \strong{\emph{\code{\link{waic}}}} .
#'@slot convergence A logical \R object TRUE or FALSE. If TRUE,
#' then it means your model is good in the R hat criterion.
#'@slot PreciseLogLikelihood  This is TRUE or FALSE. If TRUE,
#' then target formulation is used in the stan file. However,
#'  non-target formulation has warning for non-linear Jacobian
#'   issue. So, the author use target formulations
#'   for all \code{.stan} files, and thus this slot is now, redandunt.
#'@slot chisquare This is a chi square calculated with
#' Expected A Posterior estimates, i.e.,
#'  the posterior mean estimates.
#'Chi square statistic is \eqn{\chi^2 (Data|\theta)},
#'there are three simple ways to get it.
#'
#'(1) \eqn{ \int \chi^2(Data|\theta ) f(Data|\theta)\pi(\theta|Data)d\theta }
#'
#'(2) \eqn{  \chi^2(Data|\int \theta \pi(\theta|Data)d\theta) }
#'
#'(3) \eqn{ \int \chi^2(Data|\theta ) f(Data|\theta)\pi(\theta|Data)d\theta }
#'
#'where, \eqn{f( Data|\theta )} denotes a likelihood and \eqn{\pi(\theta| Data )} is a posterior.
#'This slot retains the (2)
#'
#'
#'  Note that this is \emph{not} calculated by integrating
#'  the posterior predictive measure. Do not confuse
#'   with the p value calculated with the posterior
#'    predicitive measure implemented
#'    in the function \code{\link{ppp}()}
#'@slot index This is for programming phase.
#'@slot Divergences This is a number of the
#'divergence transitions in the MCMC simulation.
#'@slot MCMC.Iterations A MCMC iterations which
#'does not count the burn-in period.
#'@slot Divergence.rate A divergence rate,
#'that is the number of the divergence
#'iterations over total MCMC iterations.
#'Burn-in period is not included.
#'
#'@slot model_name A slot of the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class defined in the \strong{\emph{rstan }}package.
#'@slot model_pars A slot of the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class in the package \strong{\emph{rstan}}.
#'@slot par_dims A slot of the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class in the package \strong{\emph{rstan}}.
#'@slot  mode A slot of the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class in the package \strong{\emph{rstan}}.
#'@slot sim A slot of the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class in the package \strong{\emph{rstan}}.
#'@slot inits A slot of the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class in the package \strong{\emph{rstan}}.
#'@slot stan_args A slot of the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class in the package \strong{\emph{rstan}}.
#'@slot stanmodel A slot of the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class in the package \strong{\emph{rstan}}.
#'@slot date A slot of the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class in the package \strong{\emph{rstan}}.
#'@slot .MISC A slot of the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class in the package \strong{\emph{rstan}}.

#'
#' @name stanfitExtended
#' @rdname stanfitExtended
#' @export stanfitExtended
# devtools::document();help("stanfit.extended") # Confirm reflection
#' @importClassesFrom rstan stanfit
#' @importFrom methods new representation prototype setClass
#'
#'
stanfitExtended <- methods::setClass("stanfitExtended",
                   # New slots
 methods::representation(
                     plotdataMRMC ="list", #slot
                     plotdata     ="list", #slot
                     metadata     ="list", #slot
                     dataList     ="list",  #slot

                     dataList.Name ="character",  #Under Construction 2019 Jun 21
                     studyDesign   ="character",  #slot "MRMC" "srsc"

                     convergence          ="logical",  #slot
                     PreciseLogLikelihood ="logical",
                     ModifiedPoisson      ="logical",
                     prototype            ="logical",

                     WAIC             ="numeric",  #slot
                     chisquare        ="numeric",   #slot
                     index            ="numeric",
                     Divergences      ="numeric",   #slot
                     MCMC.Iterations  ="numeric",
                     Divergence.rate  ="numeric"    #slot
                   ),



                   # Initial values for new slots
 methods::prototype(
                     plotdataMRMC  = list(), #slot
                     plotdata      = list(), #slot
                     metadata      = list(), #slot
                     dataList      = list(), #slot
                     dataList.Name ="",

                     studyDesign = "studyDesign",#slot

                     # is.logical(NA);is.logical(NaN);is.logical(NULL)
                     #is.list(NA);is.list(NaN);is.list(NULL)
                     # is.character(NA);is.character(NaN);is.character(NULL)
                     # is.numeric(NA);is.numeric(NaN);is.numeric(NULL)
                     ModifiedPoisson      =  NA,#  logical
                     prototype            = NA,#  logical
                     PreciseLogLikelihood = NA,#  logical
                     convergence          = NA, # "It is not assigned. To evaluate this, you have to input PreciseLogLikelihood = TRUE in the fit_Bayesian_FROC " #slot

                     chisquare       =  NaN,  #slot
                     index           =  NaN,  #slot
                     WAIC            =  NaN,   #slot
                     Divergences     = NaN,   #slot
                     MCMC.Iterations = NaN,
                     Divergence.rate = NaN
                      ),
                   # Stan
 contains = "stanfit"
)


