#'@title  stanfitExtended  (S4 class)
#'
#'
#'@description Inherits from the class \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class defined in  the package \strong{\emph{rstan }} :


#'
#'@details  Revised in 2019.Jun 5
#'
#'
#'@slot plotdataMRMC Plot data for MRMC case.
#'@slot plotdata This is a data frame with four components which is used to draw curves such as FROC curves and AFROC curves.
#'@slot dataList This is a dataset. Using the dataset, the fitting has done.
#'@slot studyDesign This is character, e.g., "srsc.per.image",  "srsc.per.lesion", according to False Positive Fraction (FPF) is per image or per lesion.
#'@slot metadata This is additional data calculated from dataList, such as cumulative hits and false alarms,...,etc.
#'@slot WAIC This is a WAIC calculated by the function \strong{\emph{\code{\link{waic}}}} .
#'@slot convergence This is TRUE or FALSE. If TRUE, then it means your model is good in the R hat criterion.
#'@slot PreciseLogLikelihood  This is TRUE or FALSE. If TRUE, then target formulation is used in the stan file. However, non-target formulation has warning for non-linear Jacobian issue. So, the author use target formulations for all \code{.stan} files, and thus this slot is now, redandunt.
#'@slot chisquare This is a chi square calculated with Expected A Posterior estimates, i.e., the posterior mean estimates.
#'Chi square statistic is \eqn{\chi^2 (Data|\theta)}, there are three simple ways to get it.
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
#'  Note that this is \emph{not} calculated by integrating the posterior predictive measure. Do not confuse with the p value calculated with the posterior predicitive measure implemented in the function \code{\link{p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit}()}
#'@slot index THis is for programming phase.
#'@slot Divergences This is a number of the divergence transitions in the MCMC simulation.
#'@slot MCMC.Iterations A MCMC iterations which does not count the burn-in period.
#'@slot Divergence.rate A divergence rate, that is the number of the divergence iterations over total MCMC iterations. Burn-in period is not included.
#'
#'@slot model_name A slot from the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class defined in the \strong{\emph{rstan }}package.
#'@slot model_pars A slot from the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class in the package \strong{\emph{rstan}}.
#'@slot par_dims A slot from the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class in the package \strong{\emph{rstan}}.
#'@slot  mode A slot from the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class in the package \strong{\emph{rstan}}.
#'@slot sim A slot from the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class in the package \strong{\emph{rstan}}.
#'@slot inits A slot from the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class in the package \strong{\emph{rstan}}.
#'@slot stan_args A slot from the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class in the package \strong{\emph{rstan}}.
#'@slot stanmodel A slot from the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class in the package \strong{\emph{rstan}}.
#'@slot date A slot from the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class in the package \strong{\emph{rstan}}.
#'@slot .MISC A slot from the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class in the package \strong{\emph{rstan}}.

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
                     plotdata ="list", #slot
                     metadata ="list", #slot
                     dataList ="list",  #slot
                     dataList.Name ="character",  #Under Construction 2019 Jun 21
                     studyDesign ="character",  #slot "MRMC" "srsc"
                     WAIC ="numeric",  #slot
                     convergence ="logical",  #slot
                     PreciseLogLikelihood ="logical",
                     chisquare ="numeric",   #slot
                     index = "numeric",
                     Divergences ="numeric",   #slot
                     MCMC.Iterations = "numeric",
                     Divergence.rate ="numeric"   #slot

                   ),
                   # Initial values for new slots
                   methods::prototype(
                     plotdataMRMC = list(), #slot
                     plotdata = list(), #slot
                     metadata = list(), #slot
                     dataList = list(), #slot
                     dataList.Name ="",

                     studyDesign = "studyDesign",#slot

                     WAIC =  NaN,   #slot
                     convergence = NA, # "It is not assigned. To evaluate this, you have to input PreciseLogLikelihood = TRUE in the fit_Bayesian_FROC " #slot
                     # is.logical(NA);is.logical(NaN);is.logical(NULL)
                     #is.list(NA);is.list(NaN);is.list(NULL)
                     # is.character(NA);is.character(NaN);is.character(NULL)
                     # is.numeric(NA);is.numeric(NaN);is.numeric(NULL)

                     PreciseLogLikelihood = NA,
                     chisquare =  NaN,  #slot
                     index =  NaN,  #slot

                     Divergences = NaN,   #slot
                     MCMC.Iterations = NaN,
                     Divergence.rate = NaN
                      ),
                   # Stan
                   contains = "stanfit"
)


