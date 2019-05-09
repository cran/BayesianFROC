#'@title  stanfitExtended  (S4 class)
#'
#'
#'@description  New Slots are added to  stanfit  which is an S4 class in  rstan :
#'@field    dataList Object of class  "list" , containing data that needs to go in further analysis.
#'
#'@field     metadata Object of class  "list" , containing meta data from slot1, e.g., the cumulative hits and cumulative false alarms.
#'
#'@field      studyDesign Object of class  "character" , indicating MRMC (multiple reader and multiple modality) or srsc (single reader and single case)
#'


#'
#'@slot plotdataMRMC Plot data for MRMC case.
#'@slot plotdata This is a data frame with four components which is used to draw curves such as FROC curves and AFROC curves.
#'@slot dataList This is a dataset. Using the dataset, the fitting has done.
#'@slot studyDesign This is character, e.g., "srsc.per.image",  "srsc.per.lesion",
#'@slot metadata This is additional data calculated from dataList, such as cumulative hits and false alarms,...,etc.
#'@slot WAIC This is a WAIC.
#'@slot convergence This is TRUE or FALSE. If TRUE, then it means your model is good in the R hat criterion.
#'@slot PreciseLogLikelihood  This is TRUE or FALSE. If TRUE, then target formulation is used in the stan file.
#'@slot chisquare This is a chi square calculated with Expected A Posterior estimates, i.e., the posterior mean estimator.
#'@slot index THis is for programming phase.
#'@slot Divergences This is a number of the divergence transitions in the MCMC simulation.
#'@slot MCMC.Iterations A MCMC iterations which does not count the burn-in period.
#'@slot Divergence.rate A divergence rate, that is the number of the divergence iterations over total MCMC iterations. Burn-in period is not included.
#'
#'@slot model_name This is a slot from the stanfit which is an S4 class defined in the rstan package.
#'@slot model_pars This is a slot from the stanfit which is an S4 class defined in the rstan package.
#'@slot par_dims This is a slot from the stanfit which is an S4 class defined in the rstan package.
#'@slot  mode This is a slot from the stanfit which is an S4 class defined in the rstan package.
#'@slot sim This is a slot from the stanfit which is an S4 class defined in the rstan package.
#'@slot inits This is a slot from the stanfit which is an S4 class defined in the rstan package.
#'@slot stan_args This is a slot from the stanfit which is an S4 class defined in the rstan package.
#'@slot stanmodel This is a slot from the stanfit which is an S4 class defined in the rstan package.
#'@slot date This is a slot from the stanfit which is an S4 class defined in the rstan package.
#'@slot .MISC This is a slot from the stanfit which is an S4 class defined in the rstan package.

#'
#' @name stanfitExtended
#' @rdname stanfitExtended
#' @export stanfitExtended
# devtools::document();help("stanfit.extended") # Confirm reflection
#' @importClassesFrom rstan stanfit
#' @importFrom methods new representation prototype setClass
stanfitExtended <- methods::setClass("stanfitExtended",
                   # New slots
                   methods::representation(
                     plotdataMRMC ="list", #slot
                     plotdata ="list", #slot
                     metadata ="list", #slot
                     dataList ="list",  #slot
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
