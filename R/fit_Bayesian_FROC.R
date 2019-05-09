#' @title Fit model to  data
#'
#'
#'
#'
#'@description
#'
#'
#'
#'
#'  Build the S4 object by Stan to fit the author's Bayesian models introduced in the author's paper.
#'  The output of the \code{rstan::}\code{\link[rstan]{sampling}}() is an object of the S4 class called  \strong{\emph{\code{\link[rstan]{stanfit}}}}.
#'  But, in this package, we extended the \emph{stanfit} class to an S4 class named  \emph{stanfitExtended}.
#'  The new S4 class \strong{\code{ \link{stanfitExtended}}} included new slots for the FROC analysis.
#'  So, the return value of the function is not the S4 class \emph{stanfit} but the new S4 class \strong{\code{ \link{stanfitExtended}}}.
#'  Thus, to apply the functions in the \strong{rstan} package for an object of the return value of this function, we have to change the class of the S4 object using the function \code{methods::}\code{\link[methods]{as}}() such as
#'  by the code \code{methods::as( object = return.value, "stanfit")}.
#'
#' The following items are main substances of this function.
#'
#'@description
#' \describe{
#' \item{ \strong{\emph{FIT}}}{\code{rstan::}\code{\link[rstan]{sampling}}()   runs for estimates.  }
#' \item{ \strong{\emph{PLOT CURVES}}}{If dataset is a single reader and single modality, then the curves are drawn in default. But in the MRMC case,it tooks long time, so the plot is not done for this case.}
#' \item{ \strong{\emph{MAKE AN S4 OBJECT}}}{The return value of this function is an S4 object whose class is inherited from the S4 class of the rstan package, which called stanfit.}
#' \item{ \strong{\emph{PRINT} }}{Estimates are printed in the R (Studio) console.}

#' }


#'@param dataList  This is a variable in the function \code{rstan::sampling()} in which it is named \code{data}.
#'
#' To make this R object \code{dataList}, please use one of the following codes:
#'
#'   \code{ \link{convertFromJafroc}}, \code{ \link{dataset_creator_new_version}}, \code{ \link{create_dataset}}
#'
#' \describe{
#' \item{ \strong{\code{ \link{convertFromJafroc}}}}{ If you have already have a data as a JAFROC formulation.}
#' \item{ \strong{\code{ \link{dataset_creator_new_version}}}}{ Enter TP and FP data into table. }
#' \item{ \strong{\code{ \link{create_dataset}}}}{ Enter TP and FP data by interactive manner.}

#' }
#'
#' This package includes FROC datasets.
#' Before running the function, we can confirm dataset is correctly formulated by the function \strong{\code{ \link{viewdata}}}.
#'
#'
#'-------------------------------------------------------------------------------------------------
#'
#'
#' ------------  \strong{Single reader and single modality (SRSC) case.}   --------------
#'
#'
#'------------------------------------------------------------------------------------------------
#'
#'In single reader and single modality case (srsc), it should be a list which includes  \code{f, h, NL, NI, C}.
#'
#' \describe{
#' \item{ \code{f}  }{Number of False Alarms. This is a vector for each confidence level.}
#' \item{ \code{h}  }{Number of Hits. This is a vector for each confidence level.}
#' \item{ \code{NL} }{Number of Lesions. This is a positive integer.}
#' \item{ \code{NI} }{Number of Images. This is a positive integer.}
#' \item{ \code{C}  }{ Confidence level. This is a positive integer.}
#' }
#'
#'
#'
#'
#'The detail of these dataset, see the datasets  endowed with this package.
#''Note that the maximal number of confidence level, denoted by  \code{C}, are included,
#' however,
#'\code{c } is not required, since it is created by \code{  c <-c(rep(C:1))} in the program and do not refer from input data, where \code{C} is the number of confidence levels.
#'So, you should write down your hits and false alarms vector so that it is compatible with this automatically created \code{c} vector.
#'
#'
#'\strong{\emph{ Example data:}}
#'
#'  \emph{            A single reader and single modality case   }
#'
#'------------------------------------------------------------------------------------------------------

#' \tabular{rccc}{
#' \code{NI=63,NL=124}   \tab \strong{ confidence level } \tab \strong{ No. of false alarms  } \tab \strong{No. of  hits}  \cr
#'  In R console ->      \tab \code{ c} \tab   \code{f }  \tab   \code{h}  \cr
#'   ------------------------\tab ------------------------ \tab ------------------------------ \tab ------------- \cr
#' definitely present = \tab  5 \tab 1 \tab 41 \cr
#'  probably present  = \tab  4 \tab 2 \tab 22 \cr
#'  equivocal         = \tab  3 \tab 5 \tab 14  \cr
#'  probably absent   = \tab  2 \tab 11 \tab 8  \cr
#'  questionable      = \tab  1 \tab 13 \tab 1  \cr
#'  }
#'
#'---------------------------------------------------------------------------------------------------

#'
#'The first column do not required, since from \code{C} the program generate it by the \code{rep(C:1)}.
#'So you should check the compatibility of your data and the program's generating new confidence  level column by
#'a function \code{viewdata()}.
#'
#'
#'Note that the above example data must be made by the following manner:
#' \code{ dat <- list(       }
#'
#' \code{ h = c(41,22,14,8,1),   }
#'
#' \code{ f = c(1,2,5,11,13),    }
#'
#' \code{ NL = 124,     }
#'
#' \code{ NI = 63,    }
#'
#' \code{ C = 5)         }
#'
#'And using this object \code{dat}, we can apply \code{fit_Bayesian_FROC()} as \code{fit_Bayesian_FROC(dat)}.
#'---------------------------------------------------------------------------------------
#'
#'   \strong{Multiple readers and multiple modalities case, i.e., MRMC case}
#'
#'
#'---------------------------------------------------------------------------------------
#'
#'
#'For  multiple readers and multiple modalities case, i.e., MRMC case,
#'to apply the function \code{fit_Bayesian_FROC()}, an R list object of data
#'must have components \code{m,q,c,h,f,NL,C,M,Q}:
#' \describe{
#' \item{ \code{C }  }{The highest number of confidence level, this is a scalar.}
#' \item{ \code{M }  }{The number of modalities.}
#' \item{ \code{Q }  }{The number of readers.}
#' \item{ \code{c }  }{The confidence level vector This vector must be made by \code{rep(rep(C:1), M*Q)} }
#' \item{ \code{m }  }{The modality ID vector.}
#' \item{ \code{q }  }{The reader ID vector.}
#' \item{ \code{h }  }{The number of hits vector.}
#' \item{ \code{f }  }{The number of false alarm vector.}
#' \item{ \code{NL }  }{The Total number of lesions for all images, this is a scalar.}
#' }

#'The detail of these dataset, please see the example datasets ( the section \strong{See Also} in the below) in this package.
#'
#'
#'
#'Note that the maximal number of confidence level, denoted by  \code{C}, are included,
#' however,
#'its each confidence level vector also created in the program by \code{C}. So, to confirm
#'your false positives and hits are correctly correspond
#'to confidence levels,
#'you should confirm the orders by the function \code{viewdata_MRMC}.

#'
#'
#'\strong{\emph{ Example data:  }}
#'
#'  \emph{          Multiple readers and multiple modalities case, i.e., MRMC case   }
#'---------------------------------------------------------------------------------------------------
#' \tabular{ccccc}{
#'  \strong{ ReaderID} \tab   \strong{ModalityID }  \tab  \strong{ Confidence levels} \tab   \strong{No. of false alarms} \tab   \strong{No. of hits}.\cr
#'   \code{q} \tab  \code{ m}  \tab   \code{c} \tab  \code{ f} \tab \code{ h}\cr
#'   -------- \tab -------------- \tab ------------------------- \tab  ------------------- \tab -----------------\cr
#'   1 \tab 1 \tab 5 \tab  1\tab 15\cr
#'   1 \tab 2 \tab 4  \tab 3\tab 14\cr
#'   1 \tab 3 \tab 3  \tab 5\tab 5\cr
#'   1 \tab 1 \tab 2  \tab 5\tab 3\cr
#'   1 \tab 2 \tab 1  \tab 9\tab 4\cr
#'   1 \tab 3 \tab 5  \tab 1\tab 14\cr
#'   1 \tab 1 \tab 4  \tab 2\tab 13\cr
#'   1 \tab 2 \tab 3  \tab 2\tab 5\cr
#'   1 \tab 3 \tab 2 \tab 5\tab 3\cr
#'   2 \tab 1 \tab 1 \tab  6\tab 4\cr
#'   2 \tab 2 \tab 5  \tab 1\tab 14\cr
#'   2 \tab 3 \tab 4  \tab 1\tab 4\cr
#'   2 \tab 1 \tab 3  \tab 1\tab 1\cr
#'   2 \tab 2 \tab 2  \tab 2\tab 2\cr
#'   2 \tab 3 \tab 1  \tab 3\tab 2\cr
#'   2 \tab 1 \tab 5  \tab 1\tab 13\cr
#'   2 \tab 2 \tab 4 \tab 2\tab 4\cr
#'   2 \tab 3 \tab 3  \tab 1\tab 2\cr }
#'---------------------------------------------------------------------------------------------------


#'@param cha An argument of \code{rstan::stan()} in which it is named \code{chains}. It means the number of chains generated by Hamiltonian Monte Carlo method,
#'and, default = 1.
#'
#'@param ite An argument of \code{rstan::stan()} in which it is named \code{iter}.  It means the  number of samples generated by Hamiltonian Monte Carlo method,
#'and, default = 10000. If your model could not converge, then raise this number.


#'@param dig An argument of \code{rstan::stan()} in which it is named \code{iter}.  It means the Significant digits, used in stan Cancellation.
#'default = 5,
#'
#'@inheritParams fit_srsc_per_image_test
#'@param war An argument of \code{rstan::stan()} in which it is named \code{warmup}.  It means the Burn in period,
#'war = floor(ite/5)=10000/5=2000,
#'
#'
#'@param see An argument of \code{rstan::stan()} in which it is named \code{seed}.  It means the a seed used in stan,
#' default = 1234567.
#'
#'@param PreciseLogLikelihood  If \code{PreciseLogLikelihood  = TRUE}, then Stan calculates the precise log likelihood.
#'
#'If \code{PreciseLogLikelihood  = FALSE}, then Stan calculates the log likelihood by dropping the constant terms in the likelihood function.


#'@param ModifiedPoisson This is dichotomous, that is \code{TRUE} or \code{FALSE}. If
#'\code{ModifiedPoisson = TRUE}, then Poisson rate of false alarm are \code{per lesion},
#'and if \code{ModifiedPoisson = FALSE}, then Poisson rate of false alarm are \code{per image}.
#'To know detail, refer the author's paper in which I explained per image and per lesion.
#'@param mesh.for.drawing.curve This is a number of points using draw the curves, default =10000

#'@param DrawCurve This is a dichotomous, i.e., TRUE or FALSE. If you want to draw the FROC and AFROC curves, then you set \code{DrawCurve =TRUE}, if not then \code{DrawCurve =FALSE}.
#' The reason why the author make this variable \code{DrawCurve} is that it takes long time to draw curves, and thus default value is \code{FALSE}.
#'@param Drawcol This is a dichotomous, i.e., TRUE or FALSE. This designate a colour of (A)FROC curve in graphs.The default value is a \code{TRUE}.

#'@param significantLevel This is a number between 0 and 1. The results are shown if posterior probabilities are greater than this quantity.
#'
#'  If your model could not converge,
#' then change this number.
#'
#'
#'@param new.imaging.device TRUE or FALSE. If TRUE (default), then open a new device to draw curve.
#'  Using this we can draw curves in same plain by new.imaging.device=FALSE.

#'
#'@param  summary TRUE or FALSE. If TRUE then summary and estimates are printed in the R console. If FALSE, the output is minimal.
#'
#'@param  make.csv.file.to.draw.curve TRUE or FALSE. If TRUE then csv file is created in your desktop to draw an FROC curve and cumulative hits and false alarms by scatter plot. Default is FALSE since it took times to create csv files.
#'
#'@return  An S4 class, created by  \code{rstan::stan} for your data by the author's FROC Bayesian models.
#' More precisely this S4 object belongs to some inherited class from \strong{\emph{\code{\link[rstan]{stanfit}}}} which is the S4 class of \code{rstan::stan()}.
#' To include your data and metadata, the \strong{\emph{\code{\link[rstan]{stanfit}}}} is extended to some class.
#' So, Using \code{returnvalue@dataList} or \code{returnvalue@metadata }, you can see your data
#'
#' Using S4  object, you can go ahead to the next step,
#'that is, drawing the FROC curve and alternative FROC (AFROC) curves.
#'
#'@return  ------------------------------------------------------------------------------------
#'@return  ----       \strong{Outputs of single reader and single modality case }       ------
#'@return  ------------------------------------------------------------------------------------

#'@return \code{ w  }    The lowest threshold of the Gaussian assumption (binormal assumption). so \code{w}=\code{z[1]}.
#'@return \code{dz[1]  } The difference of the first and second threshold of the Gaussian assumption.
#'@return \code{dz[2]  } The difference of the second and third threshold of the Gaussian assumption.
#'@return \code{dz[3]  } The difference of the  third and fourth threshold of the Gaussian assumption.
#'@return \code{...}


#'@return \code{m  }The mean of the Latent Gaussian distribution for diseased images.
#'@return \code{v  }The variance of the Latent Gaussian distribution for diseased images.
#'@return \code{p[1]} Hit rate with confidence level 1.
#'@return \code{p[2]} Hit rate with confidence level 2.
#'@return \code{p[3]} Hit rate with confidence level 3.
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

#'@return \code{z[1]} The lowest threshold of the (Gaussian) binormal assumption.
#'@return \code{z[2]}  The 2nd threshold of the (Gaussian) binormal assumption.
#'@return \code{z[3]} The 3rd threshold of the (Gaussian) binormal assumption.
#'@return \code{z[4]} The fourth threshold of the (Gaussian) binormal assumption.
#'@return \code{a} This is defined by \code{m/v}, please contact the author's paper for detail.
#'@return \code{b} This is defined by \code{1/v}, please contact the author's paper for detail.
#'@return \code{A} The area under alternative ROC curve.
#'@return \code{lp__} The logarithmic likelihood of our model for your data.
#'
#'

#'@return  ---------------------------------------------------------------------------------------------------------------
#'@return  ---- \strong{Notations and simbols:  }Outputs of Multiple Reader and Multiple Modality case       ------
#'@return  ------------------------------------------------------------------------------------------------------------------

#'@return \code{ w  }    The lowest threshold of the Gaussian assumption (binormal assumption). so \code{w}=\code{z[1]}.
#'@return \code{dz[1]  } The difference of the first and second threshold of the Gaussian assumption.
#'@return \code{dz[2]  } The difference of the second and third threshold of the Gaussian assumption.
#'@return \code{dz[3]  } The difference of the  third and fourth threshold of the Gaussian assumption.
#'@return \code{...}


#'@return \code{mu  }The mean of the Latent Gaussian distribution for diseased images.
#'@return \code{v  }The variance of the Latent Gaussian distribution for diseased images.
#'@return \code{ppp[1,1,1]} Hit rate with confidence level 1, modaity 1, reader 1.
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

#'@return \code{z[1]} The lowest threshold of the (Gaussian) binormal assumption.
#'@return \code{z[2]}  The 2nd threshold of the (Gaussian) binormal assumption.
#'@return \code{z[3]} The 3rd threshold of the (Gaussian) binormal assumption.
#'@return \code{z[4]} The fourth threshold of the (Gaussian) binormal assumption.
#'@return \code{aa} This is defined by \code{m/v}, please contact the author's paper for detail.
#'@return \code{bb} This is defined by \code{1/v}, please contact the author's paper for detail.
#'@return \code{AA} The area under alternative ROC curve.
#'@return \code{A} The area under alternative ROC curve.
#'@return \code{hyper_v} Standard deviation of \code{AA} around \code{A}.

#'@return \code{lp__} The logarithmic likelihood of our model for your data.
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
#'@slot model_name This is a slot from the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class defined in the \strong{\emph{rstan }}package.
#'@slot model_pars This is a slot from the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class defined in the \strong{\emph{rstan }}package.
#'@slot par_dims This is a slot from the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class defined in the \strong{\emph{rstan }}package.
#'@slot  mode This is a slot from the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class defined in the \strong{\emph{rstan }}package.
#'@slot sim This is a slot from the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class defined in the \strong{\emph{rstan }}package.
#'@slot inits This is a slot from the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class defined in the \strong{\emph{rstan }}package.
#'@slot stan_args This is a slot from the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class defined in the \strong{\emph{rstan }}package.
#'@slot stanmodel This is a slot from the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class defined in the \strong{\emph{rstan }}package.
#'@slot date This is a slot from the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class defined in the \strong{\emph{rstan }}package.
#'@slot .MISC This is a slot from the \strong{\emph{\code{\link[rstan]{stanfit}}}} which is an S4 class defined in the \strong{\emph{rstan }}package.





#'
#'@seealso
#'\strong{Before analysis: create a dataset}
#'
#'\code{ \link{convertFromJafroc}}
#'
#'\code{ \link{dataset_creator_new_version}}
#'
#'\code{ \link{create_dataset}}
#'
#'\strong{Further analysis: Plot curves}
#'
#'\code{ \link{DrawCurves}} for drawing free response ROC curves.
#'
#'\strong{Further analysis: Validation of the Model}
#'\code{ \link{p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit}}  Calculation of a p-value in the Bayesian paradigm.

#'
#'
#'  \strong{Example data:}
#'
#'\code{\link{dataList.Chakra.1}} for an example dataset of single reader and single modality data
#'
#'\code{\link{dataList.Chakra.2}} for an example dataset of single reader and single modality data
#'
#'\code{\link{dataList.Chakra.3}} for an example dataset of single reader and single modality data
#'
#'\code{\link{dataList.Chakra.4}} for an example dataset of single reader and single modality data
#'
#'
#'\code{\link{dataList.high.ability}} for an example dataset of single reader and single modality data
#'
#'\code{\link{dataList.low.ability}} for an example dataset of single reader and single modality data
#'

#'\code{\link{dataList.Chakra.Web}} for an example dataset of multiple readers and  multiple modalities data
#'
#'\code{\link{data.hier.ficitious}} for an example dataset of  multiple readers and  multiple modalities data

#'
#'
#'
#'
#'
#'
#'@seealso
#'
#'
#'  **************************************************************************************
#'
#'                           See  Vignettes for more details.
#'
#'  **************************************************************************************
#'
#'
#'@examples
#' \donttest{

#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#================The first example======================================
#'
#'             # Making FROC Data and Fitting a Model to the data
#'  #Notations
#'   # h = hits = TP = True Positives
#'   # f = False alarms = FP = False Positives
#'
#' #1) Build the data for singler reader and single modality  case.
#'
#' dat <- list(c=c(3,2,1),    #Confidence level
#'             h=c(97,32,31), #Number of hits for each confidence level
#'             f=c(1,14,74),  #Number of false alarms for each confidence level
#'
#'             NL=259,       #Number of lesions
#'             NI=57,        #Number of images
#'             C=3)          #Number of confidence level
#'
#'
#'
#'

#'# where, c denotes Confidence level,
#'#        h denotes number of Hits for each confidence level,
#'#        f denotes number of False alarms for each confidence level,
#'#        NL denotes Number of Lesions,
#'#        NI denotes Number of Images,
#'
#'
#'# For example, in the above example data,
#'#  the number of hits with confidence level 3 is 97,
#'#  the number of hits with confidence level 2 is 32,
#'#  the number of hits with confidence level 1 is 31,
#'
#'#  the number of false alarms with confidence level 3 is 1,
#'#  the number of false alarms with confidence level 3 is 14,
#'#  the number of false alarms with confidence level 3 is 74,
#'
#' #2) Fit the FROC model.
#'   #Since dataset named dat are single reader and single modality,
#'   #the function build the such model by running the following code.
#'
#'           fit <-   BayesianFROC::fit_Bayesian_FROC(dat,
#'                                                    ite=1111,#To run in time <5s.
#'                                                    cha=1
#'                                                    )
#'
#'
#'
#' #3) Using the S4 object fit, we can go further step, such as calculation of the
#' # Chisquare and the p value of the Bayesian version for testing the goodness of fit.
#' # I think p value has problems that it relies on the sample size with monotonicity.
#' # But it is well used, thus I hate but I implement the p value.
#'
#'
#'
#'
#'

#'
#'# (( REMARK ))
#'#Note that users not allowed to write the above data as follows:
#'
#'# MANNER (A)   dat <- list(c=c(1,2,3),h=c(31,32,97),f=c(74,14,1),NL=259,NI=57,C=3)
#'
#'
#'# Even if user write data in the above MANNER (A),
#'# the program interpret it as the following MANNER (B);
#'
#'# MANNER (B)   dat <- list(c=c(3,2,1),h=c(31,32,97),f=c(74,14,1),NL=259,NI=57,C=3)

#'
#'#This package is very rigid format, so please be sure that your format is
#'#exactly same to the data in this package.
#'#More precisely, the confidence level vector should be denoted rep(C:1) (Not rep(1:C)).
#'
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

#'
#'#=======  The Second Example:  From data endowed in this package to fitting ======
#'
#'
#'#(1)First, we prepare the data from this package.
#'
#'          dat  <- BayesianFROC::dataList.Chakra.1
#'
#'
#'#(2)Second, we run fit_Bayesian_FROC() in which the rstan::stan() is implemented.
#'#with data named "dat"  and the author's Bayesian model.
#'
#'
#'          fit <-  fit_Bayesian_FROC(dat)
#'
#'
#'
#'
#'
#'
#' #Now, we get the stan's out put, i.e.,  an S4 class object named "fit".
#'
#' #<< Minor Comments>>
#' #More precisely, this is an S4 object of some inherited class (named stanfitExtended)
#' #which is extended using stan's S4 class named "stanfit". This new S4 class
#' # has new slots for the informations such as user data, plotting data for FROC curves,
#' # input data to run this function, etc.
#'
#' #Using the output "fit",
#'
#' #we can use the functions in the "rstan" package, for example, as follows;
#'
#'
#'          rstan::stan_trace(fit)# stochastic process of a posterior estimate
#'          rstan::stan_hist(fit) # Histogram of a posterior estimate
#'          rstan::stan_rhat(fit) # Histogram of rhat for all parameters
#'          rstan::summary(fit)   # summary of fit by rstan
#'
#'
#'
#'
#'
#'
#'

#'
#'#================The third example:  From Hand made data to fitting  ==========
#'
#'   #Primitive way for fitting.
#'
#' #1) Build the data for singler reader and single modality  case.
#'
#' dat <- list(c=c(3,2,1),    #Confidence level
#'             h=c(97,32,31), #Number of hits for each confidence level
#'             f=c(1,14,74),  #Number of false alarms for each confidence level
#'
#'             NL=259,       #Number of lesions
#'             NI=57,        #Number of images
#'             C=3)          #Number of confidence level
#'
#'
#'
#'
#'# where, c denotes each confidence level,
#'#        h denotes number of Hits for each confidence level,
#'#        f denotes number of False alarms for each confidence level,
#'#        NL denotes Number of Lesions,
#'#        NI denotes Number of Images,
#'
#'
#' #2) Fit  and draw FROC and AFROC curves.
#'
#'            fit <-   fit_Bayesian_FROC(dat, DrawCurve = TRUE)
#'
#'
#'
#'# (( REMARK ))
#'#Changing the hist and false alarms denoted by h and  f in the dataset denoted by dat, respectively,
#'# user can draw the verious curves.
#'#Enjoy drawing the curves for various single reader and single modality data.
#'
#'
#'
# ----1---- ----2---- ----3---- ----4---- ----5---- ----6---- ----7---- ----8---- ----9----
#'#==========   The 4th example: From the data creation step   ==========
#'
#'
#'
#' #1) Build the data by create_dataset() which endowed in this package.
#'
#'                       dataList <-  create_dataset()
#'
#'#Now, as as a return value of create_dataset(), we get the FROC data (list) named dataList.
#'
#' #2) Fit an MRMC or srsc FROC model.
#'
#'                       fit <-  fit_Bayesian_FROC(dataList)
#'
#'
#'
#'
#'
# #####1**** ****2**** ****3**** ****4**** ****5**** ****6**** ****7**** ****8**** ****9****
#'#======== The 5-th example:Comparison of the posterior probability for AUC  ===
#'# This example shows how to use the stanfit (stanfit.Extended) object.
#'# Using stanfit object, we can extract posterior samples and using these samples,
#'# we can calculate the posterior probability of research questions.
#'
#'     fit <- fit_Bayesian_FROC(dataList.Chakra.Web.orderd,ite = 1111,summary =FALSE)
#'
#'#    For example, we shall show the code to compute the posterior probability of the evet
#'#    that  the AUC of modality 1 is larger than that of modality 2:
#'
#'                           e <- extract(fit)
#'
#'
#'# This code means that the MCMC samples are retained in the object e for all parameters.
#'# For example, the AUC is extracted by the code e$A and it is a two dimensional array.
#'# The first component indicate the MCMC samples and
#'# the second component indicate the modality ID.
#'# For example, the code e$A[,1] means the vector of MCMC samples of the first modality.
#'
#'#    To calculate the posterior probability of the evet
#'#    that  the AUC of modality 1 is larger than that of modality 2,
#'#    we excute the following:
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
#' # true distribution is belongs to models. Thus using this procedure, we will get
#' # the true parameter if sample size  take any large number.
#'
#'
#' #      Close the graphic device to avoid errors in R CMD check.
#'
#'                       Close_all_graphic_devices()
#'
#'
#'}# dottest

# devtools::document();help("fit_Bayesian_FROC")

#'@references Bayesian Models for Free-response Receiver Operating Characteristic Analysis
#'
#'@author Issei Tsunoda
#'
#'
#'@details This function \code{fit_Bayesian_FROC} is available both single reader and single modality case and multiple reader
#'and multiple modality case.
#'  Confidence level vector is not required but it is implicitly refered as the decreasing oreder,
#' For example, if C=3, then it would be a form  \code{c=c(3,2,1,3,2,1,...)}.
#'  Even if you write your data according to the order
#' \code{c=c(1,2,3,1,2,3,...)}, the program does not consider as your order, but \code{c=c(3,2,1,3,2,1,...)} instead.


# devtools::document();help("fit_Bayesian_FROC")

#' @export fit_Bayesian_FROC
#'@importFrom Rcpp evalCpp cpp_object_initializer
#'
# #devtools::use_package("base") #This cause the error, so not run.



fit_Bayesian_FROC <- function(dataList,
                              ModifiedPoisson = FALSE,
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
                              see = 1234567){

  if ( length(dataList[["m"]])==0  ) {# the reader =0 occur even the case of MRMC
    # message("\n* srsc case ")
    message(crayon::silver$bold("Study Design: "),crayon::green$bold("srsc case"),"\n")
    cat(crayon::silver$bold("False Positive Fraction is calculated"))

    fit_srsc(
      dataList =dataList ,
      new.imaging.device=new.imaging.device,
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
      see = see
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
      fit_MRMC_test(
        dataList = dataList,
        DrawCurve =  DrawCurve,
        # PreciseLogLikelihood = PreciseLogLikelihood,
        PreciseLogLikelihood = TRUE,

        mesh.for.drawing.curve=mesh.for.drawing.curve  ,
        significantLevel =significantLevel,
        summary=summary,

        cha  = cha,
        war  = war,
        ite  = ite,
        dig  = dig,
        see  = see
      )


    } else

      message("Confirm your data's column of modality ID is named by exactly m.")



  }
