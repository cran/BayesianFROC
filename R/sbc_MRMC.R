


#' @title Simiulation Based Calibration (SBC) for a single reader and a single modality case
#' @description Implements the SBC algorithm for the a single reader and a single modality case.
#' @details The implementation is done using the rstan::sbc. The stan file is \code{SBC.stan}
#'
#' @param ww A real number representing parameter of prior, indicating mean of prior for the first threshold
#' @param www A real number representing parameter of prior, variance of prior for the first threshold
#' @param mm A real number representing parameter of prior, mean of prior for the mean of signal distribution
#' @param mmm A real number representing parameter of prior, variance of prior for the variance of signal distribution
#' @param vv A real number representing parameter of prior,  mean of prior for the mean of signal distribution
#' @param vvv  A real number representing parameter of prior, variance of prior for the variance of signal distribution
#' @param zz A real number representing parameter of prior, mean of prior for the  differences of thresholds
#' @param zzz A real number representing parameter of prior, variance of prior for the  differences of thresholds


#' @param A_mean A real number representing parameter of prior, indicating mean of prior for the \code{A}
#' @param A_variance A real number representing parameter of prior, indicating mean of prior for the \code{A}
#' @param vv_hyper_v A real number representing parameter of prior, indicating mean of prior for the \code{hyper_v}
#' @param vvv_hyper_v A real number representing parameter of prior, indicating variance of prior for the  \code{hyper_v}





#' @param NL number of lesions
#' @param NI numver of images
#' @param C number of confidence levels
#'
#' @param M number of modalities
#' @param Q number of readers

#' @return  A list of S3 class "sbc", which is an outputs of  the \code{sbc} function in rstan.
#' @export
#'
#'
#' @references
#'
#'
#' Talts, S., Betancourt, M., Simpson, D., Vehtari, A., and Gelman, A. (2018).
#'  Validating Bayesian Inference Algorithms with Simulation-Based Calibration.
#'  arXiv preprint arXiv:1804.06788
#'
#'
#'@seealso
#'
#'\code{rstan::\link[rstan]{sbc}},\strong{\emph{ which implements SBC.}}
#'
#'\strong{\emph{ Stan file: }}  SBC_MRMC.stan
#'
# @examples

# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9#####========================================================================================
#               MRMC              SBC via rstan::sbc
#========================================================================================


#  Provides an Simulatiation Based Calibration for validation of our sampling.
#  We can confirmed that my model has very exact MCMC sampling.
#  SBC require suitable priors, and for the author, it seems very informative priors.
#  If we do not use the informative priors, then the odd data are generated from
#  the likelihood with the parameters drawn from priors. Such odd data has not fitted
#  our model, causing odd sampling.
#  If we do not choose the informative priors in suitable way, then it causes bias
#  in model. Even if the MCMC sampling is good in the sence of SBC, but the choise of
#  priors has no reasen, then it will cause bias. So, the author of this package
#  consider that  the bias of MCMC sampling and the bias of priors are trade off.
#  I write this program with no good condition of health or not good environment,
#  I want to die, I want to die, with great pain pain pain pain pain pain die
#  not enough money. So I write this with pain, pain in body, pain in life, pain in
#  money. So, this program let me be happy? I have to live. I must live.
#  All my pains let me take a CT images of my brain, my body, my teeth.
#  So, I have many CT images of mine, so I want to include them, but thier size
#  is very big, thus, I cannot. So, some section of CT iamge will be aploaded.
#  Healthy condition gives us wings for life. Pains gives us pain and small life.
#  I need wings to work or walk or write or calculation of mathematics. I must live.
#  Many reviewer gives me wrong or misunderstand comments. I won't hear them anymore.
#  So, I upload this program to avoid such damm comments. Fack. My life is damm damm.

# The default is three confidence levels,

# Now, I have no internet environment, thus I cannot gives the reference.
# Please search with internet for the details of SBC.
# I won't, won't, won't, ... 2019 July 18 with pain.

# I have no money for research no envoronment, no books, damm. Amateur. Amateur.
#
# \dontrun{
# fit<-
# Simulation_Based_Calibration_via_rstan_sbc_MRMC(
#                ww=-0.81,www =0.001,
#                mm=0.65,mmm=0.001,
#                vv=5.31,vvv=0.001,
#                zz= 1.55,zzz=0.001 )
#
#
# }#dontrun
#'
#'
Simulation_Based_Calibration_via_rstan_sbc_MRMC <- function(
  ww=-0.81,www =0.001,
  mm=0.65,mmm=0.001,
  vv=5.31,vvv=0.001,
  zz= 1.55,zzz=0.001,


  A_mean = 0.6,
  A_variance = 0.1,
  vv_hyper_v = 0.05,
  vvv_hyper_v = 0.01,


  NL = 259, NI = 57,C=3,M=5,Q=4
){

  scr <-system.file("extdata",
                    "SBC_MRMC.stan",
                    package = "BayesianFROC")

  stanmodel <- rstan::stan_model(scr)

  fit <- rstan::sbc(stanmodel,
                    data = list(

                      www= www,
                      mmm= mmm,
                      vvv= vvv,
                      zzz =zzz,

                      ww=ww,
                      mm=mm,
                      vv=vv,
                      zz=zz,

                      N = C, NL = NL, NI = NI,C=C,M=M,Q=Q

                    ), M = 500, refresh = 0)



  print(plot(fit, bins = 111)) # it is best to specify the bins argument yourself

  return(fit)

}








# fit <- tttttt( ww=-0.81,www =0.001,
#                  mm=0.65,mmm=0.001,
#                  vv=5.31,vvv=0.001,
#                  zz= 1.55,zzz=0.001 )


