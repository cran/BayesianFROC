


#' @title Simiulation Based Calibration (SBC) for single reader and single modality case
#' @description Implements the SBC algorithm for the single reader and single modality case.
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


#' @param NL number of lesions
#' @param NI numver of images
#' @param C number of confidence levels
#'
#'
#'
#' @return  A list of S3 class "sbc", which is an outputs of  the \code{sbc} function in rstan.
#' @export
#'@author Issei Tsunoda

#' @examples
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#----------------------------------------------------------------------------------------
#'#                                SBC via rstan::sbc
#'#----------------------------------------------------------------------------------------
#'
#'
#'#  Provides an Simulatiation Based Calibration for validation of our sampling.
#'#  We can confirmed that my model has very exact MCMC sampling.
#'#  SBC require suitable priors, and for the author, it seems very informative priors.
#'#  If we do not use the informative priors, then the odd data are generated from
#'#  the likelihood with the parameters drawn from priors. Such odd data has not fitted
#'#  our model, causing odd sampling.
#'#  If we do not choose the informative priors in suitable way, then it causes bias
#'#  in model. Even if the MCMC sampling is good in the sence of SBC, but the choise of
#'#  priors has no reasen, then it will cause bias. So, the author of this package
#'#  consider that  the bias of MCMC sampling and the bias of priors are trade off.
#'#  I write this program with no good nodition of health or not good environment,
#'#  not enogh money. So I write this with pain, pain in body, pain in life, pain in
#'#  money. So, this program let me be happy? I have to live. I must live.
#'#
#'
#'
#'# The default is three confidence levels,
#'
#' \donttest{
#'#----------------------------------------------------------------------------------------
#'#                                SBC via rstan::sbc
#'#----------------------------------------------------------------------------------------
#'
#'fit<-Simulation_Based_Calibration_single_reader_single_modality_via_rstan_sbc(
#'                ww=-0.81,www =0.001,
#'                mm=0.65,mmm=0.001,
#'                vv=5.31,vvv=0.001,
#'                zz= 1.55,zzz=0.001 )
#'
#'#----------------------------------------------------------------------------------------
#'#                                SBC via rstan::sbc
#'#----------------------------------------------------------------------------------------
#'
#'
#'# The following examle, we specify the variance of prior of first thresholds
#'# as 1 which is  very large for variance. If we take more large variance,
#'# then Stan cannot start sampling since its
#'
#'# Run SBC algorithm
#'fit <-
#'Simulation_Based_Calibration_single_reader_single_modality_via_rstan_sbc(www=1)
#'
#'
#'# Check uniformity intuitively

#'plot(fit,bins=10)# Not required since the above funtion also plot the rank statistics.
#'
#'#----------------------------------------------------------------------------------------
#'#        Using default variables             SBC via rstan::sbc
#'#----------------------------------------------------------------------------------------
#'
#'# Run SBC algorithm
#' fit <-
#' Simulation_Based_Calibration_single_reader_single_modality_via_rstan_sbc()
#'
#'
#'# Check uniformity intuitively
#'
#' plot(fit,bins=10)
#'
#'#----------------------------------------------------------------------------------------
#'#      Number of confidence level is 4        SBC via rstan::sbc
#'#----------------------------------------------------------------------------------------
#'# Run SBC algorithm
#' fit <-
#' Simulation_Based_Calibration_single_reader_single_modality_via_rstan_sbcv(C=4)
#'
#'# Check uniformity intuitively
#'
#' plot(fit,bins=11)
#'}#donttest
#'
#'
Simulation_Based_Calibration_single_reader_single_modality_via_rstan_sbc <- function(
                 ww=-0.81,www =0.001,
                 mm=0.65,mmm=0.001,
                 vv=5.31,vvv=0.001,
                 zz= 1.55,zzz=0.001,


                 NL = 259, NI = 57,C=3
                 ){




  scr <-system.file("extdata", "SBC.stan",
                    package = "BayesianFROC")

  stanmodel <- rstan::stan_model(scr)


  message("
\n

* We will obtain rank statistics by the SBC algorithm.
  If the histgrams are uniformly distributed,
  then it indicates that
  the author's Bayesian model is good!
  I love you!



* Now, The Hamiltonian Monte Carlo Simulation is running ...

  Please wait ...
                    ")



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

    N = C, NL = NL, NI = NI,C=C,c=C:1

    ), M = 500, refresh = 0)








  print(plot(fit, bins = 111)) # it is best to specify the bins argument yourself

  return(fit)

}








# fit <- tttttt( ww=-0.81,www =0.001,
#                  mm=0.65,mmm=0.001,
#                  vv=5.31,vvv=0.001,
#                  zz= 1.55,zzz=0.001 )


