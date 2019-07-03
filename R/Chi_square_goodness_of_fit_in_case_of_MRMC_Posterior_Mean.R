
#' @title Chi square in the case of MRMC.
#' @description Not a posteriror predective chi square, but the posterior mean.
#'
#' Let \eqn{\chi(y|\theta)} be a chi square goodness of fit statistics of our hierarchical Bayesian Model for , i.e.,
#'
#' \deqn{\chi(D|\theta) := \sum _{r=1}^R \sum _{m=1}^M \sum _{c=1}^C \biggr( \frac{[ H_{c,m,r}-N_L\times p_{c,m,r}]^2}{N_L\times p_{c,m,r}}+
#'                                                                  \frac{[F_{c,m,r}-(\lambda _{c} -\lambda _{c+1} )\times N_{L}]^2}{(\lambda _{c} -\lambda _{c+1} )\times N_{L} }\biggr).
#'}
#'
#'where  a dataset \eqn{D} denotes \eqn{ (F_{c,m,r}, H_{c,m,r}) } and model parameter \eqn{\theta}.
#'
#' Then the function calculates the quantities
#'
#'      \deqn{\chi(D|\theta_1),\chi(D|\theta_2),\chi(D|\theta_3),   \cdots,  \chi(D|\theta_N)        }
#'
#'Foe MCMC samples;
#'
#'
#'
#'       \deqn{   \theta_1 ,   \theta_2 ,   \theta_3,    \cdots ,    \theta_N         }.
#'
#'
#'
#'
#'Also,  it evaluate the posterior mean of the chi square, statistc, that is,
#'
#'
#'       \deqn{\frac{1}{N} \sum _i ^N \chi(D|\theta_i)}
#'
#'which is approximation of the following posterior integral;
#'
#'
#'       \deqn{\int   \chi(D|\theta)\pi(\theta|D) d\theta}
#'
#'where \eqn{\pi(\theta|D)} denotes the posterior probability.

#'
#'
#'
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves

#'
#' @return A list, contains \{ \eqn{\chi^2(Data|\theta_i)} ; i= 1,2,3,...N\}, where N is the numberMCMC samples.
#' @export Chi_square_goodness_of_fit_in_case_of_MRMC_Posterior_Mean
#'
# @examples

Chi_square_goodness_of_fit_in_case_of_MRMC_Posterior_Mean  <-  function(  StanS4class ,summary=TRUE){

  fit <-StanS4class
  dataList <- StanS4class@dataList
  NL <- dataList$NL

  harray <- array_of_hit_and_false_alarms_from_vector(dataList = dataList)$harray
  farray <- array_of_hit_and_false_alarms_from_vector(dataList = dataList)$farray

  ppp <- extract(fit)$ppp # Preserving array format of the declaration in stan file, but adding the first index as MCMC samples
for (md in 1:dim(ppp)[3]) {for (qd in 1:dim(ppp)[4]){
  ppp[,,md,qd] <-  aperm(apply(aperm(ppp[,,md,qd]),2,rev))# <--  Very important    Make a matrix such that B[i,j]=A[i,J-j]
}}



  A <- ppp*NL
  B <- harray

  C <- array(aperm(sapply(1:dim(A)[1], function(i) A[i,,,] - B)), dim(A))

  hit.term <- C^2/A



  dl <- extract(fit)$dl # [MCMC, C]           Preserving array format of the declaration in stan file, but adding the first index as MCMC samples
  dl <-  aperm(apply(aperm(dl),2,rev))# <--  Very important    Make a matrix such that B[i,j]=A[i,J-j]

  A <- farray #[C,M,Q]
  B <- dl*NL  #[MCMC, C]



  C <- array(NA, c(dim(B)[1], dim(A)))

  for (h in 1 : dim(B)[1]){
    for(i in 1 : dim(A)[1]){
      C[h, i,, ] <-  A[i,, ] - B[h, i]
       }
    }
# C^2

  FalseAlarm.term <- sweep(C^2, c(1,2), B,`/`)

  chi.square <- hit.term + FalseAlarm.term

  df <-data.frame(
    chi.square=mean(chi.square),
    hit.term=mean(hit.term),
    FalseAlarm.term=mean(FalseAlarm.term)
  )

  print(knitr::kable(df, format = "pandoc"))


 if (summary==TRUE) message("\n* Posterior Mean of test statistics")
  invisible(
    list(
      chi.square=chi.square,
      hit.term=hit.term,
      FalseAlarm.term=FalseAlarm.term
    )

  )
}#function



# get_posterior_mean(fit,"ppp")[,"mean-all chains"]

