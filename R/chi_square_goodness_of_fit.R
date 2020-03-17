
#'@title   \emph{\strong{Chi square goodness of
#' fit statistics}} at each MCMC sample w.r.t. a given dataset.
#'
#'
#'@description
#' Calculates a vector, consisting of \emph{\strong{the Goodness of Fit (Chi Square)}}
#' for a given dataset \eqn{D} and
#'  each posterior MCMC samples \eqn{\theta_i=\theta_i(D), i=1,2,3,....}, namely,
#'
#'
#'    \deqn{ \chi^2 (D|\theta_i)}
#'
#'
#'
#' for \eqn{i=1,2,3,....} and thus its dimension is the number of MCMC iterations..
#'
# here ----
#'Note that
#'  In MRMC cases, it is defined as follows.
#'
#' \deqn{\chi^2(D|\theta) := \sum_{r=1}^R \sum_{m=1}^M \sum_{c=1}^C \biggr( \frac{[ H_{c,m,r}-N_L\times p_{c,m,r}]^2}{N_L\times p_{c,m,r}}+\frac{[F_{c,m,r}-(\lambda _{c} -\lambda _{c+1} )\times N_{L}]^2}{(\lambda_{c} -\lambda_{c+1} )\times N_{L} }\biggr).}
#'
#'where  a dataset \eqn{D} consists of the
#'pairs of the number of False Positives and the number of True
#' Positives  \eqn{ (F_{c,m,r}, H_{c,m,r}) }
#' together with the number of lesions \eqn{N_L}
#' and the number of images \eqn{N_I} and
#'  \eqn{\theta} denotes the model parameter.
#'
#'
#'
#'
#'
#'@details To calculate the chi square (goodness of fit) \eqn{\chi^2 (y|\theta)}
#' test statistics, the two variables
#' are required; one is an observed dataset \eqn{y}
#'  and the other is an estimated parameter \eqn{\theta}.
#'In the classical chi square values,
#' MLE(maximal likelihood estimator) is used
#'for an estimated parameter \eqn{\theta} in \eqn{\chi^2 (y|\theta)}.
#' However, in the Bayesian context,
#'the parameter is not deterministic and
#'we consider it is a random variable such as
#' samples from the posterior distribution.
#' And such samples are obtained in the Hamiltonian
#' Monte Carlo Simulation.
#' Thus we can calculate chi square values for each MCMC sample.
#'@return Chi squares for each MCMC sample.
#'\deqn{\chi^2 = \chi^2 (D|\theta_i),i=1,2,...,N}
#' So, the return values is a vector of length \eqn{N} which denotes
#'  the number of MCMC iterations
#'  except the warming up period.
#'  Of course if MCMC is not only one chain,
#'   then all samples of chains are used to calculate the chi square.
#'
#'   In the sequel, we use the notation
#'
#'    for a prior \eqn{\pi(\theta)},
#'
#'    posterior \eqn{\pi(\theta|D)},
#'
#'      likelihood \eqn{f(D|\theta)},
#'
#'      parameter \eqn{\theta},
#'
#'    datasets \eqn{D} as follows;
#'
#'  \deqn{ \pi(\theta|D) \propto f(D|\theta) \pi(\theta).}
#'
#'
#'
#'  Let us denote the \strong{posterior MCMC samples} of
#'  size  \eqn{N} by
#'
#'    \deqn{\theta_1, \theta_2, \theta_3,...,\theta_N}
#'
#'
#'    which is drawn from
#'    posterior \eqn{\pi(\theta|D)} of given data \eqn{D}.
#'
#'
#'Recall that the chi square goodness of fit statistics \eqn{\chi}
#'depends on the model parameter \eqn{\theta} and data \eqn{D}, namely,
#'
#'\deqn{\chi^2 = \chi^2 (D|\theta)}.
#'
#'Then return value is a vector
#'of length \eqn{N} whose components is given by:
#'
#'
#'
#'\deqn{\chi^2 (D|\theta_1), \chi^2 (D|\theta_2), \chi^2 (D|\theta_3),...,\chi^2 (D|\theta_N),}
#'
#'which is a vector and  a return value of this function.
#'
#'  As an application of this return value,
#'   we  can calculate
#'    the posterior mean of \eqn{\chi = \chi (D|\theta)},
#'     namely, we get
#'
#'\deqn{ \chi^2 (D) =\int \chi^2 (D|\theta)  \pi(\theta|D)     d\theta.}
#'
#'
#'
#' In my model, almost all example,
#'  result of calculation shows that
#'
#'
#'\deqn{  \int \chi^2 (D|\theta)  \pi(\theta|D)     d\theta > \chi^2 (D| \int \theta   \pi(\theta|D)     d\theta) }
#'
#'
#'The above inequality is true for all \eqn{D}?? I conjecture it.
#'
#' Revised 2019 August 18
#' Revised 2019 Sept. 1
#' Revised 2019 Nov 28

#'
#'
#'  Our data is \strong{2C categories}, that is,
#'
#'
#'  the number of hits        :h[1], h[2], h[3],...,h[C] and
#'
#'  the number of false alarms: f[1],f[2], f[3],...,f[C].
#'
#'
#'    Our model has \strong{C+2 parameters}, that is,
#'
#'    the thresholds of the bi normal assumption z[1],z[2],z[3],...,z[C] and
#'
#'    the mean and standard deviation of the signal distribution.
#'
#'
#'  So, the degree of freedom of this statistics is calculated by
#'
#'
#'      No. of categories  -  No. of parameters  - 1 = 2C-(C+2)-1 =C -3.
#'
#'This differ from Chakraborty's  result C-2. Why ?
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
#'@param f A vector of positive integers,
#'representing the number of false alarms.
#' This variable was made in order to
#'  substitute the false alarms data drawn
#'  from the posterior predictive distributions.
#'In famous Gelman's book, he explain
#'how to use the test statistics in the
#'Bayesian context. In this context I need
#' to substitute the replication data from
#' the posterior predictive distributions.

#'@param h  A vector of positive integers,
#' representing the number of hits.
#' This variable was made in order to
#'  substitute the hits data drawn
#'  from the posterior predictive distributions.
#'In famous Gelman's book, he explain how
#'to use the test statistics in the Bayesian context.
#' In this context I need to substitute the replication
#'  data from the posterior predictive distributions.

#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@examples
#'
#' \donttest{

# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'
#'#  Get the MCMC samples from a dataset.
#'
#'        fit <- fit_Bayesian_FROC(BayesianFROC::dataList.Chakra.1,
#'                            ite = 1111,
#'                            summary =FALSE,
#'                            cha = 2)
#'
#'#   The chi square discrepancies are calculated by the following code
#'
#'          Chi.Square.for.each.MCMC.samples   <-   chi_square_goodness_of_fit(fit)
#'
#'
#'
#'#'
#'
#'
#'          # With Warning
#'          chi_square_goodness_of_fit(fit)
#'
#'          # Without warning
#'           chi_square_goodness_of_fit(fit,
#'                                      h=fit@dataList$h,
#'                                      f=fit@dataList$f)
#'
#'
#'
#'
#'
#'
#'
#'#  Get posterior mean of the chi square discrepancy.
#'
#'                     m<-   mean(Chi.Square.for.each.MCMC.samples)
#'
#'
#'
#'
#'# The author read at 2019 Sept. 1, it helps him. Thanks me!!
#'
#'
#'
#'
#' # Calculate the p-value for the posterior mean of the chi square discrepancy.
#'
#'                      stats::pchisq(m,df=1)
#'
#'
#'# Difference between chi sq. at EAP and EAP of chi sq.
#'
#'    mean( fit@chisquare - chi_square_goodness_of_fit(fit))
#'
#'
#'}# dottest

#'
#' @export

chi_square_goodness_of_fit <- function(StanS4class,
                                       dig=3,
                                       h=StanS4class@dataList$h,
                                       f=StanS4class@dataList$f

){
  C <-StanS4class@dataList$C

  if (StanS4class@studyDesign=="MRMC") {
    return(message("Now, MRMC not be allowed"))
  }

  #This hits and false positives should be changed to the simultaion
  # from the posterior predictive distributions
  if (missing(h)==TRUE)  {
    message("Since hits are missing, we use the data which is used to estimates the model parameter to run this function")
    h <-StanS4class@dataList$h
  }

  if (missing(f)==TRUE){
    message("Since false alarms are missing, we use the data which is used to estimates the model parameter to run this function")
    f <-StanS4class@dataList$f
  }

  NL <-StanS4class@dataList$NL
  NI <-StanS4class@dataList$NI
  ModifiedPoisson <-(StanS4class@studyDesign =="srsc.per.lesion")
  if (ModifiedPoisson)  NX <- NL
  if (!ModifiedPoisson)  NX <- NI

  fit <-methods::as(StanS4class, "stanfit")

  MCMC <- length(
    rstan::get_divergent_iterations(fit)
  ) # = cha*(ite-war)

  # MCMC=(ite-war)*cha
  chisquare <- vector()

  #--------- chi ^2 -----------Start
  p<-rstan::extract(fit)$p
  lambda<-rstan::extract(fit)$l


  ss <- array(0,dim = c(MCMC,C))
  tt <- array(0,dim = c(MCMC,C))
  for(cd in 1:C){
    ss[,cd]<-(h[C+1-cd]-NL*p[,cd])^2/(NL*p[,cd])

    if(!cd==C)tt[,cd]<-(f[C+1-cd]-NX*(lambda[,cd]-lambda[,cd+1]))^2/(NX*(lambda[,cd]-lambda[,cd+1]))
    if(cd==C)  tt[,cd]<-(f[C+1-cd]-NX*(lambda[,cd]- 0))^2/(NX*(lambda[,cd]- 0))

  }
  chi.Square.for.each.MCMC.samples <- apply(ss, 1,sum)+apply(ss,1, sum)
  chi.Square.for.each.MCMC.samples <- signif(chi.Square.for.each.MCMC.samples, digits = dig)
  return(chi.Square.for.each.MCMC.samples)
}











#'@title  Not vetor: The Goodness of Fit (Chi Square) Calculator
#'@description Chi square goodness of fit
#' statistics for each MCMC sample with a fixed dataset.
#'
#'
#'
#'    Our data is 2C categories, that is,
#'
#'
#'  the number of hits        :h[1], h[2], h[3],...,h[C] and
#'
#'  the number of false alarms: f[1],f[2], f[3],...,f[C].
#'
#'
#'    Our model has C+2 parameters, that is,
#'
#'    the thresholds of the bi normal assumption z[1],z[2],z[3],...,z[C] and
#'
#'    the mean and standard deviation of the signal distribution.
#'
#'
#'  So, the degree of freedom of this statistics is calculated by
#'
#'
#'             2C-(C+2)-1 =C -3.
#'
#'This differ from Chakraborty's  result C-2. Why ?

#'
#'
#'
#'
#'@details To calculate the chi square test statistics, the two quantities are needed, that is, data and parameter. In the classical (frequentists) chi square values, as the estimates of parameter, for example, MLE (maximal likelihood estimator) is chosen. In Bayesian sense, the parameter can be taken for all MCMC iterations, that is, parameter is not deterministic and we consider it is a random variable or samples from the posterior distribution. And such samples are obtained in the Hamiltonian Monte Carlo Simulation with the author's  Bayesian Model. Thus we can calculate chi square values with MCMC samples.
#'@param f A vector of non-negative integers, indicating the number of false alarms.
#' The reason why the author includes this variable is to substitute the false alarms from the posterior predictive distribution.
#'In famous Gelman's book, he explain how to make test statistics in the Bayesian context,  and it require the samples from posterior predictive distribution.
#'So, in this variable author substitute the replication data from the posterior predictive distributions.

#'@param h  A vector of non-negative integers, indicating the number of hits.
#'The reason why the author includes this variable is to substitute the false alarms from the posterior predictive distribution.
#'In famous Gelman's book, he explain how to make test statistics in the Bayesian context,  and it require the samples from posterior predictive distribution.
#'So, in this variable author substitute the replication data from the posterior predictive distributions.


#'@param p  A vector of non-negative integers, indicating hit rate. A vector whose length is number of confidence levels.
#'@param lambda A vector of non-negative integers, indicating False alarm rate. A vector whose length is number of confidence levels.
#'@param NI An integer, representing Number of Images
#'@param NL An integer, representing Number of Lesions
#'
#'
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@return A number !! Not list nor dataframe nor vector !!
#' Only A number represent the chi square for your input data.
#'@examples
#'\donttest{
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'
#'#  Make a stanfit object (more precisely its inherited S4 class object)
#'
#'        fit <- fit_Bayesian_FROC(BayesianFROC::dataList.Chakra.1,
#'                            ite = 1111,
#'                            summary =FALSE,
#'                            cha = 2)
#'
#'#   The chi square discrepancies (Goodness of Fit) are calculated
#'#   by the following code with the posterior mean as a parameter.#
#'
#'
#'   NI          <-  fit@dataList$NI
#'   NL          <-  fit@dataList$NL
#'   f.observed  <-  fit@dataList$f
#'   h.observed  <-  fit@dataList$h
#'   C           <-  fit@dataList$C
#'
#'  # p <-  rstan::get_posterior_mean(fit, par=c("p"))
#'  #lambda <- rstan::get_posterior_mean(fit, par=c("l"))
#' # Note that get_posterior_mean is not a number but a matrix when
#' # Chains is not 1.
#' # So, instead of it, we use
#' #
#'
#'   e     <- extract_EAP_CI(fit,"l",fit@dataList$C )
#'  lambda <- e$l.EAP
#'
#'   e <- extract_EAP_CI(fit,"p",fit@dataList$C )
#'   p <- e$p.EAP
#'
#'          Chi.Square <-   chi_square_goodness_of_fit_from_input_all_param(
#'
#'                           h   =   h.observed,
#'                           f   =   f.observed,
#'                           p   =   p,
#'                       lambda  =   lambda,
#'                           NL  =   NL,
#'                           NI  =   NI
#'                                )
#'
#'#  Get posterior mean of the chi square discrepancy.
#'
#'                     Chi.Square
#'
#' # Calculate the p-value for the posterior mean of the chi square discrepancy.
#'
#'                      stats::pchisq(Chi.Square,df=1)
#'
#'
#'
#'
#'
#'
#'
#'}# dottest
#'
#' @export

chi_square_goodness_of_fit_from_input_all_param <- function(
  # StanS4class,
  h,
  f,
  p,
  lambda,
  NL ,
  NI ,
  ModifiedPoisson = FALSE,
  dig = 3
){



  if (ModifiedPoisson == FALSE) NX <- NI
  if (ModifiedPoisson == TRUE )  NX <- NL




  C <-length(h)

  chisquare <- vector()

  ss <- array(0,dim = c(C))
  tt <- array(0,dim = c(C))


  for(cd in 1:C){
    ss[cd]<-(h[C+1-cd]-NL*p[cd])^2/(NL*p[cd])

    if(!cd==C)tt[cd]<-(f[C+1-cd]-NX*(lambda[cd]-lambda[cd+1]))^2/(NX*(lambda[cd]-lambda[cd+1]))
    if(cd==C)  tt[cd]<-(f[C+1-cd]-NX*(lambda[cd]- 0))^2/(NX*(lambda[cd]- 0))
  }
  chi.Square.for.each.MCMC.samples <- sum(ss)+sum(tt)
  chi.Square.for.each.MCMC.samples <- signif(chi.Square.for.each.MCMC.samples, digits = dig)
  return(chi.Square.for.each.MCMC.samples)
}









#' @title Chi square statistic (goodness of fit) in the case
#'  of MRMC at the pair of given data
#'   and each MCMC sample
#'
# @description ---------
#' @description
#'
#' Revised 2019 Oct 16.
#' Revised 2019 Nov 1.
#' Revised 2019 Nov 27.
#' Revised 2019 Dec 1.

#'
#'In the following, we explain what this function calculates.
#'
#' Let \eqn{\chi^2(y|\theta)} be a
#' \emph{chi square goodness of fit} statistic which is defined by
#'
#'
#'  ( Observed data - Expectation \eqn{)^2}/Exectation.
#'
#'
#'
#'
#'
#'
#'  In MRMC cases, it is defined as follows.
#'
#' \deqn{\chi^2(D|\theta) := \sum_{r=1}^R \sum_{m=1}^M \sum_{c=1}^C \biggr( \frac{[ H_{c,m,r}-N_L\times p_{c,m,r}]^2}{N_L\times p_{c,m,r}}+\frac{[F_{c,m,r}-(\lambda _{c} -\lambda _{c+1} )\times N_{L}]^2}{(\lambda_{c} -\lambda_{c+1} )\times N_{L} }\biggr).}
#'
#'where  a dataset \eqn{D} consists of the
#'pairs of the number of False Positives and the number of True
#' Positives  \eqn{ (F_{c,m,r}, H_{c,m,r}) }
#' together with the number of lesions \eqn{N_L}
#' and the number of images \eqn{N_I} and
#'  \eqn{\theta} denotes the model parameter.
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'  Note that
#'
#'   \deqn{\chi^2(D|\theta) := \sum_{r=1}^R \sum_{m=1}^M \sum_{c=1}^C \biggr( \frac{[ H_{c,m,r}-  E[H_{c,m,r}] ]^2}{E[H_{c,m,r}]}+\frac{[F_{c,m,r}- E[F_{c,m,r}]   ]^2}{    E[F_{c,m,r}]   }\biggr).}

#'
#'So, the chi square has two terms.
#'
#'1) The first term is the difference of hit
#' and its expectation.
#'
#'2) The second term   is the differences of observed false alarms and its expectatioins.
#'
#'   In this function, we calculates each terms, separately.
#'    So, return values retain these two terms, separately.
#'
#'
#'
#' In this function, we calculates the following (I) and (II):
#'
#'
#'
#' \strong{ (I) A vector -------------------}
#'
#'
#'Let us denote a collection of
#' posterior MCMC samples for a given dataset \eqn{D} by
#'
#'\deqn{   \theta_1 ,   \theta_2 ,   \theta_3,    \cdots ,    \theta_N.}
#' Substituing these MCMC samples into the above definition of the chi square,
#'  we obtain the following vector as a return value of this function.
#'
#'      \deqn{ \chi^2(D|\theta_1),}
#'      \deqn{ \chi^2(D|\theta_2),}
#'      \deqn{ \chi^2(D|\theta_3),}
#'      \deqn{          :         }
#'      \deqn{          :         }
#'      \deqn{ \chi^2(D|\theta_N).}
#'
#'
#

#'
#'  \strong{ (II) A mean of the above vector, namely, the posterior mean of the chi square over all MCMC samples -------------}
#'
#'
#'Using the above vector \eqn{(\chi^2(D|\theta_i);i=1,...,N)},
#'the function also calculates the posterior mean of the chi square statistic, namely,
#'
#'
#'       \deqn{\frac{1}{N} \sum _{i=1} ^N \chi^2(D|\theta_i),}
#'
#'which is an approximation of the following integral;
#'
#'
#'       \deqn{\int   \chi^2(D|\theta)\pi(\theta|D) d\theta,}
#'
#'where \eqn{\pi(\theta|D)} denotes the posterior probability density under the given data \eqn{D}.
#'
#'Do not confuse it with the following
#'
#'       \deqn{   \chi^2(D| ).}
#'
#' where \eqn{\theta^*} denotes the posterior estimates \eqn{\int \theta \pi(\theta|D) d\theta}.


#'


# @details -----
#' @details This function is implemented by
#' vectorizations and further techinics.
#' When the author review this, I
#'  find my past work is great,...
#'  I forget that I made this.
#' But this function is great.
#'
#' Revised 2019 Nov 1
#'
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'
# @param --------
#'@param   dl_is_an_array_of_C_only_and_not_C_M_Q A Boolean, if \code{TRUE},
#'then false rate \code{lambda} simply
#'denoted by \code{l} in \R script ( \eqn{\lambda} )
#' is an vector \code{l[C]}. If false, then the false
#'  alarm rate is an array \code{l[C,M,Q]}.
#'
#'
#  @return ----
#' @return A list, calculated by each modality reader and cofidence level, and MCMC samples.
#'  A one the component of list contains \{ \eqn{\chi^2(Data|\theta_i)} ; i= 1,2,3,...n\}, where \eqn{n} is the number of MCMC iterations.
#'
#' Each component
#' of list isan array
#'  whose index indicats  \code{[MCMC, Confidence, Modality, Reader]  }.
#'
#' Each component of list is
#'   \code{an array whose index indicats [MCMC, C, M, Q]}.
#'
#' To be passed to the calculation of Posterior predictive p value,
#' I need the sum of return value,
#' that is, sum of C,M,Q and resulting quantities
#' construct a vetor whose length is a same as the number of MCMC iterations.
#' I love you. I need you. So, to calculate such quantites,
#'  the author .... will make a new function.
#'
#'
#'  Also, retains the posterior mean of chi square:
#'
#'
#'    \deqn{\chi^2(Data)  = \int \chi^2(Data|\theta) d  \theta}

#'
#'
#'
#' @export Chi_square_goodness_of_fit_in_case_of_MRMC_Posterior_Mean
#'
#' @examples
#'
#'
#' \donttest{
#'
#'#----------------------------------------------------------------------------------------
#'#            1) Create a fitted model object with data named only one word dd
#'#----------------------------------------------------------------------------------------
#'
#'
#'
#'                           fit <- fit_Bayesian_FROC(dd )
#'
#'
#'
#'#----------------------------------------------------------------------------------------
#'#            2) Calculate a chi square and meta data
#'#----------------------------------------------------------------------------------------
#'
#'
#'
#'             a <- Chi_square_goodness_of_fit_in_case_of_MRMC_Posterior_Mean(fit)
#'
#'
#'
#'#----------------------------------------------------------------------------------------
#'#            3) Extract a chi square
#'#----------------------------------------------------------------------------------------
#'
#'
#'
#'
#'
#'
#'                               chi.square  <- a$chi.square
#'
#'
#'
#'
#'#----------------------------------------------------------------------------------------
#'#           A single reader case is special in the programming perspective
#'#                                                                         2020 Feb 24
#'#----------------------------------------------------------------------------------------
#'
#'
#' f <- fit_Bayesian_FROC( ite  = 1111,  cha = 1, summary = T, dataList = dddd )
#' Chi_square_goodness_of_fit_in_case_of_MRMC_Posterior_Mean(f)
#'
#'
#'
#'# Revised 2019 August 19
#'#         2019 Nov 1
#'
#'}# donttest
#'
#'
#'

Chi_square_goodness_of_fit_in_case_of_MRMC_Posterior_Mean  <-  function(
  StanS4class ,
  summary=TRUE,
  dl_is_an_array_of_C_only_and_not_C_M_Q=TRUE


){
  # if (StanS4class@dataList$Q==1) return(message("under construction"))

  fit <-StanS4class
  dataList <- StanS4class@dataList
  NL <- dataList$NL
  NI <- dataList$NI
  if (fit@ModifiedPoisson==TRUE) NX <- NL
  if (fit@ModifiedPoisson==FALSE) NX <- NI

  harray <- array_of_hit_and_false_alarms_from_vector(dataList = dataList)$harray
  farray <- array_of_hit_and_false_alarms_from_vector(dataList = dataList)$farray

  ppp <- extract(fit)$ppp # Preserving array format of the declaration in stan file, but adding the first index as MCMC samples
  for (md in 1:dim(ppp)[3]) {for (qd in 1:dim(ppp)[4]){
    ppp[,,md,qd] <-  aperm(apply(aperm(ppp[,,md,qd]),2,rev))# <--  Very important    Make a matrix such that B[i,j]=A[i,J-j]
  }}



  A <- ppp*NL
  B <- harray
# browser()
if (!StanS4class@dataList$Q==1) {C <- array(aperm(sapply(1:dim(A)[1], function(i) A[i,,,] - B)), dim(A))
               hit.term.array <- C^2/A



                  dl <- extract(fit)$dl # [MCMC, C]           Preserving array format of the declaration in stan file, but adding the first index as MCMC samples
                  if(dl_is_an_array_of_C_only_and_not_C_M_Q == FALSE){
                    for (md in 1:dim(ppp)[3]) {for (qd in 1:dim(dl)[4]){
                      dl[,,md,qd] <-  aperm(apply(aperm(dl[,,md,qd]),2,rev))# <--  Very important    Make a matrix such that B[i,j]=A[i,J-j]
                    }}



                    A <- dl*NL
                    B <- farray

                    C <- array(aperm(sapply(1:dim(A)[1], function(i) A[i,,,] - B)), dim(A))

                    FalseAlarm.term.array <- C^2/A
                  }

                  if(dl_is_an_array_of_C_only_and_not_C_M_Q){
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

                    FalseAlarm.term.array <- sweep(C^2, c(1,2), B,`/`)
                  }



}#!StanS4class@dataList$Q==1 2020 Feb 24






  if (StanS4class@dataList$Q==1){ A <-  A[,,,1];B <- B[,,1] #2020 Feb 24
                                    C <- array(aperm(sapply(1:dim(A)[1], function(i) A[i,,] - B)), dim(A)) #2020 Feb 24
                                    hit.term.array <- C^2/A #2020 Feb 24



                                    dl <- extract(fit)$dl # [MCMC, C]           Preserving array format of the declaration in stan file, but adding the first index as MCMC samples
                                    if(dl_is_an_array_of_C_only_and_not_C_M_Q == FALSE){
                                      for (md in 1:dim(ppp)[3]) {for (qd in 1:dim(dl)[4]){
                                        dl[,,md,qd] <-  aperm(apply(aperm(dl[,,md,qd]),2,rev))# <--  Very important    Make a matrix such that B[i,j]=A[i,J-j]
                                      }}



                                      A <- dl*NL
                                      B <- farray

                                      C <- array(aperm(sapply(1:dim(A)[1], function(i) A[i,,,] - B)), dim(A))

                                      FalseAlarm.term.array <- C^2/A
                                    }

                                    if(dl_is_an_array_of_C_only_and_not_C_M_Q){
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

                                      FalseAlarm.term.array <- sweep(C^2, c(1,2), B,`/`)
                                      FalseAlarm.term.array <- FalseAlarm.term.array[,,,1] #2020 Feb 24
                                    }

                                    # browser()

  }#  StanS4class@dataList$Q==1 2020 Feb 24








  chi.square.array <- hit.term.array + FalseAlarm.term.array

  hit.term <- apply(hit.term.array, c(1), sum)
  FalseAlarm.term <- apply(FalseAlarm.term.array, c(1), sum)
  chi.square <- apply(chi.square.array, c(1), sum)
  df <-data.frame(
    chi.square.posterior.mean=mean(chi.square),
    hit.term.posterior.mean=mean(hit.term),
    FalseAlarm.term.posterior.mean=mean(FalseAlarm.term)
  )

  if (summary==TRUE)print(knitr::kable(df, format = "pandoc"))


  if (summary==TRUE) message("\n* Posterior Mean of test statistics")
  invisible(
    list(

      chi.square=chi.square,
      hit.term=hit.term,
      FalseAlarm.term=FalseAlarm.term,
      df.chisquare.posterior.mean =df
    )

  )
}#function



# get_posterior_mean(fit,"ppp")[,"mean-all chains"]





#' @title Chi square in the case of MRMC at a given dataset and a given parameter.
#' @description Given parameter and data, the chi square is calculated.

#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@author Issei Tsunoda
#'
#' @return A list, contains  \eqn{\chi^2(Data|\theta)},
#'  where \eqn{Data} and \eqn{\theta} are specified by user.
#'
#' @param ppp An array of \code{[C,M,Q]}, representing hit rate,
#' where \code{C,M,Q} denotes the number of confidences, modalities,
#'  readers, respectively.
#' @param dl An vector of length \code{C M Q} representing
#' false alarm rate,
#' where \code{C,M,Q} denotes the number of
#'  confidences, modalities, readers, respectively.
#'
#' @export
#'
#' @examples
#'
#'
#' \donttest{
#'#----------------------------------------------------------------------------------------
#'#  0)
#'#----------------------------------------------------------------------------------------
#'
#'#        Chi square depend on data and model parameter, thus what we have to do is:
#'#        prepare data and parameter
#'
#'                 # In the follwoing, we use data named ddd as a dataset,
#'                 # and use parameter from posterior mean estimates
#'
#'#  So, we calculate chi square at using data ddd
#'#  and parameter EAP estimated using ddd.
#'
#'
#'
#'       fit <- fit_Bayesian_FROC(  dataList = ddd )
#'
#'
#'#----------------------------------------------------------------------------------------
#'#  1)  prepare hit rate and false alarm rate
#'#----------------------------------------------------------------------------------------
#'
#'
#'          e <-extract_estimates_MRMC(fit);
#'          dl <- e$dl.EAP;
#'          ppp <- e$ppp.EAP;
#'
#'
#'#----------------------------------------------------------------------------------------
#'# 2)  Calculate chi square using above hit rate and false alarm rate and data named ddd
#'#----------------------------------------------------------------------------------------
#'
#'          chi_square_goodness_of_fit_from_input_all_param_MRMC(ppp,dl,ddd)
#'
#'}# donttest
#'
#'
#'




chi_square_goodness_of_fit_from_input_all_param_MRMC <-  function(  ppp,
                                                                    dl,
                                                                    dataList,
                                                                    summary=TRUE
){

  # fit <-StanS4class
  # dataList <- StanS4class@dataList
  NL <- dataList$NL
  NI <- dataList$NI
  m <- dataList$m
  c <- dataList$c
  q <- dataList$q

  h <- dataList$h
  f <- dataList$f
  # h[n] ~ binomial(NL, ppp[c[n],m[n],q[n]]);
  # ff[n] ~ poisson(l[c[n]]*NL);//Chakraborty's model
  hit.term.vector <-vector()
  FalseAlarm.term.vector <- vector()


  #In case of a single reader and multple modaity case, an array of type, e.g., [3,1] is reduced to a vector of dimension 3 and it caused some error. So, the author fixed it.

 if (length(dim(ppp))==2) {#2020 Feb 24
    M <- dataList$M
    C <- dataList$C
    Q <- dataList$Q
    #2020 Feb 24
    ppp2<- array(NA,dim = c(C,M,Q))#2020 Feb 24

    ppp2[,,1]<-ppp#2020 Feb 24
    ppp <-ppp2#2020 Feb 24


  }





  for (n in 1:length(h)) {
    hit.term.vector[n] <- (h[n]-ppp[c[n],m[n],q[n]]*NL)^2/(ppp[c[n],m[n],q[n]]*NL)
  }
  hit.term<-sum(hit.term.vector)

  a <- metadata_to_fit_MRMC(dataList)
  ff <- a$ff
  for (n in 1:length(h)) {
    FalseAlarm.term.vector[n] <-( f[n] - dl[c[n]]*NL)^2/(dl[c[n]]*NL)
  }

  FalseAlarm.term <- sum(FalseAlarm.term.vector)


  chi.square <- hit.term + FalseAlarm.term

  df <-data.frame(
    chi.square=chi.square,
    hit.term=hit.term,
    FalseAlarm.term=FalseAlarm.term
  )

  if (summary==TRUE) print(knitr::kable(df, format = "pandoc"))
  # browser()
  return(
    list(
      chi.square=chi.square,
      hit.term=hit.term,
      FalseAlarm.term=FalseAlarm.term,
      data.frame.of.metadata.for.chisquare =df,
      hit.term.vector=hit.term.vector,
      FalseAlarm.term.vector=FalseAlarm.term.vector
    )

  )
}#function



# get_posterior_mean(fit,"ppp")[,"mean-all chains"]




#' @title chi square at replicated data drawn (only one time) from model with each MCMC samples.
#'@description
#' To pass the return  value to  the  calculator of the posterior predictive p value.
#'@inheritParams DrawCurves
#'@inheritParams fit_Bayesian_FROC
#'@param seed This is  used only in programming phase.
#'If seed is passed, then, in procedure indicator the seed is printed.
#'This parameter is only for package development.
#  @return -----
#' @return
#'
#' From any given posterior MCMC samples
#'  \eqn{\theta_1,\theta_2,...,\theta_i,....,\theta_n}
#'   (provided by stanfitExtended object),
#' it calculates a return value as a vector
#'  of the form \eqn{\chi(y_i|\theta_i),i=1,2,....},
#'  where each dataset \eqn{y_i} is drawn
#'  from the corresponding
#'   likelihood \eqn{likelihood(.|\theta_i),i=1,2,...},
#'  namely,
#'
#'    \deqn{y_i \sim likelihood(.| \theta_i).}
#'
#' The return value also retains these \eqn{y_i, i=1,2,..}.
#'
#'
#'
#' Revised 2019 Dec. 2

#'@details For a given dataset \eqn{D_0},
#' let \eqn{\pi(|D_0)} be a posterior
#' of the given data \eqn{D_0}, then we can draw poterior samples.
#'
#'
#'    \deqn{\theta_1 \sim \pi(.|  D_0),}
#'    \deqn{\theta_2 \sim \pi(.|  D_0),}
#'    \deqn{\theta_3 \sim \pi(.|  D_0),}
#'    \deqn{....,}
#'    \deqn{\theta_n \sim \pi(.|  D_0).}
#'
#'
#'
#' We let \eqn{L(|\theta)}  be a likelihood function.
#' Then we can draw samples in \strong{only one time} from
#' the collection of likelihoods  \eqn{L(\\theta_1),L(\\theta_2),...,L(\\theta_n)}.
#'
#'
#'
#'    \deqn{y_1 \sim L(.| \theta_1),}
#'    \deqn{y_2 \sim L(.| \theta_2),}
#'    \deqn{y_3 \sim L(.| \theta_3),}
#'    \deqn{....,}
#'    \deqn{y_n \sim L(.| \theta_n).}
#'
#'
#'
#'
#'
#'    Altogether, using these pair of samples \eqn{(y_i, \theta_i), i= 1,2,...,n} we calculates the
#'    \strong{return value} of this function. That is,
#'
#'
#'    \deqn{\chi(y_1|\theta_1),}
#'    \deqn{\chi(y_2|\theta_2),}
#'    \deqn{\chi(y_3|\theta_3),}
#'    \deqn{....,}
#'    \deqn{\chi(y_n|\theta_n).}
#'
#'    \emph{ \strong{This is contained in a return value}},
#'
#'    so the return value is a vector of length
#'    is the number of MCMC iterations
#'     except the burn-in period.
#'
#'
#'
#'
#'
#' \emph{ \strong{ Application of this return value: calculate the so-called \emph{Posterior Predictive P value.} } }
#'
#' In other functions,
#' the author use this function with many seeds, namely,
#' chaning seed, we can obtain
#'
#'
#'
#'\deqn{y_1^1,y_1^2,y_1^3,...,y_1^j,....,y_1^J \sim L ( . |\theta_1), }
#'\deqn{y_2^1,y_2^2,y_2^3,...,y_2^j,....,y_2^J \sim L ( . |\theta_2),}
#'\deqn{y_3^1,y_3^2,y_3^3,...,y_3^j,....,y_3^J \sim L ( .|\theta_3),}
#'\deqn{...,}
#'\deqn{y_i^1,y_i^2,y_i^3,...,y_i^j,....,y_i^J \sim L ( . |\theta_i),}
#'\deqn{...,}
#'\deqn{y_I^1,y_I^2,y_I^3,...,y_I^j,....,y_I^J \sim L ( . |\theta_I),}
#'
#'
#'
#' And thus, we will obatin
#'
#'\deqn{\chi(1|\theta_1),\chi(1|\theta_2),\chi(1|\theta_3),...,\chi(1|\theta_j),....,\chi(1|\theta_J),   }
#'\deqn{\chi(2|\theta_1),\chi(2|\theta_2),\chi(2|\theta_3),...,\chi(2|\theta_j),....,\chi(2|\theta_J),  }
#'\deqn{\chi(3|\theta_1),\chi(3|\theta_2),\chi(3|\theta_3),...,\chi(3|\theta_j),....,\chi(3|\theta_J),   }
#'\deqn{...,}
#'\deqn{\chi(i|\theta_1),\chi(i|\theta_2),\chi(i|\theta_3),...,\chi(i|\theta_j),....,\chi(i|\theta_J),  }
#'\deqn{...,}
#'\deqn{\chi(I|\theta_1),\chi(I|\theta_2),\chi(I|\theta_3),...,\chi(I|\theta_j),....,\chi(I|\theta_J),  }
#'
#'whih are used when we calculate
#' the so-called \emph{Posterior Predictive P value} to test the
#' \emph{null hypothesis} that our model is fitted a data well.
#'
#'
#' Revised 2019 Sept. 8
#' Revised 2019 Dec. 2

#'
#'
#'
#'
#'
#'
#' @inheritParams validation.dataset_srsc
#' @export
#'@examples
#'
#'
#' \donttest{
#'
#'   fit <- fit_Bayesian_FROC( ite  = 1111,  dataList = ddd )
#'  a <- chi_square_at_replicated_data_and_MCMC_samples_MRMC(fit)
#'
#'  b<-a$List_of_dataList
#'  lapply(b, plot_FPF_and_TPF_from_a_dataset)
#'
#'
#'
#'}
#'
#'
chi_square_at_replicated_data_and_MCMC_samples_MRMC  <- function(
  StanS4class,
  summary=TRUE,
  seed = NA,
  serial.number=NA
) {

  fit <- StanS4class
  dataList <- fit@dataList
  NL <- fit@dataList$NL
  NI <- fit@dataList$NI

  e    <- extract(fit)
  MCMC <- dim(e$mu)[1]
  List_of_dataList <- list()
  chi.square <- vector()

  for (mcmc in 1:MCMC) {
    mu <-   e$mu[mcmc,,]
    v <-   e$v[mcmc,,]
    z <-   e$z[mcmc,]

    List_of_dataList[[mcmc]]  <- create_dataList_MRMC(
      z.truth=z, #c(0.1,0.2,0.3,0.4,0.5),
      mu.truth = mu, #array of (M,Q),
      v.truth  = v, #array of (M,Q),
      NI =NI,
      NL=NL,
      ModifiedPoisson =TRUE,#Since NI = NULL, if it is FALSE, then false alarms cannot be created 2019 August 24
      seed =123,
      summary = FALSE
    )

    # harray <- array_of_hit_and_false_alarms_from_vector(dataList =  List_of_dataList[[mcmc]])$harray
    # farray <- array_of_hit_and_false_alarms_from_vector(dataList =  List_of_dataList[[mcmc]])$farray
    ppp <-   e$ppp[mcmc,,,]
    dl <-   e$dl[mcmc,]

    if (summary==TRUE){
      # Here is printed -----
      if(is.na(seed)) message(mcmc,"/",MCMC)
      if(!is.na(seed)) message("seed=",seed,"/",serial.number, ",   ",mcmc,"/",MCMC)

    }


    a<- chi_square_goodness_of_fit_from_input_all_param_MRMC(
      ppp,
      dl,
      dataList,
      summary=summary
    )
    chi.square[mcmc] <- a$chi.square
    # browser()
  }

  invisible(list(
    List_of_dataList =List_of_dataList,
    chi.square =chi.square

  ))


}

