

#' @title Thresholds from its difference
#' @description Thresholds are created from its differece
#'
#'  \deqn{z[1] = w }
#'  \deqn{z[2] = z[1]+ (z[2]-z[1] ) }
#'  \deqn{z[3] = z[1]+ (z[2]-z[1] )+(z[3]-z[2]) }
#'  \deqn{z[4] = z[1]+ (z[2]-z[1] )+(z[3]-z[2])+(z[4]-z[3]) }
#'
#'
#'
#'
#'
#'
#' @param w a real number, indicating the first threshold
#' @param dz a vector of real numbers, indicating the difference of thresholds
#'
#' @return A vector of real numbers
#' @export
#'
#' @examples
#'
#' z_from_dz(1,c(2,3))
#'
#' z_from_dz(1,c(0.2,0.03))
#'
#' z_from_dz(1,c(0.2,0.03,0.004))
#'
#'
#'
#'
#'   dz <-runif(3,    # sample size
#'              0.01, # lower bound
#'              1     # upper bound
#'              )
#'
#'
#'   w  <- rnorm(1,
#'               0,
#'               1
#'               )
#'
#'   z_from_dz(w,dz  )
#'
#'
#'
#'
#'
 z_from_dz  <- function(w,dz){

  z <-vector()
  z[1]<-w
  s<-w
  for (cd in 1:length(dz)) {

       z[cd+1]<-s+dz[cd]
       s <-s+dz[cd]
  }


  return(z)

}



#'@title  False Alarm Creator for both cases of MRMC and srsc
#'@description From threshold, mean and S.D.,
#' data of False Alarm are created.
#'@details From threshold, mean and S.D.
#'of the latent Gaussian noise distribution
#'in the bi-normal assumption,
#' data of False Alarm are created.
#' For the process of this drawing false
#'  alarm samples, its rate are also created.
#'   So, in the return values of the function,
#'    the rates for each confidence level is also attached.
#'
#'@param z.truth  Vector of dimension = C represents the thresholds of bi-normal assumption.
#'@param seed  The seed for creating a collection of the number of false alarms synthesized by the Poisson  distributions using the specified seed.
#'@param NI  The number of images.
#'@param NL  The number of lesions.
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@inheritParams validation.dataset_srsc
#'@inheritParams hits_creator_from_rate
#'
#'
#' @return A list of vectors, indicating a true parameter and a sample.
#'
#' A vector indicating a true parameter: False rate from thresholds.
#'
#' A vector indicating a sample, more precisely,
#' The truth parameter of false alarm rate calculated by true thresholds \code{z} and
#' also, one-time drawn samples of false alarms from the calculated false rates.

#'@examples
#'  \donttest{

#' false.rate <- false_and_its_rate_creator()
#'
#'
#'
#'#----------------------------------------------------------------------------------------
#'#  In SBC, Poisson rate = 0,..so,... i have to investigate.
#'#----------------------------------------------------------------------------------------
#'
#'   set.seed( 1234 )
#'
#'   dz <-runif(3,    # sample size
#'              0.01, # lower bound
#'              1     # upper bound
#'              )
#'
#'
#'   w  <- rnorm(1,
#'               0,
#'               1
#'               )
#'
#'   z <- z_from_dz(w,dz  )
#'
#'
#'  false_and_its_rate_creator(z )
#'
#'
#'

#'#----------------------------------------------------------------------------------------
#'#        Poisson rate  is OK
#'#----------------------------------------------------------------------------------------
#'
#'   set.seed( 1234 )
#'
#'   dz <-runif(3,    # sample size
#'              0.01, # lower bound
#'              1     # upper bound
#'              )
#'
#'
#'   w  <- rnorm(1,
#'               0,
#'               10 # It cause the  poisson rate become small
#'               )
#'
#'   z <- z_from_dz(w,dz  )
#'
#'
#'  false_and_its_rate_creator(z )
#'
#'
 #'

 #'#----------------------------------------------------------------------------------------
 #'#  In SBC, Poisson rate is small
 #'#----------------------------------------------------------------------------------------
 #'
 #'   set.seed( 1234 )
 #'
 #'   dz <-runif(3,    # sample size
 #'              0.01, # lower bound
 #'              1     # upper bound
 #'              )
 #'
 #'
 #'   w  <- rnorm(1,
 #'               0,
 #'               10 # It cause the  poisson rate become small
 #'               )
 #'
 #'   z <- z_from_dz(w,dz  )
 #'
 #'
 #'  false_and_its_rate_creator(z )
 #'
 #'#----------------------------------------------------------------------------------------
 #'#               Poisson rate = 0
 #'#----------------------------------------------------------------------------------------
 #'
 #'   set.seed( 1234 )
 #'
 #'   dz <-runif(3,    # sample size
 #'              0.01, # lower bound
 #'              10 # It cause the  poisson rate become exactly 0   # upper bound
 #'              )
 #'
 #'
 #'   w  <- rnorm(1,
 #'               0,
 #'               1
 #'               )
 #'
 #'   z <- z_from_dz(w,dz  )
 #'
 #'
 #'  false_and_its_rate_creator(z )
 #'#'}
#' @export
#'
#'
#'


false_and_its_rate_creator <- function(
  z.truth=BayesianFROC::z_truth, #c(0.1,0.2,0.3,0.4,0.5),
  NI =333,
  NL=111,
  ModifiedPoisson =FALSE,
  seed =12345
){

  C <-length(z.truth)

  if(ModifiedPoisson==F){ NX <- NI}
  if(ModifiedPoisson==T){ NX <- NL}



  f.inv <- vector()#these should in for sentence
  f <- vector()


  l.truth  <- vector()
  dl.truth <- vector()

  for(cd in 1 : C) {
    l.truth <- -log( stats::pnorm(z.truth))
  }






  for(cd in 1:C){
    if (cd==C) {dl.truth[cd]<-abs(l.truth[cd]-0);
    }else{

      dl.truth[cd]=abs(l.truth[cd]-l.truth[cd+1]);#not fabs for C++
    }
  }





  for (cd in 1:C) {

    if(ModifiedPoisson==F){
      if (cd==C) {
        set.seed(seed =  seed);
        f.inv[cd]  <- stats::rpois(n= 1, lambda = (l.truth[cd]-0)*NI )
      }else{
        set.seed(seed =  seed);
        f.inv[cd]  <- stats::rpois(n= 1, lambda = (l.truth[cd]-l.truth[cd+1])*NI )
      }#else
    }# if  ModifiedPoisson==F




    if(ModifiedPoisson==T){
      if (cd==C) {
        set.seed(seed =  seed);
        f.inv[cd]  <- stats::rpois(n= 1, lambda = (l.truth[cd]-0)*NL )
      }else{
        set.seed(seed =  seed);
        f.inv[cd]  <- stats::rpois(n= 1, lambda = (l.truth[cd]-l.truth[cd+1])*NL)
      }#else      set.seed(seed =  seed); hits <-   stats::rbinom(n=1,size = NL,prob = p.truth[cd])
    }#  if ModifiedPoisson==T

    f[C-cd+1] <- f.inv[cd]

  }#  for cd in 1:C

  return(
    list(
      f =f,
      l.truth  =l.truth,
      dl.truth =dl.truth
    )
  )

}#function









#'@title MRMC: False Alarm Creator For each Modality and each Reader.
#'@description From threshold, mean and S.D., data of False Alarm are created.#'@details
#'In our hierarchical model, false alarm rate does not depend on the readers or modalities. Thus this sampling function merely draws samples from the Poisson distribution of the same false alarm rate. Of course, this same false rate of the Poisson distributions is not desired one. Since we should assume that each reader with different modality should differ. To accomplish this, we have to assume that threshold parameter of Gaussian assumption should depend on the reader and modality. But such model does not converge in the Hamiltonian Monte Carlo simulation.
#'@inheritParams false_and_its_rate_creator
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@inheritParams validation.dataset_srsc
#'@inheritParams hits_creator_from_rate

#' @param M Number of modalities
#' @param Q Number of readers
#'
#' @return Vector for false alarms as an element of list of MRMC data.

#' @export
#'
#' @examples
#' \donttest{
#'
#'
#'         false_and_its_rate_creator_MRMC()
#'
#'
#'}
false_and_its_rate_creator_MRMC <- function(
  z.truth=BayesianFROC::z_truth, #c(0.1,0.2,0.3,0.4,0.5),
  NI =333,
  NL=111,
  ModifiedPoisson =FALSE,
  seed =12345,
  M=5,
  Q=4,
  summary = TRUE
){


  C <-length(z.truth)
  f<-list()
  s <- 0

  for (md in 1:M) {
    for (qd in 1:Q) {
      s <- s+1
      a<- false_and_its_rate_creator(
        z.truth=z.truth, #c(0.1,0.2,0.3,0.4,0.5),
        NI =NI,
        NL=NL,
        ModifiedPoisson =ModifiedPoisson,
        seed =seed+100*md+1000*qd
      )
      f[[s]] <-a$f

    }}#for md qd

  f <- unlist(f)
  f.mat <-matrix(f, nrow = C, ncol = M*Q)
  # print(knitr::kable(as.data.frame(f.mat), format = "pandoc"))
  d <-as.data.frame(f.mat)
  colnames(d) <-NULL
  rownames <- vector()
  for (cd in 1:C) {
    rownames[cd] <- paste("f[",cd,"]",sep = "")
  }
  rownames(d) <- rownames

  if( summary == TRUE){
    message("\n* Resulting False Alarms \n")
    print(d)
  }

  return(f)

}# function









#'@title Transform from an \strong{ \emph{array}} to a \strong{ \emph{vector}}
#'
#'
#'@description
#' Transform a vector into an array
#'
#' @details
#'  In stan files of this package,
#' the number of hits,
#' false alarms
#' and hit rates in binomial assumption for MRMC case
#' are written with \strong{the three indexed array} format.
#'  Three index indicates
#'  confidence levels, modality ID, reader ID.
#'However, hit data passed to
#'the function \code{BayesianFROC::\link{fit_Bayesian_FROC}()}
#'are written with \strong{the vector}.
#'So, in order to connect these different format, (i.e. vector and array, ) the author made this function.

#' @aliases array to vector
#' @param Three.dim.array Three dimensional array, such as the number of hits for each confidence level, modality and reader. Or false alarms. Since the author construct the substituting data list as one dimensional (one index) array, it needs to reconstruct to the three indexed array  from one dimensional array whose subscript is [confidence level, modality, reader ] or vice versa.
#'
#' @return A vector, transformed from three dimensional array.

#' @export
#'
#' @examples
#' \donttest{
#'#----------------------------------------------------------------------------------------
#'#               Practical example
#'#----------------------------------------------------------------------------------------
#'
#'   h.array.etc <- hits_from_thresholds()
#'   h.array.etc$h
#'   h.vector     <- from_array_to_vector(h.array.etc$h)
#'   h.vector
#'
#'
#'#----------------------------------------------------------------------------------------
#'#               Educational example 1
#'#----------------------------------------------------------------------------------------
#'
#'
#'   a <- array_easy_example()
#'   a
#'   a.vector <- from_array_to_vector(a)
#'   a.vector
#'
#'#----------------------------------------------------------------------------------------
#'#               Educational example 2
#'#----------------------------------------------------------------------------------------
#'
#'   a <- array_easy_example(2,3,2)
#'   a
#'   a.vector <- from_array_to_vector(a)
#'   a.vector

#'
#'                            # Revised 2019 August 20
#'                            # Revised 2020 Jan
#'
#'   }
from_array_to_vector <- function(Three.dim.array){

  one.dim.array <-vector()

  C <- dim(Three.dim.array)[1]
  M <- dim(Three.dim.array)[2]
  Q <- dim(Three.dim.array)[3]

  s<-0
  #NOte that this order of for sentence is important 2019 Sept 9 I fixed my mistake
    for (md in 1:M) {
      for (qd in 1:Q) {
        for (cd in 1:C) {

        s<-s+1
        one.dim.array[s] <- Three.dim.array[cd,md,qd]

      }
    }
  }

  return(one.dim.array)

}#function



#
# one.dim.array <- vector()
# s<-0
# for (cd in 1:2) {
#   for (md in 1:3) {
#     for (qd in 1:4) {
#       s<-s+1
#       one.dim.array[s] <- paste("h[" , cd,  ",",  md, ",", qd ,"]", sep="")
#     }
#   }
# }


























#'@title MRMC Dataset Creator From Hit Rate.
#'@description From hit rates, data of hits are created.
#'@param NL  Number of Lesions.
#'@param p.truth Array of dimension (C, M, Q), where
#'  C = number of confidence levels,
#'  M = number of modalities,
#'  Q = number of readers.
#'@param seed  The seed for creating data consisting of the number of hits synthesized by the binomial distributions with the specified seed.
#'@return Hits Data, an \code{array of dimension [Confidence, Modality, Reader]}.

#'
#'
#'@details
#'
#' Random variables of hits are distributed as follows.
#'
# TeX Adjusted hit rate -----
#'      \deqn{h_{5,m,r} \sim Binomial (p_{5,m,r}, N_L ),}
#'
#' then \eqn{h_{4,m,r}} should be drawn from the binomial distribution with remaining targets
#'
#'      \deqn{h_{4,m,r} \sim Binomial (\frac{p_{4,m,r}}{1-p_{5,m,r}}, N_L - h_{5,m,r}).}
#'
#' Similarly,
#'
#'      \deqn{h_{3,m,r} \sim Binomial (\frac{p_{3,m,r}}{1-p_{5,m,r}-p_{4,m,r}}, N_L - h_{5,m,r} -h_{4,m,r}).}
#'
#'      \deqn{h_{2,m,r} \sim Binomial (\frac{p_{2,m,r}}{1-p_{5,m,r}-p_{4,m,r}-p_{3,m,r}}, N_L - h_{5,m,r} -h_{4,m,r}-h_{3,m,r}).}
#'
#'      \deqn{h_{1,m,r} \sim Binomial (\frac{p_{1,m,r}}{1-p_{5,m,r}-p_{4,m,r}-p_{3,m,r}-p_{2,m,r}}, N_L - h_{5,m,r} -h_{4,m,r}-h_{3,m,r}-h_{2,m,r}).}
#'
#'
#'
#'
#'
#'     \code{p.truth}  is an array representing \eqn{p_{c,m,r}}.
#'     By specifying the array \code{p.truth} ( and hence \eqn{p_{c,m,r}} ),
#'     with the above model,
#'     we can calculate hit data \eqn{h_{c,m,r}} for each \eqn{c,m,r}.
#'
#'
#'


#'@examples
#' \donttest{
#'#----------------------------------------------------------------------------------------
#'#2019 Sept 6 1)    Using the default hit values, hit data are created as follows;
#'#----------------------------------------------------------------------------------------
#'
#'
#'            hits <- hits_creator_from_rate()
#'
#'
#'
#'
#'
#'
#'
#'
#'#----------------------------------------------------------------------------------------
#'#2019 Sept 6 2)   If user want to use their own hit rates, then use the following codes:
#'#----------------------------------------------------------------------------------------
#'
#'
#'
#'  h <- hits_creator_from_rate(
#'
#'   NL=252,
#'   seed =123,
#'   p.truth =
#'    array(c(
#'      c(0.03,0.13,0.2,0.3,0.4,   #for M=1 Q=1
#'        0.04,0.23,0.3,0.4,0.5) , #for M=2 Q=1 ,
#'
#'      c(0.05,0.33,0.4,0.5,0.6,   #for M=1 Q=2
#'        0.06,0.43,0.5,0.6,0.7)  ,#for M=2 Q=2 ,
#'
#'      c(0.07,0.53,0.6,0.7,0.8,   #for M=1 Q=3
#'        0.08,0.63,0.7,0.8,0.9)   #for M=2 Q=3 ,
#'        ),
#'
#'   dim = c(5,2,3) #C M Q
#'   )#array
#'
#' )
#'
#'
#'
#'
#'
#'
#'#----------------------------------------------------------------------------------------
#'#2019 Sept 6 3)   If user want to use their own hit rates, then use the following codes:
#'#----------------------------------------------------------------------------------------
#'
#'
#'
#'
#'
#'
#'
#'  h <- hits_creator_from_rate(
#'
#'   NL=252,
#'   seed =123,
#'   p.truth =
#'    array(c(
#'
#'      c(0.03,0.1,0.2,0.3,0.4,   #for M=1 Q=1
#'        0.04,0.2,0.3,0.4,0.5,   #for M=2 Q=1
#'        0.05,0.3,0.4,0.5,0.6),  #for M=3 Q=1
#'
#'      c(0.05,0.33,0.4,0.5,0.6,   #for M=1 Q=2
#'        0.06,0.43,0.5,0.6,0.7,   #for M=2 Q=2
#'        0.05,0.3,0.4,0.5,0.6),   #for M=3 Q=2
#'
#'      c(0.07,0.53,0.6,0.7,0.8,   #for M=1 Q=3
#'        0.08,0.63,0.7,0.8,0.9,   #for M=2 Q=3
#'        0.05,0.3,0.4,0.5,0.6)    #for M=3 Q=3
#'
#'        ),
#'
#'   dim = c(5,3,3) #C M Q
#'
#'   )#array
#'
#' )
#'
#'
#'
#'
#'
#'
#'#----------------------------------------------------------------------------------------
#'#2019 Sept 6 3)   Only one reader
#'#----------------------------------------------------------------------------------------
#'
#'
#'
#'  h <- hits_creator_from_rate(
#'
#'   NL=252,
#'   seed =123,
#'   p.truth =
#'    array(c(
#'
#'      c(0.03,0.1,0.2,0.3,0.4,   #for M=1 Q=1
#'        0.04,0.2,0.3,0.4,0.5,   #for M=2 Q=1
#'        0.05,0.3,0.4,0.5,0.6)   #for M=3 Q=1
#'
#'        ),
#'
#'   dim = c(5,3,1) #C M Q
#'
#'   )#array
#'
#' )
#'
#'
#'
#'
#'#----------------------------------------------------------------------------------------
#'#
#'#----------------------------------------------------------------------------------------
#'
#'#================The third example======================================
#'
#'#     The hits rate cannot take any values, since there is a trend that a hit rate of
#'#   a higher confidence level is a higher. So, If it is difficult for user to create
#'#   a true hit rates, then  by taking estimates as true parameters,
#'#   user can replicate datasets.
#'#     To do so, work follow is first fitting, secondly extracting estimates,
#'#   thirdly apply this function (hits_creator_from_rate() ).
#'
#'
#'# * Fitting
#'
#'      fit <- fit_Bayesian_FROC(
#'              dataList.Chakra.Web.orderd,
#'              ite = 1111,  #  For simplicity, we take small MCMC samples.
#'              summary =FALSE)
#'
#'# * Extracting
#'
#'         estimates <- extract_estimates_MRMC(fit)
#'
#'          ppp <- estimates$ppp.EAP
#'
#'#      Note that ppp is an array
#'#     whose dimension is constituted by number of confidence levels, modalities, readers.
#'
#'
#'# *  Replicating as an true values is ppp
#'
#'
#'         hits  <-   hits_creator_from_rate(p.truth = ppp )
#'
#'
#'# <<Remark>>
#'#     ppp is an array.  ignoring its indices, we can write that
#'
#'#          hits ~ Binomial(ppp, NL)
#'
#'#    Where NL is a number of lesions.
#'
#'#   By writing its component explicitly, we can write
#'
#'#        Hits[c,m,r] ~ Binomial(ppp[c,m,r], NL)
#'
#'#        Where c means the c-th confidence level,
#'#              m  means the m-th modality,
#'#              r means the r-th reader.
#'}# dottest

#' @export hits_creator_from_rate
#'
#'
#'
#'


hits_creator_from_rate <- function(
  # C= dim(p.truth)[1],
  # M= dim(p.truth)[2],
  # Q= dim(p.truth)[3],
  NL=252,
  seed =123,
  p.truth = BayesianFROC::p_truth
  #  array(c(
  #    c(0.03,0.13,0.2,0.3,0.4,   #for M=1 Q=1
  #      0.04,0.23,0.3,0.4,0.5) , #for M=2 Q=1 ,
  #
  #    c(0.05,0.33,0.4,0.5,0.6,   #for M=1 Q=2
  #      0.06,0.43,0.5,0.6,0.7)  ,#for M=2 Q=2 ,
  #
  #    c(0.07,0.53,0.6,0.7,0.8,   #for M=1 Q=3
  #      0.08,0.63,0.7,0.8,0.9)   #for M=2 Q=3 ,
  #      ),
  #
  # dim = c(5,2,3) #C M Q
  # )#array

){#function

  # if ( missing(p.truth)==TRUE) {
  #   ppp_truth_example <- BayesianFROC::ppp_truth_example
  #   p.truth <- ppp_truth_example
  # }
  if(missing(p.truth)==TRUE){message("\n* We use the default hit rates (= BayesianFROC::ppp_truth_example ) .\n\n\n")}

  C <- dim(p.truth)[1]
  M <- dim(p.truth)[2]
  Q <- dim(p.truth)[3]

  hits <- array(NA,dim = c(C,M,Q))


# hit rate are adjusted ---------- 2019 Oct 8

  deno <- array(NA,dim = c(C,M,Q))
  hit_rate.truth<- array(NA,dim = c(C,M,Q))
c<-C:1
  for(md in 1 : M) {
    for(qd in 1 : Q) {
      deno[C-1,md,qd]=1-p.truth[C,md,qd];
      for(cd in 3:C){  deno[c[cd],md,qd]=deno[c[cd-1],md,qd]-p.truth[c[cd-1],md,qd];  }
    }}

# hit rate are well adjusted in the past, I confirmed again, 2019 Dec 17

  for(md in 1 : M) {
    for(qd in 1 : Q) {
      for(cd in 1:C-1){
        hit_rate.truth[cd,md,qd]=p.truth[cd,md,qd]/deno[cd,md,qd];
      }
        hit_rate.truth[C,md,qd]=p.truth[C,md,qd];

    }}









  # ?possible to Vectorization
  # set.seed(seed =  seed); hits <-   stats::rbinom(n=1,size = NL,prob = p.truth)
    for (md in 1:M) {
      for (qd in 1:Q) {
        s <-0# 2019 Sept 9. This is a key idea
        for (cd in 1:C) {
        if(!cd==1)  s <- s + hits[cd-1,md,qd]# 2019 Sept 9. This is a key idea

        set.seed(seed =  seed);
 hits[cd,md,qd] <-   stats::rbinom(
                                   n = 1,
                                size = NL-s, # 2019 Sept 9. This is a key idea    # In order to avoid the sum of hits may be greater than NL
                                prob = hit_rate.truth[cd,md,qd] )# 2019 Oct 8 here modified hit rate

         }
    }
  }





  return(hits)
}#function





#' @title mu of MRMC model paramter
#'
#' @param M An integer, indicating a number of modalities
#' @param Q An integer, indicating a number of readers
#'
#' @return An array, representing a mu of MRMC model paramter
#' @export
#'
#' @examples
#'
#'
#'     m <- mu_truth_creator_for_many_readers_MRMC_data(M=4,Q=50)
#'
#'
#'
#' \donttest{

#'
#'#----------------------------------------------------------------------------------------
#'#                  Large number of readers cause non-convergence
#'#----------------------------------------------------------------------------------------
#'
#'
#'   v <- v_truth_creator_for_many_readers_MRMC_data(M=4,Q=6)
#' m <- mu_truth_creator_for_many_readers_MRMC_data(M=4,Q=6)
#' d <-create_dataList_MRMC(mu.truth = m,v.truth = v)
#' fit <- fit_Bayesian_FROC( ite  = 1111,  cha = 1, summary = T, dataList = d )
#'
#' plot_FPF_and_TPF_from_a_dataset(fit@dataList)
#'
#'
#'
#'
#'#----------------------------------------------------------------------------------------
#'#                             convergence
#'#----------------------------------------------------------------------------------------
#'
#'
#'
#'
#'  v  <- v_truth_creator_for_many_readers_MRMC_data(M=2,Q=21)
#'  m  <- mu_truth_creator_for_many_readers_MRMC_data(M=2,Q=21)
#'  d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#' fit <- fit_Bayesian_FROC( ite  = 200,  cha = 1, summary = T, dataList = d)
#'
#'
#'#----------------------------------------------------------------------------------------
#'#                            non-convergence
#'#----------------------------------------------------------------------------------------
#'
#'
#'
#' v  <- v_truth_creator_for_many_readers_MRMC_data(M=5,Q=6)
#'  m  <- mu_truth_creator_for_many_readers_MRMC_data(M=5,Q=6)
#'  d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#' fit <- fit_Bayesian_FROC( ite  = 200,  cha = 1, summary = T, dataList = d)
#'
#'
#'
#'#----------------------------------------------------------------------------------------
#'#                           convergence
#'#----------------------------------------------------------------------------------------
#'
#'
#' v  <- v_truth_creator_for_many_readers_MRMC_data(M=1,Q=36)
#' m  <- mu_truth_creator_for_many_readers_MRMC_data(M=1,Q=36)
#' d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#' fit <- fit_Bayesian_FROC( ite  = 2000,  cha = 1, summary = T, dataList = d)
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
#'#----------------------------------------------------------------------------------------
#'#                            non-convergence
#'#----------------------------------------------------------------------------------------
#'
#'
#' v  <- v_truth_creator_for_many_readers_MRMC_data(M=1,Q=37)
#' m  <- mu_truth_creator_for_many_readers_MRMC_data(M=1,Q=37)
#' d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#' fit <- fit_Bayesian_FROC( ite  = 2000,  cha = 1, summary = T, dataList = d)
#'
#'
#'
#'
#'
#'
#'
#'}
#'
#'
#'
#'
mu_truth_creator_for_many_readers_MRMC_data <- function( M,Q){

  rep <- stats::runif(M*Q, 0.6, 1.8)

  mu <- array(rep,dim = c(M,Q))

  return(mu)

}









#' @title v of MRMC model paramter
#'
#' @param M An integer, indicating a number of modalities
#' @param Q An integer, indicating a number of readers
#'
#' @return An array, representing v of MRMC model paramter
#' @export
#'
#' @examples
#'
#'  v <- v_truth_creator_for_many_readers_MRMC_data(M=4,Q=50)
#'
#'
v_truth_creator_for_many_readers_MRMC_data <- function( M,Q){

  rep <- stats::runif(M*Q, 1.05, 3.22)

  v <- array(rep,dim = c(M,Q))

  return(v)
}











#'@title MRMC Hit Rates Creator from Thresholds, Mean and S.D.
#'@description From thresholds, data of hit rate are created.
#'
#' Note that the return values has changed from \eqn{p} (in \R notation:\code{ppp})
#' to
#'
#' \deqn{hit rate_c :=  \frac{p_c(\theta)}{1- p_C(\theta)-p_{C-1}(\theta)-  ... - p_{c+1}(\theta)}  }
#'
#'
#'
#'@param z.truth  Vector of dimension = C represents the thresholds of bi-normal assumption.
#'@param mu.truth array of dimension (M,Q). Mean of the signal distribution of bi-normal assumption.
#'@param v.truth  array of dimension (M,Q). Standard Deviation of represents the signal distribution of bi-normal assumption.
#'@param is_hit_rate_adjusted whether the return value is
#'a vector of \deqn{p_c(\theta)} or  \deqn{hit rate_c :=  \frac{p_c(\theta)}{1- p_C(\theta)-p_{C-1}(\theta)-  ... - p_{c+1}(\theta)}  }.
#'
#'The former is the default (FALSE) and the later is returned if \code{is_hit_rate_adjusted=TRUE}.

#'@return  A vector of the hit rate:
#'
#' \deqn{hit rate_c :=  \frac{p_c(\theta)}{1- p_C(\theta)-p_{C-1}(\theta)-  ... - p_{c+1}(\theta)}  }
#'
#'
#'
#'
#'
#'
#'
#' Do not confuse the old version \code{ppp} which is an array with three indices: \code{ppp[C,M,Q]}.

#'@examples
#'
#' \donttest{

#'#================The first example======================================
#'
#'#     Using default values for hit rates, we can create a data of hits as follows:
#'
#'         hits.rate <-hits_rate_creator()
#'
#'#================The second example======================================
#'
#'#     Using the hit rate from the hits_rate_creator(), we can get the hits data:
#'
#'         hits_creator_from_rate(p.truth =hits_rate_creator() )
#'
#'#================The remark for example======================================
#'
#'# The author does not show how to specify the hit rates or threshods.
#'# For the details of it, please see the default values of such a quantities.
#'
#'
#'#================The 4-th example======================================
#'
#'     p.truth.array <- hits_rate_creator()
#'
#'
#'
#'#----------------------------------------------------------------------------------------
#'#2019 Sept 6
#'#----------------------------------------------------------------------------------------
#'
#'
#'
#'
#'
#'
#'
#'}# dottest
#'
#' @export hits_rate_creator
#'
#'
#'
#'

hits_rate_creator <- function(
  z.truth  = BayesianFROC::z_truth,   #c(0.1,0.2,0.3,0.4,0.5),
  mu.truth = BayesianFROC::mu_truth,  #array(1:6/10,c(M,Q)),
  v.truth  = BayesianFROC::v_truth  ,  #array(1:6/10,c(M,Q)),

  is_hit_rate_adjusted = FALSE
){
  C <- length(z.truth)
  M <- dim(mu.truth)[1]
  Q <- dim(mu.truth)[2]

  #In case of a single reader and multple modaity case, an array of type, e.g., [3,1] is reduced to a vector of dimension 3 and it caused some error. So, the author fixed it.
  if (is.vector(mu.truth))  { #2020 Feb 24
    M <- length(mu.truth)  #2020 Feb 24
    Q <- 1               #2020 Feb 24
    mu.truth2<- array(NA,dim = c(M,Q))#2020 Feb 24
    v.truth2<- array(NA,dim = c(M,Q))#2020 Feb 24

    mu.truth2[,1]<-mu.truth#2020 Feb 24
    v.truth2[,1]<-v.truth#2020 Feb 24

    mu.truth <-mu.truth2#2020 Feb 24
    v.truth <-v.truth2#2020 Feb 24

  }                     #2020 Feb 24




  ppp <- array(NA,dim = c(C,M,Q))
   for(md in 1 : M) {
    for(qd in 1 : Q) {
      ppp[C,md,qd] <- 1- stats::pnorm((z.truth[C ] - mu.truth[md,qd])/v.truth[md,qd]);
      for(cd in 1 : C-1) {
          ppp[cd,md,qd] <- stats::pnorm((z.truth[cd+1] - mu.truth[md,qd])/v.truth[md,qd]) - stats::pnorm((z.truth[cd  ] - mu.truth[md,qd])/v.truth[md,qd]);
      }#cd

    }}# for md qd



# hit rate  ---------- 2019 Oct 8

  deno <- array(NA,dim = c(C,M,Q))
  hit_rate<- array(NA,dim = c(C,M,Q))
  ccc <- C:1
  for(md in 1 : M) {
    for(qd in 1 : Q) {
      deno[C,md,qd]=1 ;
      for(cd in (C-1):1){  deno[cd,md,qd]=deno[cd+1,md,qd]-ppp[cd+1,md,qd];  }
    }}


  for(md in 1 : M) {
    for(qd in 1 : Q) {
      for(cd in 1:C){
        hit_rate[cd,md,qd]=ppp[cd,md,qd]/deno[cd,md,qd];
      }
    }}


# browser()

 if(is_hit_rate_adjusted) return(hit_rate)
  if(!is_hit_rate_adjusted) return(ppp)

}#function























#' @title Check hit rate is defined correctly
#' @description
#'
#' Each hit rate is defined by dividing the area under
#' the probability density function into \code{C+1} regions.
#' Thus, the sum of hit rates over all confidence level
#' must be less than 1. This inequality is checked.
#'
#'
#'
#' This function checks the sum of
#'  all hit rate over all conf. levels is less than 1 in case of MRMC
#'
#' This code confiem the following inequality:
#'
#' \eqn{\Sigma_{cd}}\code{ppp[cd,md,qd]} < 1
#'
#' for each
#'  \code{cd,md} (  \code{cd} =1,2,...,\code{C},  \code{md} =1,2,...,\code{M} ).
#'
#'   The return value is an array consisting of logical \R objects
#'  indicating whether the above inequality is \code{TRUE} or \code{FALSE}.
#'
#'
#' 2020 Jam
#'
#'
#' @param StanS4class.or.An.array.of.ppp A stanfitExtended object or an array of component of hit rate namely \code{ppp}
#'
#' @return A  array with logical components. Its dimension costructed by number of readers and modalities.
#' @export
#'
#' @examples
#'#========================================================================================
#'#                               array: ppp
#'#========================================================================================
#'
#'               p.truth.array <- hits_rate_creator()
#'
#'
#'               Confirm_hit_rates_are_correctly_made_in_case_of_MRMC(p.truth.array)
#'
#' \donttest{
#'#========================================================================================
#'#                              fitted model object
#'#========================================================================================
#'
#'               f <- fit_Bayesian_FROC(dd)
#'
#'               Confirm_hit_rates_are_correctly_made_in_case_of_MRMC(f)
#'
#'}
Confirm_hit_rates_are_correctly_made_in_case_of_MRMC  <- function(StanS4class.or.An.array.of.ppp) {

  if (isS4(StanS4class.or.An.array.of.ppp)) {
    f <- StanS4class.or.An.array.of.ppp
    ppp <-extract_EAP_by_array(f,"ppp")
    C <- f@dataList$C
    M <-  f@dataList$M
    Q <-  f@dataList$Q
  }

  if (!isS4(StanS4class.or.An.array.of.ppp)) {
    ppp <-StanS4class.or.An.array.of.ppp
    C <- dim(ppp)[1]
    M <- dim(ppp)[2]
    Q <- dim(ppp)[3]
  }



  s<- array(0,dim = c(M,Q))

  for(md in 1 : M) {
    for(qd in 1 : Q) {
      for(cd in 1:C){
        s[md,qd] <-s[md,qd] +   ppp[cd,md,qd] ;
      }
    }}

  s<=1


}















# here -------
#
# mu <- mu_truth
# v <- v_truth
#' Title
#' @title stan code
#' @param z thresholds
#' @param mu mean
#' @param v standard deviation
#' @param T.or.F logical, if true hten a logical is return
#' hit rate <1  and if false hit rate is returned.

# @return
#' @export
#'
#' @examples
#'
#' Stan_code_validation(z=c(4.7,5,6),mu+555,v/1000000000)
#'
#'
#' Stan_code_validation(z=c(4.7,5,6),mu+5,v/10,T.or.F = FALSE)
#'
#' #ppp[1,3,4]/denoo[1,3,4]
#'





Stan_code_validation <-function(z=BayesianFROC::z,
                                mu=BayesianFROC::mu,
                                v=BayesianFROC::v,
                                T.or.F=T){
# C<-5
# z<-c(4.7,5,6);mu <-mu+555; v<-v/1000000000
  z<-c(4.7,5,6);mu<-BayesianFROC::mu+5;v<-BayesianFROC::v/10
C<-length(z)

M <- 3
Q <- 4
ppp <- array(NA,dim = c(C,M,Q))
hit_rate   <- array(NA,dim = c(C,M,Q))

deno <- array(NA,dim = c(C-1,M,Q))
# dz <- array(NA,C-1)
# z <- array(NA,C)
c<-C:1



for(md in 1 : M) {
  for(qd in 1 : Q) {
    ppp[C,md,qd] = 1- Phi((z[C] -mu[md,qd])/v[md,qd]);
    for(cd in 1 : C-1) ppp[cd,md,qd] = Phi((z[cd+1] -mu[md,qd])/v[md,qd])  - Phi((z[cd ] -mu[md,qd])/v[md,qd]);
  }
}
for(md in 1 : M) {
  for(qd in 1 : Q) {
    # // deno[C]=1;
    deno[C-1,md,qd]=1-ppp[C,md,qd];
    for(cd in 3:C){  deno[c[cd],md,qd]=deno[c[cd-1],md,qd]-ppp[c[cd-1],md,qd];  }
    # // for(cd in (C-2):1){  deno[cd,md,qd]=deno[cd+1,md,qd]-ppp[cd+1,md,qd];  }
  }}


for(md in 1 : M) {
  for(qd in 1 : Q) {
    for(cd in 1:C-1){
      hit_rate[cd,md,qd]=ppp[cd,md,qd]/deno[cd,md,qd];
    }
    hit_rate[C,md,qd]=ppp[C,md,qd];
  }}
denoo <- array(NA,dim = c(C,M,Q))

for(md in 1 : M) {
  for(qd in 1 : Q) {
    for(cd in 1:C-1){
      denoo[cd,md,qd]=deno[cd,md,qd];
    }
    denoo[C,md,qd]=1;
  }}

if(T.or.F==F)  return(hit_rate )
if(T.or.F==T)  return(hit_rate <1)


hit_rate[1,3,4]==ppp[1,3,4]/(1-ppp[2,3,4]-ppp[3,3,4])
ppp[1,3,4]+ppp[2,3,4]+ppp[3,3,4]
ppp[2,3,4]+ppp[3,3,4]

1-ppp[2,3,4]-ppp[3,3,4]
ppp[1,3,4]


deno[4,1,1] + ppp[5,1,1]
deno[3,1,1] + ppp[5,1,1] + ppp[4,1,1]
deno[2,1,1] + ppp[5,1,1] + ppp[4,1,1]  + ppp[3,1,1]
deno[1,1,1] + ppp[5,1,1] + ppp[4,1,1]  + ppp[3,1,1] + ppp[2,1,1]

deno[4,2,3] + ppp[5,2,3]
deno[3,2,3] + ppp[5,2,3]++ ppp[4,2,3]
deno[2,2,3] + ppp[5,2,3] + ppp[4,2,3]  + ppp[3,2,3]
deno[1,2,3] + ppp[5,2,3] + ppp[4,2,3]  + ppp[3,2,3] + ppp[2,2,3]


deno[2,1,1] + ppp[3,1,1]
deno[1,1,1] + ppp[3,1,1] + ppp[2,1,1]

ppp[1,3,4]/denoo[1,3,4]


denoo[3,3,4]
denoo[2,3,4] + ppp[3,3,4]
denoo[1,3,4] + ppp[3,3,4]+ ppp[2,3,4]

# deno[2,3,4] + ppp[5,3,4] + ppp[4,3,4]  + ppp[3,3,4]
# deno[1,3,4] + ppp[5,3,4] + ppp[4,3,4]  + ppp[3,3,4] + ppp[2,3,4]




}#func


#'@title MRMC Hit  Creator from thresholds, mean and S.D.
#'@description From threshold, mean and S.D.,
#'  data of hit rate are created.
#'@param z.truth  Vector of dimension = C represents
#' the thresholds of bi-normal assumption.
#'@param mu.truth array of dimension (M,Q).
#'Mean of the signal distribution
#'of bi-normal assumption.
#'@param v.truth  array of dimension (M,Q).
#' Standard Deviation of represents the signal
#'  distribution of bi-normal assumption.



#'@return Hits Data for MRMC. The reason that hits
#'is multiple reader and multiple modalities arise
#' from the multiple indices of
#'mean and S.D. of signal distribution of the
#' bi-normal assumption.

#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@inheritParams validation.dataset_srsc
#'@inheritParams hits_creator_from_rate
#'@inheritParams hits_rate_creator
#'@examples
#' \donttest{

#' hits.rate.p <-hits_from_thresholds()
#'
#'
#'}#donttest

#' @export hits_from_thresholds
#'
#'
#'
#'

hits_from_thresholds <-function(
  z.truth=BayesianFROC::z_truth, #c(0.1,0.2,0.3,0.4,0.5),
  mu.truth = BayesianFROC::mu_truth, #array(1:6/10,c(M,Q)),
  v.truth  = BayesianFROC::v_truth, #array(1:6/10,c(M,Q)),
  NL=252,############################################################
  seed =123
){
  M <- dim(mu.truth)[1]
  Q <- dim(mu.truth)[2]



  #In case of a single reader and multple modaity case, an array of type, e.g., [3,1] is reduced to a vector of dimension 3 and it caused some error. So, the author fixed it.
  if (is.vector(mu.truth))  { #2020 Feb 24
    M <- length(mu.truth)  #2020 Feb 24
    Q <- 1               #2020 Feb 24
  }                     #2020 Feb 24

  p.truth <- hits_rate_creator(
    z.truth=z.truth,
    mu.truth = mu.truth,
    v.truth  = v.truth
  )





  h <- hits_creator_from_rate(
    p.truth =p.truth  ,
    NL=NL,#############################################
    seed =seed
  )

  return(
    list(
      h=h,

      p.truth=p.truth,
      z.truth=z.truth,
      M = M,
      Q = Q,
      mu.truth = mu.truth,
      v.truth  = v.truth
    )
  )

}#function
















#'@title
#' Creates a \emph{Single} Dataset in  Case of MRMC

#'@description From a given model parameter,
#'creates a FROC dataset
#'in case of multiple readers and
#' multiple \emph{\strong{m}}odality,
#'  breafly MRM\emph{\strong{C}}.
#'The dataset consists of
#'the number of hits and false alarms
#' and ID vectors of readers, modalites,
#' confidences, etc.
#'
#'
#'The created dataset is a list
#' (which can be passed to
#'  \code{\link{fit_Bayesian_FROC}()}).
#'   Model parameters are
#'    thresholds,
#'   mean and standard deviation of signal Gaussian.
#'@param z.truth  Vector ( of  dimension C) represents the thresholds.
#'@param seed  The seed for creating hits which are synthesized by the binomial distributions with the specified seed.
#'@param NI  The number of images,
#'@param NL  The number of lesions,
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@inheritParams validation.dataset_srsc
#'@inheritParams hits_creator_from_rate
#'@inheritParams false_and_its_rate_creator
#'@inheritParams hits_from_thresholds
#'@seealso \code{\link{chi_square_at_replicated_data_and_MCMC_samples_MRMC}()}
#'\code{\link{replicate_MRMC_dataList}()}
#' (To make many MRMC datasets,
#'  see \code{\link{replicate_MRMC_dataList}()})
#'
#'@details
#' Specifying model parameters, we can replicates fake datasets.
#' Different \code{seed} gives different fake data.
#' Model parameters are the following.
#'
#'      \code{z.truth}
#'
#'      \code{mu.truth}
#'
#'      \code{v.truth}.
#'
#'
# TeX Adjusted hit rate -----
#' \strong{Probablity law of hits}
#'  Random variables of hits are distributed as follows.

#'                  \deqn{H_{5,m,r} \sim Binomial (p_{5,m,r}, N_L ),}
#'
#' then \eqn{H_{4,m,r}} should be drawn from the binomial distribution with remaining targets
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
#'
#'
#'      \deqn{F_{5,m,r} \sim Poisson(q_{5,m,r} N_X ),}
#'
#' then \eqn{F_{4,m,r}} should be drawn from the binomial distribution with remaining targets
#'
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
#'where \eqn{N_X} is the following two cases.
#'
#'
#'
#' 1)  \eqn{N_X} = \eqn{N_L} (The number of lesions), if \code{  ModifiedPoisson = TRUE}.
#'
#' 2)  \eqn{N_X} = \eqn{N_I} (The number of images),  if \code{  ModifiedPoisson = FALSE}.
#'
#'
#'
#'
# here ----
#'     The rate \eqn{p_{c,m,r}} and \eqn{q_{c,m,r}} are calculated from the model parameters.
#'
#'      \code{z.truth}
#'
#'      \code{mu.truth}
#'
#'      \code{v.truth}.
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

#' @export

#'@examples
#' \donttest{


#'      dataList  <- create_dataList_MRMC()
#'
#'
#'      fit_Bayesian_FROC(dataList,summary = FALSE)
#'
#'
#' #  In the above example, we use a default values for true parameters for
#' #  the distributions. The reason why the default values exists is difficulty
#' #  for the user who is not familiar with FROC data nor  konws the resions
#' #  in which parameters of FROC model move.
#' #   So, in the Bayesian model is merely model for FROC data.
#' #   If user input the abnormal data, then the model does not fit nor converge
#' #   in the Hamiltonian Monte Carlo simulations.
#'
#'
#'     plot_FPF_and_TPF_from_a_dataset(create_dataList_MRMC() )
#'
#'
#'
#'#----------------------------------------------------------------------------------------
#'#     plot various MRMC datasets with fixed signal distribution but change thresholds
#'#----------------------------------------------------------------------------------------
#'
#'
#'
#'
#' plot_FPF_and_TPF_from_a_dataset(create_dataList_MRMC( z.truth = c(0.1,
#'                                                                   0.2,
#'                                                                   0.3,
#'                                                                   0.4)
#' ))
#'
#' plot_FPF_and_TPF_from_a_dataset(create_dataList_MRMC( z.truth = c(-0.1,
#'                                                                   0.2,
#'                                                                   0.3,
#'                                                                   0.4)
#' ))
#'
#'
#' plot_FPF_and_TPF_from_a_dataset(create_dataList_MRMC( z.truth = c(-1,
#'                                                                   0.2,
#'                                                                   0.3,
#'                                                                   0.4)
#' ))
#'
#'
#' plot_FPF_and_TPF_from_a_dataset(create_dataList_MRMC( z.truth = c(-1,
#'                                                                   -0.2,
#'                                                                   -0.3,
#'                                                                   0.4)
#' ))
#'
#'
#' plot_FPF_and_TPF_from_a_dataset(create_dataList_MRMC( z.truth = c(-1,
#'                                                                   0.2,
#'                                                                   0.3 )
#' ))
#'
#'
#' plot_FPF_and_TPF_from_a_dataset(create_dataList_MRMC( z.truth = c(-1,
#'                                                                   1.2,
#'                                                                   2.3 )
#' ))
#'
#'
#' plot_FPF_and_TPF_from_a_dataset(create_dataList_MRMC( z.truth = c(-1,
#'                                                                   -0.5,
#'                                                                   0,
#'                                                                   1.2,
#'                                                                   2.3,
#'                                                                   3.3,
#'                                                                   4)
#' ))
#'
#'
#'
#' plot_FPF_and_TPF_from_a_dataset(create_dataList_MRMC( z.truth = c(-1,
#'                                                                   -0.5,
#'                                                                   0,
#'                                                                   1.2,
#'                                                                   2.3,
#'                                                                   3.3,
#'                                                                   4,
#'                                                                   5,
#'                                                                   6)
#' ))
#'
#'
#' plot_FPF_and_TPF_from_a_dataset(create_dataList_MRMC( z.truth = c(-1,
#'                                                                   -0.5,
#'                                                                   0,
#'                                                                   1.2,
#'                                                                   2.3,
#'                                                                   3.3,
#'                                                                   4,
#'                                                                   5,
#'                                                                   6,
#'                                                                   7)
#' ))
#'
#'
#'
#' plot_FPF_and_TPF_from_a_dataset(create_dataList_MRMC( z.truth = c(-1,
#'                                                                   -0.5,
#'                                                                   0,
#'                                                                   1.2,
#'                                                                   2.3,
#'                                                                   3.3,
#'                                                                   4,
#'                                                                   5,
#'                                                                   6,
#'                                                                   7,
#'                                                                   8,
#'                                                                   9,
#'                                                                   10)
#' ))

#'
#'
#'}
#'
# From parameter data are created -----
#create_dataList_MRMC ----
create_dataList_MRMC <-function(
  z.truth=BayesianFROC::z_truth, #c(0.1,0.2,0.3,0.4,0.5),
  mu.truth = BayesianFROC::mu_truth, #array(1:6/10,c(M,Q)),
  v.truth  = BayesianFROC::v_truth, #array(1:6/10,c(M,Q)),
  NI = 57,
  NL = 142,
  ModifiedPoisson = FALSE,
  seed = 123,
  summary = FALSE
){
  M <- dim(mu.truth)[1]
  Q <- dim(mu.truth)[2]



  #In case of a single reader and multple modaity case, an array of type, e.g., [3,1] is reduced to a vector of dimension 3 and it caused some error. So, the author fixed it.
  if (is.vector(mu.truth))  { #2020 Feb 24
    M <- length(mu.truth)  #2020 Feb 24
    Q <- 1               #2020 Feb 24
  }                     #2020 Feb 24

# f  -----
  f <- false_and_its_rate_creator_MRMC(
    z.truth=z.truth,
    NI =NI,
    NL=NL,
    ModifiedPoisson =ModifiedPoisson,
    seed =seed, # important for replication
    M=M,
    Q=Q,
    summary = FALSE
  )

  f.l.dl <- false_and_its_rate_creator(
    z.truth=z.truth,
    NI =NI,
    NL=NL,
    ModifiedPoisson =ModifiedPoisson,
    seed =seed # important for replication
  )
  # h  -----

  h.etc <- hits_from_thresholds(
    z.truth=z.truth,
    mu.truth = mu.truth,
    v.truth  = v.truth,
    NL=NL,
    seed =seed # important for replication
  )


  C <-  length(z.truth)

# browser();h.etc$h/NL>1
# browser()
  m  <-  rep(1:M, each=C*Q)
  q  <-  rep(seq(1,Q,1), M, each=C)
  c  <-  rep(rep(C:1), M*Q)
  h  <- from_array_to_vector(h.etc$h)


  if (summary ==TRUE) {

    print(knitr::kable(data.frame(
      m=m,
      q=q,
      c=c,
      f=f,
      h=h
    ), format = "pandoc")
    )
  }#if summary


  truth=list(
    l.truth = f.l.dl$l.truth,
    dl.truth  = f.l.dl$dl.truth,
    p.truth=h.etc$p.truth,
    z.truth=h.etc$z.truth,
    mu.truth = h.etc$mu.truth,
    v.truth  = h.etc$v.truth
  )



  dataList = list(
    m=m,
    q=q,
    c=c,
    f=f,
    h=h,
    NL=NL,
    NI=NI,
    C=C,
    M=M,
    Q=Q
  )
# browser()
  return(
    dataList
  )#return


}#function



#'@title MRMC: Replicates Datasets From Threshold, Mean and S.D.

#'@description Make several datasets from a given  model parameter.
#'@return A list, each component is also a list, representing an FROC dataset.
#'
#'@param initial.seed The variable \code{initial.seed} is used to replicate datasets.
#'That is, if you take initial.seed = 1234, then the seed 1234, 1235, 1236, 1237, 1238, .... etc are for the first replication, the second replication, the third replication, .... etc.
#'If the n-th model does not converge for some n,
#'then such model has no mean and thus the non-convergent models
#'are omitted to calculate the errors.
#'@param replication.number A positive integer, specifying number of replicated datasets by this function.
#'For fixed number of lesions, images,
#' the dataset of hits and false alarms are replicated,
#'  and the number of replicated datasets are specified by this variable.
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@inheritParams validation.dataset_srsc
#'@inheritParams hits_creator_from_rate
#'@inheritParams false_and_its_rate_creator
#'@inheritParams hits_from_thresholds
#' @export
#'
#'
#'
#' @examples
#'#----------------------------------------------------------------------------------------
#'#                Visualization of replicated datasets synthesized by default values
#'#----------------------------------------------------------------------------------------
#'
#'
#'
#'#  Replicates datasets from a model with user specified parameters (now, it is default).
#'          a <-replicate_MRMC_dataList()
#'
#'
#'# Calculates FPF and TPF and plot it for the first replicatec dataset
#'
#'          plot_FPF_and_TPF_from_a_dataset(a[[1]])
#'
#'
#'# Calculates FPF and TPF and plot it for the second replicatec dataset
#'
#'          plot_FPF_and_TPF_from_a_dataset(a[[2]])
#'
#'
#'
#'          # Reviesed 2019 Oct 9
#'
#'
#'

replicate_MRMC_dataList <- function(
  replication.number = 2, # number of datasets which should be created
  initial.seed=123,
  mu.truth = BayesianFROC::mu_truth,
  v.truth =  BayesianFROC::v_truth,
  z.truth  =  BayesianFROC::z_truth,
  NI = 200,
  NL = 142,
  ModifiedPoisson = TRUE,#2019 Sept 9 change the default value
  summary=FALSE
){
  M <- dim(mu.truth)[1]
  Q <- dim(mu.truth)[2]

  a <-initial.seed
  b <- initial.seed + replication.number-1
  list.of.dataList <- list()
  s<-0
  for(seed in a:b){
    s<-s+1
    list.of.dataList[[s]] <-  create_dataList_MRMC(
      z.truth = z.truth,
      mu.truth = mu.truth,
      v.truth  = v.truth,
      NI = NI,
      NL = NL,
      ModifiedPoisson = ModifiedPoisson,
      seed = seed,
      summary=summary
    )

  }# for seed


  return(list.of.dataList)




}#function















#'@title Replicate Models
#'@description Replicate Models For
#' Replicated Data From True Distributions.
#'
#'@return A list, each component is an S4 object of class \code{\link{stanfitExtended}}.
#'
#'   Revised 2019 Nov 7

#'@param initial.seed The variable
#'\code{initial.seed} is used to replicate datasets.
#'That is, if you take initial.seed = 1234,
#'then the seed 1234, 1235, 1236, 1237, 1238,
#'etc are for the first replication,
#' the second replication,
#'the third replication,etc.
#'If the n-th model does not
#' converge for some n,
#'  then such model has
#'  no mean and thus the
#'   non-convergent models
#'   are omitted to calculate the errors.
#'@param replication.number For fixed number of lesions, images, the dataset of hits and false alarms are replicated, and the number of replicated datasets are specified by this variable.
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@inheritParams validation.dataset_srsc
#'@inheritParams hits_creator_from_rate
#'@inheritParams false_and_its_rate_creator
#'@inheritParams hits_from_thresholds
#'@inheritParams replicate_MRMC_dataList
#' @export
#'
#'@examples
#'
#' \donttest{
#'#----------------------------------------------------------------------------------------
#'#             Draw  FROC curves with only one of the replicated model
#'#----------------------------------------------------------------------------------------
#'
#'
#'   list.of.fit  <- replicate_model_MRMC(replication.number = 2)
#'
#'  DrawCurves(list.of.fit[[2]],
#'             modalityID = 1:list.of.fit[[2]]@dataList$M,
#'             readerID = 1:list.of.fit[[2]]@dataList$Q )
#'
#'
#'#  Revised 2019 Sept 9
#'
#'}
replicate_model_MRMC <- function(
  initial.seed=123,
  mu.truth = BayesianFROC::mu_truth,
  v.truth =  BayesianFROC::v_truth,
  z.truth  =  BayesianFROC::z_truth,
  NI = 200,
  NL = 142,
  ModifiedPoisson = FALSE,
  replication.number = 2,
  summary=FALSE,
  ite = 1111

){



  list.of.dataList <- replicate_MRMC_dataList(
    initial.seed=initial.seed,
    mu.truth = mu.truth,
    v.truth =  v.truth ,
    z.truth  =  z.truth,
    NI = NI,
    NL = NL,
    ModifiedPoisson = ModifiedPoisson,
    replication.number = replication.number,
    summary=summary

  )

  list.of.fit <- list()
  s<-0
  message("In the following, the print message from rstan::stan() are restricted:\n")
  for (repl in 1:replication.number) {

    message("\n* The ", repl, "-th fitting.\n", sep ="")


    if(!summary){
    invisible(utils::capture.output(# For hiding the printed message
      fit <- fit_Bayesian_FROC(
        dataList = list.of.dataList[[repl]],
        ite  = ite ,
        summary = FALSE)
    ))
}

    if(summary){
        fit <- fit_Bayesian_FROC(
          dataList = list.of.dataList[[repl]],
          ite  = ite ,
          summary = summary)
    }

# here the non convergent models are omitted ----
    if( ConfirmConvergence(fit)==TRUE){
      s<-s+1
      list.of.fit[[s]] <-fit # list.of.fit contains only converget models
    }# convergence

  }# for repl

  message("\n -------- Comments --------------\n")
  message("\n* The return value (list of fitting models) contains only converget models.\n\n  ")
  message("\n* Number of Replicated Models : ", replication.number, "\n")
  message("\n* Number of Convergent Models : ", s, " \n")

  message("\n* convergence rate :", crayon::cyan( round(100*s/replication.number,3), "% \n\n"))

  size_of_return_value( summary=TRUE,object = list.of.fit)

  invisible(list.of.fit)

}








view_CFP_CTP_for_replicated_models <- function(list.of.fit){

  iii <- length(list.of.fit)
  for (sss in 1:iii) {
    fit <- list.of.fit[[sss]]
    Q <- fit@dataList$Q
    M <- fit@dataList$M

    DrawCurves(fit,
               DrawCFPCTP = T,
               DrawFROCcurve = F,
               DrawAFROCcurve = F,
               modalityID = 1:M,
               readerID = 1:Q,
               new.imaging.device = FALSE
    )
  }

}


























#'@title Extract Estimates From Replicated MRMC Model
#'
#'@inheritParams replicate_model_MRMC
#'
#'@return
#'
#' A list of estimates,
#' posterior means and
#' posterior credible interbals
#' for each model parameter.
#' EAPs and CI interbals.
#'
#'
#' @export
#'
#' @examples
#'
#'
#' \donttest{
#'
#'  list.of.estimates <- extract_parameters_from_replicated_models()
#'
#'
#'  }
#'
#'
#'
#'
extract_parameters_from_replicated_models <- function(
  initial.seed=123,
  mu.truth = BayesianFROC::mu_truth,
  v.truth =  BayesianFROC::v_truth,
  z.truth  =  BayesianFROC::z_truth,
  NI = 200,
  NL = 142,
  ModifiedPoisson = FALSE,
  replication.number = 2,
  summary=FALSE,
  ite = 1111){


  list.of.fit <- replicate_model_MRMC(
    initial.seed=initial.seed,
    mu.truth = mu.truth,
    v.truth =  v.truth,
    z.truth  =  z.truth,
    NI = NI,
    NL = NL,
    ModifiedPoisson = ModifiedPoisson,
    replication.number = replication.number,
    summary=summary,
    ite = ite)

  list.of.estimates <- list()

  for (sss in 1:length(list.of.fit)){
    list.of.estimates[[sss]] <- extract_estimates_MRMC(list.of.fit[[sss]] )
  }


  invisible(list.of.estimates)

}#function



#' @title Comparison of Estimates and Truth in case of MRMC
#' @description
#' In order to describe what this function calculates explicitly,
#'  let us denote
#'   user specifying true model parameter \eqn{\theta_0},
#'    from which fake datasets are replicated and denoted by:
#'
#'      \deqn{D_1,D_2,...,D_k,... D_K.}
#'
#'    We obtains estimates
#'
#'      \deqn{ \theta(D_1),...,\theta(D_K)}
#'
#'      for each replicated dataset.
#' Using these estimates,
#'  we calculates \strong{the mean of errors (= estimates - truth)},
#'  namely,
#'
#'    \deqn{ \frac{1}{K}\sum_{k=1}^K ( \theta(D_k) - \theta_0 ),  }
#'
#'  or  \strong{the variance of estimates}:
#'
#'  \deqn{ \frac{1}{K}\sum_{k=1}^K ( \theta(D_k) - \frac{1}{K}\sum_{k=1}^K \theta(D_k)    )^2.  }
#'
#' Revised  2019 Nov 1
#'
#' Revised  2020 Jan

#'
#'@details 2019 Sept 6 I found this program,
#' I made this in several month ago?
#'  I forgot when this function is made.
#'It well works, so it helps me now.
#'
#'
#'
#'@inheritParams replicate_model_MRMC
#'
#' @return list of errors, or vaiance of estimates over all replicated datasets.
#' @export
#'
error_MRMC   <- function(
  replication.number = 2,

  initial.seed=123,
  mu.truth = BayesianFROC::mu_truth,
  v.truth =  BayesianFROC::v_truth,
  z.truth  =  BayesianFROC::z_truth,
  NI = 200,
  # NL = 142,
  NL = 1142,

  ModifiedPoisson = FALSE,
  summary=FALSE,
  ite = 1111){


  list.of.estimates <- extract_parameters_from_replicated_models(
    initial.seed=initial.seed,
    mu.truth = mu.truth,
    v.truth =  v.truth,
    z.truth  =  z.truth,
    NI = NI,
    NL = NL,
    ModifiedPoisson = ModifiedPoisson,
    replication.number = replication.number,
    summary=summary,
    ite = ite
  )


  w.EAP <- list()
  dz.EAP <- list()
  z.EAP <- list()
  mu.EAP <- list()
  v.EAP <- list()
  ppp.EAP  <- list()
  l.EAP <- list()
  dl.EAP <- list()
  A.EAP <- list()
  AA.EAP <- list()
  aa.EAP <- list()
  bb.EAP <- list()


  for (sss in 1:length(list.of.estimates)) {

    w.EAP[[sss]] <- list.of.estimates[[sss]]$w.EAP
    dz.EAP[[sss]] <- list.of.estimates[[sss]]$dz.EAP
    z.EAP[[sss]] <- list.of.estimates[[sss]]$z.EAP
    mu.EAP[[sss]] <- list.of.estimates[[sss]]$mu.EAP
    v.EAP[[sss]] <- list.of.estimates[[sss]]$v.EAP
    ppp.EAP[[sss]] <- list.of.estimates[[sss]]$ppp.EAP
    l.EAP[[sss]] <- list.of.estimates[[sss]]$l.EAP
    dl.EAP[[sss]] <- list.of.estimates[[sss]]$dl.EAP
    A.EAP[[sss]] <- list.of.estimates[[sss]]$A.EAP
    AA.EAP[[sss]] <- list.of.estimates[[sss]]$AA.EAP
    aa.EAP[[sss]] <- list.of.estimates[[sss]]$aa.EAP
    bb.EAP[[sss]] <- list.of.estimates[[sss]]$bb.EAP

  }

  # Tp summarize over the list, i.e., over sss =1,2,3.... we use the function Reduce form package base.


  w.EAP.sum.over.list   <-  Reduce("+",  w.EAP )
  dz.EAP.sum.over.list  <-  Reduce("+",  dz.EAP )
  z.EAP.sum.over.list   <-  Reduce("+",  z.EAP )
  mu.EAP.sum.over.list  <-  Reduce("+",  mu.EAP )
  v.EAP.sum.over.list   <-  Reduce("+",  v.EAP )
  ppp.EAP.sum.over.list <-  Reduce("+",  ppp.EAP )
  l.EAP.sum.over.list   <-  Reduce("+",  l.EAP )
  dl.EAP.sum.over.list  <-  Reduce("+",  dl.EAP )
  A.EAP.sum.over.list   <-  Reduce("+",  A.EAP )
  AA.EAP.sum.over.list  <-  Reduce("+",  AA.EAP )
  aa.EAP.sum.over.list  <-  Reduce("+",  aa.EAP )
  bb.EAP.sum.over.list  <-  Reduce("+",  bb.EAP )



  w.EAP.mean.over.replicated.models   <-    w.EAP.sum.over.list/length(list.of.estimates)
  dz.EAP.mean.over.replicated.models  <-   dz.EAP.sum.over.list/length(list.of.estimates)
  z.EAP.mean.over.replicated.models   <-    z.EAP.sum.over.list/length(list.of.estimates)
  mu.EAP.mean.over.replicated.models  <-  mu.EAP.sum.over.list/length(list.of.estimates)
  v.EAP.mean.over.replicated.models   <-     v.EAP.sum.over.list/length(list.of.estimates)
  ppp.EAP.mean.over.replicated.models <-   ppp.EAP.sum.over.list/length(list.of.estimates)
  l.EAP.mean.over.replicated.models   <-    l.EAP.sum.over.list/length(list.of.estimates)
  dl.EAP.mean.over.replicated.models  <-    dl.EAP.sum.over.list/length(list.of.estimates)
  A.EAP.mean.over.replicated.models   <-    A.EAP.sum.over.list/length(list.of.estimates)
  AA.EAP.mean.over.replicated.models  <-    AA.EAP.sum.over.list/length(list.of.estimates)
  aa.EAP.mean.over.replicated.models  <-   aa.EAP.sum.over.list/length(list.of.estimates)
  bb.EAP.mean.over.replicated.models  <-     bb.EAP.sum.over.list/length(list.of.estimates)





  mu.error <-  mu.EAP.mean.over.replicated.models  -  mu.truth
  v.error  <- v.EAP.mean.over.replicated.models    -  v.truth
  z.error  <- z.EAP.mean.over.replicated.models    -  z.truth


  a <- mu.EAP.mean.over.replicated.models/v.EAP.mean.over.replicated.models# 2019 Sept 6
  b <- 1/v.EAP.mean.over.replicated.models# 2019 Sept 6
  AUC.error <- stats::pnorm( a/sqrt(b^2+1))# 2019 Sept 6






  # xx <-apply(
  #   array(unlist(x),append( dim(x[[1]]), length(x) )  ),
  #   1:length(dim(x[[1]])),
  #   var
  # )

  foo <- function(x){apply(
    array(unlist(x),append( dim(x[[1]]), length(x) )  ),
    1:length(dim(x[[1]])),
    stats::var
  )}


  w.EAP.variance.over.replicated.models   <- foo_of_a_List_of_Arrays(  w.EAP  ,stats::var)
  dz.EAP.variance.over.replicated.models  <- foo_of_a_List_of_Arrays( dz.EAP   ,stats::var)
  z.EAP.variance.over.replicated.models   <- foo_of_a_List_of_Arrays(  z.EAP  ,stats::var)
  mu.EAP.variance.over.replicated.models  <- foo_of_a_List_of_Arrays( mu.EAP   ,stats::var)
  v.EAP.variance.over.replicated.models   <- foo_of_a_List_of_Arrays( v.EAP   ,stats::var)
  ppp.EAP.variance.over.replicated.models <- foo_of_a_List_of_Arrays(  ppp.EAP  ,stats::var)
  l.EAP.variance.over.replicated.models   <- foo_of_a_List_of_Arrays(  l.EAP  ,stats::var)
  dl.EAP.variance.over.replicated.models  <- foo_of_a_List_of_Arrays( dl.EAP   ,stats::var)
  A.EAP.variance.over.replicated.models   <- foo_of_a_List_of_Arrays(  A.EAP  ,stats::var)
  AA.EAP.variance.over.replicated.models  <- foo_of_a_List_of_Arrays(  AA.EAP  ,stats::var)
  aa.EAP.variance.over.replicated.models  <- foo_of_a_List_of_Arrays(  aa.EAP  ,stats::var)
  bb.EAP.variance.over.replicated.models  <- foo_of_a_List_of_Arrays(  bb.EAP  ,stats::var)























return(
    list(
      mu.error = mu.error,
      v.error  = v.error,
      z.error  = z.error,
      AUC.error=AUC.error,# 2019 Sept 6


      w.EAP.variance.over.replicated.models   =   w.EAP.variance.over.replicated.models,
      dz.EAP.variance.over.replicated.models  =    dz.EAP.variance.over.replicated.models,
      z.EAP.variance.over.replicated.models   =   z.EAP.variance.over.replicated.models   ,
      mu.EAP.variance.over.replicated.models  =   mu.EAP.variance.over.replicated.models ,
      v.EAP.variance.over.replicated.models   =  v.EAP.variance.over.replicated.models     ,
      ppp.EAP.variance.over.replicated.models =  ppp.EAP.variance.over.replicated.models ,
      l.EAP.variance.over.replicated.models   = l.EAP.variance.over.replicated.models     ,
      dl.EAP.variance.over.replicated.models  =  dl.EAP.variance.over.replicated.models   ,
      A.EAP.variance.over.replicated.models   =   A.EAP.variance.over.replicated.models   ,
      AA.EAP.variance.over.replicated.models  =   AA.EAP.variance.over.replicated.models  ,
      aa.EAP.variance.over.replicated.models  =    aa.EAP.variance.over.replicated.models  ,
      bb.EAP.variance.over.replicated.models  =   bb.EAP.variance.over.replicated.models



    )
  )














  # w.EAP
  # dz.EAP
  # z.EAP
  # mu.EAP
  # v.EAP
  # ppp.EAP
  # l.EAP
  # dl.EAP
  # A.EAP
  # AA.EAP
  # aa.EAP
  # bb.EAP


}#function

