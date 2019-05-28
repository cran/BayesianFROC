
#' @title Initial values for HMC (Hamiltonian Moncte Carlo  Markov Chains)
#'
#' @description Internal function. Should not be interested.
#'
#'
#'@details
#'This attempt failed, that is,
#'I cannot specify the initial values
#' so that the \code{rstan::sampling()} does not say the following:
#'
#'
#'  Rejecting initial value:
#'
#'
#'   Log probability evaluates to log(0), i.e. negative infinity.
#'
#'
#'  Stan can't start sampling from this initial value.
#'

#'
#'
#'
#'
#  @param dataList
#'@inheritParams fit_Bayesian_FROC

#' @return Initial values specification. See the detailed documentation for the init argument in \code{stan()}.
#' @export
#'
#' @examples
#'
#'  init <- initial_values_specification_for_stan_in_case_of_MRMC(dataList.Chakra.Web)
#'
#'
#'  # Where init is the variable of the rstan::stan() or rstan::sampling()
#'
#'
#'
#'
initial_values_specification_for_stan_in_case_of_MRMC <- function(dataList){

C <- dataList$C
M <- dataList$M
Q <- dataList$Q


dz <- vector()
mu <-array()
hyper_v <-vector()
A <- vector()
dz <- rep(1,C-1)
w <- 0.5
mu  <- as.array(matrix(1, nrow = 3, ncol = 2))
v  <- as.array(matrix(1, nrow = 3, ncol = 2))
hyper_v <- rep(0.5,Q)
A <- rep(0.5,M)
# real    w;
# real <lower =0  >  dz[C-1];
# real               mu[M,Q];
# real <lower=0>      v[M,Q];
# real <lower=0>      hyper_v[Q];
#
# real <lower=0,upper=1>A[M];


return(list(
  dz   =  dz ,
  w  =w,
  mu  =mu ,
  v  =  v ,
  hyper_v   =hyper_v ,
  A =  A

  ))
}
