

# Sum of Unif and Gaussian random variable is a convolution and
# it can calculate explicitly.

#' @title Predict some estimates of parameter
#'
#' @param d A list of data, which can be passed to the \code{fit_Bayesian_FROC}.
#'
#' @return none
# @export
#'
# @examples
prior_predictor <- function(d=d){
f <- fit_Bayesian_FROC( ite  = 11111,  cha = 1, summary = F, dataList = d)
w <-extract(f)$w
m <-extract(f)$m
v <-extract(f)$v
z2 <-extract(f)$z[,2]
z3 <-extract(f)$z[,3]

graphics::hist((m-w)/(sqrt(v)))
(m-w)/(sqrt(v))-1

mean(   (m-w)/(sqrt(v))-1  )

a <-metadata_srsc_per_image(f@dataList)
hh <-a$hh
graphics::hist(((w-m)/v)/Phi_inv(a$hh[3]))
graphics::hist(((z2-m)/v)/Phi_inv(a$hh[2]))
graphics::hist(((z3-m)/v)/Phi_inv(a$hh[1]))


graphics::hist(Phi_inv(exp(-a$ff[3] ))/w)
mean(Phi_inv(exp(-a$ff[3] ))/w)
message("\n* This function shows that the model parameter named w can be predictied by
 the data as the following quantities

        Phi_inv(exp(-a$ff[3] ))

 where the ff[3] is the most lower FPF (false positive fraction).

 Similar manner can be avaliable for the other parameters.

        ")

}
