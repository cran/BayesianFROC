#' @title Test the Null hypothesis that all modalities are same
#' @description
#'  Test null hypothesis that all modalities have same observer performance ablity,
#'  using Bayes factor.
#'
#' @details From input data \code{dataList}, the two objects of class  \strong{\emph{\code{\link[rstan]{stanfit}}}}
#'  are created. one is fitted to the null hypothesis model and the another one representing alternative hypothesis.
#'  These two  \strong{\emph{\code{\link[rstan]{stanfit}}}}. objects are compared based on Bayes factor.
#'
#' @inheritParams fit_Bayesian_FROC
#' @param dataList MRMC case only.
#'
#' @return none
#' @export
#'
#' @examples
#' \donttest{
#'
#'  Test_Null_Hypothesis_that_all_modalities_are_same(BayesianFROC::dd)
#'
#'  }#donttest
Test_Null_Hypothesis_that_all_modalities_are_same <- function(
  dataList,
  ite =111,
  cha =1,
  summary = FALSE
){


fitH0 <- fit_Bayesian_FROC( ite  = ite, summary = summary,  cha = cha, dataList = dataList , Null.Hypothesis = TRUE)
fitH1 <- fit_Bayesian_FROC( ite  = ite, summary = summary,  cha = cha, dataList = dataList , Null.Hypothesis = FALSE)



message("\n* Null model \n")
H0 <- bridgesampling::bridge_sampler(fitH0, method = "normal", silent = TRUE)
print(H0)

message("\n* Alternative model \n")
H1 <- bridgesampling::bridge_sampler(fitH1, method = "normal", silent = TRUE)
print(H1)

message("\n* Test the Null hypothesis that all modalities are same.\n")
BF10 <- bridgesampling::bf( H1,H0)
print(BF10)

message("\n* If the number is greater, then we reject H0 with more confidence.")
}#function
