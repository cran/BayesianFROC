


#' @title Chech total hit is less than NL for each reader and each modality
#' @description This check a give dataset consisting of MRMC data satisfies the
#' condition that the number hits is less than the number of lesions for each reader and
#' each modality.
# @param dataList
#'@inheritParams fit_Bayesian_FROC
#' @return Logical,  \code{TRUE} or  \code{FALSE}. If \code{TRUE},
#' then the format of dataset is correct. If not, then the dataset is incorrect in the sense that
#' the number of hits is  greater than the number of lesions for some reader and some imaging modality.
#' @export
#'
#' @examples
#'
#'   logical <- check_hit_is_less_than_NL(BayesianFROC::dd)
#'
#'
check_hit_is_less_than_NL  <- function(dataList) {

  a <- metadata_to_fit_MRMC(dataList)
  b <-apply(a$harray, c(2,3), sum)<=dataList$NL
  c <- sum(b) == dataList$M*dataList$Q

  return(c)

}
