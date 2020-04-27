


#' @title create data for MRMC
#'
#' @param M a positive integer, specifies the number of modalities
#' @param Q a positive integer, specifies the number of  readers
#'
#' @return data, to which fit a model
#' @export
#'
#' @examples
#'
#' d <- dataset_creator_for_many_Readers(1,11)
#'
#'
dataset_creator_for_many_Readers  <- function(M,Q) {
  v  <- v_truth_creator_for_many_readers_MRMC_data(M=M,Q=Q)
  m  <- mu_truth_creator_for_many_readers_MRMC_data(M=M,Q=Q)
  d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
  return(d)
}
