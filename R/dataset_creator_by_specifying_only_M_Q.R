

#' @title Creates dataset
#' @description creates dataset
#' @param M A positive integer, indicating number of modalities.
#' @param Q A positive integer, indicating number of readers.
#'
#' @return An MRMC dataset.
#' @export
#'
#' @examples
#'
#' ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#----------------------------------------------------------------------------------------
#'#                       make a data of a single modality and 36 readers
#'#----------------------------------------------------------------------------------------

#'
#'
#'             d<-  dataset_creator_by_specifying_only_M_Q(M=1,Q=36)
#'
#'
#'
#'             check_hit_is_less_than_NL(d)
#'
#'
#'        #     plot_FPF_and_TPF_from_a_dataset(d)
#'             plot_FPF_TPF_via_dataframe_with_split_factor(d)
#'
#' ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#----------------------------------------------------------------------------------------
#'#                       make a data of 2 modalities and 36 readers
#'#----------------------------------------------------------------------------------------

#'
#'
#'             d<-  dataset_creator_by_specifying_only_M_Q(M=2,Q=36)
#'
#'
#'
#'             check_hit_is_less_than_NL(d)
#'
#'
#'          #   plot_FPF_and_TPF_from_a_dataset(d)
#'             plot_FPF_TPF_via_dataframe_with_split_factor(d)
#'
#'
#'#----------------------------------------------------------------------------------------
#'#                       make a data of 2 modalities and 6 readers
#'#----------------------------------------------------------------------------------------

#'
#'
#'             d<-  dataset_creator_by_specifying_only_M_Q(M=2,Q=6)
#'
#'
#'
#'             check_hit_is_less_than_NL(d)
#'
#'
#'          #   plot_FPF_and_TPF_from_a_dataset(d)
#'             plot_FPF_TPF_via_dataframe_with_split_factor(d)
#'
#'
#' \donttest{
#'   # Stan runs
#'           fit_a_model_to(dataList = d,
#'                     seed_for_MCMC = 1234)
#'
#'}
dataset_creator_by_specifying_only_M_Q <- function(M=2,Q=15){

v  <- v_truth_creator_for_many_readers_MRMC_data(M=M,Q=Q)
m  <- mu_truth_creator_for_many_readers_MRMC_data(M=M,Q=Q)
d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)

return(d)

}
