

#' @title Plot FPF and TPF from MRMC data
#' @description From data (srsc or MRMC), empirical FROC is plotted, namely FPF and TPF.
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams ppp_srsc
#'@seealso \link{draw.CFP.CTP.from.dataList}

#'@inheritParams DrawCurves
#' @return TPF and FPF
#' @export
#'
#' @examples
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#======================================================================================
#'#                                      srsc
#'#======================================================================================
#'  # FPF is Per image
#'
#'
#'                        plot_FPF_and_TPF_from_a_dataset(d)
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#======================================================================================
#'#                                     MRMC
#'#======================================================================================
#'
#'  # FPF is Per lesion
#'
#'
#'                         plot_FPF_and_TPF_from_a_dataset(dd)
#'
#'
plot_FPF_and_TPF_from_a_dataset  <- function(dataList, ModifiedPoisson =  FALSE) {


  if(length(dataList)==5){
                  a<-metadata_srsc_per_image(dataList,ModifiedPoisson =ModifiedPoisson )
                        TPF <- a$hh
                        FPF <-a$ff
                      }

  if(length(dataList)==9||length(dataList)==10){
    a<-metadata_to_fit_MRMC(dataList,ModifiedPoisson =ModifiedPoisson )
                            TPF <- a$hhN
                            FPF <- a$ffN
                            }


  dark_theme()
  plot(FPF,TPF)
  graphics::abline(h=1)

  return(list(
    FPF=FPF,
    TPF=TPF
  ))

}
