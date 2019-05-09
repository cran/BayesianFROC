#' @title  Print summary for AUC comparisons for MRMC
#'
#'
#'@description  This is print the results of AUC comparison for MRMC data.
#'@inheritParams DrawCurves_MRMC_pairwise
#'@inheritParams fit_Bayesian_FROC
#' @export summary_AUC_comparison_MRMC
summary_AUC_comparison_MRMC <- function(
   StanS4class,
  significantLevel=0.8,
  dig=3
){


  if(requireNamespace("crayon",quietly = TRUE)){
    summary_AUC_comparison_MRMC_with_crayon(
       StanS4class=StanS4class,
      significantLevel=significantLevel,
      dig=dig
    )
  }#if

  if(!requireNamespace("crayon",quietly = TRUE)){
message(" Package: crayon will be suggested for more confortable descriptions.")

  summary_AUC_comparison_MRMC_without_crayon(
     StanS4class=StanS4class,
    significantLevel=significantLevel,
    dig=dig
  )
}#if
}#functionr
