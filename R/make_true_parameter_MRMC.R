


#' @title Make a true model parameter and include it in this package
#'
#'@inheritParams DrawCurves
#'
# @return
# @export
#'
# @examples
make_true_parameter_MRMC  <- function(StanS4class) {
  f <- StanS4class

  z <- extract_EAP_CI(f,"z",f@dataList$C )$z.EAP
    # usethis::use_data(z)



    dz <- extract_EAP_CI(f,"dz",f@dataList$C-1 )$dz.EAP
    # usethis::use_data(dz)

    mu <- extract_EAP_by_array(f,mu)
    # usethis::use_data(mu)

    v <- extract_EAP_by_array(f,v)
    # usethis::use_data(v)

}
