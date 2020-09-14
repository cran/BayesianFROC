

#' @title Execute before submission to delete redandunt files.
#' @description This for a developer of this package
#' @return none
# @export
#'
#@examples

file_remove <- function(){

print(  dir( system.file("extdata","", package="BayesianFROC")) )


file.remove("inst/extdata/Model_srscVer2.rds")
file.remove("inst/extdata/Model_srsc_multinomial.rds")
file.remove("inst/extdata/Model_srsc.rds")
file.remove("inst/extdata/Model_Hiera_OneModalityMultipleReader_TargetFormulation.rds")
file.remove("inst/extdata/Model_srsc_prior.rds")
file.remove("inst/extdata/Model_srsc_prototype.rds")
file.remove("inst/extdata/Model_srsc_reparametrized.rds")
file.remove("inst/extdata/SBCver4.rds")
file.remove("inst/extdata/SBCver3.rds")
file.remove("inst/extdata/SBCver2.rds")
file.remove("inst/extdata/Model_MRMC_non_hierarchical.rds")
file.remove("inst/extdata/Model_Hiera_versionTWO.rds")
file.remove("inst/extdata/null_hier.rds")
file.remove("inst/extdata/SBC_reparametrized.rds")
file.remove("inst/extdata/SBC_MRMC.rds")
file.remove("inst/extdata/SBC.rds")
file.remove("inst/extdata/Model_MRMC_reparametrized.rds")
file.remove("inst/extdata/Model_MRMC_prototype.rds")
file.remove("inst/extdata/Model_MRMC.rds")
file.remove("inst/extdata/Model_Hiera_versionTHREE.rds")
file.remove("inst/extdata/Model_ANOVA.rds")

# file.show(.libPaths())
# file.show( paste( .libPaths()[1], "/BayesianFROC/extdata",sep = ""))
file.remove(system.file("extdata", "Model_srscVer2.rds", package="BayesianFROC"))
# file.remove("inst/extdata/")
# file.remove("inst/extdata/")
# file.remove("inst/extdata/")
# file.remove("inst/extdata/")
# file.remove("inst/extdata/")
# file.remove("inst/extdata/")
# file.remove("inst/extdata/")


file.remove("fit.Rda")
file.remove("fit.Rds")
print(dir( system.file("extdata","", package="BayesianFROC")) )
}
