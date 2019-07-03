.onAttach <- function(libname, pkgname) {
  packageStartupMessage(

  crayon::bgBlack$cyan("\n
* For details, see vignettes.

vignettes URL:  https://cran.r-project.org/package=BayesianFROC

Without internet enviroment, TeX script is not compiled in web browser page.
To see .html vignettes with compiled TeX scripts, we need internet environment.

or R script

vignette(package = \"BayesianFROC\",topic = \"              \")


* Also run;

   explanation_about_package_BayesianFROC()

* Demos


demo(demo_MRMC, package=\"BayesianFROC\");
demo(demo_srsc, package=\"BayesianFROC\");
demo(demo_stan, package=\"BayesianFROC\");
demo(demo_drawcurves_srsc, package=\"BayesianFROC\");
demo_Bayesian_FROC();
demo_Bayesian_FROC_without_pause();


* Example:

fit <- fit_Bayesian_FROC( ite  = 1111, summary = TRUE,  cha = 1,  dataList = dataList.Chakra.1.with.explantation )


fit <- fit_Bayesian_FROC( ite  = 1111, summary = TRUE,  cha = 1,  dataList = dd )




* Provides Graphical User Interface for fitting and estimates and drawing curve;
* The following code does not require any variables.



", crayon::bgWhite$red$bold$underline$italic("  fit_GUI()  "),"




", crayon::bgBlack$red$bold$underline$italic("* ver. 0.1."),crayon::bgBlack$white$bold$underline$italic("4  ")  )

)
  }



.onLoad <- function(libname, pkgname) {

  # packageStartupMessage("\n* The package \"BayesianFROC\" is loaded. \n")

  }
