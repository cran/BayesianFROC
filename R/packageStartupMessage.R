.onAttach <- function(libname, pkgname) {
  packageStartupMessage(

  crayon::bgBlack$cyan("\n
* For detail, see vignettes.

vignettes URL on CRAN:  https://cran.r-project.org/web/packages/BayesianFROC/index.html

Without internet enviroment, TeX script is not compiled in web browser page.
To see with compiled TeX scripts, we need internet environment

or R script

vignette(package = \"BayesianFROC\",topic = \"              \")


Also run;

   explanation_about_package_BayesianFROC()

* Demos


demo(demo_MRMC,package=\"BayesianFROC\");
demo(demo_srsc,package=\"BayesianFROC\");
demo(demo_stan,package=\"BayesianFROC\");
demo(demo_drawcurves_srsc,package=\"BayesianFROC\");
demo_Bayesian_FROC();
demo_Bayesian_FROC_without_pause();


*  Provides FROC analsys by Bayesian Analysis

Example:

fit <- fit_Bayesian_FROC( ite  = 1111, summary = TRUE,  cha = 1, Null.Hypothesis = FALSE, dataList = dataList.Chakra.1.with.explantation )


fit <- fit_Bayesian_FROC( ite  = 1111, summary = TRUE,  cha = 1, Null.Hypothesis = FALSE, dataList = dd )
        ")

)
  }



.onLoad <- function(libname, pkgname) {

  # packageStartupMessage("\n* The package \"BayesianFROC\" is loaded. \n")

  }
