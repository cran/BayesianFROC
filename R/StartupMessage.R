.onAttach <- function(libname, pkgname) {
  packageStartupMessage(

  crayon::bgBlack$cyan("\n
* vignettes (or README)

    vignettes (or README) URL:  https://CRAN.R-project.org/package=BayesianFROC

    Without internet enviroment,
    TeX script is not compiled in web browser page.
    To see .html vignettes with compiled TeX scripts,
    we need internet environment.

    * R script for vignettes (internet environment required for TeX)

    vignette(package = \"BayesianFROC\",topic = \"Theory_of_Bayesian_FROC_with_R_scripts\")
    vignette(package = \"BayesianFROC\",topic =                \"Brief_explanation\")
    vignette(package = \"BayesianFROC\",topic =           \"Very_Brief_description\")
    vignette(package = \"BayesianFROC\",topic =      \"Very_Very_Brief_description\")
    vignette(package = \"BayesianFROC\",topic = \"Very_Very_Very_Brief_description\")
    vignette(package = \"BayesianFROC\",topic = \"Very_Very_Very_Brief_Description_MRMC\")



    * Also run;

         explanation_about_package_BayesianFROC()

* Demos


        demo(demo_MRMC, package=\"BayesianFROC\");
        demo(demo_srsc, package=\"BayesianFROC\");
        demo(demo_stan, package=\"BayesianFROC\");
        demo(demo_drawcurves_srsc, package=\"BayesianFROC\");
        demo_Bayesian_FROC();
        demo_Bayesian_FROC_without_pause();


* Example: Single reader and Single Modality

      fit <- fit_Bayesian_FROC( ite  = 1111, summary = TRUE,  cha = 1,  dataList = dataList.Chakra.1.with.explantation )

* Example: Mutltiple reader and Mutltiple Modality


      fit <- fit_Bayesian_FROC( ite  = 1111, summary = TRUE,  cha = 1,  dataList = dd )


* Example: SBC for single reader and single modality via rstan::sbc

      fit <- Simulation_Based_Calibration_single_reader_single_modality_via_rstan_sbc()


*  Shiny based  Graphical User Interface for fitting and estimates and drawing curve;

", crayon::bgWhite$red$bold$underline$italic("        fit_GUI()           "),"
", crayon::bgWhite$red$bold$underline$italic("        fit_GUI_simple()    "),"
", crayon::bgWhite$red$bold$underline$italic("        fit_GUI_dashboard()    "),"





", crayon::bgBlack$red$bold$underline$italic("* ver. 0.1."),crayon::bgBlack$white$bold$underline$italic("5  ")  )

)
  }



.onLoad <- function(libname, pkgname) {

  # packageStartupMessage("\n* The package \"BayesianFROC\" is loaded. \n")

  }
