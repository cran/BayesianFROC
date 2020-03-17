.onAttach <- function(libname, pkgname) {


# fit_GUI_Shiny()


  packageStartupMessage(

  crayon::cyan("\n
# vignettes (or README)

    vignettes (or README) URL:  https://CRAN.R-project.org/package=BayesianFROC

    Without internet enviroment,
    TeX script is not compiled in web browser page.
    To see .html vignettes with compiled TeX scripts,
    we need internet environment.

    # R script for vignettes (internet environment required for TeX)

    vignette(package = \"BayesianFROC\",topic = \"Very_Very_Very_Brief_description\")



    # Also run;

         explanation_about_package_BayesianFROC()

# Demos


        demo(demo_MRMC, package=\"BayesianFROC\");
        demo(demo_srsc, package=\"BayesianFROC\");
        demo(demo_stan, package=\"BayesianFROC\");
        demo(demo_drawcurves_srsc, package=\"BayesianFROC\");
        demo(demo_ppp, package=\"BayesianFROC\");

        demo_Bayesian_FROC();
        demo_Bayesian_FROC_without_pause();

# Examples
          # Example: A Single reader and A Single Modality

               fit <- fit_Bayesian_FROC( ite  = 1111,
                                      summary = TRUE,
                                          cha = 1,
                                     dataList = dataList.Chakra.1.with.explantation )

         # Example: Mutltiple reader and Mutltiple Modality

               fit <- fit_Bayesian_FROC( ite  = 1111,
                                      summary = TRUE,
                                          cha = 1,
                                     dataList = dd )


         # Example: SBC for a single reader and a single modality via rstan::sbc

                fit <- Simulation_Based_Calibration_single_reader_single_modality_via_rstan_sbc()




         # Example: Posterior Predictive P value of chi square goodness of fit


                 p.value <- ppp(fit)





#  Shiny based  Graphical User Interface for fitting and estimates and drawing curve;



 ", crayon::bgWhite$red$bold$underline$italic("             fit_GUI_Shiny()                       "),"

 ", crayon::bgWhite$red$bold$underline$italic("             fit_GUI_Shiny_MRMC()                  "),"

# It will took a lot of time to run the codes for the first time, since stan files will be compiled.


# Reference: (pre-print, manuscript)
             Bayesian Models for Free-response Receiver Operating Characteristic Analysis


# For reviewers of my manuscript, execute the following code
  to check the results in my manuscript.

        demo(demo_for_reviewers_of_my_manuscript, package=\"BayesianFROC\");

",
 crayon::cyan$bold$underline$italic("# Ver."),
  crayon::red$bold$underline$italic("0."),
   crayon::bgBlack$white$bold$underline$italic(" 2."),
   crayon::bgBlack$white$bold$underline$italic("2")
   )
 # ,
 # "                 ",
 # crayon::cyan(  date()  ),
 # "\n",
 # "                                  ",
 # crayon::cyan("Month: "), crayon::cyan$bold$underline$italic(format(Sys.time(), "%b") )," ",
 # " ",
 # crayon::cyan("Date: "), crayon::red$bold$underline$italic(format(Sys.time(), "%d") ),"  ",
 # "",
 # crayon::cyan("Year: "), crayon::cyan$bold$underline$italic(format(Sys.time(), "%Y"   ) ),
 #
 # " ",
 # crayon::cyan("Time: "), crayon::red$bold$underline$italic(format(Sys.time(), "%H:%M") ),"\n"





)
  }



.onLoad <- function(libname, pkgname) {

  # packageStartupMessage("\n# The package \"BayesianFROC\" is loaded. \n")

  }
