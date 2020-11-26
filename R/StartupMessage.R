.onAttach <- function(libname, pkgname) {


# fit_GUI_Shiny()


  packageStartupMessage(

  crayon::blurred("\n
# vignette (or README) URL:  https://CRAN.R-project.org/package=BayesianFROC
# vignette called by R script (internet environment required for TeX)
  vignette(package = \"BayesianFROC\",topic = \"Very_Very_Very_Brief_description\")

# Demos

  demo(demo_for_compilation_of_all_models, package=\"BayesianFROC\");
  demo(demo_for_traditional_FROC_models_with_multinomial_distribution, package=\"BayesianFROC\");
  demo(demo_SBC, package=\"BayesianFROC\");
  demo(demo_MRMC, package=\"BayesianFROC\");
  demo(demo_srsc, package=\"BayesianFROC\");
  demo(demo_stan, package=\"BayesianFROC\");
  demo(demo_drawcurves_srsc, package=\"BayesianFROC\");
  demo(demo_ppp, package=\"BayesianFROC\");
  demo(demo_for_reviewers_of_my_manuscript, package=\"BayesianFROC\");
  demo_Bayesian_FROC();
  demo_Bayesian_FROC_without_pause();

# Examples
    #  A Single reader and A Single Modality

         f <- fit_a_model_to(
                dataList = BayesianFROC::d,
                number_of_parallel_chains_for_MCMC     = 1,
                number_of_iterations_for_MCMC = 111,
                seed_for_MCMC = 1)

   # Posterior Predictive P value of chi square goodness of fit


          ppp <- extract_EAP_CI( f,\"p_value_logicals\",1 )$p_value_logicals.EAP




   #  Mutltiple reader and Mutltiple Modality

        f <- fit_a_model_to(
              dataList = BayesianFROC::dd,
              number_of_parallel_chains_for_MCMC     = 1,
              number_of_iterations_for_MCMC = 1111,
              seed_for_MCMC = 1234567)




# This package is developed by a some homeless in Japan. Also deseased from MCS as initial toxicant syndet which make him to loss jobs, be a homeless be much lower quality of life.
# With painful life, blood, prurigo nodularis, aches, chronic inflammation, he made this to save his life, but he cannot. I am not interested to Statistics, but Geometry, Differentia geometry, Kodaira-Spencer thoery, pseudo holomorphic curve, Complex differential geometry, symplectic geometry. So,,, Fuck FROC! I hate such a cheap theory! I hate Statistics!

   #  SBC for a single reader and a single modality via rstan::sbc

          stanModel <- stan_model_of_sbc()

          Simulation_Based_Calibration_single_reader_single_modality_via_rstan_sbc(
           stanModel = stanModel,
             ite     = 233,
             M       = 11,
             epsilon = 0.04,BBB = 1.1,AAA =0.0091,sbc_from_rstan = T)



#  Shiny based  Graphical User Interface for fitting and estimates and drawing curve;

 ", crayon::bgWhite$red$bold$underline$italic("   fit_GUI_Shiny() "),"

 ", crayon::bgWhite$red$bold$underline$italic("   fit_GUI_Shiny_MRMC()  #for subject-specific random effect model  or MRMC              "),"





",



crayon::cyan$bold$underline$italic("\n Ver."),
  crayon::bgYellow$red$bold$underline$italic("0."),
crayon::bgRed$yellow$bold$underline$italic(" 4."),
 crayon::bgGreen$white$bold$underline$italic("0."),
crayon::bgBlack$white$bold$underline$italic("  \"Such A Couch Potato\"  "),

crayon::bgYellow$red$bold$underline$italic(" "),
crayon::bgRed$yellow$bold$underline$italic(" "),
crayon::bgGreen$white$bold$underline$italic(" ")

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
