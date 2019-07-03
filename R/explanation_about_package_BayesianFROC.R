

#' @title  Explanation of this package
#' @description In R console, explanation are shown.
#' @export
#'
#' @examples
#'
#' explanation_about_package_BayesianFROC()
#'
explanation_about_package_BayesianFROC <- function(){


  white <- crayon::make_style("white")
  xxxx <- crayon::make_style(grDevices::rgb(0.13, 0.19, 0.45), bg = TRUE)
  R <- xxxx(white("R"))
  # cat(R) Not use such a cat("R")
  R<- crayon::bold$italic(R)
  # cat(R)

  pink <- crayon::make_style("pink")
  x <- crayon::make_style(grDevices::rgb(0.33, 0.19, 0.45), bg = TRUE)
  # message(x(pink("I am not sure but is it OK to write message here ?.\n")))

  pink <- crayon::make_style("pink")
  xxxx <- crayon::make_style(grDevices::rgb(0.13, 0.19, 0.45), bg = TRUE)
  # message(x(pink("I am not sure but is it OK to write message here ?.\n")))


  pink <- crayon::make_style("pink")
  xx <- crayon::make_style(grDevices::rgb(0.03, 0.59, 0.55), bg = TRUE)
  # message(x(pink("I am not sure but is it OK to write message here ?.\n")))



  pink <- crayon::make_style("pink")
  xxx <- crayon::make_style(grDevices::rgb(0.93, 0.19, 0.65), bg = TRUE)
  # cat(y(pink("tttttttttttttttttttttttt.\n")))


  pink <- crayon::make_style("pink")
  y <- crayon::make_style(grDevices::rgb(0.83, 0.19, 0.65), bg = TRUE)
  # cat(y(pink("tttttttttttttttttttttttt.\n")))


  pink <- crayon::make_style("pink")
  yy <- crayon::make_style(grDevices::rgb(0.73, 0.49, 0.15), bg = TRUE)
  # cat(yy(pink("I am pink if your terminal wants it, too.\n")))

  pink <- crayon::make_style("pink")
  yyy <- crayon::make_style(grDevices::rgb(0.63, 0.19, 0.65), bg = TRUE)
  # cat(yyy(pink("I am pink if your terminal wants it, too.\n")))


  pink <- crayon::make_style("pink")
  z <- crayon::make_style(grDevices::rgb(0.53, 0.49, 0.15), bg = TRUE)
  # cat(z(pink("I am pink if your terminal wants it, too.\n")))


  pink <- crayon::make_style("pink")
  zz <- crayon::make_style(grDevices::rgb(0.43, 0.49, 0.15), bg = TRUE)
  # cat(zz(pink("I am pink if your terminal wants it, too.\n")))

  pink <- crayon::make_style("pink")
  zzz <- crayon::make_style(grDevices::rgb(0.33, 0.19, 0.65), bg = TRUE)
  # cat(zzz(pink("I am pink if your terminal wants it, too.\n")))


  pink <- crayon::make_style("pink")
  a <- crayon::make_style(grDevices::rgb(0.23, 0.49, 0.15), bg = TRUE)
  # cat(a(pink("I am pink if your terminal wants it, too.\n")))


  pink <- crayon::make_style("pink")
  aa <- crayon::make_style(grDevices::rgb(0.13, 0.49, 0.15), bg = TRUE)
  # cat(aa(pink("I am pink if your terminal wants it, too.\n")))
  # to show a startup message
  packageStartupMessage("

\n* The package ", crayon::cyan$bold("\"BayesianFROC\"") ,", welcome !! \n

",x(pink("
-----------------------------------------------------------
------------   Package: BayesianFROC  ---------------------
-----------------------------------------------------------
")),"

 ***  Work Flow ***

* ",y(pink(  "STEP(1): Prepare Data                                 ")),"
* ",yy(pink( "STEP(2): Fitting FROC Statistical Models              ")),"
* ",yyy(pink("STEP(3): Draw FROC curve                              ")),"
* ",z(pink(  "STEP(4): Validation of fitting model to data          ")),"
* ",zz(pink( "STEP(5): Plot Bi-Gaussian distributions               ")),"
  ",zzz(pink("                                                      ")),"
  ",a(pink(  "                                                      ")),"
  ",aa(pink( "                                                      ")),"

* Main procedure consists of STEP(1) and STEP(2).
* If only one fitted model object is specified in STEP(1) and STEP(2), a sequential analysis STEP(3), (4) and (5) is given for the fitted model.

", crayon::bgWhite$red$bold$italic$underline("
STEP (1):   Prepare Data ") ,"
         ( as an ", R, " object formulated for applying the function in this package)


    * If you already have data formulated for a software ", crayon::yellow$italic$bgBlack( "JAFROC" ) ,",
      with its extension xlsx of the Microsoft Office,
      then use ", crayon::cyan$bold("\"convertFromJafroc()\"") ,"
      in this package to convert the data file of ", crayon::yellow$italic$bgBlack( "xlsx extension" ) ,"
      from Jafroc formulation to an ",R, " object for this package formluation.

    * If user dose not have any data yet, then please create an ", R, " object of your data
      by using ", crayon::cyan$bold("\"dataset_creator_new_version()\"") ,"
      (or \"", crayon::cyan$bold("create_dataset()") ,"\") in this package.



    * Use one of the following to create user's own FROC data as an ", R," object.

    +-- ", R, " Codes: Single reader and Single Modality --+
    |
    | * ",crayon::cyan$bold("convertFromJafroc()                   "),"
    | * ",crayon::cyan$bold("dataset_creator_new_version()         "),"
    | * ",crayon::cyan$bold("create_dataset()                      "),"
    +------------------------------------------------+



     * See also the following example data

        Example ", R, " objects for data : Single reader and Single Modality

         * ",crayon::cyan$bold("dataList.Chakra.1.with.explantation"),"
         * ",crayon::cyan$bold("dataList.Chakra.2"),"
         * ",crayon::cyan$bold("dataList.Chakra.3"),"

        Example ", R, " objects for data  : Multiple reader and Multiple Modality

         * ",crayon::cyan$bold("dataList.Chakra.Web"),"




", crayon::bgWhite$red$bold$italic$underline("
STEP (2): Fitting FROC Statistical Models            ") ,"

    * Using an ",R, " object  of data prepared in the STEP (1),
      we can obtain a fitted model object of class inheriting from \"stanfit\"
      by applying the function ", crayon::cyan$bold("\"fit_Bayesian_FROC()\" ") , "



", crayon::cyan$bold("
    +---", R, " Codes-----------------------+
    |                                   |
    |  * fit_Bayesian_FROC()            |
    |                                   |
    +-----------------------------------+
") ,"




", crayon::bgWhite$red$bold$italic$underline("
STEP (3): Draw  FROC curves            ") ,"

    * Using an ",R, " object obtained in the STEP (2),
      we can plot FROC curves and AFROC curves
      by ", crayon::cyan$bold("\"DrawCurves()\"") ," in this package.

", crayon::cyan$bold("
    +---", R, " Codes------------------------+
    |                                   |
    |  * DrawCurves()                   |
    |                                   |
    +-----------------------------------+
") ,"




", crayon::bgWhite$red$bold$italic$underline("
STEP (4): Validation of resulting models for your data            ") ,"

    * Using an ",R, " object obtained in the STEP (2),
      we can evaluate the chi square p-values
      in Baeyesian Context
      by ", crayon::cyan$bold("\"p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit()\"") ," in this package.


", crayon::cyan$bold("
  +---", R, " Codes--------------------------------------------------------+
  |                                                                    |
  |  * p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit()  |
  |                                                                    |
  +--------------------------------------------------------------------+
                     ") ,"


* Note that the chi square value in the plot plane
  is calculated with posterior means.
  On the other hand, this function calculate the chi square value
  in purely Bayesian manner which is described
  in the famous Gelmann's book \" Bayesian Data Analysis\".

* There is a further validation implemeted
  in ", crayon::cyan$bold(" validation.dataset_srsc_for_different_NI_NL()") ," but
  this validation is independent on users data,
  so I think it is not important for users to know it.



", crayon::bgWhite$red$bold$italic$underline("
STEP (5): Plot Bi-Gaussian distributions") ,"

    * Using the ",R, " S4 object obtained in the STEP (2),
      we can plot the signal and the noise distribution
      by ", crayon::cyan$bold("\"draw_bi_normal()\"") ," in this package.


", crayon::cyan$bold("
  +---", R, " Codes--------------------------------------------------------+
  |                                                                    |
  |  * draw_bi_normal()                                                |
  |                                                                    |
  +--------------------------------------------------------------------+
                     ") ,"

----  Minimal explanation of the package BayesianFROC finished !! ------------

    *** For More Details ***

        * Vignettes
        * help()
        * Demos

* Please see  examples codes in ", crayon::cyan$bold("help(fit_Bayesian_FROC)" ),"
  in which for the full details are given.

", crayon::cyan$bold("
    +---", R, " Codes-----------------------------------------------------+
    | * demo(demo_MRMC,package=\"BayesianFROC\")()                      |
    | * demo(demo_srsc,package=\"BayesianFROC\")()                      |
    | * demo(demo_drawcurves_srsc,package=\"BayesianFROC\")()           |
    | * demo(demo_stan,package=\"BayesianFROC\")                        |
    L-----------------------------------------------------------------+
") ,"


* Examples of ", R, " Codes:
    [with pipe operator defined in the package ", crayon::cyan$bold("library(magrittr) "), " ]

   * First, prepare the pipe operator by the following code:

      ", crayon::bgBlack$bold$yellow$underline$italic(" \`%>%\` <- utils::getFromNamespace(\"%>%\", \"magrittr\")  "), "

     I like pipe opeartor, since it does not require to create an R objects.
     So, we can reduce the notations by using the pipe operator which made by the God of R, Hadley Wickham? I forget his name,... sorry ...  god.

   * Create dataset and fitting
", crayon::cyan$bold("
     dataset_creator_new_version() ", crayon::bgBlack$bold$yellow$underline$italic(" %>% "), " fit_Bayesian_FROC()
"), "

   * Fitting and draw the bi-normal assumption with example data
", crayon::cyan$bold("
        dataList.Chakra.1  ", crayon::bgBlack$bold$yellow$underline$italic(" %>% "), "   fit_Bayesian_FROC()  ", crayon::bgBlack$bold$yellow$underline$italic(" %>% "), "   draw_bi_normal()
"), "

   * Fitting and evaluate the p-value of the Bayesian sence
", crayon::cyan$bold("
      dataList.Chakra.1  ", crayon::bgBlack$bold$yellow$underline$italic(" %>% "), "   fit_Bayesian_FROC()  ", crayon::bgBlack$bold$yellow$underline$italic(" %>% "), "   p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit()
"), "


   * Fitting and drawing the FROC curves
", crayon::cyan$bold("
     dataList.Chakra.Web ", crayon::bgBlack$bold$yellow$underline$italic(" %>% "), "   fit_Bayesian_FROC(ite = 111)  ", crayon::bgBlack$bold$yellow$underline$italic(" %>% "), "   DrawCurves()
"), "


   * When user use the above scripts
     with the user's own data object,
     then run above code use the user own data
     instead of the example data
     such as dataList.Chakra.1, dataList.Chakra.Web.





   ", crayon::inverse$bold$italic$underline("---  Inherited S4 Class ---"),"
    * The fitted model ",R, " object created by the", crayon::cyan$bold("fit_Bayesian_FROC()"), "
      is generated by ", crayon::cyan$bold$italic$underline("\"rstan\"") ," package,
      but its class is not ", crayon::cyan$bold$italic$underline("\"stanfit\"") ,",
      but a certain extended (inherited) class from the class \"stanfit\".

      That is,
            return value of  rstan::sampling()
            is an S4 object of the S4 class \"stanfit\".
      On the other hand,
            return value of  BayesianFROC::fit_Bayesian_FROC()
            is an S4 object of an inherited  S4 class named \"stanfitExtended\".



      Thus to apply any functions of the rstan package for a fitted model object of the extended S4 class
      (such as", crayon::cyan$bold(" rstan::stan_dens(),rstan::stan_trace()") ,",...),
      we need to coerce the class of the object from the inherited class \"stanfitExtended\".
 to the class \"stanfit\"
      by the R script ", crayon::cyan$bold$underline(" methods::as( object.obtained.in.STEP.(2), \"stanfit\")  ") ,".
   ", crayon::inverse$bold$italic$underline("--- Questions ---"),"

    * For any questions,
      send me a mail (",crayon::bgBlack$yellow$bold( "tsunoda.issei1111"), crayon::bgBlack$white$bold("@"),crayon::bold$magenta("gmail.com "),").

   ", crayon::inverse$bold$italic$underline("--- Job ---"),"

", crayon::red$bold("
    * Could you employ me as data scientist ?
      (Send me a mail to tsunoda.issei1111@gmail.com )
    * My hobby is now to study differential geometry,
      complex differential geometry,
      Kodaira-Spencer deformation theory.
      "),

xxx(pink( "\n  -------------------Primary  functions  ---------------------   ")),

"
*",y(pink(  "convertFromJafroc()                                             ")),
"
*",yy(pink( "dataset_creator_new_version()                                   ")),
"
*",yyy(pink("create_dataset()                                                ")),
"
*",z(pink(  "fit_Bayesian_FROC()                                             ")),
"
*",zz(pink( "DrawCurves()                                                    ")),
"
*",zzz(pink("p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit()  ")),
"
*",a(pink(  "draw_bi_normal()                                                ")),
"
*",aa(pink( "                                                                ")),
"
*",aa(pink( "                                                                ")),""


  )#message




}
