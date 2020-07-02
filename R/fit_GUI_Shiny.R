#'
# @title ----
#' @title Fit a model with GUI of Shiny
#' @description A graphical user interface (GUI) to fit a model to data.
#' @details  First, please execute, then user will understand what it is.
#' This function is the one of the most important function in this package.
#' I do not assume that the user is familiar with R script but FROC analysis.
#'  So, I made this function to provide
#'   the Graphical User Interface (GUI) for users to avoid CUI (Characteristic User Interface).
#'  The GUI is made by the \pkg{shiny} package.
#' @param DF A dataframe as an initial data to be fitted a model
# @param outdir I use \code{  system.file("myapp", package = "BayesianFROC")    }
# @param outfilename I do not know :'-D
#' @param NL.max max number of bins indicating the maximal number in which the number of lesions can move
#' @param NI.max max number of bins indicating the maximal number in which the number of imagegs can move
#' @param NL.initial Natural number indicating the initial number of lesions, Default value =259.
#'                   The reason why the author made this is in example code, the author made a very strange data
#'                  in it, the default value = 259 is not satisfies the data format. That is in the example,
#'                  total number of hits is greater than 259 and it is impossible. So, I have to change the
#'                  default value.
#'
#' @param MCMC.chains.max max number of bins indicating number of MCMC chains
#'@author Issei Tsunoda
#' @return None
#' @export
#'
#' @examples
#'
# \dontrun{
#'## Only run examples in interactive R sessions
#'if (interactive()) {
#'#========================================================================================
#'#            1)           Use the default User Interface
#'#========================================================================================
#'#'
#'
#'  #No need to consider the variables, it is sufficient in  default values.
#'
#'
#'  #fit_GUI_Shiny()
#'
#'
#'
#'
#'#========================================================================================
#'#            2)           Change the  User Interface
#'#========================================================================================
#'
#'
#'#  We can change the max imput of the number of lesions and the max of number of images
#'#
#'
#'  fit_GUI_Shiny(NL.max = 2222,
#'                 NI.max = 3333)
#'
#'
#'
#'
#'
#'#========================================================================================
#'#            3)           Change the  Default value
#'#========================================================================================
#'
#'
#'
#'  fit_GUI_Shiny(
#'  DF= data.frame( h=dataList.Chakra.4$h,
#'                  f=dataList.Chakra.4$f
#'                )
#'               )
#'
#'# Or equivalently,
#'
#'   fit_GUI_Shiny(
#'             DF= data.frame(
#'             h = c(160,  25,  15,   7),
#'             f = c(  8,  16,  18,  13)
#'                          )
#'             )
#'
#'
#'#========================================================================================
#'#            4)           Change the user Imterface
#'#========================================================================================
#'
#'
#'
#'      fit_GUI_Shiny(
#'
#'           DF= data.frame(
#'               h = c(160,  25,  15,   7),
#'               f = c(  8,  16,  18,  13)
#'                 ),
#'
#'                 NL.max = 1192,
#'                 NI.max = 794,
#'                 MCMC.chains.max = 6
#'
#'               )
#'
#'
#'
#'
#'
#'#========================================================================================
#'#            5) CUI rather than GUI input
#'#========================================================================================
#'
#'
#'#              How to input data using CUI?
#'#                           This example gives an answer.
#'#
#'
#'# CUI:  Characteristic user interface
#'
#'
#'# Here, I show the very strange data, that is, the number of hits is all 33
#'# and replicated 10 times, that is,
#'# h is substituted by rep(33L,10) indicating  33 33 33 33 33 33 33 33 33 33
#'# f is also same as h.
#'
#'
#'
#'
#'   fit_GUI_Shiny(NL.initial=555,
#'                DF =data.frame(
#'                  h= as.integer(rep(33,10)),
#'                  f= as.integer(rep(33,10))
#'                )
#'  )
#'
#'
#'# The author made this example since, when I check my program,
#'# such as whether the color used in polygon() is appropriate or not.
#'
#'# If user thinks that it is very hard to input hits and false alarms
#'# by GUI manner, then use this characteristic like manner.
#'
#'
#'}### Only run examples in interactive R sessions

#}dontrun
#'

fit_GUI_Shiny <- function(
  # Initial data -----
  DF=data.frame(h=c( 97L,   32L,   31L),
                f=c( 1L ,  14L,   74L )
  ),

  NL.max=1111,
  NI.max=1111,

  NL.initial = 259,
  # MCMC.samples.max = 11111,
  MCMC.chains.max=4

  # counter=0


){


  outdir <- system.file("myapp", package = "BayesianFROC")
  outfilename <- "table"




  if (outdir == "") {
    stop("Could not find myapp. Try re-installing `BayesianFROC`.", call. = FALSE)
  }





  # shiny::shinyUI(

  # shiny::fluidPage(
  #ui ------
  ui <-   shiny::navbarPage("Emplooooo\\(^o^)/oooooooy   me! :'-D Now, from 2020 Jun 1, the author have been a xxxxx and  above me only sky. If you can employ, then send me a mail ... my address is \" tsunoda.issei1111@gmail.com \" ",




                            shiny::tabPanel(":) This programm cannot save my life :-'D",



                                            # shiny::tags$head(
                                            #   shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
                                            #   # , shiny::tags$script(src= "color_change.js")
                                            # ) ,# Color


                                            shiny::tags$head(
                                              shinythemes::themeSelector(),

                                              shiny::tags$style(shiny::HTML("




.ui-draggable {
 z-index: 3;
 background-color: #CCCCFF;

    "))

                                            ),#taghead

                                            shiny::titlePanel(" FROC Analysis by Issei Tsunoda, who is a xxxxx and MCS patient in Japan. Above him only sky."),

                                            shiny::h4(shiny::helpText("Change Data, then estimates and plot change accordingly. Chi square and posterior predictive p value is adaquate only the author'S model. For the classical, traditional model, it is underconstruction.")),

                                            # shiny::fluidRow(
                                            # column(6,        # shiny::h2(" FROC Data"),




                                            shiny::absolutePanel(        draggable = TRUE, style ="red",fixed=TRUE,

                                                                         # the traditional, classical model with multinomial ----
                                                                         shiny::selectInput("multi_nomial", "The Classical model  vs the author's new models",
                                                                                            c(
                                                                                              "Classical, traditional model" = TRUE,
                                                                                              "The author's new model" = FALSE
                                                                                            )
                                                                         ),
                                                                         shiny::h4("Using WAIC, the author compares these two models, but the answer is difficult. If data have many zeros, then the author recommends the author's model."),
                                            ),#absolutePanel


                                            # HMC param -----
                                            shiny::absolutePanel(        draggable = TRUE, style ="red",fixed=TRUE,

                                            shiny::h1("Parameter of HMC"),
                                            shiny::h5("HMC: Hamiltonian Monte Carlo Sampling"),

                                            shiny::h4(shiny::helpText(" Larger is better.")),

                                            shiny::sliderInput("Number_of_MCMC_samples",
                                                               "Number of MCMC samples:",
                                                               min = 111, max = 11111, value = 333),
                                            shiny::h5("If R hat is large, increase the number of MCMC samples."),
                                            shiny::h5("If data have many zeros, then such a treatment may be required."),


                                            shiny::h4(shiny::helpText(" Larger is better.")),

                                            shiny::sliderInput("Number_of_MCMC_chains",
                                                               "Number of MCMC chains:",
                                                               min = 1, max = MCMC.chains.max, value = 1)
                                            ), #absolutePanel



















                                            # Data -----
                                            shiny::absolutePanel(        draggable = TRUE, style ="red",fixed=TRUE,
                                                                         #  top = "100%",
                                                                         # left = 1,
                                                                         # right = 1,
                                                                         # bottom = 1,
                                                                         # width = 1,
                                                                         # height = 1,

                                                                         shiny::h1("Data"),
                                                                         shiny::h4(shiny::helpText(  " (to be fitted a model) " )),
                                                                         shiny::h4(shiny::helpText(  "Right-click on the table to delete/insert rows." )),
                                                                         shiny::h4(shiny::helpText(    "Double-click on a cell to edit")),
                                                                         shiny::h6(shiny::helpText(" h = hit = True Positive = TP.")),
                                                                         shiny::h6(shiny::helpText(" f = false alarms = False Positive = FP.")),

                                                                         # data GUI Handsontable-----
                                                                         rhandsontable::rHandsontableOutput("data_frame"),# Table of h and f


                                                                         shiny::h6(shiny::helpText("To edit more than 2 cells, a blank cell would help")),

                                                                         shiny::sliderInput("Number_of_lesions",
                                                                                            "Number of lesions:",
                                                                                            min =  1, max = NL.max,
                                                                                            value = NL.initial# Default
                                                                         ),


                                                                         shiny::sliderInput("Number_of_images",
                                                                                            "Number of images:",
                                                                                            min = 1, max = NI.max,
                                                                                            value = 57#Default
                                                                         ),

                                                                         shiny::h6(shiny::helpText(" Press S in key board to decrease only one")),
                                                                         shiny::h6(shiny::helpText(" Press  w in key board to increase only one"))

                                                                         # shiny::br()




                                            ), #absolutePanel

                                            shiny::sidebarLayout(
                                              shiny::sidebarPanel(width = 5,


                                                                  # wellPanel(
                                                                  # shiny::h3("Trace plot"),
                                                                  #                         radioButtons("stan_trace", "Trace plot", c("TRUE", "FALSE"))
                                                                  #                       ),






                                                                  # shiny::plotOutput("DrawCurves" ),


                                                                  # Visualizations of Data and Model----
                                                                  # draw curves ----
                                                                  shiny::h1("Visualizations of Data and Model"),
                                                                  shiny::checkboxInput("DrawCFPCTP",
                                                                                       "FPF and TPF (Data)",
                                                                                       value = TRUE),

                                                                  shiny::checkboxInput("DrawFROCcurve",
                                                                                       "FROC curve (Fitted Model)",
                                                                                       value = TRUE),

                                                                  shiny::checkboxInput("DrawAFROCcurve",
                                                                                       "AFROC curve and region of AUC  (Fitted Model)",
                                                                                       value = TRUE),

                                                                  shiny::plotOutput("DrawCurves", dblclick = shiny::dblclickOpts(id = "plot_dbl_click")),






                                                                  shiny::wellPanel(
                                                                    # divergent ----------------
                                                                    shiny::h1("Check HMC diagnostics"),

                                                                    shiny::h6("divergence"),
                                                                    shiny::verbatimTextOutput("divergent_iterations_print"),
                                                                    # treedepth ----------------

                                                                    shiny::h6("treedepth"),
                                                                    shiny::verbatimTextOutput("treedepth_print"),
                                                                    shiny::h6("E-BFMI values "),
                                                                    shiny::verbatimTextOutput("check_energy_print"),

                                                                    shiny::h6("R hat"),
                                                                    shiny::verbatimTextOutput("max_Rhat_print"),


                                                                    shiny::br()
                                                                  ),





                                                                  #Save a fitted model object-----

                                                                  shiny::h1("Save a fitted model object"),


                                                                  shiny::actionButton("trigger_save_a_fitted_model_object","Save a fitted model object in Desktop"),
                                                                  shiny::actionButton("trigger_save_a_fitted_model_object_working_directory","Save a fitted model object in Working directory"),

                                                                  shiny::h6(shiny::helpText("* Put resulting file \"fit\" in Working directory of R and execute the following R code"),       "load(\"fit\")",   shiny::helpText(" from the R console or (R studio console), then an R object named fit is available from the R console. To change the S4 class to stanfit, use the following R code "),        "fitt <- methods::as(fit, \"stanfit\")",   shiny::helpText(" Then, the R object named fitt is an R object of the S4 class stanfit. ")),



                                                                  #Save a fitted model object rename -----

                                                                  shiny::h1("Save a fitted model object, rename"),


                                                                  shiny::actionButton("trigger_save_a_fitted_model_object_rename","Save a fitted model object in Desktop"),
                                                                  # shiny::actionButton("trigger_save_a_fitted_model_object_working_directory_rename","Save a fitted model object in Working directory"),

                                                                  shiny::h6(shiny::helpText("* A file \"fit.Rds\"  is created in Desktop. By executing the following R code"),   "my_favorite_name <- readRDS(file =paste0(file.path(Sys.getenv(\"USERPROFILE\"),\"Desktop\"),\"\\\\fit.Rds\"))"  ,   shiny::helpText(" from the R console or (R studio console),  an R object named \"my_favorite_name\" is available from the R console. To change the S4 class to stanfit, use the following R code "),        "fitt <- methods::as(my_favorite_name, \"stanfit\")",   shiny::helpText(" Then, the R object named fitt is an R object of the S4 class stanfit. ")),
                                                                  # the result of cat("\\\\") is \\    2020 May 21

















                                                                  shiny::h1("Estimates"),
                                                                  shiny::h6(shiny::helpText("Internet Environment is required for TeX script.")),

                                                                  shiny::uiOutput("formula"),

                                                                  shiny::wellPanel(


                                                                    shiny::uiOutput("formula.TeX"),

                                                                    shiny::uiOutput("formula.model"),

                                                                    shiny::uiOutput("formula.AUC"),
                                                                    shiny::uiOutput("formula.WAIC"),
                                                                    shiny::uiOutput("formula.chisquare")

                                                                  ),#wellPanel

                                                                  shiny::br()

                                                                  # ,wellPanel(
                                                                  #   shiny::h3("Save"),
                                                                  #   actionButton("save", "Save table")
                                                                  # )
                                                                  # )
                                              ),#sidebarPanel

                                              # shiny::column(6,
                                              shiny::mainPanel(width = 7,


                                                               # Statistics ------
                                                               shiny::wellPanel(
                                                                 shiny::h1("Statistics"),

                                                                 shiny::h6(shiny::helpText(" Larger is better.")),
                                                                 shiny::uiOutput("formula.AUC.without.TeX"),
                                                                 shiny::h6(shiny::helpText("A number between 0 and 1, indicating observer performance.")),

                                                                 shiny::h6(shiny::helpText("------------------------------------------------------------------------------------")),

                                                                 shiny::h6(shiny::helpText(" Smaller is better.")),
                                                                 shiny::uiOutput("formula.WAIC.without.TeX"),
                                                                 shiny::h6(shiny::helpText("Evaluate whether our model is good.")),

                                                                 shiny::h6(shiny::helpText("------------------------------------------------------------------------------------")),

                                                                 shiny::h6(shiny::helpText(" Smaller is better.")),
                                                                 shiny::uiOutput("formula.chisquare.without.TeX"),
                                                                 shiny::uiOutput("EAP_of_chi_square"),
                                                                 shiny::h6(shiny::helpText("A difference between data and expectations from estimates.")),

                                                                 shiny::h6(shiny::helpText("------------------------------------------------------------------------------------")),


                                                                 shiny::h6(shiny::helpText(" Larger is better.")),

                                                                 shiny::em( shiny::uiOutput("ppp")),
                                                                 shiny::h6(shiny::helpText(" Null hypothesis is that our model fit to the data well.")),
                                                                 shiny::checkboxInput("ppp_calculate_trigger",
                                                                                      "Calculate P-value",
                                                                                      value = FALSE),
                                                                 shiny::h6(shiny::helpText("It takes quite a lot of time."))
                                                               ),




                                                               # shiny::h2(" FROC Curve and FPF,TPF"),
                                                               # shiny::h4(shiny::helpText("I love you :'-D  If you love me, then employ me :)")),
                                                               # shiny::h4(shiny::helpText("In Hit rate plain, the canonical Gaussian is also drawn to show how the graph changes.  ")),
                                                               # shiny::h4(shiny::helpText("In False rate plain, the signal Gaussian is also drawn to show how the graph changes.  ")),
                                                               # shiny::h4(shiny::helpText("The thresholds are common between false alram and hit.")),
                                                               # shiny::h6(shiny::helpText("Internet Environment is required for TeX script.")),


                                                               shiny::plotOutput("bi_normal", dblclick = shiny::dblclickOpts(id = "plot_dbl_click")),
                                                               shiny::plotOutput("false_rate", dblclick = shiny::dblclickOpts(id = "plot_dbl_click")),
                                                               shiny::h6(shiny::helpText("The most right area corresponds the rate of the most high confidence.")),

                                                               # Redundant Gadgets -----
                                                               shiny::h1("Redundant Gadgets"),

                                                               shiny::checkboxInput("Colour_plot_of_rates",
                                                                                    "Colour",
                                                                                    value = TRUE),



                                                               shiny::checkboxInput("dark_theme",
                                                                                    "dark theme",
                                                                                    value = FALSE),





                                                               shiny::checkboxInput("mathmatical.symbols",
                                                                                    "What curve are drawn?",
                                                                                    value = TRUE),





                                                               shiny::sliderInput("density",
                                                                                  "density:",
                                                                                  min =  0, max = 100,
                                                                                  value = 23# Default
                                                               ),

                                                               shiny::wellPanel(
                                                                 # shiny::checkboxInput("FPF_per_Lesion",
                                                                 #                      "FPF per Lesion",
                                                                 #                      value = FALSE),




                                                                 # per lesion or per image ----
                                                                 shiny::selectInput("FPF_per_Lesion", "FPF",
                                                                                    c(
                                                                                      "False Positive Fraction per image (per trial)" = FALSE,
                                                                                      "False Positive Fraction per lesion (per signal, per nodule)" = TRUE
                                                                                    )
                                                                 ),
                                                                 # Prior ----
                                                                 shiny::selectInput("prior", "Prior type",
                                                                                    c(
                                                                                      "Non-informative, Proper by Gaussian distributions" = -1,
                                                                                      "Non-informative, Improper by Unbounded Uniform distributions" = 0,
                                                                                      "Non-informative, Proper by Uniform distributions" = 1
                                                                                    )
                                                                 ),

                                                                 shiny::verbatimTextOutput("prior_print")



                                                               ),#wellPanel

                                                               # Posterior Estimates -----
                                                               shiny::h2(" Posterior Estimates"),
                                                               shiny::h6(shiny::helpText(" mean = posterior mean")),
                                                               # shiny::h6(shiny::helpText(" se_mean = .... ")),
                                                               shiny::h6(shiny::helpText(" sd = Standard Deviation of posterior MCMC samples")),
                                                               shiny::h6(shiny::helpText(" 2.5% = Lower bound of Posterior Credible interval")),
                                                               shiny::h6(shiny::helpText(" 97.5% = Upper bound of Posterior Credible interval")),
                                                               shiny::h6(shiny::helpText(" Rhat = A criteria of Convergence for MCMC sampling, if it is more close to 1, then it is better. ")),

                                                               shiny::verbatimTextOutput("fit_print"),






                                                               shiny::h2(" Replicated FROC datasets to calculate a posterior predictive p value"),
                                                               shiny::checkboxInput("ppp_plot_trigger",
                                                                                    "Plot replicated datasets to calculate P-value",
                                                                                    value = FALSE),
                                                               shiny::h4(shiny::helpText("It takes quite a lot of time.")),

                                                               shiny::plotOutput("plot_ppp", dblclick = shiny::dblclickOpts(id = "plot_dbl_click")),

                                                               shiny::checkboxInput("Colour",
                                                                                    "Colour",
                                                                                    value = FALSE),


                                                               shiny::checkboxInput("dark_theme_ppp",
                                                                                    "dark theme",
                                                                                    value = FALSE),









                                                               shiny::br()

                                              )#mainPanel
                                            )#sidebarLayout






                            ),#tabPanel
                            shiny::tabPanel("Ref",

                                            shiny::h2(" Reference:"),

                                            shiny::h4(shiny::a(  "See vignettes for more details",     href="https://cran.r-project.org/package=BayesianFROC")),

                                            shiny::h4(shiny::helpText("  Issei Tsunoda (2019): Pre-prepreprepreprepreprepreprint;  Bayesian Models for  Free - Response Receiver Operating Characteristic Analysis. <- I forget its precise name, reviwer always said what is new? or what a fucking english! Hahh OK. So, all people will understand what model is implemented in this package. I try in 2 years to publish my theory, but all journal rejected cuz what's new?? So, everybody knows what this package is and FROC models without my paper. Good luck! I hate statisian reviwers, I go back to my interst, geometry. Good bye!")),
                                            shiny::h4(shiny::helpText("  Issei Tsunoda (2020): Sorry, I cannot publish the paper about FROC, but mathematics paper will be published soon, in which the author study the Gromov-Hausdorff topology,...I forget the paper name,,,maybe The correspondense between ,,,??? i forget,,, ha,forget .")),

                                            shiny::h4(shiny::helpText("  Dev Chakraborty (1989) doi:10.1118/1.596358; Maximum likelihood analysis of free - response receiver operating characteristic (FROC) data.")),
                                            shiny::h4(shiny::helpText(" Dev Chakraborty; Observer Performance Methods for Diagnostic Imaging, Foundations, Modeling, and Applications with R-Based Examples "))



                            ),#tabPanel

                            shiny::tabPanel("stanmodel",

                                            shiny::verbatimTextOutput("stanmodel_print"),

                                            shiny::h4(shiny::helpText(" When we wash doggy, NEVER use every synthetic detergent (i.e., syndet) for doggy.
                                                                          Doggy does not require such a dangerous bitch!
                                                                          so they need only warm water only, Oh! and warm loves,
                                                                          balls and ,,, frysbee? I do not know the English spell! ... he dead ... cancer.
                                                                         do not use the sarfactant for them!!
                                                                         He die, he was happy.
                                                                         Liebriya was no longer in this world.
                                                                         He had gone.
                                                                         The author cannot play with him forever.
                                                                         But Ruikoboch is still fine, he is not so yunger man, his eyes are white.

                                                                       The author diseased from multiple chemical sensitivity from exposure of some syndet, then my whole body has serious chronic inflammations such as prurigo nodularis.
                                                                       And my head, brain also diseased such inflamation, NO/ONOO- cycle maybe occur in my body. I hate it. ha...two years,,,,

                                                                      ")),
                                            shiny::h4(shiny::helpText("Best regards,")),

                                            shiny::h4(shiny::a(  "doggy",     href="https://cran.r-project.org/package=BayesianFROC")),

                                            # shiny::actionButton("Riya","RiyaSan"),

                                            shiny::br()


                            ),#tabPanel


                            shiny::tabPanel("Trace Plot",

                                            shiny::actionButton("trigger_stan_trace_plot","Trace plot"),
                                            # trace plot -----


                                            shiny::plotOutput("plot_trace", dblclick = shiny::dblclickOpts(id = "plot_dbl_click")),




                                            shiny::h4(shiny::helpText("The author's older brother is biggy, his hiphop is very big hip! Piggy what are you doing, you still smoke in the water? Piggy is expert of chemistry, what are doing piggy, you still fat man? or big hip man?")),
                                            shiny::h4(shiny::helpText("Best regards,  ")),

                                            shiny::h4(shiny::a(  "Piggy",     href="https://cran.r-project.org/package=BayesianFROC"))




                            ),#tabPanel

                            shiny::tabPanel("Histogram",
                                            shiny::actionButton("trigger_stan_hist_plot","Histogram plot"),
                                            # hist plot -----
                                            shiny::plotOutput("plot_hist", dblclick = shiny::dblclickOpts(id = "plot_dbl_click")),
                                            shiny::sliderInput("hist_bins",
                                                               "Number of bins:",
                                                               min =  1, max = 777,
                                                               value = 30# Default
                                            ),


                                            shiny::h4(shiny::helpText("The author's younger brother is rat, his teeth is outer and outer and outer. What are doing rat? you still work very hard position? He is also expert of chemistry, he did chemistry in his master degree. Ha, my brothers like chemisty. I cannot understand why chemistry is funny? I like Riemannian geometry. Are you happy now? You have a lot of money and a car, but you working eight days a week, is your own life? Oh, I got a call and he will marry, I cannot believe. So, he is happy, but me, I have aches. ha... ")),
                                            shiny::h4(shiny::helpText("Best regards,  ")),

                                            shiny::h4(shiny::a(  "Rat",     href="https://cran.r-project.org/package=BayesianFROC"))
                            ),#tabPanel

                            shiny::tabPanel("Generic plot",
                                            shiny::actionButton("trigger_stan_generic_plot","Trace plot"),
                                            # generic plot -----
                                            shiny::plotOutput("generic_plot", dblclick = shiny::dblclickOpts(id = "plot_dbl_click")),
                                            shiny::h4(shiny::helpText("The author's younger and younger brother is nothing but his pokemon is tiny, pratin.")),
                                            shiny::h4(shiny::helpText("Best regards,  ")),
                                            shiny::h4(shiny::a(  "Pratin",     href="https://cran.r-project.org/package=BayesianFROC"))
                            ),#tabPanel



                            shiny::tabPanel("Cat",
                                            shiny::h4(shiny::helpText("To tell the truth, I am not interest to FROC analysis, what I want is only healthy life forever. I do not write any program for FROC analysis! I hate all such people!  ")),
                                            shiny::h4(shiny::helpText("Best regards,  ")),

                                            shiny::h4(shiny::a(  "Cat",     href="https://cran.r-project.org/package=BayesianFROC"))




                            ),#tabPanel

                            shiny::tabPanel("Guide to understanding FROC theory",
                                            shiny::h4(shiny::helpText("Under construction  ")),
                                            shiny::h4(shiny::helpText("Best regards,  ")),

                                            shiny::h4(shiny::a(  "Cat",     href="https://cran.r-project.org/package=BayesianFROC"))




                            ),#tabPanel


                            shiny::tabPanel("Fish",
                                            shiny::h4(shiny::helpText(" Why FROC, to tell the truth, I am not sure, but only things what I want to say is I do not want to ride a rice for Sushi!  ")),
                                            shiny::h4(shiny::helpText("Best regards,  ")),

                                            shiny::h4(shiny::a(  "Fish",     href="https://cran.r-project.org/package=BayesianFROC"))
                            ),#tabPanel






                            shiny::tabPanel("Carbus  University",
                                            shiny::h4(shiny::helpText(" I am a professor in both Carbus and Mosquitowbus univeristy
                                                                         in which I study how to fight to mosquito in the
                                                                         dark night. Mosquitos threaten me with his poooo00000000ooooooo0000000000ooooooooooo000000000000000ooooooon,
                                                                          voice, so I try to catch them, but their stealth cannot
                                                                         catch. I have to establish new theory how to fight them
                                                                         to ensure our stable sleepping.

                                                                         When I 17 years old, I establish the Carbus university and research how to fight Car ( which means mosquito in some language).
                                                                         Yes, I am the fist president of Carbus university.

                                                                         ")),
                                            shiny::h4(shiny::helpText("Best regards,  ")),
                                            shiny::h4(shiny::a(  "The Author",     href="https://cran.r-project.org/package=BayesianFROC"))




                            ),#tabPanel

                            shiny::tabPanel("Ghostbus university",
                                            shiny::h4(shiny::helpText(" I am a professor in the Ghostbus univeristy
                                                                         in which I study how to fight to ghost. My students are the Ghostbusters.")),
                                            shiny::h4(shiny::helpText("Best regards,  ")),
                                            shiny::h4(shiny::a(  "The Author",     href="https://cran.r-project.org/package=BayesianFROC"))




                            ),#tabPanel




                            shiny::tabPanel("Acknowleage",
                                            shiny::h4(shiny::helpText("This package is made with helps of many unknown people in stack over flows, and the author appreciate their kind helps. Also Stan developer gives me quick and appropriate advices in many times. Such many people helps me to develop this package. So, Michael Jackson says you are not alone!... but I think I am alone cuz I am a xxxxx, please .... Doggy, eleison. Cum sancto spiritu in gloria doggy. what I talking about....")),
                                            shiny::h4(shiny::helpText("Best regards,  ")),
                                            shiny::h4(shiny::a(  "The Author,",     href="https://cran.r-project.org/package=BayesianFROC"))




                            ),#tabPanel






                            shiny::tabPanel("Author",



                                            shiny::h1(" Help!"),

                                            shiny::h2(" The author: Issei Tsunoda  <tsunoda.issei1111 _at_ gmail.com>"),
                                            shiny::h3(" Employ me! :'-D, send me a mail <tsunoda.issei1111 _at_ gmail.com> I will go any country!"),
                                            shiny::h5(" I am japanese and live in Japan, but if necessary,
                                                                     I will go any country! Please help! I need somebody who employs me.
                                                                     Help! Not just any body, HELP! I need someone! HEEEELP!
                                                                     When I was younger so much, younger than today, I studied Riemannian Goematry.
                                                                     But now and then, I feel that in mathematics I cannot obtain any result.
                                                                     Help me if you can employ me in towns  and I do apprecieate,..., I love the Beatles and Bach and Beethoven ( player is Wilhelm Kempf only), Aerothmith, John Lennon, Billy Joel, Ben E King,...etc.


                                                                     "),

                                            shiny::h5("Please employ me! :'-D Now, I have no job! I study differential geometry and
                                                                     my most interest is complex differential geometry,
                                                                     Kodaira-Spencer deformation theory, Gromov-Hausdorff topology, Ricci flow, mean curvature flow, Schubelt calculas. I want to go many nature place, not urban. In several month ago, my whole body has prurigo nodularis, so I hate sarfactant except bio-surfactant."),

                                            shiny::h1("Multiple Chemical sensitivity (MCS)"),
                                            shiny::h5("In a 2017, December, 28, My exposure to surfactant (I won't use, and never have used in my own life, it is required in my company) and other chemical materials in it, makes my body very bad condition that symptoms occur in multiple organ systems. In several hours later from the exposure, irritant darmatitis occured. During  the first month from the exposure, my whole body has irritant stimulas, it is tiktik.  And 14 month later from the exposure, my whole body has prurigo nodularis and other chronic conditions, in addition other disorders caused by the exposure are present in my body in now (18 months later from the exposure).
                                                                     I am very sad since many people think it is psychogenic, but it is never such a thing, Complaints of MCS is very strong, should not dismissed  as psychogenic. We shold not be kept waiting for development of the diagnosis. I suffer from chemical sensitivity.
                                                                     All doctor who met me were lacking ability and didnot has any criteria of diagnosis for MCS.
                                                                     "),
                                            shiny::h5("Prurigos in My Mody"),

                                            shiny::img(src="prurigo.jpg",height= "222", width="222"),





                                            shiny::h1(" About my e-mail"),
                                            shiny::h5(" I publish this package with my e-mail, then
                                                          spam mails comes.
                                                          When you sends me a mail, then please specify
                                                          some knowleage about mathematics ( Reimannian Geotmery or Kodaira Spencer theory) or FROC or Bayesian analysis.
                                                          Since my spam mails are all English, so I doubt English mails.
                                                                     "),
                                            shiny::h1(" About my package"),
                                            shiny::h5(" I am not sure who is interest in my package,
                                                          Programming never finish forever,
                                                          so, ... I am tired."),



                                            shiny::h1("Paper"),
                                            shiny::h5(" My paper will publish  soon.
                                                         I research some pure mathematical things about the Gromov Hausdorff topology

                                                         My heart is alwary with mathematics.
                                                         It is my destiny.
                                                         "),

                                            shiny::h4(shiny::a(  "R package BayesianFROC",     href="https://cran.r-project.org/package=BayesianFROC"))



                            )#tabPanel









  )#navbarPage
  # )#shinyUI
  # ))


  # ______________________________________------------------
  server <- function(input, output) {

    values <- shiny::reactiveValues()

    # Data GUI Handsontable -----
    shiny::observe({
      if (!is.null(input$data_frame)) {
        DF = rhandsontable::hot_to_r(input$data_frame)
      } else {
        if (is.null(values[["DF"]]))
          DF <- DF
        else
          DF <- values[["DF"]]
      }
      values[["DF"]] <- DF
      values[["dataList"]] <- list(NL=input$Number_of_lesions,
                                   NI=input$Number_of_images,
                                   h=DF$h,
                                   f=DF$f,
                                   C=length(DF[,1])
      )

    })

    # This is use such as fit() instesad of fit
    fit <- shiny::reactive({
      # fit ----
      fit <- BayesianFROC::fit_Bayesian_FROC(

        multinomial = as.logical(input$multi_nomial),### multinomial---
        ite  = input$Number_of_MCMC_samples,
        cha =  input$Number_of_MCMC_chains,
        ModifiedPoisson = as.logical(input$FPF_per_Lesion),
        summary = TRUE,
        Null.Hypothesis  = FALSE,
        dataList = values[["dataList"]],# input$selected_data ,
        DrawCurve  = FALSE,
        dig = 5,
        prior = as.integer(input$prior)

      )
      return(fit)

    })






    output$data_frame <- rhandsontable::renderRHandsontable({
      DF <- values[["DF"]]
      if (!is.null(DF))
        rhandsontable::rhandsontable(DF,
                                     stretchH = "all")

    })

    ## Save
    # shiny::observeEvent(input$save, {
    #   finalDF <- isolate(values[["DF"]])
    #   # saveRDS(finalDF, file=file.path(outdir, sprintf("%s.Rds", outfilename)))
    #
    #
    #
    #
    #
    # })
    output$formula <- shiny::renderUI({
      # my_calculated_value <- extractAUC(fit(),dig = 4)[1]
      C<-values[["dataList"]]$C
      z<-apply( extract(fit())$z , 2, mean)
      m <- mean( extract(fit())$m)
      v <- mean( extract(fit())$v)

      s<-"Posterior Mean of Model Parameters: "
      for (cd in 1:C){
        s<-paste0(s," $$z_",cd," = ",signif(z[cd],digits = 3),",$$")
      }

      s<-paste0(s," $$\\mu = ",signif(m,digits = 3),",$$")
      s<-paste0(s," $$\\sigma = ",signif(v,digits = 3),",$$")



      shiny::withMathJax(s)
    })



    # TeX model -----

    output$formula.model <- shiny::renderUI({
      # my_calculated_value <- extractAUC(fit(),dig = 4)[1]
      C<-values[["dataList"]]$C
      NL<-values[["dataList"]]$NL
      NI<-values[["dataList"]]$NI
      h<-values[["dataList"]]$h
      f<-values[["dataList"]]$f
      h_rev <- rev(h)
      f_rev <- rev(f)


      # Calculates posterior means for each specified parameter #2020 Feb
      z <-apply( extract(fit())$z , 2, mean)
      # p <-apply( extract(fit())$p , 2, mean)
      if (!as.logical(input$multi_nomial)) p <-apply( extract(fit())$hit_rate , 2, mean)#2020 Feb
      if (as.logical(input$multi_nomial)) p_rev_Extented <-apply( extract(fit())$p_rev_Extented , 2, mean)#2020 Feb

      dl<-apply( extract(fit())$dl, 2, mean)

      m <- mean( extract(fit())$m)
      v <- mean( extract(fit())$v)
      c <- C:1
      s <-"Posterior Mean of Model Parameters: "
      for (cd in 1:C){
        s<-paste0(s," $$\\widehat{z_",cd,"} = ",signif(z[cd],digits = 3),",$$")
      }
      s<-paste0(s," $$\\widehat{\\sigma} = ",signif(v,digits = 3),",$$")
      s<-paste0(s," $$\\widehat{\\mu} = ",signif(m,digits = 3),",$$")
      s<-paste0(s," from which, we can say the followings.")

      # for (cd in 1:C){
      #   s<-paste0(s," $$H_",cd," \\sim \\text{Binomial}(",signif(p[cd],digits = 3),",", NL,"), \\text{ the realization is  } H_",cd," = ",h_rev[cd] ," .$$")
      # }
      if (!as.logical(input$multi_nomial)) {
      for (cd in 1:C){
      s<-paste0(s," $$H_",cd," \\sim \\text{Binomial}(",signif(p[cd],digits = 3),",", NL,"), \\text{ the realization is  } H_",cd," = ",h_rev[cd] ," .$$")
      }
      }

      if (as.logical(input$multi_nomial)) {
        s<-paste0(s,"Let $$H_0 := N_L- \\Sigma_{c=1}^{",C,"}H_c $$")
        s<-paste0(s," $$\\LARGE		{(H_c)_{c=0,1,2,...,",C,"} \\sim \\text{Multinomial}((p=c)_{c=0,1,2,...,",C,"}).}$$")
        s<-paste0(s,"The following Bernoulli notation is not suitable but to indicates the component of the rate vector in the multinomial distribution, we use it.")

        for (cd in 1:C){
          s<-paste0(s," $$H_",cd," \\sim \\text{Bernoulli}(",signif(p_rev_Extented[cd],digits = 3),"), \\text{ the realization is  } H_",cd," = ",h_rev[cd] ," .$$")
        }
        s<-paste0(s," $$H_0 \\sim \\text{Bernoulli}(",signif(p_rev_Extented[C+1],digits = 3),"), \\text{ the realization is  }H_0 = ",NL," -", sum(h) ,"= ",NL- sum(h) ,".$$")


      }
      for (cd in 1:C){
        s<-paste0(s," $$F_",cd," \\sim \\text{Poisson}(",signif(dl[cd]*NI,digits = 3),"), \\text{ the realization is } F_",cd," = ",f_rev[cd] ," .$$")
      }

      shiny::withMathJax(s)
    })







    # TeX -----
    output$formula.AUC <- shiny::renderUI({
      # C<-values[["dataList"]]$C
      # NL<-values[["dataList"]]$NL
      # NI<-values[["dataList"]]$NI
      #
      # z<-apply( extract(fit())$z , 2, mean)
      # p<-apply( extract(fit())$p , 2, mean)
      # dl<-apply( extract(fit())$dl , 2, mean)
      #
      # m <- mean( extract(fit())$m)
      # v <- mean( extract(fit())$v)

      a <- mean( extract(fit())$a)
      b <- mean( extract(fit())$b)
      a <- signif(a,digits = 3)
      b <- signif(b,digits = 3)
      # bb <- signif(b^2,digits = 3)



      A <- mean( extract(fit())$A)
      A <- signif(A,digits = 3)

      # c<-C:1
      s<-"The estimated observer performance ability is the following AUC, which is a positive real number between 0 and 1."
      s<-paste0(s," $$\\text{AUC}= \\Phi (\\frac{\\widehat{a}}{\\sqrt{1+\\widehat{b^2} }})=\\Phi (\\frac{",a,"}{\\sqrt{1+",b,"^2}}) = ",A,"$$ where estimates are posterior mean and $$\\widehat{a} := \\frac{\\widehat{\\mu }}{\\widehat{\\sigma} }, \\widehat{b}:= \\frac{1}{\\widehat{\\sigma }}.$$")
      shiny::withMathJax(s)
    })


    # TeX -----
    output$formula.WAIC <- shiny::renderUI({



      WAIC <- fit()@WAIC
      WAIC <- signif(WAIC,digits = 3)
      s<-"Widely Applicable Information Criterion (Watanabe-Akaike Information Criterion) is calculated as follows."
      s<-paste0(s," $$\\text{WAIC}  = ",WAIC,".$$")
      shiny::withMathJax(s)
    })

    # TeX -----

    output$formula.chisquare <- shiny::renderUI({



      chisquare <- fit()@chisquare
      chisquare <- signif(chisquare,digits = 3)
      s<-"Chi square goodness of fit  is calculated as follows."
      s<-paste0(s," $$\\chi^2(\\text{data}|\\widehat{\\theta})   = ",chisquare,"$$ where estimates  \\( \\widehat{ \\theta }\\) denotes the posterior mean of  model parameter \\(  \\theta \\).")
      shiny::withMathJax(s)
    })





    output$formula.TeX <- shiny::renderUI({
      s<-paste0(" To show \\( \\TeX \\), internnet environment is required.")
      shiny::withMathJax(s)
    })












    output$formula.AUC.without.TeX <- shiny::renderUI({
      # C<-values[["dataList"]]$C
      # NL<-values[["dataList"]]$NL
      # NI<-values[["dataList"]]$NI
      #
      # z<-apply( extract(fit())$z , 2, mean)
      # p<-apply( extract(fit())$p , 2, mean)
      # dl<-apply( extract(fit())$dl , 2, mean)
      #
      # m <- mean( extract(fit())$m)
      # v <- mean( extract(fit())$v)

      a <- mean( extract(fit())$a)
      b <- mean( extract(fit())$b)
      a <- signif(a,digits = 3)
      b <- signif(b,digits = 3)
      # bb <- signif(b^2,digits = 3)



      A <- mean( extract(fit())$A)
      A <- signif(A,digits = 3)

      # c<-C:1

      s<-paste0("AUC =  ",A)
      shiny::withMathJax(s)
    })



    output$formula.WAIC.without.TeX <- shiny::renderUI({



      WAIC <- fit()@WAIC
      WAIC <- signif(WAIC,digits = 3)

      s<-paste0(" WAIC  = ",WAIC)
      shiny::withMathJax(s)
    })


    output$formula.chisquare.without.TeX <- shiny::renderUI({



      chisquare <- fit()@chisquare
      chisquare <- signif(chisquare,digits = 3)

      s<-paste0("chi square at posterior mean  = ",chisquare)
      shiny::withMathJax(s)
    })



    #ppp ----
    output$ppp <- shiny::renderUI({

      if (input$ppp_calculate_trigger) {


        ppp <- ppp_srsc(fit(),plot=FALSE)$p.value

        ppp <- signif(ppp,digits = 3)

        if (ppp>=0.05)   s<-paste0("Posterior Predictive P value   = ",ppp)
        if (ppp< 0.05)   s<-paste0("Posterior Predictive P value   = ",ppp," *")
        if (ppp< 0.01)   s<-paste0("Posterior Predictive P value   = ",ppp," **")


      }



      if (!input$ppp_calculate_trigger)    s<-paste0("Posterior Predictive P value is not calculated")




      shiny::withMathJax(s)


    })







    output$EAP_of_chi_square <- shiny::renderUI({

      # ppp <- ppp_srsc(fit())$p.value

      Chi.Square.for.each.MCMC.samples   <-   chi_square_goodness_of_fit(fit()   )
      m<-   mean(Chi.Square.for.each.MCMC.samples)

      m <- signif(m,digits = 3)

      s<-paste0("Posterior mean of  chi square   = ",m)


      shiny::withMathJax(s)
    })




    # output$formula <- renderUI({
    #   my_calculated_value <- extractAUC(fit(),dig = 4)[1]
    #   withMathJax(paste0("Posterior mean of AUC (area under the AFROC curve): $$\\widehat{AUC} =", my_calculated_value,"$$"))
    # })

    #fit_print ----

    output$fit_print <- shiny::renderPrint({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL
      if (sum(h)<=NL) {

        fitt <-methods::as(fit(), "stanfit")
        # print( fitt, digits = 4)
        a<- summary(fitt,probs=c(0.025,0.975))$summary
        print(a)
      }# if

      if (sum(h)>NL) { print("Data format error, number of hits never greater than the number of signals. In Radiological context, signal is lesion.")  }

    })# shiny::renderPrint



    output$stanmodel_print <- shiny::renderPrint({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL
      if (sum(h)<=NL) {

        # print(prior_print_srsc(input$prior))
        print(fit()@stanmodel)

      }# if

      if (sum(h)>NL) { print("Data format error, number of hits never greater than the number of signals. In Radiological context, signal is lesion.")  }

    })# shiny::renderPrint







    output$prior_print <- shiny::renderPrint({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL
      if (sum(h)<=NL) {

        # print(prior_print_srsc(input$prior))
        prior_print_srsc(input$prior)

      }# if

      if (sum(h)>NL) { print("Data format error, number of hits never greater than the number of signals. In Radiological context, signal is lesion.")  }

    })# shiny::renderPrint


    # max_Rhat_print -----
    output$max_Rhat_print <- shiny::renderPrint({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL
      if (sum(h)<=NL) {
        fit <-fit()
        fitt <- methods::as(fit,"stanfit")

        max.rhat <-  round( max(summary(fitt)$summary[,"Rhat"]) ,digits = 5)

        print(paste("max R-hat = ", max.rhat))





      }# if

      if (sum(h)>NL) { print("Data format error, number of hits never greater than the number of signals. In Radiological context, signal is lesion.")  }

    })# shiny::renderPrint


    # divergent_iterations_print -----
    output$divergent_iterations_print <- shiny::renderPrint({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL
      if (sum(h)<=NL) {
        fit <-fit()
        fitt <- methods::as(fit,"stanfit")

        divergent <- rstan::get_divergent_iterations(fitt)
        n <- sum(divergent)
        N <- length(divergent)


        if(!n==0) print(paste( n, " of ", N, "iterations ended with a divergence.",n,"/",N,"=", round( n/N ,digits = 2)))
        if(n==0) print(paste( ":) OK, i.e.  divergent free"))


      }# if

      if (sum(h)>NL) { print("Data format error, number of hits never greater than the number of signals. In Radiological context, signal is lesion.")  }

    })# shiny::renderPrint












    # treedepth_print -----
    output$treedepth_print <- shiny::renderPrint({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL
      if (sum(h)<=NL) {
        fit <-fit()
        fitt <- methods::as(fit,"stanfit")

        # max_depth <- rstan:::get_treedepth_threshold(fitt)
        treedepths <- rstan::get_max_treedepth_iterations(fitt)
        n <- sum(treedepths)
        N <- length(treedepths)


        # if(!n==0) print(paste( n, " of ", N, "iterations saturated the maximum tree depth of.", max_depth,"."))
        # if(n==0) print(paste( ":) OK"))
        print("Not done ........... because get_treedepth_threshold is not an exported object from 'namespace:rstan' and hence the author omitted the evaluation from here. ")
      }# if

      if (sum(h)>NL) { print("Data format error, number of hits never greater than the number of signals. In Radiological context, signal is lesion.")  }

    })# shiny::renderPrint










    # check_energy_print -----
    output$check_energy_print <- shiny::renderPrint({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL
      if (sum(h)<=NL) {
        fit <-fit()
        fitt <- methods::as(fit,"stanfit")

        EBFMIs <- get_bfmi(fitt)
        bad_chains <- which(EBFMIs < 0.2)
        if (!length(bad_chains)) {
          # print("E-BFMI indicated no pathological behavior.")
          print(paste( ":) OK"))
        }
        else {
          print(paste("E-BFMI indicated possible pathological behavior:\n"
                      ,"  Chain %s: E-BFMI = %.3f\n", bad_chains,
                      EBFMIs[bad_chains], "E-BFMI below 0.2 indicates you may need to reparameterize your model."))
        }



      }# if

      if (sum(h)>NL) { print("Data format error, number of hits never greater than the number of signals. In Radiological context, signal is lesion.")  }

    })# shiny::renderPrint







    output$DrawCurves <- shiny::renderPlot({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL
      if (sum(h)<=NL) {
        # grDevices::dev.off()
        # grDevices::dev.set()

        if (input$dark_theme==TRUE) { # This cords clear plot environment in shiny GUI board.
          dark_theme();
          plot(0,0,   # Dumy plot to clean plot environment, this is required only Shiny board, I do not know the reason.
               type="n",
               axes=FALSE,
               xlim=c(0,1),
               ylim =c(0,1),
               xaxt="n",
               yaxt="n",
               xlab="",
               ylab="")
        }


        # draw curves ----

        DrawCurves(fit(),
                   # Colour  = FALSE,
                   Colour = input$dark_theme,

                   new.imaging.device  = FALSE,
                   DrawCFPCTP = input$DrawCFPCTP,
                   DrawFROCcurve = input$DrawFROCcurve,
                   DrawAFROCcurve = input$DrawAFROCcurve)
      }


      if (sum(h)> NL) {


        h.string <- as.character(h)
        for (cd  in 1:length(h.string)) {
          if (cd==1){ s<-""; s <- paste(h.string[cd],sep = "+")}
          if ( !cd==1)s <- paste(s,h.string[cd],sep = "+")

        }#for
        sum.of.h <- s
        sum.of.h <- paste(sum.of.h,"=",as.character( sum(h) ) )

        plot(0,0,xlim=c(0,1),ylim =c(0,1),xaxt="n", yaxt="n",xlab="Please fix inconsistent data",ylab="",main="Error:   Inconsistent Data \n In baseball game, \n batter's number of hits can not  be greater than his number of at-bats")

        graphics::text(0.5,0.8,c("*Now, Sum of the number of hits is greater than that of lesion; \n\n", expression(paste(h1+h2+h3+... , "         >         Number of Lesions")) ),col="blue",cex =    1.4  )
        graphics::text(0.5,0.65,paste("In the current inputed data, it is the following: " ),col="black",cex =  1.5  )
        graphics::text(0.5,0.5,paste(  sum.of.h , "      >       ",NL,sep = ""),col="red",cex = 2)

        graphics::text(0.5,0.3,c("* Please fix so that the following inequality holds \n", expression(paste(h1+h2+h3+... , "       <        Number of lesions"))),col="blue",cex =  1.5    )
        graphics::text(0.5,0.1,c("* Shoud decrease the number of hits  or \n  Shoud increase the number of lesions"),col="blue",cex = 1.5  )




      }#if

    })#shiny::renderPlot







    output$plot_ppp <- shiny::renderPlot({

      if (input$ppp_plot_trigger) {

        h<-values[["dataList"]]$h
        NL<-values[["dataList"]]$NL


        if (sum(h)<=NL) {
          ppp(fit(),Colour=input$Colour,dark_theme=input$dark_theme_ppp
          )
        }

        if (sum(h)> NL) {
          error_message(h,NL)
        }#if
      }



      if (!input$ppp_plot_trigger)    plot(0,0,type="n", axes = TRUE,xlim=c(0,1),ylim =c(0,1),xaxt="n", yaxt="n",xlab="",ylab="",main="")



    })#shiny::renderPlot


    output$false_rate <- shiny::renderPlot({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL

      if (sum(h)<=NL) {
        # draw_latent_signal_distribution(fit() , new.imaging.device = FALSE,dark_theme=FALSE,hit.rate = TRUE,false.alarm.rate = FALSE)
        draw_latent_noise_distribution(fit(),
                                       new.imaging.device = FALSE,
                                       # dark_theme=FALSE,
                                       false.alarm.rate = TRUE,
                                       hit.rate  = FALSE,
                                       both.hit.and.false.rate  = FALSE,
                                       density = input$density,
                                       dark_theme = input$dark_theme,
                                       color = input$Colour_plot_of_rates,
                                       mathmatical.symbols = input$mathmatical.symbols

        )
      }

      if (sum(h)> NL) { error_message(h,NL)}#if

    })#shiny::renderPlot


    output$bi_normal <- shiny::renderPlot({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL


      if (sum(h)<=NL) {
        draw_latent_signal_distribution(fit() ,
                                        new.imaging.device = FALSE,
                                        # dark_theme=FALSE,
                                        hit.rate = TRUE,
                                        false.alarm.rate = FALSE,
                                        both.hit.and.false.rate  = FALSE,
                                        density = input$density,
                                        dark_theme = input$dark_theme,
                                        color = input$Colour_plot_of_rates,
                                        mathmatical.symbols = input$mathmatical.symbols
        )
      }

      if (sum(h)> NL) {
        error_message(h,NL)
      }#if

    })#shiny::renderPlot





    output$print_bi_normal <- shiny::renderPrint({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL
      if (sum(h)<=NL) {

        print(       draw_latent_signal_distribution(fit() , new.imaging.device = FALSE,dark_theme=FALSE,hit.rate = TRUE,false.alarm.rate = FALSE)
                     , digits = 4)

      }# if

      if (sum(h)>NL) { print("Data format error, number of hits never greater than the number of signals. In Radiological context, signal is lesion.")  }

    })# shiny::renderPrint




    # save a fitted model in Desktop ----
    shiny::observeEvent(input$trigger_save_a_fitted_model_object,{

      fit <- fit()
      save( fit,file =paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"\\fit") )
      # save( fit,file ="fit" )


      tcltk::tkmessageBox(message=paste("\n* A file (name: \"fit\") is created in Desktop. In the file, an fitted model R object (named \"fit\")  is contained. Its class is an S4 class (stanfitExtended)\n\n\n 1) Put the resulting file \"fit\" in the working directory,\n 2) Run an R script \n                                     load(\"fit\") \n\n then the R object named \"fit\" is available from the R console. "))

    }  )


    # save a fitted model in  Working directory ----

    shiny::observeEvent(input$trigger_save_a_fitted_model_object_working_directory,{

      fit <- fit()
      save( fit,file ="fit" ) # Save an object in Working directory ----
      tcltk::tkmessageBox(message=paste("\n* A file (name: \"fit\") is created in the current working directory. In the file, an fitted model R object (name \"fit\")  is contained, which is an object of an S4 class (stanfitExtended)\n\n\n 1) Put the resulting file \"fit\" in the working directory,\n 2) Run an R script \n                                     load(\"fit\") \n\n then the R object named \"fit\" is available on your R console. "))

    }  )



    # save a fitted model in Desktop rename----
    shiny::observeEvent(input$trigger_save_a_fitted_model_object_rename,{

      fit <- fit()
      saveRDS(fit, file =paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"\\fit.Rds"))
      # save( fit,file ="fit" )


      tcltk::tkmessageBox(message=paste("\n* A file (name: \"fit.Rds\") is created in Desktop. In the file, an fitted model R object is contained. Its class is an S4 class (stanfitExtended)\n\n\n \n * Run an R script \n     my_favorite_name <- readRDS(file =paste0(file.path(Sys.getenv(\"USERPROFILE\"),\"Desktop\"),\"\\\\f.Rds\"))

 \n\n then the R object named \"my_favorite_name\" is available from the R console. "))

    }  )










    # counter -----
    counter <- shiny::reactiveValues()

    shiny::observeEvent(c(input$trigger_stan_trace_plot
                          # ,input$hot
    ), {
      counter$s <-c(counter$s, length( counter$s)+1)
    })

    shiny::observeEvent(c( input$trigger_stan_hist_plot
                           # input$hist_bins
    ), {
      counter$ss <-c(counter$ss, length( counter$ss)+1)

    })

    shiny::observeEvent(c( input$trigger_stan_generic_plot
                           # input$hist_bins
    ), {
      counter$sss <-c(counter$sss, length( counter$sss)+1)

    })



    # trace--------
    output$plot_trace <- shiny::renderPlot({
      if (input$trigger_stan_trace_plot) {

        h<-values[["dataList"]]$h
        NL<-values[["dataList"]]$NL

        print(paste(  "s = ", length(counter$s) )  )
        if(floor(length(counter$s) /2)==length(counter$s) /2){

          if (sum(h)<=NL) {

            fit <- fit()
            fit <- methods::as(fit,"stanfit")

            message("Now, we plot trace of MCMC ....")
            return(rstan::stan_trace(fit,pars = c("A")))

          }

          if (sum(h)> NL) {
            error_message(h,NL)
          }#if
        }

      }else  if (!input$trigger_stan_trace_plot||!floor(length(counter$s) /2)==length(counter$s) /2)  {
        message("Now, we omit trace of MCMC ....")
        return( plot(0,0,type="n", axes = TRUE,xlim=c(0,1),ylim =c(0,1),xaxt="n", yaxt="n",xlab="",ylab="",main="")
        )

      }

    })#shiny::renderPlot













    # hist ------
    output$plot_hist <- shiny::renderPlot({
      if (input$trigger_stan_hist_plot) {

        h<-values[["dataList"]]$h
        NL<-values[["dataList"]]$NL

        print(paste(  "ss = ", length(counter$ss) )  )
        if(floor(length(counter$ss) /2)==length(counter$ss) /2){

          if (sum(h)<=NL) {

            fit <- fit()
            fit <- methods::as(fit,"stanfit")

            message("Now, we plot trace of MCMC ....")
            return(rstan::stan_hist (fit,pars = c("A"),bins=input$hist_bins))

          }

          if (sum(h)> NL) {
            error_message(h,NL)
          }#if
        }

      }else  if (!input$trigger_stan_trace_plot||!floor(length(counter$ss) /2)==length(counter$ss) /2)  {
        message("Now, we omit trace of MCMC ....")
        return( plot(0,0,type="n", axes = TRUE,xlim=c(0,1),ylim =c(0,1),xaxt="n", yaxt="n",xlab="",ylab="",main="")
        )

      }

    })#shiny::renderPlot









    # generic plot--------
    output$generic_plot <- shiny::renderPlot({
      if (input$trigger_stan_generic_plot) {

        h<-values[["dataList"]]$h
        NL<-values[["dataList"]]$NL

        print(paste(  "sss = ", length(counter$sss) )  )
        if(floor(length(counter$sss) /2)==length(counter$sss) /2){

          if (sum(h)<=NL) {

            fit <- fit()

            fit <- methods::as(fit,"stanfit")
            message("Now, we run  plot(fit,pars=c(\"hit_rate\"))")
            return(plot(fit,pars=c("z")))

          }

          if (sum(h)> NL) {
            error_message(h,NL)
          }#if
        }

      }else  if (!input$trigger_stan_trace_plot||!floor(length(counter$sss) /2)==length(counter$sss) /2)  {
        message("Now, we omit trace of MCMC ....")
        return( plot(0,0,type="n", axes = TRUE,xlim=c(0,1),ylim =c(0,1),xaxt="n", yaxt="n",xlab="",ylab="",main="")
        )

      }

    })#shiny::renderPlot























  }

  ## run app
  shiny::runApp(list(ui=ui, server=server))
  return(invisible())
}
