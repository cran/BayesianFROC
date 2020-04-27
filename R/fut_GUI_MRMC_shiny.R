#' @title Fit with GUI via Shiny (in case of MRMC)
#' @description
#' Fit a Bayesian model with GUI.
#'
#' Revised 2019 Nov.
#'
#'
#' @details
#' In what follows, we assume that our dataset has more than two readers or modalities,
#' namely, our dataset is  MRMC case.
#' The term \emph{imaging modality},
#' we mean a set of imaging methods such as MRI, CT, PET, etc.
#'
#'
#' Revised 2019 Nov 25.
#' Revised 2020 Jan


#' @param DF A dataframe, cosisting of five vectors: reader ID, modality ID, confidence levels, hits, false alarms.
#'
#'   initial data to be fited
# @param outdir I use \code{  system.file("myapp", package = "BayesianFROC")    }
# @param outfilename I do not know :'-D
#' @param NL.max max number of bins indicating the maximal number in which the number of lesions can move
#' @param NI.max max number of bins indicating the maximal number in which the number of imagegs can move
#' @param NL.initial Natural number indicating the initial number of lesions, Default value =142.
#' @param NI.initial Natural number indicating the initial number of images, Default value =199.
#' @param MCMC.chains.max max number of bins indicating number of MCMC chains
#' @param seed.initial.of.MCMC positive integers indicating the initial seed of MCMC sampling. Default is 1234.

#' @return None
#' @export
#@examples-------

#' @examples
#'
#' \dontrun{
#'
#'#'## Only run examples in interactive R sessions
#'if (interactive()) {


#'#========================================================================================
#'#            1)           Use the default User Interface
#'#========================================================================================
#'#'
#'
#'  #No need to consider the variables, it is sufficient in  default values.
#'
#'
#'  fit_GUI_Shiny()
#'
#'
#'
#'
#'#========================================================================================
#'#            2)          From exsisting dataset, named dddddd or ddddd or ddd
#'#========================================================================================
#'
#'
#'
#'  fit_GUI_Shiny_MRMC(DF=extract_data_frame_from_dataList_MRMC(dddddd))
#'  fit_GUI_Shiny_MRMC(DF=extract_data_frame_from_dataList_MRMC(ddddd))
#'  fit_GUI_Shiny_MRMC(DF=extract_data_frame_from_dataList_MRMC(ddd))

#'
#'
#'
#'
#'#========================================================================================
#'#            2)       data of  11 readers and a single modality
#'#========================================================================================
#'
#'
#'
#'   d <- dataset_creator_for_many_Readers(1,11)
#'
#'   fit_GUI_Shiny_MRMC(DF=extract_data_frame_from_dataList_MRMC(d))
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'#========================================================================================
#'#                     see = 2345678       convergence 37readers, 1 modality
#'#========================================================================================
#'
#'
#'
#' v  <- v_truth_creator_for_many_readers_MRMC_data(M=1,Q=37)
#' m  <- mu_truth_creator_for_many_readers_MRMC_data(M=1,Q=37)
#' d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#'
#'
#'
#' fit_GUI_Shiny_MRMC(DF=extract_data_frame_from_dataList_MRMC(d),
#'                    seed.initial.of.MCMC = 2345678,
#'                    NL.initial = d$NL,
#'                    NI.initial = d$NI)
#'
#'
#'
#'
#'
#'
#'
#'
#'#========================================================================================
#'#            2)          From exsisting dataset, named dddd
#'#========================================================================================
#'
#'
#'
#'  fit_GUI_Shiny_MRMC(DF=extract_data_frame_from_dataList_MRMC(dddd))
#'
#'
#'   # This dataset named dddd is a dataset consisting of
#'   #  only a single reader and mutiple modality.
#'   # Such a single reader and mutiple modality case had error caused
#'   # by some reduction of array to vector.
#'   # So, the program was fixed so that such special case is also available
#'   # 2020 Feb 24
#'
#'   # To reflect the information of the number of lesions and images,
#'   # use the following.
#'
#'   fit_GUI_Shiny_MRMC(DF=extract_data_frame_from_dataList_MRMC(dddd),
#'   NL.initial = dddd$NL,
#'   NI.initial = dddd$NI)

#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                                example
#'#========================================================================================
#'
#'
#'
#'
#' v  <- v_truth_creator_for_many_readers_MRMC_data(M=2,Q=7)
#' m  <- mu_truth_creator_for_many_readers_MRMC_data(M=2,Q=7)
#' d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#' fit_GUI_Shiny_MRMC(DF=extract_data_frame_from_dataList_MRMC(d))
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                             non-convergent   example
#'#========================================================================================
#'
#'
#' v  <- v_truth_creator_for_many_readers_MRMC_data(M=3,Q=7)
#' m  <- mu_truth_creator_for_many_readers_MRMC_data(M=3,Q=7)
#' d  <- create_dataList_MRMC(mu.truth = m,v.truth = v)
#' fit_GUI_Shiny_MRMC(DF=extract_data_frame_from_dataList_MRMC(d),seed.initial.of.MCMC = 23)
#'
#'
#'
#'


#'}### Only run examples in interactive R sessions
#'
#'
#'}
#'

fit_GUI_Shiny_MRMC <- function(
  # Initial data -----
  DF=data.frame(
    m=as.integer(BayesianFROC::dd$m),
    q=as.integer(BayesianFROC::dd$q),
    c=as.integer(BayesianFROC::dd$c),
                h=as.integer(BayesianFROC::dd$h),
                f=as.integer(BayesianFROC::dd$f)


  ),
# DF = dd,
  NL.max=1111,
  NI.max=1111,
#NL.initial ----
  NL.initial = 142,
NI.initial = 199,
seed.initial.of.MCMC=237410,
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
  ui <-   shiny::navbarPage("Emplooooo\\(^o^)/oooooooy   me! :'-D",




                            shiny::tabPanel(":)  :-'D",



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

shiny::titlePanel(" FROC Analysis by Issei Tsunoda "),

shiny::h4(shiny::helpText("Change Data, then estimates and plotted curves are fitted accordingly. Enjoy fitting the FROC model to various datasets! Cheers! Pretty crowd!")),

# shiny::fluidRow(
# column(6,        # shiny::h2(" FROC Data"),


#checkbox vectors of modalty ID, reader ID-------
shiny::absolutePanel(        draggable = TRUE, style ="red",fixed=TRUE,
                             shiny::h1("Specify IDs"),
                             shiny::h6(shiny::helpText("Specified IDs will be plotted")),
                             shiny::uiOutput("checkbox_GUI_Readers"),
                             shiny::textOutput("txt_user_specifies_reader_IDs"),
                             shiny::uiOutput("checkbox_GUI_Modality"),
                             shiny::textOutput("txt_user_specifies_Modality_IDs"),
),





# Data -----
shiny::absolutePanel(        draggable = TRUE, style ="red",fixed=TRUE,
                     #  top = "100%",
                     # left = 1,
                     # right = 1,
                     # bottom = 1,
                     # width = 1,
                     # height = 1,

                     shiny::h1("Data"),



                     shiny::sliderInput("Number_of_lesions",
                                        "Number of lesions:",
                                        min =  1, max = NL.max,
                                        value = NL.initial# Default
                     ),


                     shiny::sliderInput("Number_of_images",
                                        "Number of images:",
                                        min = 1, max = NI.max,
                                        value = NI.initial#Default
                     ),

                     shiny::h4(shiny::helpText(  "Right-click on the table to delete/insert rows." )),
                     shiny::h4(shiny::helpText(    "Double-click on a cell to edit")),
                     shiny::h6(shiny::helpText(" h = hit = True Positive = TP.")),
                     shiny::h6(shiny::helpText(" f = false alarms = False Positive = FP.")),

                     shiny::h6(shiny::helpText(" m = modality ID")),
                     shiny::h6(shiny::helpText(" q = reader ID")),
                     shiny::h6(shiny::helpText(" c = confidence level")),


# data GUI Handsontable-----
                     rhandsontable::rHandsontableOutput("data_frame"),# Table of h and f


                     shiny::h6(shiny::helpText("To edit more than 2 cells, a blank cell would help"))




                     # shiny::br()




                                            ), #absolutePanel




#MCMC param ------

shiny::absolutePanel(        draggable = TRUE, style ="red",fixed=TRUE,
                             #  top = "100%",
                             # left = 1,
                             # right = 1,
                             # bottom = 1,
                             # width = 1,
                             # height = 1,


                             shiny::h4("Parameter of the HMC"),

                             shiny::h4(shiny::helpText(" Larger is better.")),
#Number_of_MCMC_samples-----
                             shiny::sliderInput("Number_of_MCMC_samples",
                                                "Number of MCMC samples:",
                                                min = 111, max = 11111, value = 111),

                             shiny::h4(shiny::helpText(" Larger is better.")),

                             shiny::sliderInput("Number_of_MCMC_chains",
                                                "Number of MCMC chains:",
                                                min = 1, max = MCMC.chains.max, value = 1),


# MCMC seed -----

                             shiny::h4(shiny::helpText("The number is seed.")),

                             shiny::sliderInput("Number_of_MCMC_seed",
                                                "seed to be passed to rstan::sampling():",
                                                min = 1, max = 99999999, value = seed.initial.of.MCMC)



), #absolutePanel











                                            shiny::sidebarLayout(
                                              shiny::sidebarPanel(width = 5,


                                                                  # wellPanel(
                                                                  # shiny::h3("Trace plot"),
                                                                  #                         radioButtons("stan_trace", "Trace plot", c("TRUE", "FALSE"))
                                                                  #                       ),






                                                                  # shiny::plotOutput("DrawCurves" ),



 shiny::h1("Fitted curves"),
                                                                  shiny::checkboxInput("DrawCFPCTP",
                                                                                       "FPF and TPF (Data)",
                                                                                       value = TRUE),

                                                                  shiny::checkboxInput("DrawFROCcurve",
                                                                                       "FROC curve (Fitted model)",
                                                                                       value = TRUE),

                                                                  shiny::checkboxInput("DrawAFROCcurve",
                                                                                       "AFROC curve (used for AUC) (Fitted model)",
                                                                                       value = FALSE),
# Curve Plot ----
 shiny::plotOutput("DrawCurves", dblclick = shiny::dblclickOpts(id = "plot_dbl_click")),




# Curve Plot Empirical----
      # shiny::h6(shiny::helpText("Empirical curves")),
shiny::h1("Empirical Curves"),

shiny::plotOutput("DrawEmpiricalCurves", dblclick = shiny::dblclickOpts(id = "plot_dbl_click")),







#
#                 shiny::em( "Modality ID"),
#                 shiny::h6(shiny::helpText("Specify the modality ID to be drawn")),
#                 shiny::checkboxInput("m1",
#                                      "1st",
#                                      value = TRUE),
#
#
#
#
#
#
#
#
#
# shiny::checkboxInput("m2",
#                      "2nd",
#                      value = FALSE),
#
#
# shiny::checkboxInput("m3",
#                      "3st",
#                      value = FALSE),
#
#
# shiny::checkboxInput("m4",
#                      "4nd",
#                      value = FALSE),
#
#
# shiny::checkboxInput("m5",
#                      "5st",
#                      value = FALSE),
#
#
# shiny::em( "Reader ID"),
# shiny::h6(shiny::helpText("Specify the Reader ID to be drawn")),
# shiny::checkboxInput("q1",
#                      "1st",
#                      value = TRUE),
#
# shiny::checkboxInput("q2",
#                      "2nd",
#                      value = FALSE),
#
#
# shiny::checkboxInput("q3",
#                      "3rd",
#                      value = FALSE),
#
#
#
# shiny::checkboxInput("q4",
#                      "4st",
#                      value = FALSE),
#
#
# shiny::checkboxInput("q5",
#                      "5st",
#                      value = FALSE),
#
#
#





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






# save ----
                  shiny::h1("Save a fitted model object"),


                  shiny::actionButton("trigger_save_a_fitted_model_object","Save a fitted model object in Desktop"),
                  shiny::actionButton("trigger_save_a_fitted_model_object_working_directory","Save a fitted model object in Working directory"),

                  shiny::h6(shiny::helpText("* Put resulting file \"fit\" which has no extension in working directory of R and execute the code"),       "load(\"fit\")",   shiny::helpText(" from R console, then an R object named fit is available from R console. To change the S4 class to stanfit, use fitt <- methods::as(fit, \"stanfit\")")),





                                                                  shiny::h4("Estimates"),
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



                                                               shiny::wellPanel(
                                                                 shiny::h1("Statistics"),
                                                                 shiny::verbatimTextOutput("AUC_ranking_print"),
# AUC Plot ----
                     shiny::plotOutput("AUC_CI", dblclick = shiny::dblclickOpts(id = "plot_dbl_click")),

# trace_AUC ----
                     shiny::plotOutput("trace_AUC", dblclick = shiny::dblclickOpts(id = "plot_dbl_click")),




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
                                                                 shiny::h6(shiny::helpText("It requires time."))
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

                                                               shiny::checkboxInput("Colour_plot_of_rates",
                                                                                    "Colour",
                                                                                    value = TRUE),



                                                               shiny::checkboxInput("dark_theme",
                                                                                    "dark theme",
                                                                                    value = TRUE),





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
                                                                 )



                                                               ),#wellPanel

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
                                                               shiny::h4(shiny::helpText("It requires time.")),

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

                                            shiny::h4(shiny::helpText("  Issei Tsunoda (2019): Pre-print;  Bayesian Models for  Free - Response Receiver Operating Characteristic Analysis.")),
                                            shiny::h4(shiny::helpText("  Dev Chakraborty (1989) doi:10.1118/1.596358; Maximum likelihood analysis of free - response receiver operating characteristic (FROC) data.")),
                                            shiny::h4(shiny::helpText(" Dev Chakraborty; Observer Performance Methods for Diagnostic Imaging, Foundations, Modeling, and Applications with R-Based Examples "))



                            ),#tabPanel

                            shiny::tabPanel("stanmodel",

                                            shiny::verbatimTextOutput("stanmodel_print"),

                                            shiny::h4(shiny::helpText(" When we wash doggy, Never use the washer for doggy.
                                                                          Doggy does not require to take a buth,
                                                                          so they need only warm water only,
                                                                         do not use the sarfactant for them!!
                                                                         He die, he was happy.
                                                                         Liebriya was no longer in this world.
                                                                         He had gone.
                                                                         The author cannot play with him forever.
                                                                         But Ruikoboch is still fine, he is not so yunger man. ")),
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
                                                                         in which I study how to fight to ghost. My studenta are the Ghostbusters.")),
                                            shiny::h4(shiny::helpText("Best regards,  ")),
                                            shiny::h4(shiny::a(  "The Author",     href="https://cran.r-project.org/package=BayesianFROC"))




                            ),#tabPanel




                            shiny::tabPanel("Acknowleage",
                                            shiny::h4(shiny::helpText("This package is made with helps of many unknown people in stack over flows, and the author appreciate their kind helps. Also Stan developer gives me quick and appropriate advices in many times. Such many people helps me to develop this package. So, Michael Jackson says you are not alone!")),
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


#_____________________________------
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
                                   c=DF$c,
                                   m=DF$m,
                                   q=DF$q,
                                   M=max(DF$m),
                                   Q=max(DF$q),
                                   C=max(DF$c)
      )

    })








#Check boxbbbbbbbbbbbbbbbbbb -----
    output$checkbox_GUI_Readers <- shiny::renderUI({
      shiny::checkboxGroupInput("R_object_as_the_result_of_check_boxes_for_reader",
                         "Select Reader ID",
                         choices = seq(1:max(values[["dataList"]]$q)),
                         select =c(1,max(values[["dataList"]]$q)) # This specify the initial condition
                         )
    })



    output$txt_user_specifies_reader_IDs <- shiny::renderText({
      icons <- paste(input$R_object_as_the_result_of_check_boxes_for_reader, collapse = ", ")
      paste("Selected reader IDs:", icons)
    })




    output$checkbox_GUI_Modality <- shiny::renderUI({
     shiny::checkboxGroupInput("R_object_as_the_result_of_check_boxes_for_modality",
                         "Select modality ID",
                         choices = seq(1:max(values[["dataList"]]$m)),
                         select =c(1,max(values[["dataList"]]$m)) # This specify the initial condition
      )
    })



    output$txt_user_specifies_Modality_IDs <- shiny::renderText({
      icons <- paste(input$R_object_as_the_result_of_check_boxes_for_modality, collapse = ", ")
      paste("Selected modality IDs:", icons)
    })











    # This is use such as fit() instesad of fit
    fit <- shiny::reactive({
# fit ----
      fit <- BayesianFROC::fit_Bayesian_FROC(

         # ite  = 111,
        print_CI_of_AUC = FALSE,
        ite  = input$Number_of_MCMC_samples,
        see = input$Number_of_MCMC_seed,
        cha =  input$Number_of_MCMC_chains,
        ModifiedPoisson = as.logical(input$FPF_per_Lesion),
        summary = TRUE,
        # Null.Hypothesis  = FALSE,
        dataList = values[["dataList"]],# input$selected_data ,
        DrawCurve = F
        # dig = 5,
        # prior = as.integer(input$prior)

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
    #   # saveRDS(finalDF, file=file.path(outdir, sprintf("%s.rds", outfilename)))
    #
    #
    #
    #
    #
    # })

# TeX data -----
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




    output$formula.model <- shiny::renderUI({
      # my_calculated_value <- extractAUC(fit(),dig = 4)[1]
      C<-values[["dataList"]]$C
      Q<-values[["dataList"]]$Q
      M<-values[["dataList"]]$M

      NL<-values[["dataList"]]$NL
      NI<-values[["dataList"]]$NI

      # h<-values[["dataList"]]$h
      # f<-values[["dataList"]]$f
      # c<-values[["dataList"]]$c
      d <-metadata_to_fit_MRMC(values[["dataList"]])
      h<-d$harray
      f<-d$farray

      # h_rev <- rev(h)
      # f_rev <- rev(f)


      z<-apply( extract(fit())$z , 2, mean)
      # p<-apply( extract(fit())$p , 2, mean)
      ppp<-apply( extract(fit())$ppp , c(2,3,4), mean)

      dl<-apply( extract(fit())$dl , 2, mean)

      m <- mean( extract(fit())$m)
      v <- mean( extract(fit())$v)
      c<-C:1
      s<-"Posterior Mean of Model Parameters: "
      for (cd in 1:C){
        s<-paste0(s," $$z_",cd," = ",signif(z[cd],digits = 3),",$$")
      }
      s<-paste0(s," $$\\sigma = ",signif(v,digits = 3),",$$")
      s<-paste0(s," $$\\mu = ",signif(m,digits = 3),",$$")

      s<-paste0(s," * In the following, the subscripts means level of confidence, modality ID, reader ID, respectively. ")


        for (qd in 1:Q){
          for (md in 1:M){
            for (cd in 1:C){
        s<-paste0(s," $$H_{",cd,",",md,",",qd,"} \\sim \\text{Binomial}(",signif(ppp[cd,md,qd],digits = 3),",", NL,"), \\text{ the realization is  } H_{",cd,",",md,",",qd,"} = ",h[C-cd+1,md,qd] ," .$$")
          }}
        }

      for (cd in 1:C){
        s<-paste0(s," $$F_{",cd,", m, r} \\sim \\text{Poisson}(",signif(dl[cd]*NI,digits = 3),"), \\text{for all } m = 1,...,",M,"  \\text{ and for all } r = 1,...,",Q,"$$")
      }



      for (qd in 1:Q){
        for (md in 1:M){
          for (cd in 1:C){
            s<-paste0(s," $$F_{",cd,",",md,",",qd,"} \\sim \\text{Poisson}(",signif(dl[cd]*NI,digits = 3),"), \\text{ the realization is  } F_{",cd,",",md,",",qd,"} = ",f[C-cd+1,md,qd] ," .$$")
          }}
      }

      shiny::withMathJax(s)
    })







# TeX ------
    output$formula.AUC <- shiny::renderUI({
      # C<-values[["dataList"]]$C
      # NL<-values[["dataList"]]$NL
      # NI<-values[["dataList"]]$NI
      C<-values[["dataList"]]$C
      Q<-values[["dataList"]]$Q
      M<-values[["dataList"]]$M
      # z<-apply( extract(fit())$z , 2, mean)
      # p<-apply( extract(fit())$p , 2, mean)
      # dl<-apply( extract(fit())$dl , 2, mean)
      #
      # m <- mean( extract(fit())$m)
      # v <- mean( extract(fit())$v)

      # a <- mean( extract(fit())$a)
      # b <- mean( extract(fit())$b)
      a<-apply( extract(fit())$aa , c(2,3), mean)
      b<-apply( extract(fit())$bb , c(2,3), mean)

      a <- signif(a,digits = 3)
      b <- signif(b,digits = 3)
      # bb <- signif(b^2,digits = 3)


      AA<-apply( extract(fit())$AA , c(2,3), mean)

      AA <- signif(AA,digits = 3)
      s<-"* In the following, the Posterior Mean of AUC for each modality and each reader are calculated. "

      # c<-C:1
      for (qd in 1:Q){
        for (md in 1:M){
      s<-paste0(s," $$\\text{AUC}_{",md,",",qd,"}= \\Phi (\\frac{\\hat{a}_{",md,",",qd,"}}{\\sqrt{1+\\hat{b_{",md,",",qd,"}}^2}})=\\Phi (\\frac{",a[md,qd],"}{\\sqrt{1+",b[md,qd],"^2}}) = ",AA[md,qd],"$$ where estimates are posterior mean.")
        }}




      A<-apply( extract(fit())$A , 2, mean)

      A <- signif(A,digits = 3)
      s<- paste0(s,"Posterior Mean of AUC for each modality")


      # c<-C:1
        for (md in 1:M){
          s<-paste0(s," $$\\text{AUC}_{",md,"}= \\frac{1}{",Q,"}   \\Sigma_{r=1}^{",Q,"} AUC_{",md,",r} = ",A[md],"$$ where estimates are posterior mean.")
        }

# AUC TeX -------

        shiny::withMathJax(s)
    })


# TeX ------

    output$formula.WAIC <- shiny::renderUI({



      WAIC <- fit()@WAIC
      WAIC <- signif(WAIC,digits = 3)

      s<-paste0(" $$\\text{WAIC}  = ",WAIC,".$$")
      shiny::withMathJax(s)
    })

# TeX ------

    output$formula.chisquare <- shiny::renderUI({



      chisquare <- fit()@chisquare
      chisquare <- signif(chisquare,digits = 3)

      s<-paste0(" $$\\chi^2(\\text{data}|\\widehat{\\theta})   = ",chisquare,"$$ where estimates  \\( \\widehat{ \\theta }\\) are at posterior mean of model parameters \\(  \\theta \\).")
      shiny::withMathJax(s)
    })




# TeX ------

    output$formula.TeX <- shiny::renderUI({
      s<-paste0(" To show \\( \\TeX \\), internnet environment is required.")
      shiny::withMathJax(s)
    })












    # output$formula.AUC.without.TeX <- shiny::renderUI({
    #   # C<-values[["dataList"]]$C
    #   # NL<-values[["dataList"]]$NL
    #   # NI<-values[["dataList"]]$NI
    #   #
    #   # z<-apply( extract(fit())$z , 2, mean)
    #   # p<-apply( extract(fit())$p , 2, mean)
    #   # dl<-apply( extract(fit())$dl , 2, mean)
    #   #
    #   # m <- mean( extract(fit())$m)
    #   # v <- mean( extract(fit())$v)
    #
    #   a <- mean( extract(fit())$a)
    #   b <- mean( extract(fit())$b)
    #   a <- signif(a,digits = 3)
    #   b <- signif(b,digits = 3)
    #   # bb <- signif(b^2,digits = 3)
    #
    #
    #
    #   A <- mean( extract(fit())$A)
    #   A <- signif(A,digits = 3)
    #
    #   # c<-C:1
    #
    #   s<-paste0("AUC =  ",A)
    #   shiny::withMathJax(s)
    # })



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



# ppp ------
    output$ppp <- shiny::renderUI({

      if (input$ppp_calculate_trigger) {


        ppp <- ppp(fit(),plot=FALSE)

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

      # Chi.Square.for.each.MCMC.samples   <-   chi_square_goodness_of_fit(fit()   )
      Chi.Square.for.each.MCMC.samples   <-   Chi_square_goodness_of_fit_in_case_of_MRMC_Posterior_Mean(fit()   )

      m<-   mean(Chi.Square.for.each.MCMC.samples$chi.square)

      m <- signif(m,digits = 3)

      s<-paste0("Posterior mean of  chi square   = ",m)


      shiny::withMathJax(s)
    })




    # output$formula <- renderUI({
    #   my_calculated_value <- extractAUC(fit(),dig = 4)[1]
    #   withMathJax(paste0("Posterior mean of AUC (area under the AFROC curve): $$\\widehat{AUC} =", my_calculated_value,"$$"))
    # })


    output$fit_print <- shiny::renderPrint({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL
      # if (sum(h)<=NL) {

        fitt <-methods::as(fit(), "stanfit")
        # print( fitt, digits = 4)
        a<- summary(fitt,probs=c(0.025,0.975))$summary
        print(a)
      # }# if
      #
      # if (sum(h)>NL) { print("Data format error, number of hits never greater than the number of signals. In Radiological context, signal is lesion.")  }

    })# shiny::renderPrint












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

          if ( check_hit_is_less_than_NL(values[["dataList"]])      ) {

            fit <- fit()
            message("Now, we plot trace of MCMC ....")
            return(rstan::stan_trace(fit,pars = c("lp__")))

          }

          if ( !check_hit_is_less_than_NL(values[["dataList"]])      ) {
            error_message(h,NL)
          }#if
        }

      }else  if (!input$trigger_stan_trace_plot||!floor(length(counter$s) /2)==length(counter$s) /2)  {
        message("Now, we omit trace of MCMC ....")
        return( plot(0,0,type="n", axes = TRUE,xlim=c(0,1),ylim =c(0,1),xaxt="n", yaxt="n",xlab="",ylab="",main="")
        )

      }

    })#shiny::renderPlot 2020 march 12



# hist ------
    output$plot_hist <- shiny::renderPlot({
      if (input$trigger_stan_hist_plot) {

        h<-values[["dataList"]]$h
        NL<-values[["dataList"]]$NL

        print(paste(  "ss = ", length(counter$ss) )  )
        if(floor(length(counter$ss) /2)==length(counter$ss) /2){

          if ( check_hit_is_less_than_NL(values[["dataList"]])      ) {


            fit <- fit()
            message("Now, we plot trace of MCMC ....")
            return(rstan::stan_hist (fit,pars = c("A"),bins=input$hist_bins))

          }

          if (! check_hit_is_less_than_NL(values[["dataList"]])      ) {

            # error_message(h,NL)


            plot(0,0,xlim=c(0,1),ylim =c(0,1),xaxt="n", yaxt="n",xlab="Please fix inconsistent data",ylab="",main="Error:   Inconsistent Data \n In baseball game, \n batter's number of hits can not  be greater than his number of at-bats")
            graphics::text(0.5,0.8,c("*Now, Sum of the number of hits is greater than that of lesion; \n\n", expression(paste(h1+h2+h3+... , "         >         Number of Lesions")) ),col="white",cex =    1.4  )


          }#if
        }

      }else  if (!input$trigger_stan_trace_plot||!floor(length(counter$ss) /2)==length(counter$ss) /2)  {
        message("Now, we omit trace of MCMC ....")
        return( plot(0,0,type="n", axes = TRUE,xlim=c(0,1),ylim =c(0,1),xaxt="n", yaxt="n",xlab="",ylab="",main="")
        )

      }

    })#shiny::renderPlot


























    output$stanmodel_print <- shiny::renderPrint({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL
      # if (sum(h)<=NL) {

        # print(prior_print_srsc(input$prior))
        print(fit()@stanmodel)

      # }# if
      #
      # if (sum(h)>NL) { print("Data format error, number of hits never greater than the number of signals. In Radiological context, signal is lesion.")  }

    })# shiny::renderPrint







    output$prior_print <- shiny::renderPrint({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL
      # if (sum(h)<=NL) {

        # print(prior_print_srsc(input$prior))
        prior_print_srsc(input$prior)

      # }# if
      #
      # if (sum(h)>NL) { print("Data format error, number of hits never greater than the number of signals. In Radiological context, signal is lesion.")  }

    })# shiny::renderPrint


    # max_Rhat_print -----
    output$max_Rhat_print <- shiny::renderPrint({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL
      # if (sum(h)<=NL) {
        fit <-fit()
        fitt <- methods::as(fit,"stanfit")

        max.rhat <-  round( max(summary(fitt)$summary[,"Rhat"]) ,digits = 5)

        print(paste("max R-hat = ", max.rhat))





      # }# if

      # if (sum(h)>NL) { print("Data format error, number of hits never greater than the number of signals. In Radiological context, signal is lesion.")  }

    })# shiny::renderPrint


# divergent_iterations_print -----
    output$divergent_iterations_print <- shiny::renderPrint({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL
      # if (sum(h)<=NL) {
        fit <-fit()
        fitt <- methods::as(fit,"stanfit")

        divergent <- rstan::get_divergent_iterations(fitt)
        n <- sum(divergent)
        N <- length(divergent)


        if(!n==0) print(paste( n, " of ", N, "iterations ended with a divergence.",n,"/",N,"=", round( n/N ,digits = 2)))
        if(n==0) print(paste( ":) OK"))


      # }# if

      # if (sum(h)>NL) { print("Data format error, number of hits never greater than the number of signals. In Radiological context, signal is lesion.")  }

    })# shiny::renderPrint












# treedepth_print -----
    output$treedepth_print <- shiny::renderPrint({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL
      # if (sum(h)<=NL) {
        fit <-fit()
        fitt <- methods::as(fit,"stanfit")

        # max_depth <- rstan:::get_treedepth_threshold(fitt)
        treedepths <- rstan::get_max_treedepth_iterations(fitt)
        n <- sum(treedepths)
        N <- length(treedepths)


        # if(!n==0) print(paste( n, " of ", N, "iterations saturated the maximum tree depth of.", max_depth,"."))
        # if(n==0) print(paste( ":) OK"))
        print("Not done ........... because get_treedepth_threshold is not an exported object from 'namespace:rstan' and hence the author omitted the evaluation from here. ")
      # }# if

      # if (sum(h)>NL) { print("Data format error, number of hits never greater than the number of signals. In Radiological context, signal is lesion.")  }

    })# shiny::renderPrint










# check_energy_print -----
    output$check_energy_print <- shiny::renderPrint({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL
      # if (sum(h)<=NL) {
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



      # }# if
      #
      # if (sum(h)>NL) { print("Data format error, number of hits never greater than the number of signals. In Radiological context, signal is lesion.")  }

    })# shiny::renderPrint






# save a fitted model in Desktop ----
    shiny::observeEvent(input$trigger_save_a_fitted_model_object,{

      fit <- fit()
      save( fit,file =paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"\\fit") )
      # save( fit,file ="fit" )


      tcltk::tkmessageBox(message=paste("\n* A file (name: \"fit\") is created in Desktop. In the file, an fitted model R object (name \"fit\")  is contained, which is an object of an S4 class (stanfitExtended)\n\n\n 1) Put the resulting file \"fit\" in the working directory,\n 2) Run an R script \n                                     load(\"fit\") \n\n then the R object named \"fit\" is available on your R console. "))

    }  )


# save a fitted model in  Working directory ----

    shiny::observeEvent(input$trigger_save_a_fitted_model_object_working_directory,{

      fit <- fit()
      save( fit,file ="fit" )
      tcltk::tkmessageBox(message=paste("\n* A file (name: \"fit\") is created in the current working directory. In the file, an fitted model R object (name \"fit\")  is contained, which is an object of an S4 class (stanfitExtended)\n\n\n 1) Put the resulting file \"fit\" in the working directory,\n 2) Run an R script \n                                     load(\"fit\") \n\n then the R object named \"fit\" is available on your R console. "))

    }  )




# AUC_ranking_print -----
    output$AUC_ranking_print <- shiny::renderPrint({
      # h<-values[["dataList"]]$h
      # NL<-values[["dataList"]]$NL
      # if (sum(h)<=NL) {

        # fitt <-methods::as(fit(), "stanfit")
        # print( fitt, digits = 4)
        # a<- summary(fitt,probs=c(0.025,0.975))$summary

        # print(sortAUC(fit()))
      sortAUC(fit())

      # }# if

      # if (sum(h)>NL) { print("Data format error, number of hits never greater than the number of signals. In Radiological context, signal is lesion.")  }

    })# shiny::renderPrint















# Plor curve -----
    output$DrawCurves <- shiny::renderPlot({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL
      M<-values[["dataList"]]$M
      Q<-values[["dataList"]]$Q
      # if (sum(h)<=NL) {
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




      if(check_hit_is_less_than_NL(values[["dataList"]] )  ){

        DrawCurves(fit(),
                   # Colour  = FALSE,
                   Colour = input$dark_theme,
                   readerID    = as.integer(input$R_object_as_the_result_of_check_boxes_for_reader),
                   modalityID  = as.integer(input$R_object_as_the_result_of_check_boxes_for_modality),
                   new.imaging.device  = FALSE,
                   DrawCFPCTP     = input$DrawCFPCTP,
                   DrawFROCcurve  = input$DrawFROCcurve,
                   DrawAFROCcurve = input$DrawAFROCcurve)
      # }
      }


if(!check_hit_is_less_than_NL(values[["dataList"]] )  ){
        # if (sum(h)> NL) {

#
#         h.string <- as.character(h)
#         for (cd  in 1:length(h.string)) {
#           if (cd==1){ s<-""; s <- paste(h.string[cd],sep = "+")}
#           if ( !cd==1)s <- paste(s,h.string[cd],sep = "+")
#
#         }#for
#         sum.of.h <- s
#         sum.of.h <- paste(sum.of.h,"=",as.character( sum(h) ) )
#
        plot(0,0,xlim=c(0,1),ylim =c(0,1),xaxt="n", yaxt="n",xlab="Please fix inconsistent data",ylab="",main="Error:   Inconsistent Data \n In baseball game, \n batter's number of hits can not  be greater than his number of at-bats")
#
        graphics::text(0.5,0.8,c("*Now, Sum of the number of hits is greater than that of lesion; \n\n", expression(paste(h1+h2+h3+... , "         >         Number of Lesions")) ),col="white",cex =    1.4  )
#         graphics::text(0.5,0.65,paste("In the current inputed data, it is the following: " ),col="black",cex =  1.5  )
#         graphics::text(0.5,0.5,paste(  sum.of.h , "      >       ",NL,sep = ""),col="red",cex = 2)
#
#         graphics::text(0.5,0.3,c("* Please fix so that the following inequality holds \n", expression(paste(h1+h2+h3+... , "       <        Number of lesions"))),col="blue",cex =  1.5    )
#         graphics::text(0.5,0.1,c("* Shoud decrease the number of hits  or \n  Shoud increase the number of lesions"),col="blue",cex = 1.5  )
#
#
#
}

      # }#if

    })#shiny::renderPlot













# plot  curve Empirical-----

# Plor curve -----
    output$DrawEmpiricalCurves <- shiny::renderPlot({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL
      M<-values[["dataList"]]$M
      Q<-values[["dataList"]]$Q
      # if (sum(h)<=NL) {
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




      if(check_hit_is_less_than_NL(values[["dataList"]] )  ){

        plot_empirical_FROC_curves(fit()@dataList,
                                   ModifiedPoisson = as.logical(input$FPF_per_Lesion),
                   readerID    = as.integer(input$R_object_as_the_result_of_check_boxes_for_reader),
                   modalityID  = as.integer(input$R_object_as_the_result_of_check_boxes_for_modality),
    )
        # }
      }


      if(!check_hit_is_less_than_NL(values[["dataList"]] )  ){
        # if (sum(h)> NL) {

        #
        #         h.string <- as.character(h)
        #         for (cd  in 1:length(h.string)) {
        #           if (cd==1){ s<-""; s <- paste(h.string[cd],sep = "+")}
        #           if ( !cd==1)s <- paste(s,h.string[cd],sep = "+")
        #
        #         }#for
        #         sum.of.h <- s
        #         sum.of.h <- paste(sum.of.h,"=",as.character( sum(h) ) )
        #
        plot(0,0,xlim=c(0,1),ylim =c(0,1),xaxt="n", yaxt="n",xlab="Please fix inconsistent data",ylab="",main="Error:   Inconsistent Data \n In baseball game, \n batter's number of hits can not  be greater than his number of at-bats")
        #
        graphics::text(0.5,0.8,c("*Now, Sum of the number of hits is greater than that of lesion; \n\n", expression(paste(h1+h2+h3+... , "         >         Number of Lesions")) ),col="white",cex =    1.4  )
        #         graphics::text(0.5,0.65,paste("In the current inputed data, it is the following: " ),col="black",cex =  1.5  )
        #         graphics::text(0.5,0.5,paste(  sum.of.h , "      >       ",NL,sep = ""),col="red",cex = 2)
        #
        #         graphics::text(0.5,0.3,c("* Please fix so that the following inequality holds \n", expression(paste(h1+h2+h3+... , "       <        Number of lesions"))),col="blue",cex =  1.5    )
        #         graphics::text(0.5,0.1,c("* Shoud decrease the number of hits  or \n  Shoud increase the number of lesions"),col="blue",cex = 1.5  )
        #
        #
        #
      }

      # }#if

    })#shiny::renderPlot

















# AUC plot -----
    output$AUC_CI <- shiny::renderPlot({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL
      M<-values[["dataList"]]$M
      Q<-values[["dataList"]]$Q
      # if (sum(h)<=NL) {
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


      nnname <- vector()
      for (md in 1:M) {
        nnname[md] <-paste("A[",md,"]",sep = "")
      }

      print( rstan::plot(methods::as(fit(), "stanfit"),par=nnname))
# extractAUC( fit() ,print_CI_of_AUC = TRUE, new.imaging.device = FALSE)


    })#shiny::renderPlot













#  trace plot for AUC trace_AUC -----
    output$trace_AUC <- shiny::renderPlot({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL
      M<-values[["dataList"]]$M
      Q<-values[["dataList"]]$Q
      # if (sum(h)<=NL) {
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


      nnname <- vector()
      for (md in 1:M) {
        nnname[md] <-paste("A[",md,"]",sep = "")
      }

      print( rstan::stan_trace(methods::as(fit(), "stanfit"),par=nnname))
      # extractAUC( fit() ,print_CI_of_AUC = TRUE, new.imaging.device = FALSE)


    })#shiny::renderPlot
















  }

  ## run app
  shiny::runApp(list(ui=ui, server=server))
  return(invisible())
}
