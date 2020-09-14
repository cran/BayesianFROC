#'
# @title ----
#' @title Fit a model with GUI of Shiny
#' @description A graphical user interface (GUI) to fit a model to data.
# @ details ----
#' @details  First, please execute, then user will understand what it is.
#' This function is the one of the most important function in this package.
#' I do not assume that the user is familiar with R script but FROC analysis.
#'  So, I made this function to provide
#'   the Graphical User Interface (GUI) for users to avoid CUI (Characteristic User Interface).
#'  The GUI is made by the \pkg{shiny} package.
# @param -----
#' @param DF A dataframe as an initial data to be fitted a model
#' @param NL.min min number of bins indicating the minimal number in which the number of lesions can move
#' @param NL.max max number of bins indicating the maximal number in which the number of lesions can move
#' @param NI.max max number of bins indicating the maximal number in which the number of imagegs can move
#' @param NL.initial Natural number indicating the initial number of lesions, Default value =259.
#' @param NI.initial Natural number indicating the initial number of images, Default value =57
#' @param MCMC_initial_sample_size Natural number indicating the initial number of MCMC samplings, Default value =333
#' @param MCMC_initial_seed Natural number indicating the initial number of MCMC samplings, Default value =333
#' @param MCMC_initial_chains Natural number indicating the initial number of MCMC samplings, Default value =333
#' @param DF_NL A data-frame, consisting of a positive number representing the number of lesions
#' @param DF_NI A data-frame, consisting of a positive number representing the number of images
#' @param width_of_data_input_panel width of data panel
#' @param MCMC.chains.max max number of bins indicating number of MCMC chains
#' @param min_MCMC_initial_sample_size Natural number indicating the initial minimum number of MCMC samplings, Default value =333
#' @param max_MCMC_initial_sample_size Natural number indicating the initial maximal number of MCMC samplings, Default value =333
#' @param seed.MCMC.max Natural number indicating the initial possible maximal seed of MCMC samplings, Default value =111111
#'@author Issei Tsunoda
#' @return None
#' @export
#'
#' @examples
#'
#'## Only run examples in interactive R sessions
#'if (interactive()) {
#'#========================================================================================
#'#            1)           Use the default User Interface
#'#========================================================================================
#'#'
#'
#'#No need to consider the variables, it is sufficient in  default values.
#'
#'
#'  fit_GUI_Shiny()
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
#'
#'
#'
#'
#'
#'#========================================================================================
#'#            6) Change maximul possible number of chains
#'#========================================================================================
#'
#'
#'# We can generate at most 8 chains in MCMC sampling
#'
#'
#'     fit_GUI_Shiny(  MCMC.chains.max = 8 )
#'
#'
#'
#'
#'
#'
#'}### Only run examples in interactive R sessions
#'

fit_GUI_Shiny <- function(
# Initial data -----
  DF=data.frame(h=c( 97L,   32L,   31L),
                f=c( 1L ,  14L,   74L )
  ),
  NL.min = 1,
  NL.max = 1111,
  NI.max = 1111,
width_of_data_input_panel = 555,

MCMC_initial_sample_size =333,
min_MCMC_initial_sample_size =111,
max_MCMC_initial_sample_size =11111,
seed.MCMC.max =111111,

MCMC_initial_seed =1,
MCMC_initial_chains =1,

  NL.initial = 259,
  NI.initial = 57,
DF_NL = data.frame(
  NL.initial = 259
),

DF_NI = data.frame(
  NI.initial = 57
),
# MCMC.samples.max = 11111,
  MCMC.chains.max= parallel::detectCores()

# counter=0


){

  # MCMCiterationReputation <- c("too small","small","good","best")

  outdir <- system.file("myapp", package = "BayesianFROC")
  outfilename <- "table"




  if (outdir == "") {
    stop("Could not find myapp. Try re-installing `BayesianFROC`.", call. = FALSE)
  }





# shiny::shinyUI(

# shiny::fluidPage(
 # ui ------
ui <- shiny::navbarPage("Now, I am a free, without roof, ",
      shiny::tabPanel(":) This programm cannot save my life :-'D",



# shiny::tags$head(
#   shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
## , shiny::tags$script(src= "color_change.js")
# ) ,# Color


shiny::tags$head(
  shinythemes::themeSelector(),


  # tags$button(id="trigger_save_a_fitted_model_object_rename",
  #             type="button",
  #             class="btn action-button btn-large btn-primary",
  #             HTML('<i class="icon-star"></i>Large button')),

   # css ----
  shiny::tags$style(shiny::HTML("
  /*Dragabble panel style heeeeeehaaaaaa*/
      .ui-draggable {
      z-index: 11;
      background-color:#B0A9A9;
font-size: 14pt;
border-top: 1px solid #991313; border-bottom: 5px solid 991313; border-radius: 23px; border: 3px solid white;
       }

/*Slider_input From here heeehaaaa*/
.irs-single {font-family: 'arial'; color:#B30E0E;    background:#E0D162;  font-size:17pt; }
.irs-slider {width: 65px; height: 60px; top: 35px; background:#870C0C;  border-radius: 100px;}

.irs-max {font-family: 'arial'; color: black;  background:#9E9898;  font-size: 14pt}
.irs-min {font-family: 'arial'; color: black; background:#9E9898;  font-size: 14pt}

.irs-bar {      width: 100%; height: 15px; background: #991313; border-top: 1px solid #2e6da4; border-bottom: 1px solid black; border-radius: 33px; border: 1px solid #2e6da4; }
.irs-bar-edge { width: 40px; height: 15px; background: #991313; border-top: 1px solid #2e6da4; border-bottom: 1px solid black; border-radius: 33px; border: 1px solid #2e6da4; }
.irs-line {                   height: 15px; border-radius: 33px;border: 1px solid black; }

.irs-grid-pol {display: none;}
/*Slider_input From here heeehaaaa*/




     .checkbox { /* checkbox is a div class*/
        line-height: 30px;
        margin-bottom: 40px; /*set the margin, so boxes don't overlap*/
         font-size:17pt;
         background:#C2C2C2;
         color:black;/* font color*/
         border-radius: 100px;

      }
      input[type='checkbox']{ /* style for checkboxes */
        width: 32px; /*Desired width*/
        height: 32px; /*Desired height*/
        line-height: 32px;

      }

      span {
          margin-left: 15px;  /*set the margin, so boxes don't overlap labels*/
          line-height: 20px;
      }

"))
# 337ab7  this is a defult like color of slider

  # .irs-bar {width: 100%; height: 25px; background: black; border-top: 1px solid black; border-bottom: 1px solid black;}
  # .irs-bar-edge {background: black; border: 1px solid black; height: 25px; border-radius: 0px; width: 20px;}
  # .irs-line {border: 1px solid black; height: 25px; border-radius: 0px;}
  # .irs-grid-text {font-size: 14pt; font-family: 'arial'; color: white; bottom: 17px; z-index: 1;}

  # .irs-grid-pol {display: none;}
  # .irs-max {font-family: 'arial'; color: black;}
  # .irs-min {font-family: 'arial'; color: black;}
  # .irs-line {border: 1px solid black; height: 25px; border-radius: 13px;width: 0px;}









  #
  # ,header=tags$head(tags$style(type='text/css', ".irs-grid-text { font-size: 200pt; }"))

),#taghead# cols <- colourpicker::colourPicker(5) "#CC5858"#FF8800 "#B0A9A9"   "#B8940263" "#70BBC7"   "#C29999"#F07878#E8906D9B#CCCCFF#9DCDD1#F26666  background-color:#CCCCCF;


shiny::titlePanel(" FROC Analysis by Issei Tsunoda, who is a xxxxx and an MCS patient in Japan. Above him only sky."),

shiny::h4(shiny::helpText(" My life is painful caused by syndet. I regret 3 years for the moment of using syndet in K.")),

# shiny::fluidRow(
# shiny::column(6,# shiny::h2(" FROC Data"),




shiny::absolutePanel(        draggable = TRUE, style ="red",fixed=TRUE,
                             shiny::h1("Options of Model"),

 # the traditional, classical model with multinomial ----
                     shiny::selectInput("multi_nomial", "The Classical model  vs the author's new models",
                                        c(
                                          "Classical, traditional model" = TRUE,
                                          "The author's new model" = FALSE
                                        )
                     ),
         # shiny::h4("Using WAIC, the author compares these two models, but the answer is difficult. If data have many zeros, then the author recommends the author's model."),
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


     "informative, Proper 4" = 4,
     "informative, Proper 3" = 3,
     "informative, Proper" = 2,
     "Non-informative, Proper by Gaussian distributions" = -1,
     "Non-informative, Improper by Unbounded Uniform distributions" = 0,
     "Non-informative, Proper by Uniform distributions" = 1
   )
),

  shiny::verbatimTextOutput("prior_print")



),#wellPanel
# Prior ----
shiny::uiOutput("UI_prior_w") ,
shiny::uiOutput("UI_prior_m") ,
shiny::uiOutput("UI_prior_v") ,
shiny::uiOutput("UI_prior_z") ,


shiny::numericInput("ww",
                    "Input ww",
                    value = 0# Default
),

shiny::numericInput("www",
                    "Input www",
                    value = 1# Default
),

shiny::numericInput("mmm",
                    "Input mmm",
                    value = 1# Default
),

shiny::numericInput("mm",
                    "Input mm",
                    value = 0# Default
),

shiny::numericInput("vvv",
                    "Input vvv",
                    value = 1# Default
),

shiny::numericInput("vv",
                    "Input vv",
                    value = 0# Default
),

shiny::numericInput("zzz",
                    "Input zzz",
                    value = 1# Default
),

shiny::numericInput("zz",
                    "Input zz",
                    value = 0# Default
),





),#absolutePanel


# HMC param -----
    shiny::absolutePanel(        draggable = TRUE, style ="red",fixed=TRUE,

    shiny::h1("Options of HMC"),
    shiny::h5("HMC: Hamiltonian Monte Carlo Sampling"),
  # HMCMC_input_format -----
# shiny::selectInput("MCMC_input_format", "Input format",
#                    c(
#                      "slide   (Recommended)" = TRUE,
#                      "input   (Oh! Shit! Good night!)" = FALSE
#                    )
# ),
shiny::selectInput("MCMC_input_format", "Input format",
                   c(
                     "slide       ( Fire! Great!)" = 1,
                     "input       ( bad! Holy Moly!)" = 2,
                     "table       ( Good! Amazing!)" =3
                   )
),

# shiny::h4(shiny::helpText(" Larger is better.")),

# shiny::sliderInput("Number_of_MCMC_samples",
#                    "Number of MCMC samples:",
#                    min = 111, max = 11111, value = 333),


# shiny::h3(  shiny::span(   shiny::textOutput("txt_Number_of_MCMC_samples"),  style="color:red")          ),
shiny::h3(   shiny::textOutput("txt_Number_of_MCMC_samples")       ),
# shiny::h3(  shiny::uiOutput("ui_of_Number_of_MCMC_samples")),
# shiny::h4(  shiny::strong(   shiny::span(     shiny::uiOutput("ui_of_Number_of_MCMC_samples") , style="color:red")  )    ),# Table of only a single cell
shiny::h3(  shiny::strong(   shiny::span(  shiny::uiOutput("ui_of_Number_of_MCMC_samples"),  style="text-align: center;color:#B30E0E; font-size:17pt;")   )      ), #Table of only a single cell


    shiny::h5("If R hat is large, then more MCMC iterations may help."),
    # shiny::h5("If data have many zeros, then such a treatment may be required."),


# shiny::h4(shiny::helpText(" Larger is better.")),

#     shiny::sliderInput("Number_of_MCMC_chains",
#                        "Number of MCMC chains:",
#                        min = 1, max = MCMC.chains.max, value = 1),
 ## seed ------
#     shiny::sliderInput("Seed_of_MCMC_chains",
#                        "Seed of MCMC chains:",
#                        min = 1, max = 1111111, value = 1)


shiny::actionButton("trigger_parallel",paste("Use",parallel::detectCores(), "CPU cores") , shiny::icon("paper-plane"),
                    style="color: #fff; background-color: #BD3E3E; border-color: #2e6da4" ),
shiny::textOutput("txt_CPU_cores"),


shiny::h3(shiny::textOutput("txt_Number_of_MCMC_chains")),
# shiny::textOutput("txt_Number_of_MCMC_chains"),
# shiny::h3(shiny::uiOutput("ui_of_Number_of_MCMC_chains")),
# shiny::h4(  shiny::strong(   shiny::span(     shiny::uiOutput("ui_of_Number_of_MCMC_chains") , style="color:red")    )  ),# Table of only a single cell
shiny::h3(  shiny::strong(   shiny::span(  shiny::uiOutput("ui_of_Number_of_MCMC_chains"),  style="text-align: center;color:#B30E0E; font-size:17pt;")   )      ), #Table of only a single cell


shiny::h3(shiny::textOutput("txt_Seed_of_MCMC_chains")),
# shiny::textOutput("txt_Seed_of_MCMC_chains"),
# shiny::h3(shiny::uiOutput("ui_of_Seed_of_MCMC_chains")),
# shiny::h4(  shiny::strong(   shiny::span(     shiny::uiOutput("ui_of_Seed_of_MCMC_chains") , style="color:red")   )   ),# Table of only a single cell
shiny::h3(  shiny::strong(   shiny::span(  shiny::uiOutput("ui_of_Seed_of_MCMC_chains"),  style="text-align: center;color:#B30E0E; font-size:17pt;")   )      ), #Table of only a single cell
# shiny::numericInput("Number_of_MCMC_chains",
#                     "Number of MCMC chains:",
#                     value = 1# Default
# ),
# shiny::numericInput("Seed_of_MCMC_chains",
#                     "Seed of MCMC chains:",
#                     value = 1# Default
# ),

shiny::h4(shiny::helpText("I love you!"))


    ),#absolutePanel



















  # Data -----
shiny::absolutePanel(        draggable = TRUE, style ="red",fixed=TRUE,
#  top = "100%",
# left = 1,
# right = 1,
# bottom = 1,
width = width_of_data_input_panel,
# height = 1,

               shiny::h1("Data"),
       shiny::h4(shiny::helpText(  " (to be fitted a model) " )),

   # Data_input_format -----
# shiny::selectInput("Data_input_format", "Input format",
#                    c(
#                      "slide" = TRUE,
#                      "input" = FALSE
#                    )
# ),
# Data_input_format -----
       shiny::selectInput("Data_input_format", "Input format",
                          c(
                            "slide       (fire!)" = 1,
                            "input       (bad!)" = 2,
                            "table       (good!)" =3
                          )
       ),
               shiny::h4(shiny::helpText(  "Right-click on the table to delete/insert rows." )),
               shiny::h4(shiny::helpText(    "Double-click on a cell to edit")),
# shiny::h6(shiny::helpText(" h = hit = True Positive = TP.")),
# shiny::h6(shiny::helpText(" f = false alarms = False Positive = FP.")),

# TABLE data Handsontable-----
shiny::h4( shiny::strong(  shiny::span(     rhandsontable::rHandsontableOutput("data_frame") ,
style="text-align: center;color:#B30E0E; font-size:17pt;")  )    ),# Table of h and f


# shiny::h6(shiny::helpText("To edit more than 2 cells, a blank cell would help")),

# shiny::sliderInput("Number_of_lesions",
#                    "Number of lesions:",
#                    min =  1, max = NL.max,
#                    value = NL.initial# Default
# ),



# shiny::numericInput("Number_of_lesions",
#                    "Number of lesions:",
#                    value = NL.initial# Default
# ),
# shiny::numericInput("Number_of_images",
#                     "Number of images:",
#                     value = NI.initial# Default
# ),


#
#                shiny::sliderInput("Number_of_images",
#                                   "Number of images:",
#                                   min = 1, max = NI.max,
#                                   value = 57#Default
#                ),
shiny::h3(shiny::textOutput("txt_Number_of_lesions")   ),
shiny::h3(  shiny::strong(   shiny::span(  shiny::uiOutput("ui_of_Number_lesions"),  style="text-align: center;color:#B30E0E; font-size:17pt;")   )      ),
# shiny::h3(  shiny::uiOutput("ui_of_Number_lesions")),
shiny::h3(shiny::textOutput("txt_Number_of_images")),
shiny::h3(   shiny::strong(  shiny::span(  shiny::uiOutput("ui_of_Number_images"),  style="text-align: center;color:#B30E0E; font-size:17pt;")    )      ),

# shiny::h3(  shiny::uiOutput("ui_of_Number_images")),


# shiny::h6(shiny::helpText(" Press S in key board to decrease only one")),
# shiny::h6(shiny::helpText(" Press  w in key board to increase only one")),
       shiny::h6(shiny::helpText(" Ich leibe dich!"))

# shiny::br()




),#absolutePanel

shiny::sidebarLayout(
shiny::sidebarPanel(width = 5,


# wellPanel(
# shiny::h3("Trace plot"),
#                         radioButtons("stan_trace", "Trace plot", c("TRUE", "FALSE"))
#                       ),






# shiny::plotOutput("DrawCurves" ),



# Visualizations of Data and Model----
# PLOT curves ----
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

shiny::downloadButton("download_Plot", "Download Image"),

# download_Plot oooooooooooooooooooooooooooooooooooooo----
# shiny::selectInput("extension_for_plot_save", "Choose a curve for save:",
#             choices =c(  "png", "jpeg", "bmp",
#                          "tiff",   "pdf")
#             ),



# check HMC diagnostics -----
shiny::wellPanel(
# divergent ----------------
shiny::h1("Check HMC diagnostics"),

shiny::h6("divergence"),
shiny::strong(  shiny::verbatimTextOutput("divergent_iterations_print")),
# shiny::h3(  shiny::strong(   shiny::span(  shiny::verbatimTextOutput("divergent_iterations_print"),  style="text-align: center;color:#B30E0E; font-size:22pt;")   )      ), #Table of only a single cell

# treedepth ----------------
shiny::h6("treedepth"),
# shiny::verbatimTextOutput("treedepth_print"),
# shiny::h3(  shiny::strong(   shiny::span(  shiny::verbatimTextOutput("treedepth_print"),  style="text-align: center;color:#B30E0E; font-size:17pt;")   )      ), #Table of only a single cell
shiny::strong(  shiny::verbatimTextOutput("treedepth_print")),

shiny::h6("E-BFMI values "),
# shiny::verbatimTextOutput("check_energy_print"),
# shiny::h3(  shiny::strong(   shiny::span(  shiny::verbatimTextOutput("check_energy_print"),  style="text-align: center;color:#B30E0E; font-size:17pt;")   )      ), #Table of only a single cell
shiny::strong(  shiny::verbatimTextOutput("check_energy_print")),

shiny::h6("R hat"),
# shiny::verbatimTextOutput("max_Rhat_print"),
# shiny::h3(  shiny::strong(   shiny::span(  shiny::verbatimTextOutput("max_Rhat_print"),  style="text-align: center;color:#B30E0E; font-size:17pt;")   )      ), #Table of only a single cell
shiny::strong(  shiny::verbatimTextOutput("max_Rhat_print")),


shiny::br()
),





#Save a fitted model object-----

shiny::h1("Save a fitted model object"),


shiny::actionButton("trigger_save_a_fitted_model_object","Save a fitted model object in Desktop" ,
                    shiny::icon("save"),
                    style="color: #fff; background-color: #BD3E3E; border-color: #2e6da4"),
# shiny::h5(shiny::helpText("* A file ", shiny::strong(" \"fit.Rda\" "), "will be created in Desktop and execute the following R code"),    shiny::code( shiny::strong(     "load(file =paste0(file.path(Sys.getenv(\"USERPROFILE\"),\"Desktop\"),\"\\\\fit.Rda\"))"  )),   shiny::helpText(" from the R console or (R studio console), then an R object named ", shiny::code( shiny::strong(" \"fit\" ")), "   is available on the R (or R studio) console. To change the S4 class to stanfit, use the following R code "),     shiny::code( shiny::strong(    "fitt <- methods::as(fit, \"stanfit\")" )) ,   shiny::helpText(" Then, the R object named ", shiny::code( shiny::strong(" \"fitt\" ")), "   is an R object of the S4 class stanfit. ")),
shiny::textInput("Name_of_file", "Enter a character as the name of the file name in which the fitted model object is saved:",
                 value =c(  "Name_Of_File"),
                 placeholder="Enter a character as a file name to save a fitted model object"
),

shiny::textInput("Name_of_fit", "Enter the name of fitted model object:",
                 value =c(  "Name_Of_Fitted_Model_object"),
                 placeholder="Enter a name of the fitted model object (without spaces) "

),
# UI save ---------
shiny::uiOutput("UI_save_Txt") ,
shiny::uiOutput("UI_save_Txt2") ,
shiny::uiOutput("UI_save_Txt3") ,
shiny::span( shiny::uiOutput("UI_save_Txt4"), style="text-align: center; color:#B30E0E; font-size:17pt;"),
shiny::uiOutput("UI_save_Txt5") ,
shiny::uiOutput("UI_save_Txt6") ,
shiny::uiOutput("UI_save_Txt7") ,
shiny::uiOutput("UI_save_Txt8") ,
shiny::uiOutput("UI_save_Txt9") ,
shiny::uiOutput("UI_save_Txt10") ,
shiny::uiOutput("UI_save_Txt11") ,

shiny::actionButton("trigger_save_a_fitted_model_object_working_directory","Save a fitted model object in Working directory",

                    shiny::icon("save"),
                    style="color: #fff; background-color: #BD3E3E; border-color: #2e6da4"
                    ),

shiny::h5(shiny::helpText("* A file ", shiny::strong(" \"fit.Rda\" "), " will be created in Working directory of R. Executing the following R code"),    shiny::code( shiny::strong(    "load(\"fit.Rda\")"  )),   shiny::helpText(" from the R console or (R studio console),  an R object named ", shiny::code( shiny::strong(" \"fit\" ")), "   is available on the R (or R studio) console. To change the S4 class to stanfit, use the following R code "),  shiny::code( shiny::strong(       "fitt <- methods::as(fit, \"stanfit\")" )),   shiny::helpText(" Then, the R object named ", shiny::code( shiny::strong(" \"fitt\" ")), "   is an R object of the S4 class stanfit. ")),



#Save a fitted model object RENAME -----

shiny::h1("Save a fitted model object, rename"),


shiny::actionButton("trigger_save_a_fitted_model_object_rename","Save a fitted model object in Desktop",
                    shiny::icon("save"),
                    style="color: #fff; background-color: #BD3E3E; border-color: #2e6da4"
),


shiny::h5(shiny::helpText("#  A file ", shiny::strong(" \"fit.Rds\" "), "  will be created in Desktop. Put the resulting file \"fit.Rds\" in Working directory. By executing the following R code"),  shiny::code( shiny::strong(  "my_favorite_name <- readRDS(file =paste0(file.path(Sys.getenv(\"USERPROFILE\"),\"Desktop\"),\"\\\\fit.Rds\"))"  )),   shiny::helpText(" from the R console or (R studio console),  an R object renamed ", shiny::code( shiny::strong(" \"my_favorite_name\" ")),    "  is available on the R (or R studio) console. To change the S4 class to stanfit, use the following R code "),    shiny::code( shiny::strong(     "fitt <- methods::as(my_favorite_name, \"stanfit\")")),   shiny::helpText(" Then, the R object named ", shiny::code( shiny::strong(" \"fitt\" ")), "   is an R object of the S4 class stanfit. ")),
# the result of cat("\\\\") is \\    2020 May 21



shiny::actionButton("trigger_save_a_fitted_model_object_rename_working_directory","Save a fitted model object in Working directory",
                    shiny::icon("save"),
                    style="color: #fff; background-color: #BD3E3E; border-color: #2e6da4"
),


shiny::h5(shiny::helpText("#  A file ", shiny::strong(" \"fit.Rds\" "), "  will be created in Working directory.  By executing the following R code"), shiny::code( shiny::strong(   "my_favorite_name <- readRDS(file =\"fit.Rds\")  " )) ,   shiny::helpText(" from the R console or (R studio console),  an R object named ", shiny::code( shiny::strong(" \"my_favorite_name\" ")), " is available on the R (or R studio) console. To change the S4 class to stanfit, use the following R code "),      shiny::code( shiny::strong(     "fitt <- methods::as(my_favorite_name, \"stanfit\")" )),   shiny::helpText(" Then, the R object named ", shiny::code( shiny::strong(" \"fitt\" ")), "   is an R object of the S4 class stanfit. ")),













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

# shiny::shiny::column(6,
shiny::mainPanel(width = 7,


shiny::plotOutput("bi_normal", dblclick = shiny::dblclickOpts(id = "plot_dbl_click")),
shiny::plotOutput("false_rate", dblclick = shiny::dblclickOpts(id = "plot_dbl_click")),
shiny::h6(shiny::helpText("The most right area corresponds the rate of the most high confidence.")),
shiny::plotOutput("trace_Plot", dblclick = shiny::dblclickOpts(id = "plot_dbl_click")),


# Statistics ------
shiny::wellPanel(
shiny::h1("Statistics"),

shiny::h6(shiny::helpText(" Larger is better.")),
# shiny::uiOutput("formula.AUC.without.TeX"),
shiny::h3(  shiny::strong(   shiny::span(  shiny::uiOutput("formula.AUC.without.TeX"),  style="text-align: center;color:#B30E0E; font-size:17pt;")   )      ), #Table of only a single cell
shiny::h6(shiny::helpText("A number between 0 and 1.")),

shiny::h6(shiny::helpText("------------------------------------------------------------------------------------")),

shiny::h6(shiny::helpText(" Smaller is better.")),
# shiny::uiOutput("formula.WAIC.without.TeX"),
shiny::h3(  shiny::strong(   shiny::span(  shiny::uiOutput("formula.WAIC.without.TeX"),  style="text-align: center;color:#B30E0E; font-size:17pt;")   )      ), #Table of only a single cell
shiny::h6(shiny::helpText("Evaluate whether our model is good.")),

shiny::h6(shiny::helpText("------------------------------------------------------------------------------------")),

shiny::h6(shiny::helpText(" Smaller is better.")),
shiny::uiOutput("formula.chisquare.without.TeX"),
shiny::uiOutput("EAP_of_chi_square"),
shiny::h6(shiny::helpText("A difference between data and expectations from estimates.")),

shiny::h6(shiny::helpText("------------------------------------------------------------------------------------")),


shiny::h6(shiny::helpText(" Larger is better.")),

# shiny::em( shiny::uiOutput("ppp")),
shiny::h3(  shiny::strong(   shiny::span(  shiny::uiOutput("ppp"),  style="text-align: center;color:#B30E0E; font-size:17pt;")   )      ), #Table of only a single cell

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
                    width = '100%',

                    min =  0, max = 100,
                    value = 23,# Default
                    step = 1
 ),



# Posterior Estimates -----
shiny::h2(" Posterior Estimates"),
shiny::h6(shiny::helpText(" mean = posterior mean")),
# shiny::h6(shiny::helpText(" se_mean = .... ")),
shiny::h6(shiny::helpText(" sd = Standard Deviation of posterior MCMC samples")),
shiny::h6(shiny::helpText(" 2.5% = Lower bound of Posterior Credible interval")),
shiny::h6(shiny::helpText(" 97.5% = Upper bound of Posterior Credible interval")),
shiny::h6(shiny::helpText(" Rhat = A criteria of Convergence for MCMC sampling, if it is more close to 1, then it is better. ")),

shiny::verbatimTextOutput("fit_print"),
# tableOutput("fit_print"),






shiny::h2(" Replicated FROC datasets to calculate a posterior predictive p value"),
shiny::checkboxInput("ppp_plot_trigger",
"Plot replicated datasets to calculate P-value",
value = FALSE),
shiny::h4(shiny::helpText("It takes quite a lot of time.")),

shiny::plotOutput("plot_ppp", dblclick = shiny::dblclickOpts(id = "plot_dbl_click")),

shiny::checkboxInput("Colour",
"Colour",
value = TRUE),


shiny::checkboxInput("dark_theme_ppp",
"dark theme",
value = TRUE),









shiny::br()

)#mainPanel
)#sidebarLayout






                            ),#tabPanel
                            shiny::tabPanel("Ref", icon = shiny::icon("list-alt"), #tabPanel ref ------

                                            shiny::h2(" Reference:"),

                                            shiny::h4(shiny::a(  "See vignettes for more details",     href="https://cran.r-project.org/package=BayesianFROC")),

                                            shiny::h4(shiny::helpText("  Issei Tsunoda (2019): Pre-prepreprepreprepreprepreprint;  Bayesian Models for  Free - Response Receiver Operating Characteristic Analysis. <- I forget its precise name, reviwer always said what is new? or what a fucking english! Hahh OK. So, all people will understand what model is implemented in this package. I try in 2 years to publish my theory, but all journal rejected cuz what's new?? So, everybody knows what this package is and FROC models without my paper. Good luck! I hate statisian reviwers, I go back to my interst, geometry. Good bye!")),
                                            shiny::h4(shiny::helpText("  Issei Tsunoda (2020): Sorry, I cannot publish the paper about FROC, but mathematics paper will be published soon, in which the author study the Gromov-Hausdorff topology,...I forget the paper name,,,maybe The correspondense between ,,,??? i forget,,, ha,forget .")),

                                            shiny::h4(shiny::helpText("  Dev Chakraborty (1989) doi:10.1118/1.596358; Maximum likelihood analysis of free - response receiver operating characteristic (FROC) data.")),
                                            shiny::h4(shiny::helpText(" Dev Chakraborty; Observer Performance Methods for Diagnostic Imaging, Foundations, Modeling, and Applications with R-Based Examples "))



                            ),#tabPanel

                            shiny::tabPanel("stanmodel", icon = shiny::icon("list-alt"), #tabPanel stanmodel ----

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










                            shiny::tabPanel("Trace Plot 2", icon = shiny::icon("bar-chart-o"),

# shiny::actionButton("trigger_stan_trace_plot","Trace plot"),
# trace plot 2 -----

#param name for  trace plot  -------
shiny::absolutePanel( draggable = TRUE, style ="red",fixed=TRUE,


                 shiny::sliderInput("stan_trace_size",
                                    "Size of trace plot:",
                                    width = '100%',

                                    min =  1, max = 100,
                                    value = 30# Default
                 ),

                 # shiny::sliderInput("R_object_as_the_result_of_bins_for_legs",
                 #                    "For stan_ac, the maximum number of lags to show:",
                 #                    min =  1, max = 222,
                 #                    step = 1, width = '100%',
                 #
                 #
                 #                    value = 30# Default
                 # ),

                 # render UI ACACACACACACACACACACAC-----

                 shiny::uiOutput("UI_R_object_as_the_result_of_bins_for_legs") ,


                 shiny::sliderInput("hist_bins2",
                                    "Number of bins:",
                                    min =  1, max = 777,
                                    step = 1,
                                    width = '100%',

                                    value = 30# Default
                 ),


                 shiny::h1("Specify param name"),
                 shiny::h6(shiny::helpText("Specified IDs will be plotted")),
                 shiny::uiOutput("name_of_model_parameter"),
                 shiny::textOutput("txt_name_of_model_parameter"),
),
shiny::plotOutput("plot_trace_for_specified_param"  ),
shiny::plotOutput("plot_ac_for_specified_param"  ),
shiny::plotOutput("plot_hist_for_specified_param" ),



shiny::h4(shiny::helpText("The parameter having the maxmal R hat is shown.")),
shiny::h4(shiny::helpText("Best regards,  ")),

shiny::h4(shiny::a(  "Piggy",     href="https://cran.r-project.org/package=BayesianFROC"))




                            ),#tabPanel






shiny::tabPanel("Pairs Plot", icon = shiny::icon("bar-chart-o"),
                shiny::actionButton("trigger_stan_pairs_plot","Pairs plot"),
                shiny::h4(shiny::helpText("This button plays important role for pairs plots. Because initial values of GUI does not work. If without it, then the plot reads the specified parameter is regarded as none and  pairs plots are done for all parameters and will take a lot of times.")),

# shiny::actionButton("trigger_stan_trace_plot","Trace plot"),
# Pairs plot  -----

#param name for  Pairs plot  -------
                shiny::absolutePanel(        draggable = TRUE, style ="red",fixed=TRUE,



# shiny::sliderInput("hist_bins2",
#                    "Number of bins:",
#                    min =  1, max = 777,
#                    value = 30# Default
# ),
#
# shiny::sliderInput("stan_trace_size",
#                    "Size of trace plot:",
#                    min =  1, max = 100,
#                    value = 30# Default
# ),
                           shiny::h1("Specify param name"),
                           shiny::h6(shiny::helpText("Specified IDs will be plotted")),
                           shiny::uiOutput("name_of_model_parameter_for_pairs_plot"),
# shiny::textOutput("txt_name_of_model_parameter_pairs_plot"),
),
shiny::plotOutput("pairs_plot"  ),



shiny::h4(shiny::helpText("The author's older brother is biggy, his hiphop is very big hip! Piggy what are you doing, you still smoke in the water? Piggy is expert of chemistry, what are doing piggy, you still fat man? or big hip man?")),
shiny::h4(shiny::helpText("Best regards,  ")),

shiny::h4(shiny::a(  "Piggy",     href="https://cran.r-project.org/package=BayesianFROC"))




),#tabPanel












shiny::tabPanel("scatter plot", icon = shiny::icon("bar-chart-o"),

# shiny::actionButton("trigger_stan_trace_plot","Trace plot"),
# scatter plot  -----

                shiny::absolutePanel(        draggable = TRUE, style ="red",fixed=TRUE,
                                             shiny::h1("Specify param name for the X-axis"),
                                             shiny::uiOutput("name_of_model_parameter_1st"),
# shiny::textOutput("txt_name_of_model_parameter"),
                                             shiny::h1("Specify param name for the Y-axis"),
                                             shiny::uiOutput("name_of_model_parameter_2nd"),
                                             shiny::h4(shiny::helpText("A and m have very simple linear relation.  ")),

# shiny::textOutput("txt_name_of_model_parameter"),
                ),
                shiny::plotOutput("plot_scatter_for_specified_param" ),



                shiny::h4(shiny::helpText("The author's older brother is biggy, his hiphop is very big hip! Piggy what are you doing, you still smoke in the water? Piggy is expert of chemistry, what are doing piggy, you still fat man? or big hip man?")),
                shiny::h4(shiny::helpText("Best regards,  ")),

                shiny::h4(shiny::a(  "Piggy",     href="https://cran.r-project.org/package=BayesianFROC"))




),#tabPanel





















  shiny::tabPanel("Trace Plot", icon = shiny::icon("bar-chart-o"),

                  shiny::actionButton("trigger_stan_trace_plot","Trace plot"),
# trace plot -----


                  shiny::plotOutput("plot_trace", dblclick = shiny::dblclickOpts(id = "plot_dbl_click")),




                  shiny::h4(shiny::helpText("The author's older brother is biggy, his hiphop is very big hip! Piggy what are you doing, you still smoke in the water? Piggy is expert of chemistry, what are doing piggy, you still fat man? or big hip man?")),
                  shiny::h4(shiny::helpText("Best regards,  ")),

                  shiny::h4(shiny::a(  "Piggy",     href="https://cran.r-project.org/package=BayesianFROC"))




  ),#tabPanel

  shiny::tabPanel("Histogram", icon = shiny::icon("bar-chart-o"),
                  shiny::actionButton("trigger_stan_hist_plot","Histogram plot"),
# hist plot -----
                  shiny::plotOutput("plot_hist", dblclick = shiny::dblclickOpts(id = "plot_dbl_click")),
                  shiny::sliderInput("hist_bins",
                                     "Number of bins:",
                                     width = '100%',
                                     min =  1, max = 777,
                                     value = 30# Default
                  ),


                  shiny::h4(shiny::helpText("The author's younger brother is rat, his teeth is outer and outer and outer. What are doing rat? you still work very hard position? He is also expert of chemistry, he did chemistry in his master degree. Ha, my brothers like chemisty. I cannot understand why chemistry is funny? I like Riemannian geometry. Are you happy now? You have a lot of money and a car, but you working eight days a week, is your own life? Oh, I got a call and he will marry, I cannot believe. So, he is happy, but me, I have aches. ha... ")),
                  shiny::h4(shiny::helpText("Best regards,  ")),

                  shiny::h4(shiny::a(  "Rat",     href="https://cran.r-project.org/package=BayesianFROC"))
  ),#tabPanel

  shiny::tabPanel("Generic plot",icon = shiny::icon("bar-chart-o")  ,
                  shiny::actionButton("trigger_stan_generic_plot","Trace plot"),
# generic plot -----
                  shiny::plotOutput("generic_plot", dblclick = shiny::dblclickOpts(id = "plot_dbl_click")),
                  shiny::h4(shiny::helpText("The author's younger and younger brother is nothing but his pokemon is tiny, pratin.")),
                  shiny::h4(shiny::helpText("Best regards,  ")),
                  shiny::h4(shiny::a(  "Pratin",     href="https://cran.r-project.org/package=BayesianFROC"))
  ),#tabPanel














# papers -----
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

      shiny::h4(shiny::helpText("      Emplooooo\\(^o^)/oooooooy   me! :'-D Now, from 2020 Sept 1, the author have been a xxxxx and  above me only sky without any address without roof without money without any loves, lovers, and there is a . If you can employ, then send me a mail ... my address is \" tsunoda.issei1111@gmail.com \" I would work like a dog in Eight days a week! and Hard days night! So, please, please me! Love me do!  ")),


shiny::h4(shiny::a(  "Fish",     href="https://cran.r-project.org/package=BayesianFROC"))
),#tabPanel





# papers -----

shiny::tabPanel("Carbus  University",
        shiny::h4(shiny::helpText(" I am a professor in both Carbus and Mosquitowbus univeristy
                                     in which I study how to fight to mosquito in the
                                     dark night. Mosquitos threaten me with his poooo00000000ooooooo0000000000ooooooooooo000000000000000ooooooon,
                                      voice, so I try to catch them, but their stealth cannot
                                     catch. I have to establish new theory how to fight them
                                     to ensure our stable sleepping.

                                     When I 17 years old, I establish the Carbus university and research how to fight Car ( which means mosquito in some language).
                                     Yes, I am the first president of Carbus university. I am editor in cheif of the journal \" Annals of Mosquitics\".

                                     ")),

        shiny::h4(shiny::helpText(" Issei Tsunoda (1981); Mosquito's love and thier demand of blood; pp.123-233; Carbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1981); How to Accept Mosquito's love and human blood; pp.123-233; Carbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1981); New notion of Mosquito's shape of my blood; pp.123-233; Carbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1981); Mosquito's life and my blood's life; pp.123-233; Carbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda, Sampota Ikuzo (1981); Introduction to Mosquitics and its application to Carbustics, Ghostics ; pp.123-233; Mosquitobus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda, Sampota Ikuzo (1981); Mosquitics and Ghostics wich complex phase for Carbustics  ; pp.123-233; Mosquitobus Univeristy Press")),

        shiny::h4(shiny::helpText(" Issei Tsunoda, Sampota Ikuzo (1981); The hot night of Mosquito and me ; pp.123-233; Mosquitobus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda, Rieblach Ikuzo(1981); Mosquito and me; pp.123-233; Mosquitobus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda, Riyabo Koi(1981); Mosquito's construction of Exotic blood; pp.123-233; Mosquitobus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda, Riburibu Koi (1981); Mosquito's structure on my blood; pp.123-233; Proceedings of the Mosquitobus Academy of Science 55(6) Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda, Riyakoboch Rieblow(1981); Introduction to Mosquito theory; pp.123-233; Proceedings of the Mosquitobus Academy of Science 55(6) Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda, Riberiya Ruikoboch (1981); Mosquitoric classes; pp.123-233; Proceedings of the Mosquitobus Academy of Science 55(6) Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda, Riyakobach Samnpo; On mosquito's love to homology. Annals of Mosquitics. Vol43. No.2. July 1988 pp.123-233; Proceedings of the Mosquitobus Academy of Science 55(6) Press")),



        shiny::h4(shiny::helpText("Best regards,  ")),
        shiny::h4(shiny::a(  "The Author",     href="https://cran.r-project.org/package=BayesianFROC"))




),#tabPanel


# papers -----

shiny::tabPanel("Ghostbus university",
        shiny::h4(shiny::helpText(" I am a professor in the Ghostbus univeristy
                                     in which I study how to fight to ghost. My students are the Ghostbusters.")),


        shiny::h4(shiny::helpText(" Issei Tsunoda (1999); what is a ghostlogical geometry and thier symmetry; pp.123-233; Ghostbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1989); what is a kernel of ghostlogical substance and thier symmetry; pp.123-233; Ghostbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1981); Ghostlogical society and thier symmetry; pp.123-233; Ghostbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1982); Ghostlogical society and thier sex; pp.123-233; Ghostbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1982); How to beat it from Ghost of Gozira; pp.123-233; Ghostbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1982); Ghost and their application to manufacture; pp.123-233; Ghostbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1983); Ghostical analytics and their application to Lagrange submanifolds; pp.123-233; Ghostbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1983); Differential ghost equation and ghost kernel; pp.123-233; Ghostbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1983); What is the gravity of ghosts?; pp.123-233; Ghostbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1984); Ghost cohomology and their algebra; pp.123-233; Ghostbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1983); Ghost kernel and humanian cokernel; pp.123-233; Ghostbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1984); I hate hell, prurigo nodularis and syndet; pp.123-233; Ghostbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1984); What is death and what is syndet; pp.123-233; Ghostbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1984); Ghost life is also beatiful; pp.123-233; Ghostbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1984); What is death and what should be death; pp.123-233; Ghostbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1984); Syndet sythesize death; pp.123-233; Ghostbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1984); Ghostical matter and its application to Ghothegotics; pp.123-233; Ghostbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1984); Syndet by death; pp.123-233; Ghostbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1984); Death let me free from multiple chemical sensitivity?; pp.123-233; Ghostbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1984); Birth as a Ghoust; pp.123-233; Ghostbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1984); Head ache as a Ghoust; pp.123-233; Ghostbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1984); Prurigo nodularis as a Ghoust; pp.123-233; Ghostbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1984); Modern Ghoustlogical analysis; pp.123-233; Ghostbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1984); Momentum of Ghoust; pp.123-233; Ghostbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1984); Existence Thorem for Ghousts and compactness of ghostics.; pp.123-233; Ghostbus Univeristy Press")),
        shiny::h4(shiny::helpText(" Issei Tsunoda (1984); Vanishing theorem of the first Ghoustical cohomology; pp.123-233; Ghostbus Univeristy Press")),


        shiny::h4(shiny::helpText("Best regards,  ")),
        shiny::h4(shiny::a(  "The Author",     href="https://cran.r-project.org/package=BayesianFROC"))




),#tabPanel




shiny::tabPanel("Acknowleage",
        shiny::h4(shiny::helpText("This package is made with helps of many unknown people in stack over flows, and the author appreciate their kind helps. Also Stan developer gives me quick and appropriate advices in many times. Such many people helps me to develop this package. So, Michael Jackson says you are not alone!... but I think I am alone cuz I am a xxxxx, please .... Doggy, eleison. Cum sancto spiritu in gloria doggy. what I talking about....
                                  ")),
        shiny::h4(shiny::helpText("This research was supported by foundation of Moskitwobus university and   foudation named God Execuse Me of Carbus university. Also Gostbus university gives  a nice place to stay for research and programming cuz above me only sky. The author thanks Bem E king for his nice helpful lyrics and Johm Lemmom for helpful suggestions in the Goutbus university.
                                  ")),
        shiny::h4(shiny::helpText("This research was realy supported by foundation of my Grand mother who is a fat woman, maybe,,,, she also works as a prof. ,,,, in Gostbus university as a proffesional of sewing or .
                                  To contact him, send a mail to  tsunoda@caubus.ac.jp or   tsunoda@mosqutobus.ac.jp or   tsunoda@ghostbus.ac.jp and as a informal address, use this package mail address.")),
        shiny::h4(shiny::helpText("Best regards,  ")),
        shiny::h4(shiny::a(  "The Author",     href="https://cran.r-project.org/package=BayesianFROC"))




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


#______________________________________------------------
server <- function(input, output) {

    values <- shiny::reactiveValues()






# UI ac auto corr ------

    output$UI_R_object_as_the_result_of_bins_for_legs <- shiny::renderUI({

    shiny::sliderInput("R_object_as_the_result_of_bins_for_legs",
                       "For stan_ac, the maximum number of lags to show:",
                       min =  1, max = max(fit()@sim$iter - fit()@sim$warmup-2,10),
                       step = 1, width = '100%',
                       value = 30# Default
    )
    })#shiny::renderPlot


# UI Txt prior -----


    output$UI_prior_w <- shiny::renderUI({
      shiny::code( shiny::strong(paste(  "w  ~  normal(",input$ww,",",input$www,")",sep = "" )))
    })#shiny::renderPlot



    output$UI_prior_m <- shiny::renderUI({
    shiny::code( shiny::strong(paste(  "m  ~  normal(",input$mm,",",input$mmm,")",sep = "" )))
    })#shiny::rende

    output$UI_prior_z <- shiny::renderUI({
      if(input$prior == 3)   shiny::code( shiny::strong(paste(  "0 < dz  ~  normal(",input$zz,",",input$zzz,")",sep = "" )))
      else if(input$prior == 4)   shiny::code( shiny::strong(paste(  "  dz  ~  exponential(",input$zzz,")",sep = "" )))

       })#shiny::renderPlot

    output$UI_prior_v <- shiny::renderUI({
      if(input$prior == 3)  shiny::code( shiny::strong(paste(  "0 < v  ~  normal(",input$vv,",",input$vvv,")",sep = "" )))
      else if(input$prior == 4)  shiny::code( shiny::strong(paste(  "  v  ~  exponential(",input$vvv,")",sep = "" )))

        })#shiny::rende

# UI save ------
    output$UI_save_Txt <- shiny::renderUI({
        shiny::h5(shiny::helpText("#Hitting the above button, then a file ", shiny::strong( paste(" \" ", input$Name_of_file ,".Rda\" ", sep = "") ), "will be created in Desktop.  Executing the following R code"))
    })#shiny::renderPlot


    output$UI_save_Txt2 <- shiny::renderUI({
       shiny::code( shiny::strong(   paste(  "load(file =paste0(file.path(Sys.getenv(\"USERPROFILE\"),\"Desktop\"),\"\\\\", input$Name_of_file ,".Rda\"))", sep = "")   ))
    })#shiny::renderPlot

    output$UI_save_Txt3 <- shiny::renderUI({
     shiny::helpText("# from the R console or (R studio console), an R object named ")
    })#shiny::renderPlot
    output$UI_save_Txt4 <- shiny::renderUI({
       shiny::code( shiny::strong(input$Name_of_fit  ))
    })#shiny::renderPlot

    output$UI_save_Txt5 <- shiny::renderUI({
      shiny::helpText("# is available on the R (or R studio) console. To change the S4 class to stanfit, use the following R code ")
    })#shiny::renderPlot
    output$UI_save_Txt6 <- shiny::renderUI({
       shiny::code( shiny::strong( paste(    input$Name_of_fit ,"_stanfit <- methods::as(", input$Name_of_fit ,", \"stanfit\")",sep = "" )))
    })#shiny::renderPlot

    output$UI_save_Txt7 <- shiny::renderUI({
      shiny::helpText("# Then, the R object named ")
    })#shiny::renderPlot
    output$UI_save_Txt8 <- shiny::renderUI({
    shiny::code( shiny::strong(paste( input$Name_of_fit ,"_stanfit",sep = "" )))
    })#shiny::renderPlot
    output$UI_save_Txt9<- shiny::renderUI({
      shiny::helpText("#  is created, which is an R object of the S4 class stanfit. Copy  the above sentences, and paste it on the R console, then the desired objects will be created! What a kind man I am ;)  so a kind homeless! ")
    })#shiny::renderPlot

    output$UI_save_Txt10<- shiny::renderUI({
      shiny::helpText("#   Best regards,")
    })#shiny::renderPlot

    output$UI_save_Txt11<- shiny::renderUI({
      shiny::helpText("#   Doggy")
    })#shiny::renderPlot






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
      values[["NL"]] <- if(is.numeric(  input$Number_of_lesions  ))   input$Number_of_lesions  else as.integer( input$Number_of_lesions[[1]][[1]]   ) #Very important because  rhandsontable is not same class for slide and input in Shiny
      values[["NI"]] <- if(is.numeric(  input$Number_of_images   ))   input$Number_of_images   else as.integer( input$Number_of_images[[1]][[1]] )#Very important because  rhandsontable is not same class for slide and input in Shiny

      values[["dataList"]] <- list(NL= if(is.numeric(  input$Number_of_lesions  ))   input$Number_of_lesions  else as.integer( input$Number_of_lesions[[1]][[1]]   )    ,
                                   NI= if(is.numeric(  input$Number_of_images   ))   input$Number_of_images   else  as.integer( input$Number_of_images[[1]][[1]] ),
                                   h=DF$h,
                                   f=DF$f,
                                   C=length(DF[,1])
      )

      values[["sum_h"]] <- sum( values[["dataList"]]$h )
# To avoid unnecessary fitting <<<<<<<<<------


      # values[["Number_of_MCMC_samples"]] <- input$Number_of_MCMC_samples
      # values[["Number_of_MCMC_chains"]] <- input$Number_of_MCMC_chains
      # values[["Seed_of_MCMC_chains"]] <- input$Seed_of_MCMC_chains
      values[["Number_of_MCMC_samples"]] <- if(is.numeric(  input$Number_of_MCMC_samples  ))   input$Number_of_MCMC_samples  else as.integer( input$Number_of_MCMC_samples[[1]][[1]]   ) #Very important because  rhandsontable is not same class for slide and input in Shiny
      values[["Number_of_MCMC_chains"]]  <- if(is.numeric(  input$Number_of_MCMC_chains   ))   input$Number_of_MCMC_chains   else as.integer( input$Number_of_MCMC_chains[[1]][[1]] )#Very important because  rhandsontable is not same class for slide and input in Shiny
      values[["Seed_of_MCMC_chains"]]    <- if(is.numeric(  input$Seed_of_MCMC_chains     ))   input$Seed_of_MCMC_chains     else as.integer( input$Seed_of_MCMC_chains[[1]][[1]]   ) #Very important because  rhandsontable is not same class for slide and input in Shiny




    })





    # # Download  Plot ooooooooooooooooooooooooooooooooooooooooooo----
    plotInput = function() {
# plotInput   <- shiny::reactive({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL
      if (sum(h)<=NL) {
        # grDevices::dev.off()
        # grDevices::dev.set()

        if (input$dark_theme==TRUE) {# This cords clear plot environment in shiny GUI board.
          dark_theme();
          plot(0,0,# Dumy plot to clean plot environment, this is required only Shiny board, I do not know the reason.
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
# return(
        DrawCurves(fit(),
                   # Colour  = FALSE,
                   Colour = input$dark_theme,

                   new.imaging.device  = FALSE,
                   DrawCFPCTP = input$DrawCFPCTP,
                   DrawFROCcurve = input$DrawFROCcurve,
                   DrawAFROCcurve = input$DrawAFROCcurve)
# )
             }


      else if (sum(h)> NL) {


        h.string <- as.character(h)
        for (cd  in 1:length(h.string)) {
          if (cd==1){ s<-""; s <- paste(h.string[cd],sep = "+")}
          if ( !cd==1)s <- paste(s,h.string[cd],sep = "+")

        }#for
        sum.of.h <- s
        sum.of.h <- paste(sum.of.h,"=",as.character( sum(h) ) )

        plot(0,0,xlim=c(0,1),ylim =c(0,1),xaxt="n", yaxt="n",xlab="Please fix inconsistent data",ylab="",main="Error:   Inconsistent Data \n In baseball game, \n batter's number of hits can not  be greater than his number of at-bats")

        graphics::text(0.5,0.8,c("*Now, Sum of the number of hits is greater than that of lesions; \n\n", expression(paste(h1+h2+h3+... , "         >         Number of Lesions")) ),col="blue",cex =    1.4  )
        graphics::text(0.5,0.65,paste("In the current inputed data, it is the following: " ),col="black",cex =  1.5  )
        graphics::text(0.5,0.5,paste(  sum.of.h , "      >       ",NL,sep = ""),col="red",cex = 2)

        graphics::text(0.5,0.3,c("* Please fix so that the following inequality holds \n", expression(paste(h1+h2+h3+... , "       <        Number of lesions"))),col="blue",cex =  1.5    )
        graphics::text(0.5,0.1,c("* Shoud decrease the number of hits  or \n  Shoud increase the number of lesions"),col="blue",cex = 1.5  )




      }#if
      }
      # )#reactivevalue

# download plot 00000000000000000000000000000000000-----
    output$download_Plot = shiny::downloadHandler(
      # filename = paste("aaaaaaaaaaaa.",as.character(input$extension_for_plot_save), sep = ""),
      # filename = "aaaaaaaaaaaa." ,

      # filename = paste(as.character(input$file_name_for_plot_save),".png", sep = ""),
      filename = "fileName.png",


         content = function(file) {
        device <- function(..., width, height) {
      grDevices::png(..., width = width, height = height,res = 300, units = "in")

      # if(input$extension_for_plot_save == "png")    grDevices::png(..., width = width, height = height,res = 300, units = "in")
      #     else if(input$extension_for_plot_save == "jpeg")    grDevices::jpeg(..., width = width, height = height,res = 300, units = "in")
      #     else if(input$extension_for_plot_save == "bmp")    grDevices::bmp(..., width = width, height = height,res = 300, units = "in")
      #     else if(input$extension_for_plot_save == "tiff")    grDevices::tiff(..., width = width, height = height,res = 300, units = "in")
      #     else if(input$extension_for_plot_save == "pdf")    grDevices::pdf(..., width = width, height = height,res = 300, units = "in")
      #


           }
        ggplot2::ggsave(file, plot = plotInput(), device = device)
      })



 # Table h f 2020 July 31 -----
output$data_frame <- rhandsontable::renderRHandsontable({
      DF <- values[["DF"]]
      if (!is.null(DF))
        if(nrow(DF)==2) rowHeaders <- c("Definitely", "Uncertenly")
        if(nrow(DF)==3) rowHeaders <- c("Definitely", "subtle", "Uncertenly")
        if(nrow(DF)==4) rowHeaders <- c("Definitely",  "Absolutely", "subtle", "Uncertenly")
        if(nrow(DF)==5) rowHeaders <- c("Definitely", "Absolutely", "Probably","subtle","Uncertenly")
        if(nrow(DF)==6) rowHeaders <- c("Definitely", "Absolutely","Equivocal","Probably","subtle","Uncertenly")
        if(nrow(DF)==7) rowHeaders <- c("Definitely", "Absolutely","Honestly","Equivocal","Probably","subtle","Uncertenly")
        if(nrow(DF)==8) rowHeaders <- c("Definitely", "Absolutely","Honestly","such","Equivocal","Probably","subtle","Uncertenly")
        if(nrow(DF)==9) rowHeaders <- c("Definitely", "Absolutely","Honestly","such", "a","Equivocal","Probably","subtle","Uncertenly")
        if(nrow(DF)==10) rowHeaders <- c("Definitely", "Absolutely","Honestly","such", "a","lonely","Equivocal","Probably","subtle","Uncertenly")
        if(nrow(DF)>=11) rowHeaders <- c("Definitely", "Absolutely","Honestly","such",  "a","lonely", rep("word",(nrow(DF)-10)),"subtle","Probably","subtle","Uncertenly")


        tableAAA<-        rhandsontable::rhandsontable(DF,
                                                stretchH = "all",
# width = 350, height = 500,
                                                rowHeaders = rowHeaders,
                                                colHeaders = c("No. of Hits","No. of False Alarms")
# ,rowHeaderWidth=88
# ,highlightCol = TRUE
# ,highlightRow = TRUE#decolartion for table

        )

        tableBBB<-        rhandsontable::hot_table(#decolartion for table
          tableAAA
          ,highlightCol = TRUE
          , highlightRow = TRUE#decolartion for table
          , stretchH = "all"
          ,enableComments=TRUE#decolartion for table
          ,rowHeaderWidth=160
        )

        tableCCC<- rhandsontable::hot_heatmap(tableBBB,
        cols = c(1, 2), color_scale = c("#EBE4BC" ,  "#EDE7BE00"    )
        )# Table color  cols <- colourpicker::colourPicker(5)   ---------
#"#84ED53", "#F5ED0E"     "#EBA85091", "#FAC944D6"

        # tableDDD <- rhandsontable::hot_cell(tableCCC,1, 1, comment = "The number of hits with highest rating. \"hits\" = True Positives = TP.")

        comment <- "Editing this table, Stan runs and a model is fitted to this table. \n \"false alarms\" = False Positives = FP.\n \"hits\" = True Positives = TP. \n Data is not only this table, but also the number of lesions and the number of images are also consisting data.\n The number of lesions is the number of present stimulus presentations. \n The number of images are the number of trials (subjects)"

        tableDDD <- rhandsontable::hot_cell(tableCCC,1, 2, comment = comment)


        tableDDD<-        rhandsontable::hot_table(#decolartion for table
          tableDDD ,stretchH = "all"

        )

#
#         tableEEE <- rhandsontable::hot_table(#decolartion for table
#           tableDDD
#           ,highlightCol = TRUE
#           , highlightRow = TRUE#decolartion for table
#           , stretchH = "all"
#           ,enableComments=TRUE#decolartion for table
#           ,rowHeaderWidth=55
#         )

    })

## Save
# shiny::observeEvent(input$save, {
#   finalDF <- isolate(values[["DF"]])
## saveRDS(finalDF, file=file.path(outdir, sprintf("%s.Rds", outfilename)))
#
#
#
#
#
# })

    # ui_of_Number_of_MCMC_samples  -------
output$ui_of_Number_of_MCMC_samples <- shiny::renderUI({

      if(input$MCMC_input_format==1){

        if( length(  counter$counter_Number_of_MCMC_samples ) >=2 )MCMC_initial_sample_size<-  values[["Number_of_MCMC_samples"]]
        if( (length(  counter$counter_Number_of_MCMC_samples ) >=2)&&( max_MCMC_initial_sample_size<  values[["Number_of_MCMC_samples"]]) )max_MCMC_initial_sample_size<-(  values[["Number_of_MCMC_samples"]])*2
        if( (length(  counter$counter_Number_of_MCMC_samples ) >=2)&&( min_MCMC_initial_sample_size >  values[["Number_of_MCMC_samples"]]) )min_MCMC_initial_sample_size<-trunc (  values[["Number_of_MCMC_samples"]]/2)

        shiny::sliderInput("Number_of_MCMC_samples",
                           "",
                           # "Number of MCMC samples:",
                           width = '100%',
                           min =   min_MCMC_initial_sample_size, max = max_MCMC_initial_sample_size,
                           value = MCMC_initial_sample_size,# Default
                           step = 1)

      } else if(input$MCMC_input_format==2){




        shiny::numericInput("Number_of_MCMC_samples",
                            "",
                            # "Number of MCMC samples:",
                            value = values[["Number_of_MCMC_samples"]]# Default
        )
      }else if(input$MCMC_input_format==3){
        rhandsontable::rHandsontableOutput("Number_of_MCMC_samples")#, width = "150%", height = "100%")# Table of h and f

      }

    })


# ui_of_Number_of_MCMC_chains  -------
output$ui_of_Number_of_MCMC_chains <- shiny::renderUI({

      if(input$MCMC_input_format==1){

        if( length(  counter$counter_Number_of_MCMC_samples ) >=2 )MCMC_initial_chains<-  values[["Number_of_MCMC_chains"]]
        if( (length(  counter$counter_Number_of_MCMC_samples ) >=2)&&( MCMC_initial_chains<  values[["Number_of_MCMC_chains"]]) )MCMC.chains.max<-(  values[["Number_of_MCMC_chains"]])*2

        shiny::sliderInput("Number_of_MCMC_chains",
                           "",
                           # "Number of MCMC chains:",
                           width = '100%',
                           # paste("Parallel MCMC chains =",   values[["Number_of_MCMC_chains"]]  ),
                           min =   1, max = MCMC.chains.max,
                           value = values[["Number_of_MCMC_chains"]],# Default
                           step = 1)

      } else if(input$MCMC_input_format==2){




        shiny::numericInput("Number_of_MCMC_chains",
                            "",
                            # "Number of MCMC chains:",
                            value = values[["Number_of_MCMC_chains"]]# Default
        )
      }else if(input$MCMC_input_format==3){
        rhandsontable::rHandsontableOutput("Number_of_MCMC_chains")#, width = "150%", height = "100%")# Table of h and f

      }

    })








    # ui_of_Seed_of_MCMC_chains  -------
output$ui_of_Seed_of_MCMC_chains <- shiny::renderUI({

      if(input$MCMC_input_format==1){

        if( length(  counter$counter_Number_of_MCMC_samples ) >=2 )MCMC_initial_seed<-  values[["Seed_of_MCMC_chains"]]
        if( (length(  counter$counter_Number_of_MCMC_samples ) >=2)&&( MCMC_initial_seed<  values[["Seed_of_MCMC_chains"]]) )seed.MCMC.max<-(  values[["Seed_of_MCMC_chains"]])*2

        shiny::sliderInput("Seed_of_MCMC_chains",
                           "",
                           # "Seed of MCMC:",
                           width = '100%',
                           min =   1, max = seed.MCMC.max,
                           value = values[["Seed_of_MCMC_chains"]],# Default
                           step = 1)

      } else if(input$MCMC_input_format==2){




        shiny::numericInput("Seed_of_MCMC_chains",
                            "",
                            # "Seed of MCMC:",
                            value = values[["Seed_of_MCMC_chains"]]# Default
        )
      }else if(input$MCMC_input_format==3){
        rhandsontable::rHandsontableOutput("Seed_of_MCMC_chains")#, width = "150%", height = "100%")# Table of h and f

      }

    })










#
#     #UI of MCMC input
#output$ui_of_Seed_of_MCMC_chains <- shiny::renderUI({
#
#       if(input$MCMC_input_format){
#         if( length(  counter$counter_Number_of_MCMC_samples ) >=2 )MCMC_initial_seed <- input$Seed_of_MCMC_chains
#         if( (length(  counter$counter_Number_of_MCMC_samples ) >=2)&&( MCMC.chains.max< input$Seed_of_MCMC_chains) )seed.MCMC.max<-( input$Seed_of_MCMC_chains)*2
#
#         shiny::sliderInput("Seed_of_MCMC_chains",
#                            "Seed of MCMC chains:",
#                            min = 1, max = seed.MCMC.max,
#                            value = MCMC_initial_seed,
#                            step = 1)
#
#       } else
#
#         shiny::numericInput("Seed_of_MCMC_chains",
#                             "Seed of MCMC chains:",
#                             value = input$Seed_of_MCMC_chains# Default
#         )
#
#
#
#
#
#     })
#
#








    #UI of MCMC input
    #output$ui_of_Number_of_MCMC_chains <- shiny::renderUI({
    #
    #   if(input$MCMC_input_format){
    #     if( length(  counter$counter_Number_of_MCMC_samples ) >=2 )MCMC_initial_chains<- input$Number_of_MCMC_chains
    #     if( (length(  counter$counter_Number_of_MCMC_samples ) >=2)&&( MCMC.chains.max< input$Number_of_MCMC_chains) )MCMC.chains.max<-( input$Number_of_MCMC_chains)*2
    #
    #
    #
    #
    #     shiny::sliderInput("Number_of_MCMC_chains",
    #                        "Number of MCMC chains:",
    #                        min = 1, max = MCMC.chains.max,
    #                        value = MCMC_initial_chains,
    #                        step = 1
    #     )
    #
    #
    #   } else
    #
    #
    #
    #
    #
    #     shiny::numericInput("Number_of_MCMC_chains",
    #                         "Number of MCMC chains:",
    #                         value = input$Number_of_MCMC_chains# Default
    #     )
    #
    #
    #
    #
    #
    # })
#UI of MCMC input
 #output$ui_of_Number_of_MCMC_samples <- shiny::renderUI({
 #
 #      if(input$MCMC_input_format){
 # if( length(  counter$counter_Number_of_MCMC_samples ) >=2 )MCMC_initial_sample_size<- input$Number_of_MCMC_samples
 # if( (length(  counter$counter_Number_of_MCMC_samples ) >=2)&&( max_MCMC_initial_sample_size< input$Number_of_MCMC_samples) )max_MCMC_initial_sample_size<-( input$Number_of_MCMC_samples)*2
 #
 #        shiny::sliderInput("Number_of_MCMC_samples",
 #                           "Number of MCMC samples:",
 #                           min = min_MCMC_initial_sample_size, max = max_MCMC_initial_sample_size, value = MCMC_initial_sample_size)
 #
 #
 #
 #        } else
 #
 #
 #
 #          shiny::numericInput("Number_of_MCMC_samples",
 #                              "Number of MCMC samples:",
 #                              value = input$Number_of_MCMC_samples# Default
 #          )
 #
 #
 #    })






    # NL table ----
    output$Number_of_lesions <- rhandsontable::renderRHandsontable({
      DF_NL<- data.frame( NL = values[["NL"]]  )# here 2020 Sept 10 ----
      comments_of_DF <- "Edit the number of lesions, images in the table in order to get own dataset.I Love you! \n Best regards, \n Doggy"
      DF_comment <- data.frame( comments_of_DF = comments_of_DF   )

      tableAAA<-  rhandsontable::rhandsontable(DF_NL,
                                               # width = 800,
                                               # height = 820,
                                               colHeaders = c( "No. of lesions"),
                                               rowHeaders = NULL,
                                               comments = DF_comment,
                                               stretchH = "all"
      )

      tableCCC<- rhandsontable::hot_heatmap(tableAAA,
                                            cols = c(1),
                                            color_scale = c("#35B2BD",   "#F00000")#"red","red")#"#C2AB4E1F", "#C2AB4E1F")#"#FABBBB" ,  "#FABBBB")#"#FABBBB" ,  "#FABBBB1F"    )
      )# Table color  cols <- colourpicker::colourPicker(5)


      tableDDD <- rhandsontable::hot_table(#decolartion for table
        tableCCC
        ,highlightCol = TRUE
        , highlightRow = TRUE#decolartion for table
        , stretchH = "all"
        ,enableComments=TRUE#decolartion for table
        ,rowHeaderWidth=55
      )

    })





# ui_of_Number_lesions  -------
output$ui_of_Number_lesions <- shiny::renderUI({

      if(input$Data_input_format==1){

         if( length(  counter$counter_Number_of_lesions ) >=2 )NL.initial<-  values[["NL"]]
         if( (length(  counter$counter_Number_of_lesions ) >=2)&&( NL.max<  values[["NL"]]) )NL.max<- max(  values[["NL"]]*2, values[["sum_h"]]*10)
         if( ( length(  counter$counter_Number_of_lesions ) >=2)&& ( values[["sum_h"]] > values[["NL"]])      )NL.initial<-  values[["NL"]]

        shiny::sliderInput("Number_of_lesions",
                           "",
                         # "Number of lesions:",
                         width = '100%',
                         # min =   1,
                         # min = sum( values[["dataList"]]$h),
                         min = values[["sum_h"]],
                         max = values[["sum_h"]]*10,

                         # max = NL.max,
                         value = NL.initial,# Default
                         step = 1)

      } else if(input$Data_input_format==2){




      shiny::numericInput("Number_of_lesions",
                          "",
                         # "Number of lesions:",
                         value = values[["NL"]]# Default
      )
      }else if(input$Data_input_format==3){

        rhandsontable::rHandsontableOutput("Number_of_lesions")#, width = "150%", height = "100%")# Table of h and f

      }

    })






output$ui_of_Number_images <- shiny::renderUI({

      if(input$Data_input_format==1){
        if( length(  counter$counter_Number_of_lesions ) >=2 )NI.initial<-  values[["NI"]]
        if( (length(  counter$counter_Number_of_lesions ) >=2)&&( NI.max< values[["NI"]]) )NI.max<-( values[["NI"]])*2

        shiny::sliderInput("Number_of_images",
                           "",# "Number of images:",
                           width = '100%',
                           min = 1,
                           max = NI.max,
                           value = NI.initial,#Default
                           step = 1)

      } else if(input$Data_input_format==2){

       shiny::numericInput("Number_of_images",
                           "",  # "Number of images:",
                           value =  values[["NI"]]# Default
       )
          }else if(input$Data_input_format==3){
            rhandsontable::rHandsontableOutput("Number_of_images")# Table of h and f

      }


    })


# txt_lesions _________________________________ --------
output$txt_Number_of_lesions <- shiny::renderText({
      return(paste("The number of lesions:", values[["NL"]]  ))
    })#shiny::renderPlot

# txt_images _________________________________ --------
output$txt_Number_of_images <- shiny::renderText({
      return(paste("The number of images:", values[["NI"]]  ))
    })#shiny::renderPlot




    # # txt_lesions _________________________________ --------
    #output$txt_Number_of_MCMC_samples <- shiny::renderText({
    #   return(paste("The number of lesions:", values[["Number_of_MCMC_samples"]]  ))
    # })#shiny::renderPlot
    # Txt HMC _________________________________ --------
output$txt_Number_of_MCMC_samples <- shiny::renderText({
      # return(paste("The number of MCMC samples:", input$Number_of_MCMC_samples,"."))
      return(paste("Iteration:",   values[["Number_of_MCMC_samples"]]    ))

    })#shiny::renderPlot


    # Txt HMC _________________________________ --------
output$txt_Number_of_MCMC_chains <- shiny::renderText({
      return(paste("Parallel chains:", values[["Number_of_MCMC_chains"]]  ))
    })#shiny::renderPlot

    # Txt HMC _________________________________ --------
output$txt_Seed_of_MCMC_chains <- shiny::renderText({
      return(paste("Seed for HMC:", values[["Seed_of_MCMC_chains"]]  ))
    })#shiny::renderPlot
# NI table ----

output$Number_of_images <- rhandsontable::renderRHandsontable({
      DF_NI <- data.frame( NI =   values[["dataList"]]$NI)
      comments_of_DF <- "Edit the number of lesions, images in the table in order to get own dataset.I Love you! \n Best regards, \n Doggy"
      DF_comment <- data.frame(comments_of_DF = comments_of_DF)

      tableAAA<-  rhandsontable::rhandsontable(DF_NI,
# width = 300,
# height = 20,
                                               colHeaders = c( "No. of images"),
                                               rowHeaders = NULL,
                                               comments =DF_comment,
                                               stretchH = "all")


      tableDDD <- rhandsontable::hot_table(#decolartion for table
                 tableAAA
                  ,highlightCol = TRUE
                  , highlightRow = TRUE#decolartion for table
                  , stretchH = "all"
                  ,enableComments=TRUE#decolartion for table
                  ,rowHeaderWidth=55
                )
      tableDDD<- rhandsontable::hot_heatmap(tableDDD,
                                            # cols = 0,#1,
                                            color_scale = c("#ED3D3D" ,  "#FABBBB")#"#FABBBB" ,  "#FABBBB1F"    )
      )# Table color  cols <- colourpicker::colourPicker(5)



      # tableDDD <-  rhandsontable::hot_table(tableDDD
      # ,
      #   customBorders = list(list(
      #   range = list(from = list(row = 1, col = 1),
      #                to = list(row = 1, col = 1)),
      #   top = list(width = 2, color = "red"),
      #   left = list(width = 2, color = "red"),
      #   bottom = list(width = 2, color = "red"),
      #   right = list(width = 2, color = "red"))))

      # col_highlight = 1
      # row_highlight = 1
      # row_highlight = c(5, 7)
#
#       tableDDD <- rhandsontable::rhandsontable(DF_NI,
#                                                col_highlight = 0,
#                     row_highlight = 0,
#                     # width = 550, height = 300
#                     )
#
#       tableDDD <-  rhandsontable::hot_cols(tableDDD,renderer = "
#     function(instance, td, row, col, prop, value, cellProperties) {
#       Handsontable.renderers.TextRenderer.apply(this, arguments);
#
#       tbl = this.HTMLWidgets.widgets[0]
#
#       hcols = tbl.params.col_highlight
#       hcols = hcols instanceof Array ? hcols : [hcols]
#       hrows = tbl.params.row_highlight
#       hrows = hrows instanceof Array ? hrows : [hrows]
#
#       if (hcols.includes(col) && hrows.includes(row)) {
#         td.style.background = 'red';
#       }
#       else if (hcols.includes(col)) {
#         td.style.background = 'lightgreen';
#       }
#       else if (hrows.includes(row)) {
#         td.style.background = 'pink';
#       }
#
#       return td;
#   }")
    })



#  table Seed_of_MCMC_chains xxxxxxxxxxxxxxxxx----
output$Seed_of_MCMC_chains <- rhandsontable::renderRHandsontable({

    #output$Number_of_MCMC_chains <- rhandsontable::renderRHandsontable({
      DF_NL<- data.frame( seed=   values[["Seed_of_MCMC_chains"]])
      comments_of_DF <- "Edit the number of MCMC seed to run HMC! I Love you! \n Best regards, \n Doggy"
      DF_comment <- data.frame( comments_of_DF = comments_of_DF   )

      tableAAA<-  rhandsontable::rhandsontable(DF_NL,
# width = 800,
# height = 820,
                                               colHeaders = c( "Seed"),
                                               rowHeaders = NULL,
                                               comments = DF_comment,
                                               stretchH = "all"
                                                       )

      tableCCC<- rhandsontable::hot_heatmap(tableAAA,
                                             cols = c(1),
                                            color_scale = c("#35B2BD",   "#F00000")#"red","red")#"#C2AB4E1F", "#C2AB4E1F")#"#FABBBB" ,  "#FABBBB")#"#FABBBB" ,  "#FABBBB1F"    )
      )# Table color  cols <- colourpicker::colourPicker(5)


      tableDDD <- rhandsontable::hot_table(#decolartion for table
                    tableCCC
                    ,highlightCol = TRUE
                    , highlightRow = TRUE#decolartion for table
                    , stretchH = "all"
                    ,enableComments=TRUE#decolartion for table
                    ,rowHeaderWidth=55
                  )

    })


    #  table Number_of_MCMC_samples pppppppppppp----
output$Number_of_MCMC_samples <- rhandsontable::renderRHandsontable({
      DF_NL<- data.frame( samples=   values[["Number_of_MCMC_samples"]])
      comments_of_DF <- "Edit the number of MCMC samples.I Love you! If HMC does not converge, then try to increase this! \n Best regards, \n Doggy"
      DF_comment <- data.frame( comments_of_DF = comments_of_DF   )

      tableAAA<-  rhandsontable::rhandsontable(DF_NL,
                                               # width = 800,
                                               # height = 820,
                                               colHeaders = c( "Size of MCMC samples"),
                                               rowHeaders = NULL,
                                               comments = DF_comment,
                                               stretchH = "all"
      )

      tableCCC<- rhandsontable::hot_heatmap(tableAAA,
                                            cols = c(1),
                                            color_scale = c("#35B2BD",   "#F00000")#"red","red")#"#C2AB4E1F", "#C2AB4E1F")#"#FABBBB" ,  "#FABBBB")#"#FABBBB" ,  "#FABBBB1F"    )
      )# Table color  cols <- colourpicker::colourPicker(5)


      tableDDD <- rhandsontable::hot_table(#decolartion for table
        tableCCC
        ,highlightCol = TRUE
        , highlightRow = TRUE#decolartion for table
        , stretchH = "all"
        ,enableComments=TRUE#decolartion for table
        ,rowHeaderWidth=55
      )

    })

    #  table Number_of_MCMC_chains pppppppppppp----
output$Number_of_MCMC_chains <- rhandsontable::renderRHandsontable({

      DF_NL<- data.frame( chains=   values[["Number_of_MCMC_chains"]])
      comments_of_DF <- "Edit the number of MCMC chains! I Love you! \n Best regards, \n Doggy"
      DF_comment <- data.frame( comments_of_DF = comments_of_DF   )

      tableAAA<-  rhandsontable::rhandsontable(DF_NL,
                                               # width = 800,
                                               # height = 820,
                                               colHeaders = c( "Parallel MCMC chains"),
                                               rowHeaders = NULL,
                                               comments = DF_comment,
                                               stretchH = "all"
      )

      tableCCC<- rhandsontable::hot_heatmap(tableAAA,
                                            cols = c(1),
                                            color_scale = c("#35B2BD",   "#F00000")#"red","red")#"#C2AB4E1F", "#C2AB4E1F")#"#FABBBB" ,  "#FABBBB")#"#FABBBB" ,  "#FABBBB1F"    )
      )# Table color  cols <- colourpicker::colourPicker(5)


      tableDDD <- rhandsontable::hot_table(#decolartion for table
        tableCCC
        ,highlightCol = TRUE
        , highlightRow = TRUE#decolartion for table
        , stretchH = "all"
        ,enableComments=TRUE#decolartion for table
        ,rowHeaderWidth=55
      )

    })































# This is use such as fit() instesad of fit
    fit <- shiny::reactive({
# fit ----
      fit <- BayesianFROC::fit_Bayesian_FROC(
        ww   =input$ww,
        www  =input$www,
        mm   =input$mm,
        mmm  =input$mmm,
        vv   =input$vv,
        vvv  =input$vvv,
        zz   =input$zz,
        zzz  =input$zzz,
        multinomial = as.logical(input$multi_nomial),### multinomial---
        see  =  values[["Seed_of_MCMC_chains"]],
        ite  =  values[["Number_of_MCMC_samples"]],#input$Number_of_MCMC_samples,
        cha  =  values[["Number_of_MCMC_chains"]],
        ModifiedPoisson = as.logical(input$FPF_per_Lesion),
        summary = TRUE,
        Null.Hypothesis  = FALSE,
        dataList = values[["dataList"]],# input$selected_data ,
        DrawCurve  = FALSE,
        dig = 5,
        prior = as.integer(input$prior),
        model_reparametrized = FALSE# 2020 July 30

      )
      return(fit)

    })


# values <- shiny::reactiveValues()





#param name for  trace plot -------
output$name_of_model_parameter <- shiny::renderUI({
  shiny::column(
    width = 11,
    shiny::div(
      # h3("AAA"),
      shiny::checkboxGroupInput("R_object_as_the_result_of_check_boxes_for_name_of_model_parameter",
                                "Select a model parameter",
                                choices = names( rstan::get_posterior_mean(fit())[,1]),
                                select  = name_of_param_whose_Rhat_is_maximal(fit())# This specify the initial condition

                                ),
      style = "font-size: 20px; "
    )
  )
    })



#param name for  pairs plot -------
output$name_of_model_parameter_for_pairs_plot <- shiny::renderUI({
      shiny::checkboxGroupInput("R_object_as_the_result_of_check_boxes_for_name_of_model_parameter_for_pairs_plot",
                                "Select a model parameter",
                                choices = names( rstan::get_posterior_mean(fit())[,1]),
                                select  =  c(names( rstan::get_posterior_mean(fit())[,1])[1],# This specify the initial condition
                                             names( rstan::get_posterior_mean(fit())[,1])[2])# This specify the initial condition

      )
    })






output$name_of_model_parameter_1st <- shiny::renderUI({

      shiny::selectInput("R_object_as_the_result_of_check_boxes_for_name_of_model_parameter_1st",
                                "Select model parameter",
                                choices = names( rstan::get_posterior_mean(fit())[,1]),
                              selected  = names( rstan::get_posterior_mean(fit())[,1])[1]# This specify the initial condition


      )
    })

output$name_of_model_parameter_2nd <- shiny::renderUI({
      shiny::selectInput("R_object_as_the_result_of_check_boxes_for_name_of_model_parameter_2nd",
                         "Select model parameter",
                         choices   = names( rstan::get_posterior_mean(fit())[,1]),
# selected  = names( rstan::get_posterior_mean(fit())[,1])[2]# This specify the initial condition
                         selected  =  "lp__"# This specify the initial condition


      )
    })

output$txt_name_of_model_parameter <- shiny::renderText({
      icons <- paste(input$R_object_as_the_result_of_check_boxes_for_reader, collapse = ", ")
      paste("Selected reader IDs:", icons)
    })



















output$formula <- shiny::renderUI({
# my_calculated_value <- extractAUC(fit(),dig = 4)[1]
      # if (class( stanfit_from_its_inherited_class(  fit()  ))=="stanfit" ) {

# if ( !("fit()" %in%     objects())) {
 # if ( !("values[[\"dataList\"]]" %in%     objects())) {


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
#
#       } else
#         shiny::withMathJax("Now, we fit a model, wait ...")

    })



# TeX __________model -----

output$formula.model <- shiny::renderUI({
# my_calculated_value <- extractAUC(fit(),dig = 4)[1]
      C<-values[["dataList"]]$C
      NL<-values[["dataList"]]$NL
      NI<-values[["dataList"]]$NI
      h<-values[["dataList"]]$h
      f<-values[["dataList"]]$f
      h_rev <- rev(h)
      f_rev <- rev(f)
      h_cumsum <- cumsum(h)
      h_cumsum_rev <- rev( h_cumsum)
      h_cumsum_rev <- c( h_cumsum_rev,0)
      h_cumsum_rev <- h_cumsum_rev[2:(length(h)+1)]

# Calculates posterior means for each specified parameter#2020 Feb
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
      s<-paste0(s," $$H_",cd," \\sim \\text{Binomial}(",signif(p[cd],digits = 3),",", NL-h_cumsum_rev[cd],"), \\text{ the realization is  } H_",cd," = ",h_rev[cd] ," .$$")
      }
      }

      if (as.logical(input$multi_nomial)) {
        s<-paste0(s,"Let $$H_0 := N_L- \\Sigma_{c=1}^{",C,"}H_c $$")
        s<-paste0(s," $$\\LARGE		{(H_c)_{c=0,1,2,...,",C,"} \\sim \\text{Multinomial}((p_c)_{c=0,1,2,...,",C,"}).}$$")
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







# TeX _________AUC-----
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


# TeX ____________WAIC-----
output$formula.WAIC <- shiny::renderUI({



      WAIC <- fit()@WAIC
      WAIC <- signif(WAIC,digits = 3)
      s<-"Widely Applicable Information Criterion (Watanabe-Akaike Information Criterion) is calculated as follows."
      s<-paste0(s," $$\\text{WAIC}  = ",WAIC,".$$")
      shiny::withMathJax(s)
    })

# TeX __________Chi_square-----

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

      s<-paste0("AUC = \"observer recognition performance\" =  ",A)
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


        xxx <- ppp_srsc(fit(),plot=FALSE)
         # values[["ppp_srsc"]] <- xxx

        ppp <- xxx$p.value
        # values[["ppp"]] <- ppp

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




#output$formula <- renderUI({
#   my_calculated_value <- extractAUC(fit(),dig = 4)[1]
#   withMathJax(paste0("Posterior mean of AUC (area under the AFROC curve): $$\\widehat{AUC} =", my_calculated_value,"$$"))
# })

#fit_print ----
# output$fit_print <- shiny::renderTable({

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
# r hat print -----
         if(fit@convergence) cat(paste(":) OK       max R-hat = ", max.rhat," achieved by the parameter ",name_of_param_whose_Rhat_is_maximal(fit())  ))
         if(!(fit@convergence)) cat(paste(" max R-hat = ", max.rhat," achieved by the parameter ",name_of_param_whose_Rhat_is_maximal(fit())  ))





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


        if(!n==0) cat(paste( n, " of ", N, "iterations ended with a divergence.",n,"/",N,"=", round( n/N ,digits = 2)))
        if(n==0) cat(paste( ":) OK         divergence does not occur"))


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

        max_depth <- get_treedepth_threshold(fitt)
        treedepths <- rstan::get_max_treedepth_iterations(fitt)
        n <- sum(treedepths)
        N <- length(treedepths)


        if(!n==0) cat(paste( n, " of ", N, "iterations saturated the maximum tree depth of.", max_depth,"."))
        if(n==0) cat(paste( ":) OK"))
# print("Not done ........... because get_treedepth_threshold is not an exported object from 'namespace:rstan' and hence the author omitted the evaluation from here. ")
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
          cat(paste( ":) OK         E-BFMI indicated no pathological behavior."))
        }
        else {
cat(paste("E-BFMI indicated possible pathological behavior:\n"
,"  Chain" , bad_chains,": E-BFMI = ",EBFMIs[bad_chains],"\n E-BFMI below 0.2 indicates you may need to reparameterize your model."))
        }



      }# if

      if (sum(h)>NL) { print("Data format error, number of hits never greater than the number of signals. In Radiological context, signal is lesion.")  }

    })# shiny::renderPrint






# Plot ooooooooooooooooooooooooooooooooooooooooooo----

# output$DrawCurves <- shiny::renderPlot({
#       h<-values[["dataList"]]$h
#       NL<-values[["dataList"]]$NL
#       if (sum(h)<=NL) {
#         if (input$dark_theme==TRUE) {# This cords clear plot environment in shiny GUI board.
#           dark_theme();
#           plot(0,0,# Dumy plot to clean plot environment, this is required only Shiny board, I do not know the reason.
#                type="n",
#                axes=FALSE,
#                xlim=c(0,1),
#                ylim =c(0,1),
#                xaxt="n",
#                yaxt="n",
#                xlab="",
#                ylab="")
#         }
# # draw curves ----
#         DrawCurves(fit(),
#                    Colour = input$dark_theme,
#
#                    new.imaging.device  = FALSE,
#                    DrawCFPCTP = input$DrawCFPCTP,
#                    DrawFROCcurve = input$DrawFROCcurve,
#                    DrawAFROCcurve = input$DrawAFROCcurve)
#       }
#       if (sum(h)> NL) {
#         h.string <- as.character(h)
#         for (cd  in 1:length(h.string)) {
#           if (cd==1){ s<-""; s <- paste(h.string[cd],sep = "+")}
#           if ( !cd==1)s <- paste(s,h.string[cd],sep = "+")
#         }#for
#         sum.of.h <- s
#         sum.of.h <- paste(sum.of.h,"=",as.character( sum(h) ) )
#
#         plot(0,0,xlim=c(0,1),ylim =c(0,1),xaxt="n", yaxt="n",xlab="Please fix inconsistent data",ylab="",main="Error:   Inconsistent Data \n In baseball game, \n batter's number of hits can not  be greater than his number of at-bats")
#
#         graphics::text(0.5,0.8,c("*Now, Sum of the number of hits is greater than that of lesions; \n\n", expression(paste(h1+h2+h3+... , "         >         Number of Lesions")) ),col="blue",cex =    1.4  )
#         graphics::text(0.5,0.65,paste("In the current inputed data, it is the following: " ),col="black",cex =  1.5  )
#         graphics::text(0.5,0.5,paste(  sum.of.h , "      >       ",NL,sep = ""),col="red",cex = 2)
#
#         graphics::text(0.5,0.3,c("* Please fix so that the following inequality holds \n", expression(paste(h1+h2+h3+... , "       <        Number of lesions"))),col="blue",cex =  1.5    )
#         graphics::text(0.5,0.1,c("* Shoud decrease the number of hits  or \n  Shoud increase the number of lesions"),col="blue",cex = 1.5  )
#       }#if
# })#shiny::renderPlot

# # draw curves ----


output$DrawCurves <- shiny::renderPlot({
  plotInput()
})


## trace_Plot ----

output$trace_Plot <- shiny::renderPlot({

  traceplot(stanfit_from_its_inherited_class(fit()),
            pars= name_of_param_whose_Rhat_is_maximal(fit() )   )
  #  dark_theme();trace_Plot(fit(),  new.imaging.device = FALSE,  type = 2,   param_name = name_of_param_whose_Rhat_is_maximal(fit() ))
  })



# ppp plot ----

output$plot_ppp <- shiny::renderPlot({

      if (input$ppp_plot_trigger) {

        h<-values[["dataList"]]$h
        NL<-values[["dataList"]]$NL


        if (sum(h)<=NL) {
          # xxx <-ppp_srsc(fit(),Colour=input$Colour,dark_theme=input$dark_theme_ppp   )
          ppp_srsc(fit(),Colour=input$Colour,dark_theme=input$dark_theme_ppp   )

          # values[["ppp_srsc"]] <- xxx
          #
          # ppp <- xxx$p.value
          # values[["ppp"]] <- ppp

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

      repres <- parse(text=paste(input$Name_of_fit, " <- fit" ))
      eval(repres)

      # save( fit,file =paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"\\", input$Name_of_fit ,".Rda") )


       text<-paste("save( ",input$Name_of_fit, ",file =paste0(file.path(Sys.getenv(\"USERPROFILE\"),\"Desktop\"),\"\\\\", input$Name_of_file ,".Rda\",sep=\"\"))",sep="")
      represent <- parse(text = text)
      eval(represent)
# save( fit,file ="fit" )
# object name is does not change now August 2020 19

      tcltk::tkmessageBox(message=paste("\n* A file (name: \"", input$Name_of_file ,".Rda\") is created in Desktop. In the file, an fitted model R object (named \"", input$Name_of_fit ,"\")  is contained. Its class is an S4 class (stanfitExtended)\n\n\n 1) Put the resulting file \"", input$Name_of_file ,".Rda\" in the working directory,\n 2) Run the following R script \n                                     load(\"", input$Name_of_file ,".Rda\") \n\n then the R object named \"", input$Name_of_fit," is available on the R (R-studio) console. "))

    }  )


# save a fitted model in  Working directory ----

    shiny::observeEvent(input$trigger_save_a_fitted_model_object_working_directory,{

      fit <- fit()
      save( fit,file ="fit.Rda" )# Save an object in Working directory ----
      tcltk::tkmessageBox(message=paste("\n* A file (name: \"fit.Rda\") is created in the current working directory. In the file, an fitted model R object (name \"fit\")  is contained, which is an object of an S4 class (stanfitExtended)\n\n\n 1) Put the resulting file \"fit.Rda\" in the working directory,\n 2) Run the following R script \n                                     load(\"fit.Rda\") \n\n then the R object named \"fit\" is available on the R (R-studio) console. "))

    }  )



# save a fitted model in Desktop rename----
    shiny::observeEvent(input$trigger_save_a_fitted_model_object_rename,{

      fit <- fit()
      saveRDS(fit, file =paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"\\fit.Rds"))
# save( fit,file ="fit" )


      tcltk::tkmessageBox(message=paste("\n* A file (name: \"fit.Rds\") is created in Desktop. In the file, an fitted model R object is contained. Its class is an S4 class (stanfitExtended)\n\n\n \n * Run the following R script \n     my_favorite_name <- readRDS(file =paste0(file.path(Sys.getenv(\"USERPROFILE\"),\"Desktop\"),\"\\\\f.Rds\"))

 \n\n then the R object named \"my_favorite_name\" is available on the R (or R studio) console. "))

    }  )





# save a fitted model renamable in  Working directory ----

    shiny::observeEvent(input$trigger_save_a_fitted_model_object_rename_working_directory,{

      fit <- fit()
      saveRDS(fit, file = "fit.Rds")
# Save an object in Working directory ----
      tcltk::tkmessageBox(message=paste("\n* A file (name: \"fit.Rds\") is created in Working directory. In the file, an fitted model R object is contained. Its class is an S4 class (stanfitExtended)\n\n\n \n * Run the following R script \n      my_favorite_name <- readRDS(file =\"fit.Rds\")
 \n\n then the R object named \"my_favorite_name\" is available on the R (or R studio) console. "))

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


    shiny::observeEvent(c( input$trigger_stan_pairs_plot
# input$hist_bins
    ), {
      counter$ssss <-c(counter$ssss, length( counter$ssss)+1)
# print(length( counter$counter_Number_of_lesions )==1)
# print( counter$counter_Number_of_lesions   )
    })


    shiny::observeEvent(c( input$Data_input_format
# input$hist_bins
    ), {
      counter$counter_Number_of_lesions <-c(counter$counter_Number_of_lesions, length( counter$counter_Number_of_lesions)+1)
# NL.initial <- input$Number_of_lesions
# print( NL.initial  )

    })

    shiny::observeEvent(c( input$MCMC_input_format
# input$hist_bins
    ), {
      counter$counter_Number_of_MCMC_samples <-c(counter$counter_Number_of_MCMC_samples, length( counter$counter_Number_of_MCMC_samples)+1)
# NL.initial <- input$Number_of_lesions
# print( NL.initial  )

    })


# Parallel  ---------
    shiny::observeEvent(c(input$trigger_parallel
    ), {
      counter$sssss <-c(counter$sssss, length( counter$sssss)+1)

# print(counter$sssss )


      if(   (floor(length(counter$sssss) /2)==length(counter$sssss) /2 )&& (length(counter$sssss)<3)      ){
        options(mc.cores = parallel::detectCores())
        rstan::rstan_options(auto_write = TRUE)
        Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')
        message("
# We executed the following codes:

      options(mc.cores = parallel::detectCores())
      rstan::rstan_options(auto_write = TRUE)
      Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')

# For execution on a local, multicore CPU with excess RAM

    options(mc.cores = parallel::detectCores())

# To avoid recompilation of unchanged Stan programs,

    rstan_options(auto_write = TRUE)

# For improved execution time,

    Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')

# although this causes Stan to throw an error on a few processors.




              ")
      }else if( length(counter$sssss)>2  ){
        message("
# codes have already done.
      options(mc.cores = parallel::detectCores())
      rstan::rstan_options(auto_write = TRUE)
      Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')
                ")
     }

    })





# Parallel --------
output$txt_CPU_cores <- shiny::renderText({
if(   (floor(length(counter$sssss) /2)==length(counter$sssss) /2 )&& (length(counter$sssss)<3)      ){
    return(paste("We use ",parallel::detectCores()," CPU cores on the current host for parallel calculation."))
} else if(   length(counter$sssss)==3      ){
  return(paste("Yes! ",parallel::detectCores()," CPU cores are used for parallel calculation."))
}else if( length(counter$sssss)==4   ){
  return(paste("Of course! ",parallel::detectCores()," CPU cores are used."))
}else if( length(counter$sssss)==5   ){
  return(paste("I love you! ",parallel::detectCores()," lovers, and parallel loves!"))
}else if( length(counter$sssss)==6   ){
  return(paste("I love you!  ",parallel::detectCores()," cores!"))
}else if( length(counter$sssss)==7   ){
  return(paste("Are you sure? I love you and ",parallel::detectCores()," cores!"))
}else if( length(counter$sssss)==8   ){
  return(paste("Sorry, I have no time, but ",parallel::detectCores()," cores!"))
}else if( length(counter$sssss)==9   ){
  return(paste("Byebye! But don't be afraid, cuz ",parallel::detectCores()," cores are with you!"))
}else if( length(counter$sssss)==10   ){
  return(paste("Sayonara ... I love you."))
}else if( length(counter$sssss)==11   ){
  return(paste("....."))
}else if( length(counter$sssss)==12   ){
  return(paste("..........."))
}else if( length(counter$sssss)==13   ){
  return(paste("Are you with me tonight?"))
}else if( length(counter$sssss)==14   ){
  return(paste("...Are you ..sure? I love you!"))
}else if( length(counter$sssss)==15   ){
  return(paste("..."))
}else{
  return(paste("All you need is love!"))

}




})#shiny::renderPlot









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
            return(rstan::stan_trace(fit,pars = name_of_param_whose_Rhat_is_maximal(fit)))

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







# trace for specified param --------
output$plot_trace_for_specified_param <- shiny::renderPlot({

        h<-values[["dataList"]]$h
        NL<-values[["dataList"]]$NL


          if (sum(h)<=NL) {

            fit <- fit()
            fit <- methods::as(fit,"stanfit")

            message("Now, we plot trace of MCMC ....")
            return(rstan::stan_trace(fit,size = (input$stan_trace_size)/50,
                                     pars = input$R_object_as_the_result_of_check_boxes_for_name_of_model_parameter))

          }

          if (sum(h)> NL) {
            error_message(h,NL)
          }#if
        return( plot(0,0,type="n", axes = TRUE,xlim=c(0,1),ylim =c(0,1),xaxt="n", yaxt="n",xlab="",ylab="",main="")
        )


    })#shiny::renderPlot











# ac for specified param --------
output$plot_ac_for_specified_param <- shiny::renderPlot({

      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL


      if (sum(h)<=NL) {

        fit <- fit()
        fit <- methods::as(fit,"stanfit")

        message("Now, we plot ac of MCMC ....")
        return(rstan::stan_ac(fit,
# size = (input$stan_trace_size)/50,
                                 pars = input$R_object_as_the_result_of_check_boxes_for_name_of_model_parameter
                                ,lags = input$R_object_as_the_result_of_bins_for_legs
 )
               )

      }

      if (sum(h)> NL) {
        error_message(h,NL)
      }#if
      return( plot(0,0,type="n", axes = TRUE,xlim=c(0,1),ylim =c(0,1),xaxt="n", yaxt="n",xlab="",ylab="",main="")
      )


    })#shiny::renderPlot














# pairs plot ------
#output$pairs_plot_for_specified_param <- shiny::renderPlot({
#
#      h<-values[["dataList"]]$h
#      NL<-values[["dataList"]]$NL
#
#
#      if (sum(h)<=NL) {
#
#        fit <- fit()
#        fit <- methods::as(fit,"stanfit")
#
#        message("Now, we plot trace of MCMC ....")
#        return(graphics::pairs(fit,
# pars = input$R_object_as_the_result_of_check_boxes_for_name_of_model_parameter_for_pairs_plot))
#
#      }
#
#      if (sum(h)> NL) {
#        error_message(h,NL)
#      }#if
#      return( plot(0,0,type="n", axes = TRUE,xlim=c(0,1),ylim =c(0,1),xaxt="n", yaxt="n",xlab="",ylab="",main="")
#      )
#
#
#    })#shiny::renderPlot








# pairs plot ------
output$pairs_plot <- shiny::renderPlot({
      if (input$trigger_stan_pairs_plot) {

        h<-values[["dataList"]]$h
        NL<-values[["dataList"]]$NL

        print(paste(  "ssss = ", length(counter$ssss) )  )
        if(floor(length(counter$ssss) /2)==length(counter$ssss) /2){

          if (sum(h)<=NL) {

            fit <- fit()
            fit <- methods::as(fit,"stanfit")

            message("Now, pairs plot are drawn of MCMC ....")
                   return(graphics::pairs(fit,
            pars = input$R_object_as_the_result_of_check_boxes_for_name_of_model_parameter_for_pairs_plot))

          }

          if (sum(h)> NL) {
            error_message(h,NL)
          }#if
        }

      }else  if (!input$trigger_stan_trace_plot||!floor(length(counter$ssss) /2)==length(counter$ssss) /2)  {
        message("Now, we omit trace of MCMC ....")
        return( plot(0,0,type="n", axes = TRUE,xlim=c(0,1),ylim =c(0,1),xaxt="n", yaxt="n",xlab="",ylab="",main="")
        )

      }

    })#shiny::renderPlot










# scatter for specified param --------
output$plot_scatter_for_specified_param <- shiny::renderPlot({

      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL


      if (sum(h)<=NL) {

        fit <- fit()
        fit <- methods::as(fit,"stanfit")

        message("Now, we plot scatter of MCMC ....")
        return(rstan::stan_scat(fit,
                                pars =c(
                                  input$R_object_as_the_result_of_check_boxes_for_name_of_model_parameter_1st,
                                  input$R_object_as_the_result_of_check_boxes_for_name_of_model_parameter_2nd
                                )
                                  ))

      }

      if (sum(h)> NL) {
        error_message(h,NL)
      }#if
      return( plot(0,0,type="n", axes = TRUE,xlim=c(0,1),ylim =c(0,1),xaxt="n", yaxt="n",xlab="",ylab="",main="")
      )


    })#shiny::renderPlot



# hist for specified param --------
output$plot_hist_for_specified_param <- shiny::renderPlot({

      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL


      if (sum(h)<=NL) {

        fit <- fit()
        fit <- methods::as(fit,"stanfit")

        message("Now, we plot histogram of MCMC ....")
        return(rstan::stan_hist(fit,pars = input$R_object_as_the_result_of_check_boxes_for_name_of_model_parameter,bins=input$hist_bins2))

      }

      if (sum(h)> NL) {
        error_message(h,NL)
      }#if
      return( plot(0,0,type="n", axes = TRUE,xlim=c(0,1),ylim =c(0,1),xaxt="n", yaxt="n",xlab="",ylab="",main="")
      )


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
