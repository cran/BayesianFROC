ui <- shiny::shinyUI(shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    # , shiny::tags$script(src= "color_change.js")
    ) # Color


  ,shiny::titlePanel(" FROC Analysis by Issei Tsunoda")
  # ,shiny::h4(shiny::helpText("Change Data, then estimates and plot change accordingly.")),
  # , shiny::a(href = "javascript::changeBG(`red`)", "redddd")#,
  # , shiny::a(href = "javascript::changeBG(`blue`)", "redddd")#,


  ,shiny::sidebarLayout(
    shiny::sidebarPanel(


 tabsetPanel(type="pills",
   # type="tabs",

tabPanel("Data",
      shiny::h1("Data"),

      shiny::h4(shiny::helpText(  "Right-click on the table to delete/insert rows." )),
      shiny::h6(shiny::helpText(  "Double-click on a cell to edit")),
      rhandsontable::rHandsontableOutput("hot"),# Table of h and f ###########################################################
      shiny::h5(shiny::helpText(" h = hit = True Positive = TP.")),
      shiny::h5(shiny::helpText(" f = false alarms = False Positive = FP.")),

      shiny::h5(shiny::helpText("Upper row correspond to higher confidence level.")),

      shiny::sliderInput("Number_of_lesions",
                         "Number of lesions:",
                         min =  1, max = 11111,
                         value = 259# Default ####################################################################################
      ),


      shiny::sliderInput("Number_of_images",
                         "Number of images:",
                         min = 1, max = 11111,
                         value = 57#Default    ##########################################################################
      )


),

tabPanel("MCMC",

         shiny::h1("Parameter of the Hamiltonian MonteCarlo Sampling"),

         # shiny::h4(shiny::helpText(" Larger is better.")), # Warming up should not be include which distrub user.2019 July 9
         # sliderInput("Number_of_MCMC_samples", h4("Number of MCMC samples"), # Warming up should not be include which distrub user.
         #             min = 1, max = 11111,, value = c(125, 1111)), # Warming up should not be include which distrub user.
         shiny::sliderInput("Number_of_MCMC_samples",
                            "Number of MCMC samples:",
                            min = 10, max = 11111, value = 1111),

         shiny::h4(shiny::helpText(" Larger is better.")),

         shiny::sliderInput("Number_of_MCMC_chains",
                            "Number of MCMC chains:",
                            min = 1, max = 4, value = 1),


         shiny::h2("Diagnosis of MCMC sampling"),

         uiOutput("message_test")





         )
# #
# ,tabPanel("Curve",
#
# h1("What's drawn?")
#
#
# )


)


    ),

 shiny::mainPanel(

    tabsetPanel(type="pills",
                  # type="tabs",

 tabPanel("Curve",



      shiny::h2(" FROC Curve and FPF,TPF"),

       shiny::plotOutput("DrawCurves", dblclick = shiny::dblclickOpts(id = "plot_dbl_click")),
      # shinycssloaders::withSpinner(shiny::plotOutput("DrawCurves", dblclick = shiny::dblclickOpts(id = "plot_dbl_click"))),


h1("What's drawn?"),
      checkboxInput("DrawCFPCTP",
                    "FPF and TPF",
                    value = TRUE),

      checkboxInput("DrawFROCcurve",
                    "FROC curve",
                    value = TRUE),

      checkboxInput("DrawAFROCcurve",
                    "AFROC curve",
                    value = FALSE),

      shiny::h6(shiny::helpText("WAIC; Smaller is better")),
      shiny::verbatimTextOutput("WAIC"),

      shiny::h6(shiny::helpText("Chi square goodness of fit; Smaller is better")),

      shiny::verbatimTextOutput("p_value"),









      verbatimTextOutput("history"),

      # shiny::h6(shiny::helpText("p_value is calculated by integral with posterior predictive measure.")),
      # shiny::h6(shiny::helpText("As parameter, use  Posteriror mean estimates, not posterior predictive p value.")),


      # shiny::h2(" Posterior Estimates"),

      # shiny::verbatimTextOutput("print_fit")

      # ,shiny::verbatimTextOutput("check_hmc_diagnostics")
      shiny::h2("AUC")
      ,shiny::h6(shiny::helpText("Posteriror means and lower and upper 95% Credible interval. "))

      ,shiny::verbatimTextOutput("extractAUC")
      ,shiny::h6(shiny::helpText("AUC: Area under the AFROC curve"))
      ,uiOutput("formula")

        ),


tabPanel("Hit rate",

         shiny::h2("Signal Distribution and Canonical Gussian "),
         shiny::h6(shiny::helpText("Two Gaussian are shown, and one is the canonical normal distribution.")),
         shiny::h6(shiny::helpText("The reason why canonical normal distribution is plotted is that it gives the how signal distribution changes. Note that the canonical distribution is not noise distribution, noise distribution is given by the differential logaritmic Gaussian and not canonical Gaussian. Do not confuse." )),

         shiny::plotOutput("bi_normal", dblclick = shiny::dblclickOpts(id = "plot_dbl_click")),

         shiny::h6(shiny::helpText(" Signal distribution is colored by each thresholds, the colored region means hit rate for each confidence level.")),

         shiny::h6(shiny::helpText("Model is parametric and all of parameter are shown, that is model parameter is mean and variance of signal distribution and the threshods.")),


        shiny::h2(" Posterior Estimates"),

         shiny::verbatimTextOutput("print_bi_normal"),
         br()

        )



, tabPanel("False Rate",

           shiny::h2("Latent variable determines false alarm rate ")
           , shiny::h6(shiny::helpText("Two distribution are shown, one is the signal distribution and the another is the differential logarithmic Gaussian which gives us the false alarm rate by colored regions. "))

           ,shiny::plotOutput("false_rate", dblclick = shiny::dblclickOpts(id = "plot_dbl_click"))

)

, tabPanel("Posterior",

         shiny::h2("Print an fitted model object")
         # ,shiny::h2(" Here, the R code are generated to input")
         ,shiny::verbatimTextOutput("print_fit")

         , br()

)#tabPanel


, tabPanel("Report",

           shiny::h2(" Under construction "),
           shiny::h6(" In here the author make a report, that is, what means each estimates, using reactive values.")

          , br()

)


, tabPanel("Terminology",

           shiny::h2(" Under construction "),
           shiny::h6(" In here the author make a report, that is, what means each estimates, using reactive values.")

           , br()

)





),












shiny::img(src="a.jpg",height= "222", width="666"),



      shiny::h4(" Reference:"),
      shiny::h4(shiny::a(  "See vignettes for more details",     href="https://cran.r-project.org/package=BayesianFROC")),

      shiny::h4(shiny::helpText("  Issei Tsunoda (2019): Pre-print;  Bayesian Models for  Free - Response Receiver Operating Characteristic Analysis.")),





shiny::img(src="xxxxxx.jpg",height= "222", width="222"),

      shiny::br()

    )
  )
))
