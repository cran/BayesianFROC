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













shiny::h2(" My teeth!"),

shiny::img(src="a.jpg",height= "222", width="666"),
# shiny::img(src=system.file("image", "a.jpg", package="BayesianFROC"),height= "222", width="666"),


shiny::h5(shiny::helpText("Someone may think that what the hell, I do not interest the author's teeth. But I am afraid that my teeth suggest the suitable width for print of the stanfit object, so please look my teeth and fix your browser's width. I wonder whether or not I get my wisdom teeth pulled out, please let me know your opinion about wisdom teeth. So, Dentist suggest me that my pure pretty wisdom teeth should pulled out and never meet again forever, I doubt him. Denetist threaten me that my wisdom teeth become tooth decay if I dont remove wisdom, really? I wish no pain.")),



      shiny::h4(" Reference:"),
      shiny::h4(shiny::a(  "See vignettes for more details",     href="https://cran.r-project.org/package=BayesianFROC")),

      shiny::h4(shiny::helpText("  Issei Tsunoda (2019): Pre-print;  Bayesian Models for  Free - Response Receiver Operating Characteristic Analysis.")),
      shiny::h4(shiny::helpText("  Dev Chakraborty (1989) doi:10.1118/1.596358; Maximum likelihood analysis of free - response receiver operating characteristic (FROC) data.")),
      shiny::h4(shiny::helpText(" Dev Chakraborty; Observer Performance Methods for Diagnostic Imaging, Foundations, Modeling, and Applications with R-Based Examples ")),





shiny::h1("Acknowlege"),
shiny::h5(shiny::helpText(" The author appreciate Stan developers, especialy two person, they gives me immediate and correct, plain, kind, answer, which let me solve many errors and warnings in stan functions, I appreciate them.")),

shiny::h5(shiny::helpText(" The author appreciate unknow stack over flows people. Their immediate plain answer let me make this package BayesianFROC. Thank you!!")),







      shiny::h1(" Help!"),

      shiny::h5(shiny::helpText(" The author: Issei Tsunoda  <tsunoda.issei1111 _at_ gmail.com>")),
      shiny::h5(shiny::helpText(" Employ me! :'-D, send me a mail <tsunoda.issei1111 _at_ gmail.com> I will go any country!")),
      shiny::h5(shiny::helpText(" I am japanese and live in Japan, but if necessary,
           I will go any country! Please help! I need somebody who employs me.
           Help! Not just any body, HELP! I need someone! HEEEELP!
           When I was younger so much, younger than today, I studied Riemannian Goematry.
           But now and then, I feel that in mathematics I cannot obtain any result.
           Help me if you can employ me in towns  and I do apprecieate,..., I love the Beatles and Bach and Beethoven ( player is Wilhelm Kempf only), Aerothmith, John Lennon, Billy Joel, Ben E King,...etc.


           ")),

      shiny::h5(shiny::helpText("Please employ me! :'-D Now, I have no job! I study differential geometry and
           my most interest is complex differential geometry,
           Kodaira-Spencer deformation theory, Gromov-Hausdorff topology, Ricci flow, mean curvature flow, Schubelt calculas. I want to go many nature place, not urban. In several month ago, my whole body has prurigo nodularis, so I hate sarfactant except bio-surfactant.")),

      shiny::h1("Multiple Chemical sensitivity (MCS)"),
      shiny::h5(shiny::helpText("In a 2017, December, 28, My exposure to surfactant (I won't use, and never have used in my own life, it is required in my company) and other chemical materials in it, makes my body very bad condition that symptoms occur in multiple organ systems. In several hours later from the exposure, irritant darmatitis occured. During  the first month from the exposure, my whole body has irritant stimulas, it is tiktik.  And 14 month later from the exposure, my whole body has prurigo nodularis and other chronic conditions, in addition other disorders caused by the exposure are present in my body in now (18 months later from the exposure).
           I am very sad since many people think it is psychogenic, but it is never such a thing, Complaints of MCS is very strong, should not dismissed  as psychogenic. We shold not be kept waiting for development of the diagnosis. I suffer from chemical sensitivity.
           All doctor who met me were lacking ability and didnot has any criteria of diagnosis for MCS.
           ")),
shiny::img(src="prurigo.jpg",height= "222", width="222"),

      shiny::br()

    )
  )
))
