



ui <- shiny::shinyUI(shiny::fluidPage(
  shiny::tags$head(shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")), # Color


  shiny::titlePanel(" FROC Analysis Presented by Issei Tsunoda"),

  shiny::h4(shiny::helpText("Change Data, then estimates and plot change. Enjoy fitting the FROC model to various datasets! Cheers!")),


  shiny::sidebarLayout(
    shiny::sidebarPanel(





      # shiny::h3(shiny::helpText(
      #          "Right-click on the table to delete/insert rows.",
      #          "Double-click on a cell to edit")),



      # wellPanel(
      #   shiny::h3("Table options"),
      #   radioButtons("useType", "Use Data Types", c("TRUE", "FALSE"))
      # ),




      # shiny::h2(" FROC Data"),
      shiny::h1("Data"),
      shiny::h4(shiny::helpText(  "Right-click on the table to delete/insert rows." )),
      shiny::h4(shiny::helpText(    "Double-click on a cell to edit")),
      rhandsontable::rHandsontableOutput("hot"),# Table of h and f ###########################################################
      shiny::h5(shiny::helpText(" h = hit = True Positive = TP.")),
      shiny::h5(shiny::helpText(" f = false alarms = False Positive = FP.")),




      # shiny::h4(shiny::helpText(" Number of lesions should be greater than sum of TP.")),
      # shiny::h5(shiny::helpText(" Lesion = signal.")),


      shiny::sliderInput("Number_of_lesions",
                         "Number of lesions:",
                         min =  1, max = 1111,
                         value = 259# Default ####################################################################################
      ),


      shiny::sliderInput("Number_of_images",
                         "Number of images:",
                         min = 1, max = 1111,
                         value = 57#Default    ##########################################################################
      ),

      # shiny::h4("Parameter of the Hamiltonian MonteCarlo Sampling"),
      #
      # shiny::h4(shiny::helpText(" Larger is better.")),
      #
      # shiny::sliderInput("Number_of_MCMC_samples",
      #             "Number of MCMC samples:",
      #             min = 111, max = 11111, value = 1111),
      #
      # shiny::h4(shiny::helpText(" Larger is better.")),
      #
      # shiny::sliderInput("Number_of_MCMC_chains",
      #             "Number of MCMC chains:",
      #             min = 1, max = 4, value = 1),

      shiny::br()

      # ,wellPanel(
      #   shiny::h3("Save"),
      #   actionButton("save", "Save table")
      # )

    ),

    shiny::mainPanel(
      # list(
      # shiny::h3(textOutput("distPlot")),
      # shiny::h2(" FROC Data"),
      # shiny::h4(shiny::helpText(  "Right-click on the table to delete/insert rows." )),
      # shiny::h4(shiny::helpText(    "Double-click on a cell to edit")),

      # rhandsontable::rhandsontable::rHandsontableOutput("hot"),
      # shiny::h5(shiny::helpText(" h = hit = True Positive = TP.")),
      # shiny::h5(shiny::helpText(" f = false alarms = False Positive = FP.")),


      shiny::h2(" FROC Curve and FPF,TPF"),

      shiny::plotOutput("DrawCurves", dblclick = shiny::dblclickOpts(id = "plot_dbl_click")),

      shiny::h4("Parameter of the Hamiltonian MonteCarlo Sampling"),

      shiny::h4(shiny::helpText(" Larger is better.")),

      shiny::sliderInput("Number_of_MCMC_samples",
                         "Number of MCMC samples:",
                         min = 111, max = 11111, value = 1111),

      shiny::h4(shiny::helpText(" Larger is better.")),

      shiny::sliderInput("Number_of_MCMC_chains",
                         "Number of MCMC chains:",
                         min = 1, max = 4, value = 1),
      shiny::h2(" Posteriors Estimates"),

      shiny::verbatimTextOutput("distPlot"),






















      shiny::h2(" Reference:"),

      shiny::h4(shiny::helpText("  Issei Tsunoda (2019): Pre-print;  Bayesian Models for  Free - Response Receiver Operating Characteristic Analysis.")),
      shiny::h4(shiny::helpText("  Dev Chakraborty (1989) doi:10.1118/1.596358; Maximum likelihood analysis of free - response receiver operating characteristic (FROC) data.")),
      shiny::h4(shiny::helpText(" Dev Chakraborty; Observer Performance Methods for Diagnostic Imaging, Foundations, Modeling, and Applications with R-Based Examples ")),


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
           All doctor who met me were lacking ability and didnot has any criteria of diagnosis for MCS. I hope development of naturally degradable and biodegradable surfactants.
           "),

      shiny::br()

    )
  )
))
