ui <- shiny::shinyUI(shiny::fluidPage(
  shiny::tags$head(shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")), # Color


  shiny::titlePanel(" FROC Analysis by Issei Tsunoda, see [vignettes](https://cran.r-project.org/package=BayesianFROC)"),

  shiny::h4(shiny::helpText("Change Data, then estimates and plot change accordingly. Enjoy fitting the FROC model to various datasets! Cheers!")),


  shiny::sidebarLayout(
    shiny::sidebarPanel(


      shiny::sliderInput("Number_of_MCMC_samples",
                          "Number of MCMC samples:",
                          min = 10, max = 11111, value = 111),

      shiny::h4(shiny::helpText(" Larger is better."))

      ,shiny::sliderInput("Number_of_MCMC_chains",
                          "Number of MCMC chains:",
                          min = 1, max = 4, value = 1),

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


                           shiny::sliderInput("C",
                                              "Number of C:",
                                              min =  1, max = 11,
                                              value =3# ddd$C# Default ####################################################################################
                           ),


                           shiny::sliderInput("M",
                                              "Number of M:",
                                              min = 1, max = 7,
                                              value = 4#ddd$M#Default    ##########################################################################
                           ),

                           shiny::sliderInput("Q",
                                              "Number of Q:",
                                              min = 1, max =7,
                                              value = 5#ddd$Q#Default    ##########################################################################
                           ),

                           shiny::h1("Data"),
                           shiny::h4(shiny::helpText(  "Right-click on the table to delete/insert rows." )),
                           shiny::h4(shiny::helpText(  "Double-click on a cell to edit")),
                           rhandsontable::rHandsontableOutput("hot"),# Table of h and f ###########################################################
                           shiny::h5(shiny::helpText(" h = hit = True Positive = TP.")),
                           shiny::h5(shiny::helpText(" f = false alarms = False Positive = FP."))










      ),



    shiny::mainPanel(
      shiny::verbatimTextOutput("print_fit")
      ,shiny::h1("Parameter of the Hamiltonian MonteCarlo Sampling")
                     )

)
))

