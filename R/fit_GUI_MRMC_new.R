

#' @title Fit an MRMC model to data with Shiny GUI
#' @description I love you.
#' @details I need you.
#'
#' @param M mo
#' @param Q re
#' @param C con
#'
#' @return ret
# @export
#'
# @examples
#'
#'
fit_GUI_MRMC_new <- function(M=2,Q=3,C=4){


  DF<-data.frame(
    m =m_q_c_vector_from_M_Q_C(M,Q,C)$m,
    q=m_q_c_vector_from_M_Q_C(M,Q,C)$q,
    c=m_q_c_vector_from_M_Q_C(M,Q,C)$c,
    h=rep(NA,M*Q*C),
    f=rep(NA,M*Q*C)
  )


  ui <- shiny::fluidPage(
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
  )









































  server <-  function(input, output) {



    # This is use such as fit() instesad of fit
    fit <-shiny::reactive({

      # fit <- BayesianFROC::fit_Bayesian_FROC(
      #   ite  = input$Number_of_MCMC_samples,
      #   cha =  input$Number_of_MCMC_chains,
      #   summary = T,
      #   Null.Hypothesis = F,
      #   dataList = values[["dataList"]],# input$selected_data ,
      #
      #   DrawCurve = F,
      #   dig = 5)
      #
      # return(fit)

    })




    values <- shiny::reactiveValues()

    ## Handsontable
    shiny::observe({
      if (!is.null(input$hot)) {
        DF = rhandsontable::hot_to_r(input$hot)
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
                                   m=DF$m,
                                   q=DF$q,
                                   c=DF$c,
                                   C=input$C,
                                   M=input$M,
                                   Q=input$Q


      )

    })

    output$hot <- rhandsontable::renderRHandsontable({
      DF <- values[["DF"]]
      if (!is.null(DF))
        rhandsontable::rhandsontable(DF,
                                      stretchH = "all")

    })



    output$print_fit <- shiny::renderPrint({

      fitt <-methods::as(fit(), "stanfit")
      print( fitt, digits = 4)

    })# shiny::renderPrint






  }
  shiny::runApp(list(ui=ui, server=server))
  return(invisible())


}#function
