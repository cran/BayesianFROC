# library(rhandsontable::rhandsontable)
# library(shiny)

#' @title Fit with GUI via Shiny
#' @description First, please execute, then user will understand what it is.
#' This function is the one of the most important function in this package.
#' I do not assume the user is familiar with R script but FROC analysis. So, I made this function to provide the Graphical User Interface (GUI) for users.
#' I hope it helps someone in the world.
#' @param DF initial data to be fited
#' @param outdir I use \code{  system.file("myapp", package = "BayesianFROC")    }
#' @param outfilename I do not know :'-D
#'
#' @return None
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#'  #No need to consider the variables, it is sufficient in  default values.
#'  fit_GUI()
#'
#'}
#'

fit_GUI <- function(DF=data.frame(h=c( 97,   32,   31),f=c( 1 ,  14,   74 )), outdir=system.file("myapp", package = "BayesianFROC"), outfilename="table"){

  if (outdir == "") {
    stop("Could not find myapp. Try re-installing `BayesianFROC`.", call. = FALSE)
  }

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
           All doctor who met me were lacking ability and didnot has any criteria of diagnosis for MCS.
           "),

        shiny::br()

      )
    )
  ))

  server <- shiny::shinyServer(function(input, output) {

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
                                   C=length(DF[,1])
      )

    })

    output$hot <- rhandsontable::renderRHandsontable({
      DF <- values[["DF"]]
      if (!is.null(DF))
        rhandsontable::rhandsontable(DF,
                                     # useTypes = as.logical(input$useType),
                                     stretchH = "all")
      # dataList <- list(NL=100,NI=200, h=DF$h,f=DF$f,C=length(DF[,1]))

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


    output$distPlot <- shiny::renderPrint({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL
      if (sum(h)<=NL) {

        # library(BayesianFROC)
        fit <- BayesianFROC::fit_Bayesian_FROC(
          ite  = input$Number_of_MCMC_samples,
          cha =  input$Number_of_MCMC_chains,
          summary = F,
          Null.Hypothesis = F,
          dataList = values[["dataList"]],# input$selected_data ,
          DrawCurve = F,
          dig = 5)
        fitt <-methods::as(fit, "stanfit")
        print( fitt, digits = 4)

      }# if

      if (sum(h)>NL) { print("Data format error, number of hits never greater than the number of signals. In Radiological context, signal is lesion.")  }

    })




    output$DrawCurves <- shiny::renderPlot({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL
      if (sum(h)<=NL) {
        # library(BayesianFROC)
        fit <- BayesianFROC::fit_Bayesian_FROC(
          ite  = input$Number_of_MCMC_samples,
          new.imaging.device = F,
          cha = 1,
          summary = F,
          Null.Hypothesis = F,
          dataList = values[["dataList"]],# input$selected_data ,
          DrawCurve = F,dig = 5)


        DrawCurves(fit, Colour = F,        new.imaging.device = F) }
      if (sum(h)> NL) {


           h.string <- as.character(h)
        for (cd  in 1:length(h.string)) {
          if (cd==1){ s<-""; s <- paste(h.string[cd],sep = "+")}
          if ( !cd==1)s <- paste(s,h.string[cd],sep = "+")

        }#for
          sum.of.h <- s

        plot(0,0,xlim=c(0,1),ylim =c(0,1),xaxt="n", yaxt="n",xlab="",ylab="")
        graphics::text(0.5,0.98,c("Error:           number of Hits  cannot greater than number of signals"),col="red")

        graphics::text(0.5,0.8,c("* Sum of the number of hits is greater than the number of lesion, that is, \n\n", expression(paste(h1+h2+h3+... , " > Number of lesions")) ),col="red")
        graphics::text(0.5,0.6,paste("That is, the following current data is format error: \n", sum.of.h , " >       ",NL,sep = ""),col="red")

        graphics::text(0.5,0.3,c("* Please fix so that \n\n", expression(paste(h1+h2+h3+... , " < Number of lesions"))),col="red")
        graphics::text(0.5,0.1,c("* Shoud decrease the number of hits  or increase the number of lesions"),col="red")




      }#if

    })











  })

  ## run app
  shiny::runApp(list(ui=ui, server=server))
  return(invisible())
}

# df <- data.frame(h=c(44,22,3,4),f=c(11,22,33,44))
# editTable()
