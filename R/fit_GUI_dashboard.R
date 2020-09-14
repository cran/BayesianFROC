#' @title Fit with GUI via Shiny (Simple version)
#' @description simple is vest
#' @details  First, please execute, then user will understand what it is.
#' This function is the one of the most important function in this package.
#' I do not assume that the user is familiar with R script but FROC analysis. So, I made this function to provide the Graphical User Interface (GUI) for users.
#' I hope it helps someone in the world.


#'@inheritParams fit_GUI_Shiny

#'@author Issei Tsunoda
#' @return None
# @export
#'
#' @examples
#'
#' \dontrun{
#'## Only run examples in interactive R sessions
#'if (interactive()) {
#'
#'#========================================================================================
#'#            1)           Use the default User Interface
#'#========================================================================================
#'#'
#'
#'  #No need to consider the variables, it is sufficient in  default values.
#'
#'
#'  #fit_GUI_dashboard()
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
#'  #fit_GUI_dashboard(NL.max = 2222,
#'  #               NI.max = 3333)
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
#'#  fit_GUI_dashboard(
#'#  DF= data.frame( h=dataList.Chakra.4$h,
#'#                  f=dataList.Chakra.4$f
#'#                )
#'#                )
#'
#'# Or equivalently,
#'
#'#  fit_GUI_dashboard(
#'#             DF= data.frame(
#'#             h = c(160,  25,  15,   7),
#'#             f = c(  8,  16,  18,  13)
#'#                          )
#'#             )
#'
#'
#'#========================================================================================
#'#            4)           Change the user Imterface
#'#========================================================================================
#'
#'
#'
#'#fit_GUI_dashboard(
#'
#'#           DF= data.frame(
#'#               h = c(160,  25,  15,   7),
#'#               f = c(  8,  16,  18,  13)
#'#                 ),
#'
#'#                 NL.max = 1192,
#'#                 NI.max = 794,
#'#                 MCMC.chains.max = 6
#'
#'#               )
#'
#'
#'
#'
#'
#'}### Only run examples in interactive R sessions
#'
#'
#'}
#'

fit_GUI_dashboard <- function(
  DF=data.frame(h=c( 97L,   32L,   31L),f=c( 1L ,  14L,   74L )),

  NL.max=1111,
  NI.max=1111,
  # MCMC.samples.max = 11111,
  NL.initial = 259,

  MCMC.chains.max=4


){


  outdir <- system.file("myapp", package = "BayesianFROC")
  outfilename <- "table"




  if (outdir == "") {
    stop("Could not find myapp. Try re-installing `BayesianFROC`.", call. = FALSE)
  }



  ui1 <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title ="Employ me!"),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Analysis", tabName = "Analysis"),
        shinydashboard::menuItem("Ref", tabName = "Ref"),
        shinydashboard::menuItem("Author", tabName = "Author")

                  )
                ),
    # shiny::fluidPage(
    # shiny::navbarPage("Emplooooo\\(^o^)/oooooooy   me! :'-D",


    shinydashboard::dashboardBody(

      shinydashboard::tabItems(
        shinydashboard::tabItem(tabName="Analysis",



                                      # shiny::tags$head(
                                      #   shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
                                      #   # , shiny::tags$script(src= "color_change.js")
                                      # ) ,# Color


                                      shiny::tags$head(
                                        shiny::tags$style(shiny::HTML("


h1{
  font-size: 35px;
  font-weight: bold;
  font-family: Arial-Black;

  color: #800000			;

}

h2 {
  font-size: 33px;
  font-weight: bold;
  font-family: ACalibri;

  color: #800000			;

}

h3 {
  font-size: 30px;
  font-weight: bold;
  font-family: Calibri;

  color: #800000			;

}

h4 {
  font-size: 27px;
  font-weight: bold;
  font-family: Calibri;

  color: #800000			;

}



h5 {
  font-size: 24px;
  font-weight: bold;
  font-family: Calibri;

  color: #800000			;

}



h6 {
  font-size: 23px;
  font-weight: bold;
  font-family: Arial-Black;

  color: #800000			;

}



img {
	border:0;
}


body {
  font-size: 18px;
     font-weight:bolder;

  font-family: Calibri;


  color: #800000			;

  background-color:#EEEEEE;



}












p {
    color: #440000		;
}


button
{
  height:30px;
  width:150px;
  border-radius:8px;
  padding:10px;
  font-size:20px;
  font-family: 'Oswald', sans-serif;
  height:52px;
  cursor:pointer;
  background-color:wheat;
}

.ui-draggable {
 z-index: 3;
 background-color: #CCCCFF;
    "))),
                                shiny::absolutePanel(        draggable = TRUE, style ="red",fixed=TRUE,
                                                             #  top = "100%",
                                                             # left = 1,
                                                             # right = 1,
                                                             # bottom = 1,
                                                             # width = 1,
                                                             # height = 1,

                                                             shiny::h1("Data"),
                                                             shiny::h4(shiny::helpText(  "Right-click on the table to delete/insert rows." )),
                                                             shiny::h4(shiny::helpText(    "Double-click on a cell to edit")),
                                                             shiny::h6(shiny::helpText(" h = hit = True Positive = TP.")),
                                                             shiny::h6(shiny::helpText(" f = false alarms = False Positive = FP.")),

                                                             rhandsontable::rHandsontableOutput("hot"),# Table of h and f ###########################################################


                                                             shiny::h6(shiny::helpText("To edit more than 2 cells, a blank cell would help")),


                                                             # shiny::h4(shiny::helpText(" Number of lesions should be greater than sum of TP.")),
                                                             # shiny::h5(shiny::helpText(" Lesion = signal.")),


                                                             shiny::sliderInput("Number_of_lesions",
                                                                                "Number of lesions:",
                                                                                min =  1, max = NL.max,
                                                                                value = NL.initial# Default ####################################################################################
                                                             ),


                                                             shiny::sliderInput("Number_of_images",
                                                                                "Number of images:",
                                                                                min = 1, max = NI.max,
                                                                                value = 57#Default    ##########################################################################
                                                             ),
                                                             shiny::actionButton("trigger_save_a_fitted_model_object","Save")




                                ), #absolutePanel
                                      shiny::titlePanel(" FROC Analysis by Issei Tsunoda"),

                                      shiny::h4(shiny::helpText("Change Data, then estimates and plot change accordingly. Enjoy fitting the FROC model to various datasets! Cheers! Pretty crowd!")),

                                      # shiny::fluidRow(
                                      # column(6,




                                      shiny::sidebarLayout(
                                        shiny::sidebarPanel(width = 5,





                                                            # shiny::h3(shiny::helpText(
                                                            #          "Right-click on the table to delete/insert rows.",
                                                            #          "Double-click on a cell to edit")),



                                                            # wellPanel(
                                                            #   shiny::h3("Table options"),
                                                            #   radioButtons("useType", "Use Data Types", c("TRUE", "FALSE"))
                                                            # ),



#
#                                                             shinydashboard::box(title = "Data",
#                                                                 width = 12,
#                                                                 # shiny::titlePanel(" FROC Analysis by Issei Tsunoda"),
#                                                                 # background = "teal",
#                                                                 # Invalid color: gray. Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
#                                                                 collapsible  = TRUE,
#                                                                 collapsed  = FALSE,
#                                                              shiny::h1("Data"),
#                                                             shiny::h4(shiny::helpText(  "Right-click on the table to delete/insert rows." )),
#                                                             shiny::h4(shiny::helpText(    "Double-click on a cell to edit")),
#                                                             shiny::h5(shiny::helpText(" h = hit = True Positive = TP.")),
#                                                             shiny::h5(shiny::helpText(" f = false alarms = False Positive = FP.")),
#
#                                                             rhandsontable::rHandsontableOutput("hot"),# Table of h and f ###########################################################
#
#
#
#
#                                                             # shiny::h4(shiny::helpText(" Number of lesions should be greater than sum of TP.")),
#                                                             # shiny::h5(shiny::helpText(" Lesion = signal.")),
#
#
#                                                             shiny::sliderInput("Number_of_lesions",
#                                                                                "Number of lesions:",
#                                                                                min =  1, max = NL.max,
#                                                                                value = 259# Default ####################################################################################
#                                                             ),
#
#
#                                                             shiny::sliderInput("Number_of_images",
#                                                                                "Number of images:",
#                                                                                min = 1, max = NI.max,
#                                                                                value = 57#Default    ##########################################################################
#                                                             ),
#
#
#                                                             shiny::actionButton("trigger_save_a_fitted_model_object","Save")
#
#                                                             ),#box


















                                                            shinydashboard::box(title = "Parameter of the Hamiltonian MonteCarlo Sampling",
                                                                                width = 12,
                                                                                # shiny::titlePanel(" FROC Analysis by Issei Tsunoda"),
                                                                                # background = "teal",
                                                                                # Invalid color: gray. Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
                                                                                collapsible  = TRUE,
                                                                                collapsed  = FALSE,


                                                            # shiny::h4("Parameter of the Hamiltonian MonteCarlo Sampling"),

                                                            shiny::h4(shiny::helpText(" Larger is better.")),

                                                            shiny::sliderInput("Number_of_MCMC_samples",
                                                                               "Number of MCMC samples:",
                                                                               min = 111, max = 11111, value = 1111),

                                                            shiny::h4(shiny::helpText(" Larger is better.")),

                                                            shiny::sliderInput("Number_of_MCMC_chains",
                                                                               "Number of MCMC chains:",
                                                                               min = 1, max = MCMC.chains.max, value = 1),
                                                            shiny::br()
                                                            ),#box


                                                            shiny::h4("Estimates"),
                                                            shiny::h6(shiny::helpText("Internet Environment is required for TeX script.")),

                                                            shiny::uiOutput("formula"),

                                                            shiny::wellPanel(  shiny::h4("  "),

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


                                                         # shiny::h2(" FROC Curve and FPF,TPF"),
                                                         # shiny::h4(shiny::helpText("I love you :'-D  If you love me, then employ me :)")),
                                                         # shiny::h4(shiny::helpText("In Hit rate plain, the canonical Gaussian is also drawn to show how the graph changes.  ")),
                                                         # shiny::h4(shiny::helpText("In False rate plain, the signal Gaussian is also drawn to show how the graph changes.  ")),
                                                         # shiny::h4(shiny::helpText("The thresholds are common between false alram and hit.")),
                                                         # shiny::h6(shiny::helpText("Internet Environment is required for TeX script.")),


                                                         shinydashboard::box(title = "Curve",
                                                             width = 12,
                                                             # shiny::titlePanel(" FROC Analysis by Issei Tsunoda"),
                                                             background = "olive",
                                                             # Invalid color: gray. Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
                                                             collapsible  = TRUE,
                                                             collapsed  = FALSE,
                                                             shiny::h1("What's drawn?"),

                                                             shiny::checkboxInput("DrawCFPCTP",
                                                                                  "FPF and TPF",
                                                                                  value = TRUE),

                                                             shiny::checkboxInput("DrawFROCcurve",
                                                                                  "FROC curve",
                                                                                  value = TRUE),

                                                             shiny::checkboxInput("DrawAFROCcurve",
                                                                                  "AFROC curve",
                                                                                  value = FALSE),
                                                             shiny::plotOutput("DrawCurves", dblclick = shiny::dblclickOpts(id = "plot_dbl_click"))

                                                             # Invalid color: gray. Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.


                                                         ),

                                                         shinydashboard::box(title = "Gaussain (signal) and Canoninical Gaussain (NOT noise)",
                                                           # shiny::titlePanel(" FROC Analysis by Issei Tsunoda"),

                                                           background = "orange",
                                                           # Invalid color: gray. Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
                                                         collapsible  = TRUE,
                                                         collapsed  = FALSE,
                                                         shiny::plotOutput("bi_normal", dblclick = shiny::dblclickOpts(id = "plot_dbl_click"))

                                                           ),

                                                         shinydashboard::box(title = "Differential logarithmic Cumulative Gaussian (noise distribution)",
                                                           # shiny::titlePanel(" FROC Analysis by Issei Tsunoda"),
                                                           background = "light-blue",
                                                           # Invalid color: gray. Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
                                                           collapsible  = TRUE,
                                                           collapsed  = FALSE,
                                                           shiny::plotOutput("false_rate", dblclick = shiny::dblclickOpts(id = "plot_dbl_click"))

                                                           # Invalid color: gray. Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
                                                         ),

                                                         shiny::wellPanel(
                                                           shiny::h6(shiny::helpText(" Larger is better.")),

                                                           shiny::uiOutput("formula.AUC.without.TeX"),
                                                           shiny::h6(shiny::helpText(" Smaller is better.")),

                                                           shiny::uiOutput("formula.WAIC.without.TeX"),
                                                           shiny::h6(shiny::helpText(" Smaller is better.")),

                                                           shiny::uiOutput("formula.chisquare.without.TeX")
                                                         ),
                                                         shiny::h2(" Posterior Estimates"),

                                                         shiny::verbatimTextOutput("distPlot"),

                                                         # shiny::verbatimTextOutput("print_bi_normal"),###############################





















                                                         shiny::br()

                                        )#mainPanel
                                      )#sidebarLayout






                      ),#tabItem
                      #  tabItem("Ref",
                      #
                      #                 shiny::h2(" Reference:"),
                      #
                      #                 shiny::h4(shiny::a(  "See vignettes for more details",     href="https://cran.r-project.org/package=BayesianFROC")),
                      #
                      #                 shiny::h4(shiny::helpText("  Issei Tsunoda (2019): Pre-print;  Bayesian Models for  Free - Response Receiver Operating Characteristic Analysis.")),
                      #                 shiny::h4(shiny::helpText("  Dev Chakraborty (1989) doi:10.1118/1.596358; Maximum likelihood analysis of free - response receiver operating characteristic (FROC) data.")),
                      #                 shiny::h4(shiny::helpText(" Dev Chakraborty; Observer Performance Methods for Diagnostic Imaging, Foundations, Modeling, and Applications with R-Based Examples "))
                      #
                      #
                      #
                      # ),#tabPanel

        shinydashboard::tabItem(tabName = "Ref",
            shiny::titlePanel(
              shiny::h2(" Reference:")
            ),

            shinydashboard::box(title = "Reference",width=12,
                # shiny::titlePanel(" FROC Analysis by Issei Tsunoda"),

                # background = "orange",
                # Invalid color: gray. Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
                collapsible  = TRUE,
                collapsed  = FALSE,
            shiny::h4(shiny::a(  "See vignettes for more details",     href="https://cran.r-project.org/package=BayesianFROC")),

            shiny::h4(shiny::helpText("  Issei Tsunoda (2019): Pre-print;  Bayesian Models for  Free - Response Receiver Operating Characteristic Analysis.")),
            shiny::h4(shiny::helpText("  Dev Chakraborty (1989) doi:10.1118/1.596358; Maximum likelihood analysis of free - response receiver operating characteristic (FROC) data.")),
            shiny::h4(shiny::helpText(" Dev Chakraborty; Observer Performance Methods for Diagnostic Imaging, Foundations, Modeling, and Applications with R-Based Examples "))
)#Box
            ),







shinydashboard::tabItem(tabName = "Author",

        shiny::titlePanel(
          shiny::h2(" Author's Resume")
        ),

        shinydashboard::box(title = "Author",width=12,

            shiny::h4(shiny::helpText("* Exposure to surfactants changed my life.  ")),


            shiny::h4(shiny::helpText(" Karbus university and Mosquitowbus University")),
                            shiny::h4(shiny::helpText(" I am a professor in the Mosquitow university
                                   in which I study how to fight to mosquito in the
                                   dark night. Mosquitos threaten me with his poooo00000000ooooooo0000000000ooooooooooo000000000000000ooooooon,
                                    voice, so I try to catch them, but their stealth cannot
                                   catch. I have to establish new theory how to fight them
                                   to ensure our stable sleeping. My student is Kar busters and mosquito busters.

                                   When I 17 years old, I establish the Karbus university and research how to fight kar ( which means mosquito in some language).
                                   Yes, I am the fist president of Karbus university.
                                   You know, I am a professor of Mosquitowbus, Karbus, Ghostbus university.
                                   These university is made by me when I was 15~18 years old. So, when I was younger so much youneger than today I established three university.

                                   Dear Big Hip, you would only know what I say,... Dear Big Hip and Navy 16 branch Colonel Mouse, and Platin,

                                   Are you find this message? Please find, Good Bye Big Hip!! Also, Riberiya and Ruikoboch, I love you!


                                   ")),
                            shiny::h4(shiny::helpText("Best regards,  ")),
                            shiny::h4(shiny::a(  "The Author",     href="https://cran.r-project.org/package=BayesianFROC")),






            shiny::h4(shiny::helpText(" Karbus university and Mosquitowbus University and also Ghostbus university")),
                            shiny::h4(shiny::helpText(" I am a professor in the Ghostbus univeristy
                                   in which I study how to fight to ghost. My studenta are the Ghostbusters.")),
                            shiny::h4(shiny::helpText("Best regards,  ")),
                            shiny::h4(shiny::a(  "The Author",     href="https://cran.r-project.org/package=BayesianFROC"))





        ),#Box


        shinydashboard::box(title = "Reference",
                            width=4,
                             background = "orange",
                            # Invalid color: gray. Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
                            collapsible  = TRUE,
                            collapsed  = FALSE,

                            shiny::h4(shiny::a(  "The Author",     href="https://cran.r-project.org/package=BayesianFROC"))



        ),#Box
        shinydashboard::box(title = "Reference",
                            width=4,
                            background = "teal",
                            # Invalid color: gray. Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
                            collapsible  = TRUE,
                            collapsed  = FALSE,

                            shiny::h4(shiny::a(  "The Author",     href="https://cran.r-project.org/package=BayesianFROC"))



        ),#Box
        shinydashboard::box(title = "Reference",
                            width=4,
                            background = "fuchsia",
                            # Invalid color: gray. Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
                            collapsible  = TRUE,
                            collapsed  = FALSE,

                            shiny::h4(shiny::a(  "The Author",     href="https://cran.r-project.org/package=BayesianFROC"))



        ),#Box



        shinydashboard::box(title = "Reference",
                            width=12,
                            background = "maroon",
                            # Invalid color: gray. Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
                            collapsible  = TRUE,
                            collapsed  = FALSE,

                            shiny::h4(shiny::a(  "The Author",     href="https://cran.r-project.org/package=BayesianFROC"))



        ),#Box








        shiny::br()


)

)#tabItems





















    )#dashboardBody
  )#dashboardPage
  # ))









  server1 <- shiny::shinyServer(function(input, output) {



    # This is use such as fit() instesad of fit
    fit <- shiny::reactive({

      fit <- BayesianFROC::fit_Bayesian_FROC(
        ite  = input$Number_of_MCMC_samples,
        cha =  input$Number_of_MCMC_chains,
        summary = FALSE,
        Null.Hypothesis  = FALSE,
        dataList = values[["dataList"]],# input$selected_data ,
        DrawCurve  = FALSE,
        dig = 5)

      return(fit)

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
                                   C=length(DF[,1])
      )

    })

    output$hot <- rhandsontable::renderRHandsontable({
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
      NL<-values[["dataList"]]$NL
      NI<-values[["dataList"]]$NI

      z<-apply( extract(fit())$z , 2, mean)
      p<-apply( extract(fit())$p , 2, mean)
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

      for (cd in 1:C){
        s<-paste0(s," $$H_",cd," \\sim \\text{Binomial}(",signif(p[cd],digits = 3),",", NL,"),$$")
      }

      for (cd in 1:C){
        s<-paste0(s," $$F_",cd," \\sim \\text{Poisson}(",signif(dl[cd]*NI,digits = 3),"),$$")
      }

      shiny::withMathJax(s)
    })








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

      s<-paste0(" $$\\text{AUC}= \\Phi (\\frac{\\hat{a}}{\\sqrt{1+\\hat{b}^2}})=\\Phi (\\frac{",a,"}{\\sqrt{1+",b,"^2}}) = ",A,"$$ where estimates are posterior mean.")
      shiny::withMathJax(s)
    })



    output$formula.WAIC <- shiny::renderUI({



      WAIC <- fit()@WAIC
      WAIC <- signif(WAIC,digits = 3)

      s<-paste0(" $$\\text{WAIC}  = ",WAIC,"$$ where estimates are posterior mean.")
      shiny::withMathJax(s)
    })


    output$formula.chisquare <- shiny::renderUI({



      chisquare <- fit()@chisquare
      chisquare <- signif(chisquare,digits = 3)

      s<-paste0(" $$\\chi^2(\\text{data}|\\widehat{\\theta})   = ",chisquare,"$$ where estimates  \\( \\widehat{ \\theta }\\) are at posterior mean of model parameters \\(  \\theta \\).")
      shiny::withMathJax(s)
    })





    output$formula.TeX <- shiny::renderUI({
      s<-paste0(" To show \\( \\TeX \\), internnet environment is required.")
      shiny::withMathJax(s)
    })










    #################################################### Without tex





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

      s<-paste0("AUC =  ",A,".")
      shiny::withMathJax(s)
    })



    output$formula.WAIC.without.TeX <- shiny::renderUI({



      WAIC <- fit()@WAIC
      WAIC <- signif(WAIC,digits = 3)

      s<-paste0(" WAIC  = ",WAIC,".")
      shiny::withMathJax(s)
    })


    output$formula.chisquare.without.TeX <- shiny::renderUI({



      chisquare <- fit()@chisquare
      chisquare <- signif(chisquare,digits = 3)

      s<-paste0("chi^2   = ",chisquare,".")
      shiny::withMathJax(s)
    })

    #####################################################  Without tex










    # output$formula <- renderUI({
    #   my_calculated_value <- extractAUC(fit(),dig = 4)[1]
    #   withMathJax(paste0("Posterior mean of AUC (area under the AFROC curve): $$\\widehat{AUC} =", my_calculated_value,"$$"))
    # })


    output$distPlot <- shiny::renderPrint({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL
      if (sum(h)<=NL) {

        fitt <-methods::as(fit(), "stanfit")
        print( fitt, digits = 4)

      }# if

      if (sum(h)>NL) { print("Data format error, number of hits never greater than the number of signals. In Radiological context, signal is lesion.")  }

    })# shiny::renderPrint




    output$DrawCurves <- shiny::renderPlot({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL
      if (sum(h)<=NL) {

        DrawCurves(fit(), Colour  = FALSE, new.imaging.device  = FALSE,
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

        graphics::text(0.5,0.8,c("*Now, Sum of the number of hits is greater than that of lesions; \n\n", expression(paste(h1+h2+h3+... , "         >         Number of Lesions")) ),col="blue",cex =    1.4  )
        graphics::text(0.5,0.65,paste("In the current inputed data, it is the following: " ),col="black",cex =  1.5  )
        graphics::text(0.5,0.5,paste(  sum.of.h , "      >       ",NL,sep = ""),col="red",cex = 2)

        graphics::text(0.5,0.3,c("* Please fix so that the following inequality holds \n", expression(paste(h1+h2+h3+... , "       <        Number of lesions"))),col="blue",cex =  1.5    )
        graphics::text(0.5,0.1,c("* Shoud decrease the number of hits  or \n  Shoud increase the number of lesions"),col="blue",cex = 1.5  )




      }#if

    })#shiny::renderPlot




    ###############################################################################


    output$bi_normal <- shiny::renderPlot({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL


      if (sum(h)<=NL) {
        draw_latent_signal_distribution(fit() ,
                       new.imaging.device = FALSE,
                       dark_theme=FALSE,
                       hit.rate = TRUE,
                       false.alarm.rate = FALSE,
                       color = TRUE

                       )
      }

      if (sum(h)> NL) {
        error_message(h,NL)
      }#if

    })#shiny::renderPlot

    # output$print_bi_normal <- shiny::renderPrint({
    #   h<-values[["dataList"]]$h
    #   NL<-values[["dataList"]]$NL
    #   if (sum(h)<=NL) {
    #
    #     print(       draw_latent_signal_distribution(fit() , new.imaging.device = FALSE,dark_theme=FALSE,hit.rate = TRUE,false.alarm.rate = FALSE)
    #                  , digits = 4)
    #
    #   }# if
    #
    #   if (sum(h)>NL) { print("Data format error, number of hits never greater than the number of signals. In Radiological context, signal is lesion.")  }
    #
    # })# shiny::renderPrint








 shiny::observeEvent(input$trigger_save_a_fitted_model_object,{

   fit <- fit()
   save( fit,file =paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"\\fit") )
   # save( fit,file ="fit" )


   tcltk::tkmessageBox(message=paste("\n* A file is created in Desktop\n* Created file name: fit \n* Object name: fit\n* Class: S4 class\n* S4 class name: stanfitExtended\n\n\n* Using R script load(\"fit\"), the R object named \"fit\" is available from R console. "))

 }  )






    output$false_rate <- shiny::renderPlot({
      h<-values[["dataList"]]$h
      NL<-values[["dataList"]]$NL


      if (sum(h)<=NL) {
        # draw_latent_signal_distribution(fit() , new.imaging.device = FALSE,dark_theme=FALSE,hit.rate = TRUE,false.alarm.rate = FALSE)

        draw_latent_noise_distribution(fit(),
                                  new.imaging.device = FALSE,
                                  dark_theme=FALSE,
                                  false.alarm.rate = TRUE,
                                  hit.rate  = FALSE,
                                  both.hit.and.false.rate  = FALSE,
                                  color = TRUE

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



    ##################################################################################


  })

  ## run app
  shiny::runApp(list(ui=ui1, server=server1))
  return(invisible())
}

# df <- data.frame(h=c(44,22,3,4),f=c(11,22,33,44))
# editTable()
