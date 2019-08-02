#' @title Fit with GUI via Shiny (Simple version)
#' @description simple is vest
#' @details  First, please execute, then user will understand what it is.
#' This function is the one of the most important function in this package.
#' I do not assume that the user is familiar with R script but FROC analysis. So, I made this function to provide the Graphical User Interface (GUI) for users.
#' I hope it helps someone in the world.
#' @param DF initial data to be fited
# @param outdir I use \code{  system.file("myapp", package = "BayesianFROC")    }
# @param outfilename I do not know :'-D
#' @param NL.max max number of bins indicating number of lesions
#' @param NI.max max number of bins indicating number of images
#' @param MCMC.chains.max max number of bins indicating number of MCMC chains
#'@author Issei Tsunoda
#' @return None
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#'
#'#----------------------------------------------------------------------------------------
#'#            1)           Use the default User Interface
#'#----------------------------------------------------------------------------------------
#'#'
#'
#'  #No need to consider the variables, it is sufficient in  default values.
#'
#'
#'  fit_GUI_simple()
#'
#'
#'
#'
#'#----------------------------------------------------------------------------------------
#'#            2)           Change the  User Interface
#'#----------------------------------------------------------------------------------------
#'
#'
#'#  We can change the max imput of the number of lesions and the max of number of images
#'#
#'
#'  fit_GUI_simple(NL.max = 2222,
#'                 NI.max = 3333)
#'
#'
#'
#'
#'
#'#----------------------------------------------------------------------------------------
#'#            3)           Change the  Default value
#'#----------------------------------------------------------------------------------------
#'
#'
#'
#'  fit_GUI_simple(
#'  DF= data.frame( h=dataList.Chakra.4$h,
#'                  f=dataList.Chakra.4$f
#'                )
#'                )
#'
#'# Or equivalently,
#'
#'  fit_GUI_simple(
#'             DF= data.frame(
#'             h = c(160,  25,  15,   7),
#'             f = c(  8,  16,  18,  13)
#'                          )
#'             )
#'
#'
#'#----------------------------------------------------------------------------------------
#'#            4)           Change the user Imterface
#'#----------------------------------------------------------------------------------------
#'
#'
#'
#'fit_GUI_simple(
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
#'
#'
#'}
#'

fit_GUI_simple <- function(
                  DF=data.frame(h=c( 97L,   32L,   31L),f=c( 1L ,  14L,   74L )),

                  NL.max=1111,
                  NI.max=1111,
                  # MCMC.samples.max = 11111,
                  MCMC.chains.max=4


                  ){


  outdir <- system.file("myapp", package = "BayesianFROC")
   outfilename <- "table"




  if (outdir == "") {
    stop("Could not find myapp. Try re-installing `BayesianFROC`.", call. = FALSE)
  }



ui1 <- shiny::shinyUI(

  # shiny::fluidPage(
  shiny::navbarPage("Emplooooo\\(^o^)/oooooooy   me! :'-D",




shiny::tabPanel(":)",



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




    "))),

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




        # shiny::h2(" FROC Data"),
shiny::h1("Data"),
                        shiny::h4(shiny::helpText(  "Right-click on the table to delete/insert rows." )),
                        shiny::h4(shiny::helpText(    "Double-click on a cell to edit")),
                          shiny::h5(shiny::helpText(" h = hit = True Positive = TP.")),
                          shiny::h5(shiny::helpText(" f = false alarms = False Positive = FP.")),

                        rhandsontable::rHandsontableOutput("hot"),# Table of h and f ###########################################################




        # shiny::h4(shiny::helpText(" Number of lesions should be greater than sum of TP.")),
        # shiny::h5(shiny::helpText(" Lesion = signal.")),


        shiny::sliderInput("Number_of_lesions",
                           "Number of lesions:",
                           min =  1, max = NL.max,
                           value = 259# Default ####################################################################################
        ),


        shiny::sliderInput("Number_of_images",
                           "Number of images:",
                           min = 1, max = NI.max,
                           value = 57#Default    ##########################################################################
        ),

        shiny::plotOutput("DrawCurves", dblclick = shiny::dblclickOpts(id = "plot_dbl_click")),

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








        shiny::h4("Parameter of the Hamiltonian MonteCarlo Sampling"),

                      shiny::h4(shiny::helpText(" Larger is better.")),

                      shiny::sliderInput("Number_of_MCMC_samples",
                                         "Number of MCMC samples:",
                                         min = 111, max = 11111, value = 1111),

                      shiny::h4(shiny::helpText(" Larger is better.")),

                      shiny::sliderInput("Number_of_MCMC_chains",
                                         "Number of MCMC chains:",
                                         min = 1, max = MCMC.chains.max, value = 1),



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

                 shiny::wellPanel(
               shiny::h6(shiny::helpText(" Larger is better.")),

                 shiny::uiOutput("formula.AUC.without.TeX"),
               shiny::h6(shiny::helpText(" Smaller is better.")),

                 shiny::uiOutput("formula.WAIC.without.TeX"),
               shiny::h6(shiny::helpText(" Smaller is better.")),

                 shiny::uiOutput("formula.chisquare.without.TeX")
                 ),
                        # shiny::h2(" FROC Curve and FPF,TPF"),
                  # shiny::h4(shiny::helpText("I love you :'-D  If you love me, then employ me :)")),
                  # shiny::h4(shiny::helpText("In Hit rate plain, the canonical Gaussian is also drawn to show how the graph changes.  ")),
                  # shiny::h4(shiny::helpText("In False rate plain, the signal Gaussian is also drawn to show how the graph changes.  ")),
                  # shiny::h4(shiny::helpText("The thresholds are common between false alram and hit.")),
                 # shiny::h6(shiny::helpText("Internet Environment is required for TeX script.")),


                shiny::plotOutput("bi_normal", dblclick = shiny::dblclickOpts(id = "plot_dbl_click")),
                shiny::plotOutput("false_rate", dblclick = shiny::dblclickOpts(id = "plot_dbl_click")),


                shiny::h2(" Posterior Estimates"),

                                shiny::verbatimTextOutput("distPlot"),

                                shiny::verbatimTextOutput("print_bi_normal"),###############################





















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

shiny::tabPanel("doggy",
         shiny::h4(shiny::helpText(" When we wash doggy, Never use the washer for doggy. doggy does not require to take a bus, so they need only warm water only, do not use the sarfactant for them!!")),
         shiny::h4(shiny::helpText("Best regards,  ")),

         shiny::h4(shiny::a(  "doggy",     href="https://cran.r-project.org/package=BayesianFROC"))




),#tabPanel


shiny::tabPanel("Piggy",
                shiny::h4(shiny::helpText("The author's older brother is biggy, his hiphop is very big hip! Piggy what are you doing, you still smoke in the water? Piggy is expert of chemistry, what are doing piggy, you still fat man? or big hip man?")),
                shiny::h4(shiny::helpText("Best regards,  ")),

                shiny::h4(shiny::a(  "Piggy",     href="https://cran.r-project.org/package=BayesianFROC"))




),#tabPanel

shiny::tabPanel("Rat",
                shiny::h4(shiny::helpText("The author's younger brother is rat, his teeth is outer and outer and outer. What are doing rat? you still work very hard position? Are you happy now? You have a lot of money, but you working eight days a week, is your own life? ")),
                shiny::h4(shiny::helpText("Best regards,  ")),

                shiny::h4(shiny::a(  "Rat",     href="https://cran.r-project.org/package=BayesianFROC"))




),#tabPanel

shiny::tabPanel("Tiny",
                shiny::h4(shiny::helpText("The author's younger and younger brother is nothing but his pokemon is tiny, pratin.")),
                shiny::h4(shiny::helpText("Best regards,  ")),

                shiny::h4(shiny::a(  "Pratin",     href="https://cran.r-project.org/package=BayesianFROC"))




),#tabPanel

shiny::tabPanel("Tiny",
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






shiny::tabPanel("Mosquitowbus University",
         shiny::h4(shiny::helpText(" I am a professor in the Mosquitow univeristy
                                   in which I study how to fight to mosquito in the
                                   dark night. Mosquitos threaten me with his poooo00000000ooooooo0000000000ooooooooooo000000000000000ooooooon,
                                    voice, so I try to cathe them, but their sterus cannot
                                   catch. I have to establish new theory how to fight them
                                   to ensure our stable sleepping.

                                   When I 17 years old, I establish the Karbus university and research how to fight kar ( which means mosquito in some language).
                                   Yes, I am the fist president of Karbus university.

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
)#shinyUI
# ))
server1 <- shiny::shinyServer(function(input, output) {



# This is use such as fit() instesad of fit
fit <- shiny::reactive({

fit <- BayesianFROC::fit_Bayesian_FROC(
              ite  = input$Number_of_MCMC_samples,
              cha =  input$Number_of_MCMC_chains,
              summary = F,
              Null.Hypothesis = F,
              dataList = values[["dataList"]],# input$selected_data ,
              DrawCurve = F,
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

                            DrawCurves(fit(), Colour = F, new.imaging.device = F,
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




###############################################################################


    output$bi_normal <- shiny::renderPlot({
                          h<-values[["dataList"]]$h
                          NL<-values[["dataList"]]$NL


                          if (sum(h)<=NL) {
                            draw_bi_normal(fit() , new.imaging.device = FALSE,dark_theme=FALSE,hit.rate = TRUE,false.alarm.rate = FALSE)
                          }

                          if (sum(h)> NL) {
                            error_message(h,NL)
                          }#if

                        })#shiny::renderPlot

    output$print_bi_normal <- shiny::renderPrint({
                              h<-values[["dataList"]]$h
                              NL<-values[["dataList"]]$NL
                              if (sum(h)<=NL) {

                                print(       draw_bi_normal(fit() , new.imaging.device = FALSE,dark_theme=FALSE,hit.rate = TRUE,false.alarm.rate = FALSE)
                                             , digits = 4)

                              }# if

                              if (sum(h)>NL) { print("Data format error, number of hits never greater than the number of signals. In Radiological context, signal is lesion.")  }

                            })# shiny::renderPrint















    output$false_rate <- shiny::renderPlot({
                          h<-values[["dataList"]]$h
                          NL<-values[["dataList"]]$NL


                          if (sum(h)<=NL) {
                            # draw_bi_normal(fit() , new.imaging.device = FALSE,dark_theme=FALSE,hit.rate = TRUE,false.alarm.rate = FALSE)

                            draw_bi_normal_version_UP(fit(), new.imaging.device = FALSE,dark_theme=FALSE,false.alarm.rate = T,hit.rate = F,both.hit.and.false.rate = F)

                          }

                          if (sum(h)> NL) {
                            error_message(h,NL)
                          }#if

                        })#shiny::renderPlot

    output$print_bi_normal <- shiny::renderPrint({
                          h<-values[["dataList"]]$h
                          NL<-values[["dataList"]]$NL
                          if (sum(h)<=NL) {

                            print(       draw_bi_normal(fit() , new.imaging.device = FALSE,dark_theme=FALSE,hit.rate = TRUE,false.alarm.rate = FALSE)
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
