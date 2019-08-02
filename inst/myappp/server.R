
DF<-data.frame(
  m =ddd$m,
  q=ddd$q,
  c=ddd$c,
  h=ddd$h,
  f=ddd$f
)

C<-ddd$C
M<-ddd$M
Q<-ddd$Q



server <- shiny::shinyServer(function(input, output) {



  # This is use such as fit() instesad of fit
  fit <-shiny::reactive({

    fit <- BayesianFROC::fit_Bayesian_FROC(
      ite  = input$Number_of_MCMC_samples,
      cha =  input$Number_of_MCMC_chains,
      summary = T,
      Null.Hypothesis = F,
      dataList = values[["dataList"]],# input$selected_data ,
      # dataList =dd,# input$selected_data ,

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
                                   # useTypes = as.logical(input$useType),
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


  output$print_fit <- shiny::renderPrint({

      fitt <-methods::as(fit(), "stanfit")
      print( fitt, digits = 4)

                    })# shiny::renderPrint






})
