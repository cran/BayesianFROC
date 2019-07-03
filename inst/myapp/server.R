
DF <-data.frame(h=c( 97,   32,   31),f=c( 1 ,  14,   74 ))
# outdir<-system.file("myapp", package = "BayesianFROC")
outfilename<-"table"

shiny::shinyServer(function(input, output) {

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

    if (sum(h)>NL) { message("Data format error, number of hits never greater than the number of signals. In Radiological context, signal is lesion.")  }

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
