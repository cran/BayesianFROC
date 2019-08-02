
DF<-data.frame(h=c( 97L,   32L,   31L),f=c( 1L ,  14L,   74L ))  #The L corse iach numbers to integer.

server <- shiny::shinyServer(function(input, output) {



  # This is use such as fit() instesad of fit
  fit <-shiny::reactive({

    fit <- BayesianFROC::fit_Bayesian_FROC(
       ite  = input$Number_of_MCMC_samples,

      # ite  = input$Number_of_MCMC_samples[2],   # Warming up should not be include which distrub user.2019 July 9
      # war =  input$Number_of_MCMC_samples[1],  # Warming up should not be include which distrub user.2019 July 9
      cha =  input$Number_of_MCMC_chains,
      summary = F,
      Null.Hypothesis = F,
      dataList = values[["dataList"]],# input$selected_data ,
      DrawCurve = F,
      dig = 5)

    return(fit)

  })


  values <- shiny::reactiveValues( )

  # rv <- shiny::reactiveValues(prev_hot = NULL,prev_x =NULL)
  rv <- shiny::reactiveValues()
  # rvv <- shiny::reactiveValues(s =1)
#
#   observeEvent(input$hot, {
#      rvv$s <- c( rvv$s, rvv$s[length(rvv$s)] +1)
#   })

    observeEvent(c(input$Number_of_MCMC_samples, input$hot), {
    rv$s <-c(rv$s, length( rv$s)+1)

    # rv$s <-c(rv$s, length( rv$s)+1)
    # rv$tt <-length( rv$s[length(rv$s) ])/2
    # rv$t <- c(rv$t,rv$tt)
    # rv$WAIC_history <- c(rv$WAIC_history,
    #                      fit()@WAIC)
    # rvv$prev_x <- c( rvv$prev_x, rvv$prev_x +1)
  })

  # output$history <- renderPrint({
  #   paste(rv$prev_hot, collapse = ",")
  #
  # })
  output$history <- renderPrint({
                    # paste(rv$s, collapse = ",")
                    paste(rv$s, collapse = ",")

                    })

  output$formula <- renderUI({
                    my_calculated_value <- extractAUC(fit(),dig = 4)[1]
                    withMathJax(paste0("Posterior mean of AUC (area under the AFROC curve): $$\\widehat{AUC} =", my_calculated_value,"$$"))
                  })




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
                      h<-values[["dataList"]]$h
                      NL<-values[["dataList"]]$NL
                      if (sum(h)<=NL) {

                        fitt <-methods::as(fit(), "stanfit")
                        print( fitt, digits = 4)

                      }# if

                      if (sum(h)>NL) { print("Data format error, number of hits never greater than the number of signals. In Radiological context, signal is lesion.")  }

                    })# shiny::renderPrint



  output$p_value <- shiny::renderPrint({
                      h<-values[["dataList"]]$h
                      NL<-values[["dataList"]]$NL
                      if (sum(h)<=NL) {

                        # fitt <-methods::as(fit(), "stanfit")
                        # p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit(fit(),counter.plot.via.schatter.plot = F,Show.table = F)
                         print( fit()@chisquare  )

                      }# if

                      if (sum(h)>NL) { print("Data format error, number of hits never greater than the number of signals. In Radiological context, signal is lesion.")  }

                    })# shiny::renderPrint


  output$WAIC <- shiny::renderPrint({
                      h<-values[["dataList"]]$h
                      NL<-values[["dataList"]]$NL
                      if (sum(h)<=NL) {

                        # fitt <-methods::as(fit(), "stanfit")
                        # p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit(fit(),counter.plot.via.schatter.plot = F,Show.table = F)
                        print( fit()@WAIC  )

                      }# if

                      if (sum(h)>NL) { print("Data format error, number of hits never greater than the number of signals. In Radiological context, signal is lesion.")  }

                    })# shiny::renderPrint




  output$extractAUC <- shiny::renderPrint({
                        h<-values[["dataList"]]$h
                        NL<-values[["dataList"]]$NL
                        if (sum(h)<=NL) {

                          # fitt <-methods::as(fit(), "stanfit")
                          # print( check_hmc_diagnostics(fitt), digits = 4)
                           # print(  extractAUC(fit) )
                         extractAUC(fit(),dig = 5)
                         # print(AUC)
                        }# if

                        if (sum(h)>NL) { print("Data format error, number of hits never greater than the number of signals. In Radiological context, signal is lesion.")  }

                      })# shiny::renderPrint





  error_message <- function(h,NL){
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
                    }#function error messsage

  # expression_making <- function(h,NL){
  #   h.string <- as.character(h)
  #   for (cd  in 1:length(h.string)) {
  #     if (cd==1){ s<-""; s <- paste(h.string[cd],sep = ",")}
  #     if ( !cd==1)s <- paste(s,h.string[cd],sep = ",")
  #
  #   }#for
  #
  #  ( ss <- parse(text=c("(",s,")"),srcfile=NULL  )   )
  #   # parse(ss,srcfile = NULL)
  #   # parse(ss)
  #
  #
  # }#function







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



                   error_message(h,NL)



                      }#if

                    })#shiny::renderPlot








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



  output$message_test <- renderUI({
                      HTML(
                        paste(capture.output(type = "message", expr = {
                          message(capture.output(type = "output", expr = {
                            rstan::check_hmc_diagnostics(fit() )
                            # cat("test cat<br>")
                            # message("test message")
                            # cat("test cat2<br>")
                            # message("test message2")
                          }))
                        }), collapse="<br>")
                      )})














})
