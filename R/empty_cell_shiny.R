# library(rhandsontable)
# library(shiny)
#
# foo <- function(M = 2,
#                 Q = 3,
#                 C = 4) {
#   DF <- data.frame(
#     m = 1,
#     q = 2,
#     c = 3,
#
#     # To make empty cells, we should use NA_integer_ instead NA.
#
#     h= rep(NA_integer_, M * Q * C),  # Here, we should use NA_integer_ instead NA
#     f = rep(NA_integer_, M * Q * C) # Here, we should use NA_integer_ instead NA
#   )
#
#   ui <- shiny::fluidPage(
#
#     shiny::sidebarLayout(
#       shiny::sidebarPanel(
#
#
#         rhandsontable::rHandsontableOutput("hot")
#       ),
#       shiny::mainPanel()
#     )
#   )
#
#   server <-  function(input, output) {
#     values <- shiny::reactiveValues()
#
#     ## Handsontable
#     shiny::observe({
#       if
#
#       (!is.null(input$hot)) {
#         DF = rhandsontable::hot_to_r(input$hot)
#       } else {
#         if (is.null(values[["DF"]]))
#           DF <- DF
#         else
#           DF <- values[["DF"]]
#       }
#       values[["DF"]] <- DF
#       values[["dataList"]] <- list(
#         NL = input$Number_of_lesions,
#         NI = input$Number_of_images,
#         h = DF$h,
#         f = DF$f,
#         m = DF$m,
#         q = DF$q,
#         c = DF$c,
#         C = input$C,
#         M = input$M,
#         Q = input$Q
#       )
#     })
#
#     output$hot <- rhandsontable::renderRHandsontable({
#       DF <- values[["DF"]]
#       if (!is.null(DF))
#         rhandsontable::rhandsontable(DF,
#                                      stretchH = "all")
#     })
#   }
#   shiny::runApp(list(ui = ui, server = server))
#   return(invisible())
#
# } # function
#
# # foo()
