library(BayesianFROC)


#1) Build the data for singler reader and single modality  case.

data.example <- list(
  c=c(3,2,1),    #Confidence level
  h=c(97,32,31), #Number of hits for each confidence level
  f=c(1,14,74),  #Number of false alarms for each confidence level

  NL=259,       #Number of lesions
  NI=57,        #Number of images
  C=3)          #Number of confidence level


data.example <-  give_name_srsc_data(data.example)

viewdata(data.example)

draw.CFP.CTP.from.dataList(data.example)
showGM()
fit <- fit_Bayesian_FROC(data.example)




summary_EAP_CI_srsc(fit)
DrawCurves(fit,Colour = FALSE)
DrawCurves(fit,Colour = TRUE)
DrawCurves(fit,Colour = TRUE,DrawFROCcurve = FALSE,DrawAFROCcurve = TRUE)
DrawCurves(fit,Colour = TRUE,DrawFROCcurve = FALSE,DrawAFROCcurve = FALSE,DrawCFPCTP = TRUE)
DrawCurves(fit,Colour = TRUE,DrawFROCcurve = FALSE,DrawAFROCcurve = TRUE ,DrawCFPCTP = FALSE)
DrawCurves(fit,Colour = TRUE,DrawFROCcurve = TRUE,DrawAFROCcurve = FALSE ,DrawCFPCTP = FALSE)
draw_bi_normal(fit)

draw_bi_normal(fit,dark_theme = F)

# Demo finished !!



message("


        # The R scripts used in the demo.




", crayon::bgBlack$cyan$bold$italic$underline("library(BayesianFROC) "),"


#1) Build the data for singler reader and single modality  case.

", crayon::bgBlack$cyan$bold$italic$underline("data.example <- list("),"
", crayon::bgBlack$cyan$bold$italic$underline("c=c(3,2,1),   ")," #Confidence level
", crayon::bgBlack$cyan$bold$italic$underline("h=c(97,32,31), "),"#Number of hits for each confidence level
", crayon::bgBlack$cyan$bold$italic$underline("f=c(1,14,74), ")," #Number of false alarms for each confidence level

", crayon::bgBlack$cyan$bold$italic$underline("NL=259,     "),"  #Number of lesions
", crayon::bgBlack$cyan$bold$italic$underline("NI=57,      "),"  #Number of images
", crayon::bgBlack$cyan$bold$italic$underline("C=3)       "),"   #Number of confidence level

# Give   a name, redandunt?
", crayon::bgBlack$cyan$bold$italic$underline("data.example <-  give_name_srsc_data(data.example)"),"

# Show a table of data
", crayon::bgBlack$cyan$bold$italic$underline("viewdata(data.example)"),"

# Empirical FROC curve
", crayon::bgBlack$cyan$bold$italic$underline("draw.CFP.CTP.from.dataList(data.example)"),"

# Show graphical representation
", crayon::bgBlack$cyan$bold$italic$underline("showGM()"),"

# Fit a model to data
", crayon::bgBlack$cyan$bold$italic$underline("fit <- fit_Bayesian_FROC(data.example)"),"



# Show summary
", crayon::bgBlack$cyan$bold$italic$underline("summary_EAP_CI_srsc(fit)"),"

# Plot curves
", crayon::bgBlack$cyan$bold$italic$underline("DrawCurves(fit,Colour = FALSE)"),"
", crayon::bgBlack$cyan$bold$italic$underline("DrawCurves(fit,Colour = TRUE)"),"
", crayon::bgBlack$cyan$bold$italic$underline("DrawCurves(fit,Colour = TRUE,DrawFROCcurve = FALSE,DrawAFROCcurve = TRUE)"),"
", crayon::bgBlack$cyan$bold$italic$underline("DrawCurves(fit,Colour = TRUE,DrawFROCcurve = FALSE,DrawAFROCcurve = FALSE,DrawCFPCTP = TRUE)"),"
", crayon::bgBlack$cyan$bold$italic$underline("DrawCurves(fit,Colour = TRUE,DrawFROCcurve = FALSE,DrawAFROCcurve = TRUE ,DrawCFPCTP = FALSE)"),"
", crayon::bgBlack$cyan$bold$italic$underline("DrawCurves(fit,Colour = TRUE,DrawFROCcurve = TRUE,DrawAFROCcurve = FALSE ,DrawCFPCTP = FALSE)"),"
", crayon::bgBlack$cyan$bold$italic$underline("draw_bi_normal(fit)"),"

# Plot bi normal assumpution
", crayon::bgBlack$cyan$bold$italic$underline("draw_bi_normal(fit,dark_theme = F)"),"

# Demo finished !!


        ")
