


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

