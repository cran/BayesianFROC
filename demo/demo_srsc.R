# 1) First, we prepare the dataset for a single reader and a single modality.


showGM()

viewdata(dataList.Chakra.1.with.explantation)

pause()#1 /10  ----  we fit our model to the above data


fit <- fit_Bayesian_FROC( ite  = 1111, summary = TRUE,  cha=3,  dataList = dataList.Chakra.1,new.imaging.device = TRUE,DrawAFROCcurve = TRUE )


DrawCurves(fit,Colour = FALSE,DrawFROCcurve = FALSE ,DrawAFROCcurve = FALSE,DrawCFPCTP = TRUE)
DrawCurves(fit,Colour = TRUE,DrawFROCcurve = FALSE ,DrawAFROCcurve = FALSE,DrawCFPCTP = TRUE)
DrawCurves(fit,Colour = TRUE,DrawFROCcurve = FALSE ,DrawAFROCcurve = FALSE,DrawCFPCTP = TRUE, upper_y = 0.7)
DrawCurves(fit,Colour = FALSE)
DrawCurves(fit,Colour = FALSE,DrawAFROCcurve = TRUE)
DrawCurves(fit,Colour = TRUE,DrawFROCcurve = TRUE ,DrawAFROCcurve = FALSE,DrawCFPCTP = TRUE, upper_y = 0.6)

pause()#2 /10  ---- Examine the bias

datasets <-validation.dataset_srsc_for_different_NI_NL(
  NLvector = c(100,10000000,1000000000),
  ite = 2222
)
pause()#3 /10  ---- p-value





p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit(fit,plot.replicated.points = FALSE)

pause()#4 /10  ---- Bi normal assumption ----- High AUC--------



viewdata(dataList.High)


pause()#5 /10  ----- Fitting (in the high ability case).

fit.High <- fit_Bayesian_FROC(dataList.High,ite  = 1111)


pause()#6 /10  ----- draw a bi normal assumption----- High AUC--------

draw_bi_normal(fit.High)

pause()#7 /10  ---- Bi normal assumption ----- Low AUC--------


viewdata(dataList.Low)
pause()#8 /10  ----- Fitting (in the low ability case).

fit.Low <- fit_Bayesian_FROC(dataList.Low,ite  = 1111)
pause()#9 /10  ----- draw a bi normal assumption----- Low AUC--------

draw_bi_normal(fit.Low)

# Demo finished !!