
library(BayesianFROC)

# 1) First, we prepare the dataset for a single reader and a single modality.


showGM()

viewdata(dataList.Chakra.1.with.explantation)

# pause()#1 /10  ----  we fit our model to the above data


fit <- fit_Bayesian_FROC( ite  = 1111, summary = TRUE,  cha=3,  dataList = dataList.Chakra.1,new.imaging.device = TRUE,DrawAFROCcurve = TRUE )


DrawCurves(fit,Colour = FALSE,DrawFROCcurve = FALSE ,DrawAFROCcurve = FALSE,DrawCFPCTP = TRUE)
DrawCurves(fit,Colour = TRUE,DrawFROCcurve = FALSE ,DrawAFROCcurve = FALSE,DrawCFPCTP = TRUE)
DrawCurves(fit,Colour = TRUE,DrawFROCcurve = FALSE ,DrawAFROCcurve = FALSE,DrawCFPCTP = TRUE, upper_y = 0.7)
DrawCurves(fit,Colour = FALSE)
DrawCurves(fit,Colour = FALSE,DrawAFROCcurve = TRUE)
DrawCurves(fit,Colour = TRUE,DrawFROCcurve = TRUE ,DrawAFROCcurve = FALSE,DrawCFPCTP = TRUE, upper_y = 0.6)

# pause()#2 /10  ---- Examine the bias

datasets <-validation.dataset_srsc_for_different_NI_NL(
  NLvector = c(100,10000000,1000000000),
  ite = 2222
)
# pause()#3 /10  ---- p-value





p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit(fit,plot.replicated.points = FALSE)

# pause()#4 /10  ---- Bi normal assumption ----- High AUC--------



viewdata(dataList.High)


# pause()#5 /10  ----- Fitting (in the high ability case).

fit.High <- fit_Bayesian_FROC(dataList.High,ite  = 1111)


# pause()#6 /10  ----- draw a bi normal assumption----- High AUC--------

draw_bi_normal_version_UP(fit.High)

# pause()#7 /10  ---- Bi normal assumption ----- Low AUC--------


viewdata(dataList.Low)
# pause()#8 /10  ----- Fitting (in the low ability case).

fit.Low <- fit_Bayesian_FROC(dataList.Low,ite  = 1111)
# pause()#9 /10  ----- draw a bi normal assumption----- Low AUC--------

draw_bi_normal_version_UP(fit.Low)


message("
# The R scripts used in the demo.


# Graphical representation of the model.
", crayon::bgBlack$cyan$bold$italic$underline(" showGM()  "),"



# Show an example dataset
", crayon::bgBlack$cyan$bold$italic$underline(" viewdata(dataList.Chakra.1.with.explantation)"),"



# fit our model to the above data
", crayon::bgBlack$cyan$bold$italic$underline(" fit <- fit_Bayesian_FROC( ite  = 1111, summary = TRUE,  cha=3,  dataList = dataList.Chakra.1,new.imaging.device = TRUE,DrawAFROCcurve = TRUE )"),"

# Draw FROC curves using the fitted model object
", crayon::bgBlack$cyan$bold$italic$underline(" DrawCurves(fit,Colour = FALSE,DrawFROCcurve = FALSE ,DrawAFROCcurve = FALSE,DrawCFPCTP = TRUE)"),"
", crayon::bgBlack$cyan$bold$italic$underline(" DrawCurves(fit,Colour = TRUE,DrawFROCcurve = FALSE ,DrawAFROCcurve = FALSE,DrawCFPCTP = TRUE)"),"
", crayon::bgBlack$cyan$bold$italic$underline(" DrawCurves(fit,Colour = TRUE,DrawFROCcurve = FALSE ,DrawAFROCcurve = FALSE,DrawCFPCTP = TRUE, upper_y = 0.7)"),"
", crayon::bgBlack$cyan$bold$italic$underline(" DrawCurves(fit,Colour = FALSE)"),"
", crayon::bgBlack$cyan$bold$italic$underline(" DrawCurves(fit,Colour = FALSE,DrawAFROCcurve = TRUE)"),"
", crayon::bgBlack$cyan$bold$italic$underline(" DrawCurves(fit,Colour = TRUE,DrawFROCcurve = TRUE ,DrawAFROCcurve = FALSE,DrawCFPCTP = TRUE, upper_y = 0.6)"),"

#  Examine the bias in the case of convergence model only.
", crayon::bgBlack$cyan$bold$italic$underline(" datasets <-validation.dataset_srsc_for_different_NI_NL("),"
", crayon::bgBlack$cyan$bold$italic$underline("   NLvector = c(100,10000000,1000000000),"),"
", crayon::bgBlack$cyan$bold$italic$underline("   ite = 2222"),"
", crayon::bgBlack$cyan$bold$italic$underline(" )"),"



#   p-value in the Bayesian sence
", crayon::bgBlack$cyan$bold$italic$underline(" p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit(fit,plot.replicated.points = FALSE)"),"


# From Signal detection theory
# Now we fit model for two distinct datasets.
# One is high obeserver performance, and the another is low.
# Then compare the noise and signal distrubution.
# High ability case separate the noise and signal,
# On the other hand,
# the low ability case, the noise and signal distritbution
# is mixed.



#   ---- Bi normal assumption ----- High AUC--------
", crayon::bgBlack$cyan$bold$italic$underline(" viewdata(dataList.High)"),"
#   ----- Fitting (in the high ability case).
", crayon::bgBlack$cyan$bold$italic$underline(" fit.High <- fit_Bayesian_FROC(dataList.High,ite  = 1111)"),"
#  ----- draw a bi normal assumption----- High AUC--------
", crayon::bgBlack$cyan$bold$italic$underline(" draw_bi_normal_version_UP(fit.High)"),"



#   ---- Bi normal assumption ----- Low AUC--------
", crayon::bgBlack$cyan$bold$italic$underline(" viewdata(dataList.Low)"),"
#  ----- Fitting (in the low ability case).
", crayon::bgBlack$cyan$bold$italic$underline(" fit.Low <- fit_Bayesian_FROC(dataList.Low,ite  = 1111)"),"
# ----- draw a bi normal assumption----- Low AUC--------
", crayon::bgBlack$cyan$bold$italic$underline(" draw_bi_normal_version_UP(fit.Low)"),"









        ")




# Demo finished !!
