library(BayesianFROC)

dataList.Chakra.Web
# pause()
viewdata(dataList.Chakra.Web)
# pause()
fit <- fit_Bayesian_FROC(dataList.Chakra.Web,ite = 1000,summary = TRUE,PreciseLogLikelihood = TRUE)
# pause()
DrawCurves_MRMC_pairwise(fit)
# pause()
DrawCurves_MRMC_pairwise(fit,modalityID = c(1,2),readerID = c(1))
DrawCurves_MRMC_pairwise(fit,modalityID = c(1,2),readerID = c(1),Colour = FALSE)

# pause()
DrawCurves_MRMC_pairwise(fit,modalityID = c(3,4),readerID = c(1))
DrawCurves_MRMC_pairwise(fit,modalityID = c(3,4),readerID = c(1),Colour = FALSE)

# pause()
DrawCurves_MRMC_pairwise(fit,modalityID = c(2,4),readerID = c(3))
DrawCurves_MRMC_pairwise(fit,modalityID = c(2,4),readerID = c(3),Colour = FALSE)

# pause()
DrawCurves_MRMC_pairwise(fit,modalityID = c(3,1),readerID = c(1,2,3,4))
DrawCurves_MRMC_pairwise(fit,modalityID = c(3,1),readerID = c(1,2,3,4),Colour = FALSE)

# Demo finished !!


message("
                # The R scripts used in the demo.




# Multiple-reader and mutliple-modality data
", crayon::bgBlack$cyan$bold$italic$underline("dataList.Chakra.Web"),"
# Show data
", crayon::bgBlack$cyan$bold$italic$underline("viewdata(dataList.Chakra.Web)"),"
# Fit
", crayon::bgBlack$cyan$bold$italic$underline("fit <- fit_Bayesian_FROC(dataList.Chakra.Web,ite = 1000,summary = TRUE,PreciseLogLikelihood = TRUE)"),"
# Plot a curve
", crayon::bgBlack$cyan$bold$italic$underline("DrawCurves_MRMC_pairwise(fit)"),"
# Plot a curve for the 1-st 2-nd modalty of 1-st reader
", crayon::bgBlack$cyan$bold$italic$underline("DrawCurves_MRMC_pairwise(fit,modalityID = c(1,2),readerID = c(1))"),"
# Plot a curve for the 3-rd 4-th modalty of 1-st reader
", crayon::bgBlack$cyan$bold$italic$underline("DrawCurves_MRMC_pairwise(fit,modalityID = c(3,4),readerID = c(1))"),"
# Plot a curve for the 2-nd 4-th modalty of 1-st reader
", crayon::bgBlack$cyan$bold$italic$underline("DrawCurves_MRMC_pairwise(fit,modalityID = c(2,4),readerID = c(3))"),"
# Plot a curve for all possible pair of the 3-rd, 1-st modalty  and  1-st, 2-nd,3-rd,4-th reader

", crayon::bgBlack$cyan$bold$italic$underline("DrawCurves_MRMC_pairwise(fit,modalityID = c(3,1),readerID = c(1,2,3,4))"),"
# Demo finished !
        ")
