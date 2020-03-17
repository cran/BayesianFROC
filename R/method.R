
#
# setMethod("summary",
#           signature(x = "stanfitExtended",y="missing"),
#           definition = function(x){  #plot( x@plotdata$x.AFROC, x@plotdata$y.AFROC)
#             DrawCurves(x)
#           }
#
#  )

##plot(fit, "stanfitExtended")

# summary is used in old class, if we define a summary for
# new class, then the codes use summary as new class leads us to errors.
# summary and print is similar, so it seems better not to use the summary.
# setMethod("summary",
#           signature( object="stanfitExtended"),
#           definition = function(object){
#             if(object@studyDesign=="srsc"){ summary_EAP_CI_srsc(object)}
#             if(object@studyDesign=="MRMC"){ summary_AUC_comparison_MRMC(object)}
#
#             print(  methods::as(object, "stanfit"))
#
#
#           }
#          )


###################################################
#
# plot_Bayesian_FROC <- function(x,...)UseMethod("plot_Bayesian_FROC",x)
#
# plot_Bayesian_FROC.stanfitExtended <- function(object,
#                                                         modalityID,
#                                                         readerID,
#                                                         mesh.for.drawing.curve=10000,
#                                                         Colour=TRUE,
#                                                         DrawFROCcurve=TRUE,
#                                                         DrawAFROCcurve=FALSE,
#                                                         DrawCFPCTP=TRUE,
#                                                         Draw.Flexible.upper_y=TRUE,
#                                                         Draw.Flexible.lower_y=TRUE){
#   if(StanS4class@studyDesign=="MRMC"){ DrawCurves_MRMC_pairwise(
#     object,
#     modalityID,
#     readerID,
#     mesh.for.drawing.curve=10000,
#     Colour=TRUE,
#     DrawFROCcurve=TRUE,
#     DrawAFROCcurve=FALSE,
#     DrawCFPCTP=TRUE,
#     Draw.Flexible.upper_y=TRUE,
#     Draw.Flexible.lower_y=TRUE
#
#
#   )}
#   if(StanS4class@studyDesign=="srsc"){ fit_Bayesian_FROC(StanS4class@dataList)}
#
# }#function

####################################################################









# setMethod("aaa",
#           signature=c(
#             "stanfitExtended",
#                  "numeric",
#                   "numeric",
#                    "numeric",
#                  "logical",
#                 "logical",
#             "logical",
#                  "logical",
#                 "logical",
#                 "logical"
#             #
#             #               # object="stanfitExtended",
#             #               # modalityID ="numeric",
#             #               # readerID   ="numeric",
#             #               # mesh.for.drawing.curve="numeric",
#             #               # Colour="logical",
#             #               # DrawFROCcurve="logical",
#             #               # DrawAFROCcurve="logical",
#             #               # DrawCFPCTP="logical",
#             #               # Draw.Flexible.upper_y="logical",
#             #               # Draw.Flexible.lower_y="logical"
#
#           ),
#           definition = function(object,
#                                 modalityID,
#                                 readerID,
#                                 mesh.for.drawing.curve=10000,
#                                 Colour=TRUE,
#                                 DrawFROCcurve=TRUE,
#                                 DrawAFROCcurve=FALSE,
#                                 DrawCFPCTP=TRUE,
#                                 Draw.Flexible.upper_y=TRUE,
#                                 Draw.Flexible.lower_y=TRUE){
#             if(StanS4class@studyDesign=="MRMC"){ DrawCurves_MRMC_pairwise(
#               object,
#               modalityID,
#               readerID,
#               mesh.for.drawing.curve=10000,
#               Colour=TRUE,
#               DrawFROCcurve=TRUE,
#               DrawAFROCcurve=FALSE,
#               DrawCFPCTP=TRUE,
#               Draw.Flexible.upper_y=TRUE,
#               Draw.Flexible.lower_y=TRUE
#
#
#             )}
#             if(StanS4class@studyDesign=="srsc"){ fit_Bayesian_FROC(StanS4class@dataList)}
#
#           }#function
# )
#
#
#
