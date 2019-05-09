
#' @title  WAIC calculator
#'
#'
#'@description Using the S4 class by stan with \code{target += },
#'     we calculate the WAIC.
#'
#'@param dig  The number of significant digits of waic.
#'@inheritParams DrawCurves_MRMC_pairwise

#'@inheritParams fit_Bayesian_FROC
#'@param StanS4classwithTargetFormulation This is a  S4 class built by \code{stan}
#'function in the \code{rstan} package.
#'
#'In this package, we make a new S4 class which is inherited class of rstan's S4 class named "stanfit".
#'However this function can be available for stanfit S4 class.
#'
#'
#'@return  The output is the value of WAIC.
#'
#'@examples

# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#' \donttest{

#'#First, we prepare the data endowed with this package.
#'
#'         dat  <- get(data("dataList.Chakra.1"))
#'
#'
#'
#'
#'#Second, we run the stan funtion
#'#with data named "dat"   via the author's Bayesian model.
#'
#'
#'             fit <- fit_Bayesian_FROC(dat, PreciseLogLikelihood = TRUE)
#'

#'
#' #Now, As a return from above,
#' #we get the rstan::stan's out put which is S4 class named "fit".
#'
#'
#' #Using the output "fit",
#'
#'
#'
#'                  waic(fit)
#'
#'

#'
#'#The Author provide two model for FROC for single reader and single modality case.
#'#One is false alarm rates means "per lesion" and the other means "per image".
#'#The above "fit" is "per image". Now we shall consider to compare these two model
#'#by WAIC. To do so, next we shall fit the "per lesion" model as follows:
#'
#' fit2 <- fit_Bayesian_FROC(dat, PreciseLogLikelihood = TRUE, ModifiedPoisson=TRUE)
#'
#' waic(fit2)
#'
#'
#'
#'# By compare two model's WAIC we can say which model is better.
#'# Note that the smaller WAIC is better.
#'
#' waic(fit) # per lesion model
#' waic(fit2) # per image model
#'
#'
#'



#
#
#
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
# #   The second example: how WAIC depends on NI, that is number of images.
#
#
# #1) Build the data for singler reader and single modality  case.
#
#
# datf5 <- list(c=c(3,2,1),h=c(97,32,31),f=c(1,14,74),NL=259,NI=ceiling(57/5),C=3)
# datf4 <- list(c=c(3,2,1),h=c(97,32,31),f=c(1,14,74),NL=259,NI=ceiling(57/4),C=3)
# datf3 <- list(c=c(3,2,1),h=c(97,32,31),f=c(1,14,74),NL=259,NI=ceiling(57/3),C=3)
# datf2 <- list(c=c(3,2,1),h=c(97,32,31),f=c(1,14,74),NL=259,NI=ceiling(57/2),C=3)
# dat1 <- list(c=c(3,2,1),h=c(97,32,31),f=c(1,14,74),NL=259,NI=57,C=3)
# dat2 <- list(c=c(3,2,1),h=c(97,32,31),f=c(1,14,74),NL=259,NI=57*2,C=3)
# dat3 <- list(c=c(3,2,1),h=c(97,32,31),f=c(1,14,74),NL=259,NI=57*3,C=3)
# dat4 <- list(c=c(3,2,1),h=c(97,32,31),f=c(1,14,74),NL=259,NI=57*4,C=3)
#
#
# #2) Fit  and draw FROC and AFROC curves.
#
#
# fit.per.image.f5 <-   fit_Bayesian_FROC(datf5, PreciseLogLikelihood = TRUE )
# fit.per.image.f4 <-   fit_Bayesian_FROC(datf4, PreciseLogLikelihood = TRUE )
# fit.per.image.f3 <-   fit_Bayesian_FROC(datf3, PreciseLogLikelihood = TRUE )
# fit.per.image.f2 <-   fit_Bayesian_FROC(datf2, PreciseLogLikelihood = TRUE )
# fit.per.image.1 <-   fit_Bayesian_FROC(dat1, PreciseLogLikelihood = TRUE )
# fit.per.image.2 <-   fit_Bayesian_FROC(dat2, PreciseLogLikelihood = TRUE )
# fit.per.image.3 <-   fit_Bayesian_FROC(dat3, PreciseLogLikelihood = TRUE )
# fit.per.image.4 <-   fit_Bayesian_FROC(dat4, PreciseLogLikelihood = TRUE )
#
#
#
# fit.per.lesion.f5 <-   fit_Bayesian_FROC(datf5, PreciseLogLikelihood = T,
#                                          ModifiedPoisson=T)
# fit.per.lesion.f4 <-   fit_Bayesian_FROC(datf4, PreciseLogLikelihood = T,
#                                          ModifiedPoisson=T)
# fit.per.lesion.f3 <-   fit_Bayesian_FROC(datf3, PreciseLogLikelihood = T,
#                                          ModifiedPoisson=T)
# fit.per.lesion.f2 <-   fit_Bayesian_FROC(datf2, PreciseLogLikelihood = T,
#                                          ModifiedPoisson=T)
# fit.per.lesion.1 <-   fit_Bayesian_FROC(dat1, PreciseLogLikelihood = TRUE ,
#                                         ModifiedPoisson=T)
# fit.per.lesion.2 <-   fit_Bayesian_FROC(dat2, PreciseLogLikelihood = TRUE ,
#                                         ModifiedPoisson=T)
# fit.per.lesion.3 <-   fit_Bayesian_FROC(dat3, PreciseLogLikelihood = TRUE ,
#                                         ModifiedPoisson=T)
# fit.per.lesion.4 <-   fit_Bayesian_FROC(dat4, PreciseLogLikelihood = TRUE ,
#                                         ModifiedPoisson=T)
#
# x1    <- waic(fit.per.image.f5)
# x2   <- waic(fit.per.image.f4)
# x3    <- waic(fit.per.image.f3)
# x4    <- waic(fit.per.image.f2)
# x5    <- waic(fit.per.image.1)
# x6    <- waic(fit.per.image.2)
# x7    <- waic(fit.per.image.3)
# x8    <- waic(fit.per.image.4)
#
# y1    <- waic(fit.per.lesion.f5)
# y2    <- waic(fit.per.lesion.f4)
# y3    <- waic(fit.per.lesion.f3)
# y4    <- waic(fit.per.lesion.f2)
# y5    <- waic(fit.per.lesion.1)
# y6    <- waic(fit.per.lesion.2)
# y7    <- waic(fit.per.lesion.3)
# y8    <- waic(fit.per.lesion.4)
#
#
# z1     <- " datf5  "
# z2     <- " datf4  "
# z3     <- " datf3 "
# z4     <- " datf2 "
# z5     <- " dat1"
# z6     <- " dat2  "
# z7     <- " dat3  "
# z8     <-  "dat4 "
#
#
# knitr::kable( data.frame( data = c(z1,z2,z3,z4,z5,z6,z7,z8 ),
#                           per.image = c(x1,x2,x3,x4,x5,x6,x7,x8 ),
#                           per.lesion= c(y1,y2,y3,y4,y5,y6,y7,y8 )))
#
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
# #  The Third example: how WAIC depends on NI, that is number of lesions.
#
#
# #1) Build the data for singler reader and single modality  case.
#
# NI <-57
#
# datf5 <- list(c=c(3,2,1),h=c(97,32,31),f=c(1,14,74),NI=NI,NL=ceiling(259-10*5),C=3)
# datf4 <- list(c=c(3,2,1),h=c(97,32,31),f=c(1,14,74),NI=NI,NL=ceiling(259-10*4),C=3)
# datf3 <- list(c=c(3,2,1),h=c(97,32,31),f=c(1,14,74),NI=NI,NL=ceiling(259-10*3),C=3)
# datf2 <- list(c=c(3,2,1),h=c(97,32,31),f=c(1,14,74),NI=NI,NL=ceiling(259-10*2),C=3)
# dat1 <- list(c=c(3,2,1),h=c(97,32,31),f=c(1,14,74),NI=NI,NL=259,C=3)
# dat2 <- list(c=c(3,2,1),h=c(97,32,31),f=c(1,14,74),NI=NI,NL=259*2,C=3)
# dat3 <- list(c=c(3,2,1),h=c(97,32,31),f=c(1,14,74),NI=NI,NL=259*3,C=3)
# dat4 <- list(c=c(3,2,1),h=c(97,32,31),f=c(1,14,74),NI=NI,NL=259*4,C=3)
#
#
# #2) Fit  and draw FROC and AFROC curves.
#
#
# fit.per.image.f5 <-   fit_Bayesian_FROC(datf5, PreciseLogLikelihood = TRUE )
# fit.per.image.f4 <-   fit_Bayesian_FROC(datf4, PreciseLogLikelihood = TRUE )
# fit.per.image.f3 <-   fit_Bayesian_FROC(datf3, PreciseLogLikelihood = TRUE )
# fit.per.image.f2 <-   fit_Bayesian_FROC(datf2, PreciseLogLikelihood = TRUE )
# fit.per.image.1 <-   fit_Bayesian_FROC(dat1, PreciseLogLikelihood = TRUE )
# fit.per.image.2 <-   fit_Bayesian_FROC(dat2, PreciseLogLikelihood = TRUE )
# fit.per.image.3 <-   fit_Bayesian_FROC(dat3, PreciseLogLikelihood = TRUE )
# fit.per.image.4 <-   fit_Bayesian_FROC(dat4, PreciseLogLikelihood = TRUE )
#
#
#
# fit.per.lesion.f5 <-   fit_Bayesian_FROC(datf5, PreciseLogLikelihood = T,
#                                          ModifiedPoisson=T)
# fit.per.lesion.f4 <-   fit_Bayesian_FROC(datf4, PreciseLogLikelihood = T,
#                                          ModifiedPoisson=T)
# fit.per.lesion.f3 <-   fit_Bayesian_FROC(datf3, PreciseLogLikelihood = T,
#                                          ModifiedPoisson=T)
# fit.per.lesion.f2 <-   fit_Bayesian_FROC(datf2, PreciseLogLikelihood = T,
#                                          ModifiedPoisson=T)
# fit.per.lesion.1 <-   fit_Bayesian_FROC(dat1, PreciseLogLikelihood = TRUE ,
#                                         ModifiedPoisson=T)
# fit.per.lesion.2 <-   fit_Bayesian_FROC(dat2, PreciseLogLikelihood = TRUE ,
#                                         ModifiedPoisson=T)
# fit.per.lesion.3 <-   fit_Bayesian_FROC(dat3, PreciseLogLikelihood = TRUE ,
#                                         ModifiedPoisson=T)
# fit.per.lesion.4 <-   fit_Bayesian_FROC(dat4, PreciseLogLikelihood = TRUE ,
#                                         ModifiedPoisson=T)
#
#
# x1    <- waic(fit.per.image.f5)
# x2   <- waic(fit.per.image.f4)
# x3    <- waic(fit.per.image.f3)
# x4    <- waic(fit.per.image.f2)
# x5    <- waic(fit.per.image.1)
# x6    <- waic(fit.per.image.2)
# x7    <- waic(fit.per.image.3)
# x8    <- waic(fit.per.image.4)
#
# y1    <- waic(fit.per.lesion.f5)
# y2    <- waic(fit.per.lesion.f4)
# y3    <- waic(fit.per.lesion.f3)
# y4    <- waic(fit.per.lesion.f2)
# y5    <- waic(fit.per.lesion.1)
# y6    <- waic(fit.per.lesion.2)
# y7    <- waic(fit.per.lesion.3)
# y8    <- waic(fit.per.lesion.4)
#
# z1     <- " datf5  "
# z2     <- " datf4  "
# z3     <- " datf3 "
# z4     <- " datf2 "
# z5     <- " dat1"
# z6     <- " dat2  "
# z7     <- " dat3  "
# z8     <-  "dat4 "
#
#
# knitr::kable( data.frame( data = c(z1,z2,z3,z4,z5,z6,z7,z8 ),
#                           per.image = c(x1,x2,x3,x4,x5,x6,x7,x8 ),
#                           per.lesion= c(y1,y2,y3,y4,y5,y6,y7,y8 )))
#
# # devtools::document();help("waic") # Confirm reflection
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'}# dottest







#' @export waic

waic <-function(StanS4classwithTargetFormulation,dig=4,summary=TRUE){

  fit <-StanS4classwithTargetFormulation
  log_lik <- extract(fit)$lp__
  # lppd <-sum(log(colMeans(exp(log_lik))))
  lppd <-sum(log(mean(exp(log_lik))))
  p_waic <- sum(stats::var(log_lik))
  waic <- -2*lppd +2*p_waic
  waic <-signif(waic,digits = dig)
  # waic
  if(summary==TRUE) {
  message("\n \n ---------------------- \n")
  message( paste("  WAIC = ",  waic,"\n")  )
  message( " ---------------------- \n")
  message(" * WAIC; Widely Applicable Information Criterion (Watanabe-Akaike Information Criterion)\n")
}


  invisible(waic)
  # message("\n* WAIC; Widely Applicable Information Criterion,\n")
  # message("        Watanabe-Akaike Information Criterion")


  # message("Note that the S4 class should be made by target statement.\n  ")
  # message("To do so, when you bulid S4 class, set PreciseLogLikelihood = TRUE.\n  ")
}
