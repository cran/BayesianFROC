
#' @title plot datasets using calculation of ppp
#'
#' @inheritParams DrawCurves
#'
#' @return null
#' @export
#'
#' @examples
#' \dontrun{
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                              Now single reader and single modality case only
#'#========================================================================================
#'# Fit a model to data-set "d"
#'# In the resulting object contained samples from posterior predictive distribution (PPD)
#'
#'f <- fit_Bayesian_FROC( ite  = 1111, summary = TRUE,  cha = 1, dataList = d )
#'
#'
#'# Plot samples synthesized from posterior predictive distribution (PPD)
#'
#'plot_dataset_of_ppp(f)
#'
#'}#dontrun
#'
plot_dataset_of_ppp <-function(StanS4class,summary =FALSE){
  # browser()
  f <- StanS4class
  fit <-f
dark_theme(1)
e <- rstan::extract(f)
ee<-extract_EAP_CI(StanS4class = f,parameter.name = "p_value_logicals",dimension.of.parameter = 1,summary = FALSE )
p.value <- ee$p_value_logicals.EAP

# plot(e$FPeF_post_pred, e$TPF_post_pred)
FPF <- e$FPF_post_pred
TPF <- e$TPF_post_pred

FPF <- t(FPF)
TPF <- t(TPF)

FPF <- as.vector(FPF)
TPF <- as.vector(TPF)

C <- f@dataList$C
N<-C
c<-C:1
NL <- f@dataList$NL
NI <- f@dataList$NI
group <-  rep( 1:C, length(FPF)/C )
# FPF <- jitter(FPF)
# TPF <- jitter(TPF)
# browser()

 metadata<-metadata_srsc_per_image(f@dataList, ModifiedPoisson = f@ModifiedPoisson)
 ff <- metadata$ff
 hh <- metadata$hh

upper_lim_x <- max(c(FPF,ff))
# upper_lim_y <- max(1,unlist(TPF))
upper_lim_y <- max(c(TPF,hh))

main <- paste("P value = ", p.value)


BayesianFROC::small_margin()
# browser()
plot(FPF,
     TPF,
     main =main,
     cex=0.2,
     xlim = c(0, upper_lim_x),
     ylim = c(0, upper_lim_y )
)
graphics::abline(h=1)


if (fit@studyDesign=="srsc.per.image")    xlab = 'Replicated false positives per images from the posterior predictive distribution.'
if (fit@studyDesign=="srsc.per.lesion" )    xlab = 'Replicated false positives per nodule from the posterior predictive distribution.'
ylab<- "Replicated cumulative hits per lesion"
BayesianFROC::small_margin()
plot(FPF,TPF, pch=20, col=grDevices::rainbow(C+6, alpha=0.2)[group],
     cex=0.8, xlab=xlab, ylab=ylab, main =main,
     xlim = c(0, upper_lim_x),
     ylim = c(0,upper_lim_y))




DrawCurves(fit,upper_x = upper_lim_x, upper_y = upper_lim_y,new.imaging.device = FALSE,title= FALSE)







fit<-f




suppressWarnings(graphics::par(new=TRUE));
plot(fit@metadata$ff, fit@metadata$hh,col="red",
     cex=1,
     xlim = c(0, upper_lim_x),
     ylim = c(0, upper_lim_y )
)
suppressWarnings(graphics::par(new=TRUE));
plot(fit@metadata$ff, fit@metadata$hh,col="red",
     cex=2,
     xlim = c(0, upper_lim_x),
     ylim = c(0, upper_lim_y )
)
suppressWarnings(graphics::par(new=TRUE));
plot(fit@metadata$ff, fit@metadata$hh,col="blue",
     cex=2.5,
     xlim = c(0, upper_lim_x),
     ylim = c(0, upper_lim_y )
)
suppressWarnings(graphics::par(new=TRUE));
plot(fit@metadata$ff, fit@metadata$hh,col="green",
     cex=3.5,
     xlim = c(0, upper_lim_x),
     ylim = c(0, upper_lim_y )
)

df <- data.frame(FPF =FPF,TPF=TPF)

# if(summary)message("p value of chi square goodness of fit = ", p.value)
if(p.value>0.05)message("p value = ", crayon::bgBlack$red$bold$underline$italic(p.value), ", which is for the chi square goodness of fit statistic whose null hypthothesis is that the model is fitted. Now, calculation shows that the p value is not less than 0.05, so, we do not need to reject the null hypothesis .... maybe ... love you. ")

if(p.value<0.05)message("p value = ", crayon::bgBlack$red$bold$underline$italic(p.value), ", which is for the chi square goodness of fit statistic whose null hypthothesis is that the model is fitted. Now, unfortunately, calculation shows that the p value is less than 0.05, the null hypothesis may be rejected by someone who loves frequentist schemes ... maybe ... love you. ")

if(summary){return(list(df=df,p.value =p.value))
}else {invisible(df)}



}






















#' @title plot datasets using calculation of ppp
#'
#' @inheritParams DrawCurves
#'
#' @return p value whose null hypothesis is that model is fitted to data well.
#' @export
#' @examples
#' \dontrun{
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                              Now single reader and single modality case only
#'#========================================================================================
#'# Fit a model to data-set "dd"
#'# In the resulting object contained samples from posterior predictive distribution (PPD)
#'
#'f <- fit_Bayesian_FROC( ite  = 1111, summary = TRUE,  cha = 1, dataList = dd )
#'
#'
#'# Plot samples synthesized from posterior predictive distribution (PPD)
#'
#'plot_dataset_of_ppp_MRMC(f)
#'
#'}#dontrun
#'
plot_dataset_of_ppp_MRMC <-function(StanS4class,summary =FALSE){
  # browser()
  f <- StanS4class

  dark_theme()
  e <- extract(f)
  p.value <- mean(e$p_value_logicals)

  group <- e$modalityID
  FPF <- e$FPF_post_pred;TPF <- e$TPF_post_pred
  plot(FPF,TPF)
  # browser()
  upper_lim_x <- max(FPF)
  # upper_lim_y <- max(1,unlist(TPF))
  upper_lim_y <- max(TPF)
  plot(FPF,TPF, pch=20, col=grDevices::rainbow(1+6, alpha=0.8)[group],
       cex=0.8,
       # xlab=xlab, ylab=ylab,
       # main =main,
       xlim = c(0, upper_lim_x),
       ylim = c(0,upper_lim_y))

message("This plot sucks, so in the future, i draw curves which is calculated by the scatter plots of datasets, but now suc a couch potato...")
 return(p.value)

}


















