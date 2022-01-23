
#' @title plot datasets using calculation of ppp
#'
#' @inheritParams DrawCurves
#' @param verbose A logical, whether title in plot is verbose or not.
#' @param colorindex A positive integer, indicating the color of scatter plots.
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
#'plot_dataset_of_ppp(f,colorindex = 1)
#'plot_dataset_of_ppp(f,colorindex = 2)
#'plot_dataset_of_ppp(f,colorindex = 3)
#'plot_dataset_of_ppp(f,colorindex = 4)
#'
#'}#dontrun
#'
plot_dataset_of_ppp <-function(StanS4class,summary =FALSE, verbose = FALSE,colorindex = 6){
  # browser()
  f <- StanS4class
  fit <-f
dark_theme(1)
e <- rstan::extract(f)
ee<-extract_EAP_CI(StanS4class = f,parameter.name = "p_value_logicals",dimension.of.parameter = 1,summary = FALSE )
p.value <- ee$p_value_logicals.EAP

# plot(e$FPeF_post_pred, e$TPF_post_pred)


# NaN is omitted here -------------------------------------------------
# Here, if 0 rate are created in Stan file sampling,
# Then, it causes that FPF and TPF include NaN.
#  Then the author should delete such NaN.



FPF <- e$FPF_post_pred # Three index array
TPF <- e$TPF_post_pred

# FPF<- matrix(aperm(FPF, c(1,3,2)), ncol = 3)
# TPF<- matrix(aperm(TPF, c(1,3,2)), ncol = 3)

C <- f@dataList$C#2021Nov26


FPF<- matrix(aperm(FPF, c(1,3,2)), ncol = C)#2021Nov26 # Three index array are converted to two indices matrix by this command line.
TPF<- matrix(aperm(TPF, c(1,3,2)), ncol = C)#2021Nov26

# browser()

FPF <- t(FPF)
TPF <- t(TPF)


FPF <- as.vector(FPF)
TPF <- as.vector(TPF)
# browser()
if( is.nan( max(FPF) ) ) color_message("0 Poisson rates in FPF may occur, somethings happened in sampling from PPD ")
if( is.nan( max(TPF) ) ) color_message("0 hit rates in TPF may occur, somethings happened in sampling from PPD ")


DF <- data.frame(FPF = FPF, TPF = TPF)
DF_NaN_omitted <- stats::na.omit(DF)
FPF <- DF_NaN_omitted$FPF
TPF <- DF_NaN_omitted$TPF
# prod(as.vector(DF  ==DF_NaN_omitted))


# NaN is omitted here -------------------------------------------------




# C <- f@dataList$C#2021Nov26
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
lower_lim_x <- min(c(FPF,ff))
# upper_lim_y <- max(1,unlist(TPF))
upper_lim_y <- max(c(TPF,hh))
lower_lim_y <- min(c(TPF,hh))

###########################################################
# Stan sampling sometimes generates  0 hit rates or false rate
#  In such a  case, FPF from post. pred. distr. includes NaN
#  which will cause NaN in the above objects. To work fine in such a case
#  The author put the following codes to provide more stable codes
# 2020 Dec 1 with pain in my annal!!
# if( is.nan(lower_lim_x) ||is.nan( upper_lim_x )) xlim <- c(0,1)
# if( is.nan(lower_lim_y) ||is.nan( upper_lim_y )) ylim <- c(0,1)

# if( is.nan(lower_lim_x) ) color_message("0 rate in FPF appeared, maybe or somethings happened in sampling from PPD ")
# if( is.nan(upper_lim_x) ) color_message("0 rate in FPF appeared, maybe or somethings happened in sampling from PPD ")
# if( is.nan(lower_lim_y) ) color_message("0 rate in TPF appeared, maybe or somethings happened in sampling from PPD ")
# if( is.nan(upper_lim_y) ) color_message("0 rate in TPF appeared, maybe or somethings happened in sampling from PPD ")
#
# if( is.nan(lower_lim_x) ) lower_lim_x <- 0
# if( is.nan(upper_lim_x) ) upper_lim_x <- max(ff)
# if( is.nan(lower_lim_y) ) lower_lim_y <- 0
# if( is.nan(upper_lim_y) ) upper_lim_y <- max(hh)
#
ylim <-c(lower_lim_y,upper_lim_y)
xlim <-c(lower_lim_x,upper_lim_x)

################################################################



main <- paste("P value = ", p.value)
 if(verbose)   main <-  substitute(paste("Goodness of fit: Post. Pred. Pvalue = ", pvalue ," =",integral( integral( iota*(chi^2*(D*"|"* theta) - chi^2*(D[obs]*"|"* theta) )*pi(D*"|"*theta)*pi(theta*"|"*D[obs])*d*theta*d*D, Theta, .  ), "Datasets", . ) , " = ",pvalue ),list( pvalue=p.value)  )#2021Nov25

BayesianFROC::small_margin()
# browser()
plot(FPF,
     TPF,
     main =main,
     cex=0.2,
     xlim = xlim,
     ylim = ylim
     # xlim = c(0, upper_lim_x),
     # ylim = c(0, upper_lim_y )
)
graphics::abline(h=1)


if (fit@studyDesign=="srsc.per.image")    xlab = 'Replicated false positives per images from the posterior predictive distribution.'
if (fit@studyDesign=="srsc.per.lesion" )    xlab = 'Replicated false positives per nodule from the posterior predictive distribution.'
ylab<- "Replicated cumulative hits per lesion"
BayesianFROC::small_margin()
plot(FPF,TPF, pch=20,
     # col=grDevices::rainbow(C+6, alpha=0.2)[group],
     col=grDevices::rainbow(C+colorindex, alpha=0.2)[group],

     cex=0.8, xlab=xlab, ylab=ylab, main =main,
     # xlim = c(0, upper_lim_x),
     # ylim = c(0,upper_lim_y)
     xlim = xlim,
     ylim = ylim

     )




DrawCurves(fit,
           upper_x = upper_lim_x,
           upper_y = upper_lim_y,
           lower_X = lower_lim_x,
           lower_y = lower_lim_y,
           new.imaging.device = FALSE,title= FALSE)







fit<-f




suppressWarnings(graphics::par(new=TRUE));
plot(fit@metadata$ff, fit@metadata$hh,col="red",
     cex=1,
     # xlim = c(0, upper_lim_x),
     # ylim = c(0, upper_lim_y )
     xlim = xlim,
     ylim = ylim
)
suppressWarnings(graphics::par(new=TRUE));
plot(fit@metadata$ff, fit@metadata$hh,col="red",
     cex=2,
     # xlim = c(0, upper_lim_x),
     # ylim = c(0, upper_lim_y )
     xlim = xlim,
     ylim = ylim
)
suppressWarnings(graphics::par(new=TRUE));
plot(fit@metadata$ff, fit@metadata$hh,col="blue",
     cex=2.5,
     # xlim = c(0, upper_lim_x),
     # ylim = c(0, upper_lim_y )
     xlim = xlim,
     ylim = ylim
)
suppressWarnings(graphics::par(new=TRUE));
plot(fit@metadata$ff, fit@metadata$hh,col="green",
     cex=3.5,
     # xlim = c(0, upper_lim_x),
     # ylim = c(0, upper_lim_y )
     xlim = xlim,
     ylim = ylim
)

df <- data.frame(FPF =FPF,TPF=TPF)

# if(summary)message("p value of chi square goodness of fit = ", p.value)
if(p.value>0.05)message("p value = ", crayon::bgBlack$red$bold$underline$italic(p.value), ", which is for the chi square goodness of fit statistic whose null hypthothesis is that the model is fitted well. Now, calculation shows that the p value is not less than 0.05, so, we do not need to reject the null hypothesis .... maybe ... love you. ")

if(p.value<0.05)message("p value = ", crayon::bgBlack$red$bold$underline$italic(p.value), ", which is for the chi square goodness of fit statistic whose null hypthothesis is that the model is fitted well. Now, unfortunately, calculation shows that the p value is less than 0.05, the null hypothesis may be rejected by someone who loves frequentist schemes ... maybe ...I love you. ")

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


















