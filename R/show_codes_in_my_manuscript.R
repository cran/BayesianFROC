

#' @title Show \R codes used in my manuscript
#'
#'
#' @return \code{NULL}
#' @export
#'
#' @examples
#'#========================================================================================
#'#           R codes in my manuscript
#'#========================================================================================
#'
#' show_codes_in_my_manuscript()
#'
show_codes_in_my_manuscript  <- function() {




  message("
# Non intuitive AUC --------------

dat <- list(
            c=c(3,2,1),    #Confidence level
            h=c(0,5,5), #Number of hits for each confidence level
            f=c(1,1,1),  #Number of false alarms for each confidence level
            NL=10,       #Number of lesions
            NI=1,        #Number of images
            C=3)          #Number of confidence level
f1 <- fit_Bayesian_FROC(  dataList = dat ) #NX=NI
f2 <- fit_Bayesian_FROC(  dataList = dat ,ModifiedPoisson = T)#NX=NL




# Error of variance for estimates with respect to sample size NL-----



a <-error_srsc(NLvector = c(
                                100,
                                10000,
                                1000000,
                                10000000
                              ),
                        ratio=2,
                        replicate.datset =100,
                        ModifiedPoisson = FALSE,
                        mean.truth=0.6,
                        sd.truth=5.3,
                        z.truth =c(-0.8,0.7,2.38),
                        ite =5000
)


error_srsc_error_visualization(a)
error_srsc_variance_visualization(a)



# Single reader and single modality
 fit <- fit_Bayesian_FROC(    ite  = 1111,
                           summary = T,
                          dataList = dataList.Chakra.1.with.explantation,
                          )
ppp(fit,Colour = F,dark_theme = F)
DrawCurves(fit,title = F,Colour = F,DrawAUC = T,DrawAFROCcurve = T,DrawCFPCTP = T)
draw_latent_noise_distribution( fit,dark_theme = F,color = T)
draw_latent_signal_distribution(fit,dark_theme = F,color = T)





# Modality comparison
f  <- fit_Bayesian_FROC( ite  = 4111,  cha = 1, summary = T, dataList = dd,DrawCurve = T)
ppp(f)
        ")


}
