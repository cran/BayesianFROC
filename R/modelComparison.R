#'@title model comparison
#'@description This is a model comparison.
#'@param Number.of.variation.of.NL Lesion
#'@param Number.of.images images
#'@param ite iteration
#'@param e exp for false alarms

#'@inheritParams fit_Bayesian_FROC

#' @export comparison
comparison <-function(Number.of.variation.of.NL,
                      Number.of.images,
                      ite=1111,
                      DrawCurve =FALSE,
                      dig=3,
                      e=0
                      ){




  if (length(Number.of.variation.of.NL)<=1) {
    if (length(Number.of.images)<=1) {


data <-list()
fit <-list()

c=c(3,2,1)
h=c(97,32,31)
# f=c(1,14,74)# real
f=c(1,14,74)*10^e# real


C=3
 NI<-Number.of.images
  NL<-259

  A<-Number.of.variation.of.NL
 WAIC <- matrix(0,NI+1,A+1)
 Converge <- matrix(0,NI+1,A+1)

  # WAIC <- data.frame()

s<-0
for (ni in 1:NI) {
  for (a in 1:A) {
 s<-s+1
    data[[s]] <-list(

      NL=NL+a-1,
      NI=ni,

      c=c,
      h=h,
      f=f,
      C=3)}}
    # browser()
s<-0
for (ni in 1:NI) {
  for (a in 1:A) {
    s<-s+1
    fit[[s]] <-  fit_Bayesian_FROC(DrawCurve=DrawCurve,
                                   dig=dig,

      dataList =    data[[s]] ,
      ite = ite,
      PreciseLogLikelihood = TRUE,
       summary = FALSE )
    WAIC[ni+1,a+1] <-waic(fit[[s]],summary = FALSE)
    WAIC[ni+1, 1] <-paste("NI=",ni)
    WAIC[ 1, a+1] <-paste("NL=", NL+a-1)

    Converge[ni+1,a+1] <-ConfirmConvergence(fit[[s]],summary = FALSE)
    Converge[ni+1, 1] <-paste("NI=",ni)
    Converge[ 1, a+1] <-paste("NL=", NL+a-1)
  }}

WAIC[1,1] <- "NI\\NL"
Converge[1,1] <- "NI\\NL"


#
#     WAIC2 <- WAIC[c(2:NI+1),c(2:A+1)]
#        for (a in 1:A) {
#     WAIC2[,a] <- as.numeric(WAIC2[,a])
#        }
    print(Converge)
    print(WAIC)
    # print(WAIC2)
    print(knitr::kable(as.data.frame(Converge)))

    print(knitr::kable(as.data.frame(WAIC)))
    # # invisible(list(WAIC=WAIC,WAIC2=WAIC2,Converge=Converge,fit=fit,data=data))
    # invisible(list(WAIC=WAIC,            Converge=Converge,fit=fit,data=data))

    }}







  if (length(Number.of.variation.of.NL)>1) {
    if (length(Number.of.images)>1) {


      data <-list()
      fit <-list()

      c=c(3,2,1)
      h=c(97,32,31)
      f=c(1,14,74)
      C=3
      # NL<-259 #real

       NI<- length(  Number.of.images )

      A<- length( Number.of.variation.of.NL)
      WAIC <- matrix(0,NI+1,A+1)
      Converge <- matrix(0,NI+1,A+1)

      # WAIC <- data.frame()

      s<-0
      for (ni in 1:NI) {
        for (a in 1:A) {
          s<-s+1
          data[[s]] <-list(

            NL=sum(h)+Number.of.variation.of.NL[a],
            NI=Number.of.images[ni],

            c=c,
            h=h,
            f=f,
            C=3)}}
      # browser()
      s<-0
      for (ni in 1:NI) {
        for (a in 1:A) {
          s<-s+1
          fit[[s]] <-  fit_Bayesian_FROC(DrawCurve=DrawCurve,
                                         dig=dig,

            dataList =    data[[s]] ,
            ite = ite,
            PreciseLogLikelihood = TRUE,
             summary = FALSE )
          WAIC[ni+1,a+1] <-waic(fit[[s]],summary = FALSE)
          WAIC[ni+1, 1] <-paste("NI=",Number.of.images[ni])
          WAIC[ 1, a+1] <-paste("NL=",sum(h)+Number.of.variation.of.NL[a])

          Converge[ni+1,a+1] <-ConfirmConvergence(fit[[s]],summary = FALSE)
          Converge[ni+1, 1] <-paste("NI=",Number.of.images[ni])
          Converge[ 1, a+1] <-paste("NL=",sum(h)+Number.of.variation.of.NL[a])
        }}
      WAIC[1,1] <- "NI\\NL"
      Converge[1,1] <- "NI\\NL"









      ########################################


      WAIC.per.lesion <- matrix(0, 3,A+1)
      fit.per.lesion <-list()
      data.per.lesion <-list()


          c=c(3,2,1)
          h=c(97,32,31)
          f=c(1,14,74)
          C=3
          # NL<-259

          s<-0
             for (a in 1:A) {
              s<-s+1
              data.per.lesion[[s]] <-list(

                NL=sum(h)+Number.of.variation.of.NL[a],

                c=c,
                h=h,
                f=f,
                C=3)}


      s<-0
         for (a in 1:A) {
          s<-s+1
          fit.per.lesion[[s]] <-  fit_Bayesian_FROC(DrawCurve=DrawCurve,
                                         dig=dig,
                                         ModifiedPoisson = TRUE,#Caution !! ###########

                                         dataList =    data.per.lesion[[s]] ,
                                         ite = ite,
                                         PreciseLogLikelihood = TRUE,
                                         summary = FALSE )
          WAIC.per.lesion[1,a+1] <-paste("NL=",sum(h)+Number.of.variation.of.NL[a])
          WAIC.per.lesion[2,a+1] <-waic(fit.per.lesion[[s]],summary = FALSE)
          WAIC.per.lesion[3,a+1] <-ConfirmConvergence(fit.per.lesion[[s]],summary = FALSE)


             }

      WAIC.per.lesion[1,1] <- paste("NL")
      WAIC.per.lesion[2,1] <- paste("WAIC")
      WAIC.per.lesion[3,1] <- paste("Convergence")



      message("\n* If WAIC depends on the upper bound of the highest thershold of bi normal assumption, then this calculus is meaningless.")


      message("\n*per image models: converge")
      print(knitr::kable(as.data.frame(Converge)))

      message("\n*per image models: WAIC")
      print(knitr::kable(as.data.frame(WAIC)))


      message("\n* per lesion model:\n")
       print(knitr::kable(as.data.frame(WAIC.per.lesion), format = "pandoc"))




      # invisible(list(WAIC=WAIC,WAIC2=WAIC2,Converge=Converge,fit=fit,data=data))
      invisible(list(WAIC=WAIC,            Converge=Converge,fit=fit,data=data, fit.per.lesion=fit.per.lesion,data.per.lesion=data.per.lesion))

  }

  }#NI>1




}









#'@title model comparison
#'@description This is a model comparison.
#'@param NI images
#'@param ite iteration

#'####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#' @export
compare <-function(NI,ite=1111){

  dat <- list()
  fit <- list()
  Ideal.NI <- vector()
  WAIC <- vector()
  convergence <-vector()
  s<-0

  for (ni in NI) {
    s <-s+1
dat[[s]] <- list(c=c(3,2,1),    #Confidence level
            h=c(97,32,31), #Number of hits for each confidence level
            f=c(1,14,74),  #Number of false alarms for each confidence level

            NL=259,       #Number of lesions
            NI=ni,        #Number of images
            C=3)          #Number of confidence level


fit[[s]] <-   BayesianFROC::fit_Bayesian_FROC(dat[[s]],PreciseLogLikelihood = TRUE,ite=ite)
summar <-summary_EAP_CI_srsc(fit[[s]])
# browser()
Ideal.NI[s] <- sum(dat[[s]]$f)/summar$l.FalseRate.EAP[1]
WAIC[s] <-fit[[s]]@WAIC
convergence[s] <-fit[[s]]@convergence

  }
  print(knitr::kable(data.frame(NI=NI,
                                Ideal.NI=Ideal.NI,
                                WAIC=WAIC,
                                convergence=convergence
                                )))
  message("This indicates that the poisson rate of the lowest confidence level is depend on the number of images strongly.")
}



