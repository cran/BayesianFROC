#'@title    Summary
#'@description    EAP and CI
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@export summary_EAP_CI_srsc
#'@return The estimates
#'@param dig digits of estimates.
#'@examples
#' \dontrun{
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#================The first example=======================================================
#'
#'
#' #1) Build the data for singler reader and single modality  case.
#'
#' dat <- list(c=c(3,2,1),    #Confidence level
#'             h=c(97,32,31), #Number of hits for each confidence level
#'             f=c(1,14,74),  #Number of false alarms for each confidence level
#'
#'             NL=259,       #Number of lesions
#'             NI=57,        #Number of images
#'             C=3)          #Number of confidence level
#'
#'# where, c denotes Confidence level,
#'#        h denotes number of Hits for each confidence level,
#'#        f denotes number of False alarms for each confidence level,
#'#        NL denotes Number of Lesions,
#'#        NI denotes Number of Images,
#'
#'
#' #   2) Fit the FROC model to the above data
#'
#'           fit <-   BayesianFROC::fit_Bayesian_FROC(dat)
#'
#' #   3) Extract estimates, that is posterior means and 95% credible intervals
#'
#'
#'         estimates <- summary_EAP_CI_srsc(  fit )
#'
#'
#'}# dottest
#'
#'
#'


summary_EAP_CI_srsc <- function(StanS4class,
                                dig=5,
                                summary = TRUE){
  multinomial <- StanS4class@multinomial

  PreciseLogLikelihood <-StanS4class@PreciseLogLikelihood
  prototype    <-  StanS4class@prototype


# dataList ----
  dataList <- StanS4class@dataList
  h <- dataList$h
  NL <- dataList$NL

    c <-  (StanS4class@dataList$C):1
  aaaa <- metadata_srsc_per_image(dataList = dataList, ModifiedPoisson = StanS4class@ModifiedPoisson)

   hitExtended <- aaaa$hitExtended
# browser()

  fit <- methods::as(StanS4class, "stanfit")

  if (summary == TRUE) {
    # summary of parameters -----------------START
  message("\n \n \n \n \n")
    message(crayon::silver("  +*+  +*+  +*+                                  -*-  -*-  -*-        \n"))
                   message("  ***  ***  ***    Estimates of an FROC model    ***  ***  -*-       \n")
    message(crayon::silver("  +*+  +*+  +*+                                  -*-  -*-  -*-        \n"))

   message(crayon::silver("\n* In a single reader and a single modality case, the Bayesian model has three kind of parameter, that is, thresholds and mean and standard deviation of the signal distribution of the latent Gaussian random variable. From these parameter, the so-called Area under the curve (AUC) and hit rate for each confidence levels and false alarm rate for each confidence levels are calculated. In the following, the posterior means and 95% credible intervals are shown. I consider the most important parameter is the AUC. So, First, we will see the AUC. \n"))
  }

  A.EAP  <- as.data.frame(summary(fit)[[1]])["A","mean"]
  A.CI.lower <- as.data.frame(summary(fit)[[1]])["A","2.5%"]
  A.CI.upper <- as.data.frame(summary(fit)[[1]])["A","97.5%"]

  A.EAP  <-signif(A.EAP,digits = dig)
  A.CI.lower <-signif(A.CI.lower,digits = dig)
  A.CI.upper<-signif(A.CI.upper,digits = dig)
  # if (summary == TRUE) {
  # message("* Area under the curve, where \"the curve\" means the AFROC curve:\n")
  # message("\n(AUC)     A=", A.EAP,", [",A.CI.lower,", ",A.CI.upper,"].\n",sep = "")
  # }

  if (summary == TRUE) {
    message("\n \n \n \n \n")
    message(crayon::silver("  +*+  +*+  +*+                                  -*-  -*-  -*-        \n"))
                   message("  ***  ***  ***              AUC                 ***  ***  -*-       \n")
    message(crayon::silver("  +*+  +*+  +*+                                  -*-  -*-  -*-        \n"))
  }


  if (summary == TRUE){
    message("* Area under the Curve (AUC), where \"the curve\" means the AFROC curve:")

    print( knitr::kable( data.frame( Parameter = "AUC",
                                            posterior.mean = A.EAP,
                                            lowerCI = A.CI.lower,
                                            upperCI=A.CI.upper),
                                            format = "pandoc"
  )
  )
  }








  if (summary == TRUE) {
    message("\n \n \n \n \n")
    message(crayon::silver("  +*+  +*+  +*+                                  -*-  -*-  -*-        \n"))
                   message("  ***  ***  ***        Model Parameters          ***  ***  -*-       \n")
    message(crayon::silver("  +*+  +*+  +*+                                  -*-  -*-  -*-        \n"))
  }



  C <- as.integer(fit@par_dims$p)

  if (summary == TRUE) message("\n* Thresholds")


  z.EAP       <- array(0, dim=c(C))
  z.CI.lower  <- array(0, dim=c(C))
  z.CI.upper  <- array(0, dim=c(C))
  lowname     <- array(0, dim=c(C))

#   w.EAP  <- as.data.frame(summary(fit)[[1]])["w","mean"]
#   w.CI.lower <- as.data.frame(summary(fit)[[1]])["w","2.5%"]
#   w.CI.upper <- as.data.frame(summary(fit)[[1]])["w","97.5%"]
#   w.EAP  <-signif(w.EAP,digits = dig)
#   w.CI.lower <-signif(w.CI.lower,digits = dig)
#   w.CI.upper<-signif(w.CI.upper,digits = dig)
#
#   if (summary == TRUE) {
#   message("\n        z[1]=", w.EAP,", [",w.CI.lower,", ",w.CI.upper,"].\n",sep = "")
# }
  for (cd in 1:C) {

    lowname[cd]    <- paste("z[",cd,"]",sep = "")
    z.EAP[cd]      <- as.data.frame(summary(fit)[[1]])[lowname[cd],"mean"]
    z.CI.lower[cd] <- as.data.frame(summary(fit)[[1]])[lowname[cd],"2.5%"]
    z.CI.upper[cd] <- as.data.frame(summary(fit)[[1]])[lowname[cd],"97.5%"]
    z.EAP[cd]  <-signif(z.EAP[cd],digits = dig)
    z.CI.lower[cd] <-signif(z.CI.lower[cd],digits = dig)
    z.CI.upper[cd]<-signif(z.CI.upper[cd],digits = dig)

    # if (summary == TRUE) {
    # message("\n        ",  lowname[cd]  , "=", z.EAP[cd],", [",z.CI.lower[cd],", ",z.CI.upper[cd],"].\n",sep = "")
    # }

  }

  if (summary == TRUE){print( knitr::kable( data.frame( Parameter = lowname,
                                 posterior.mean = z.EAP,
                                 lowerCI = z.CI.lower,
                                 upperCI=z.CI.upper),
                     format = "pandoc"
                     )
       )
}
  #here


  if (summary == TRUE)   message("\n\n\n* Differences of Thresholds of Gaussian Assumption:")
  if (summary == TRUE)   message("\n

              For example, dz[1]:= z[2]-z[1],
                           dz[2]:= z[3]-z[2],
                                  :
                                  :
                                 ")


 dz.EAP       <- vector()
 dz.CI.lower  <-  vector()
 dz.CI.upper  <-  vector()
 dz.lowname     <-  vector()

 D <-C-1
  for (cd in 1:D) {

    dz.lowname[cd] <- paste("dz[",cd,"]",sep = "")

    dz.EAP[cd]  <- as.data.frame(summary(fit)[[1]])[dz.lowname[cd],"mean"]
    dz.CI.lower[cd] <- as.data.frame(summary(fit)[[1]])[dz.lowname[cd],"2.5%"]
    dz.CI.upper[cd] <- as.data.frame(summary(fit)[[1]])[dz.lowname[cd],"97.5%"]

    dz.EAP[cd]  <-signif(dz.EAP[cd],digits = dig)
    dz.CI.lower[cd] <-signif(dz.CI.lower[cd],digits = dig)
    dz.CI.upper[cd]<-signif(dz.CI.upper[cd],digits = dig)

    # if (summary == TRUE) message("\n        ",  dz.lowname[cd]  , "=", dz.EAP[cd],", [",dz.CI.lower[cd],", ",dz.CI.upper[cd],"].\n",sep = "")



  }#for cd

 if (summary == TRUE){print( knitr::kable( data.frame( Parameter = dz.lowname,
                                                       posterior.mean = dz.EAP,
                                                       lowerCI = dz.CI.lower,
                                                       upperCI=dz.CI.upper),
                                           format = "pandoc"
 )
 )
 }

  #here


























  if (summary == TRUE) {
  message("\n\n\n* Mean and Standard Deviation (S.D.) of the signal Gaussian distribution:")
}
     # message("\n*** Mean  of noise distribution:\n")
  m.EAP  <- as.data.frame(summary(fit)[[1]])["m","mean"]
  m.CI.lower <- as.data.frame(summary(fit)[[1]])["m","2.5%"]
  m.CI.upper <- as.data.frame(summary(fit)[[1]])["m","97.5%"]
  m.EAP  <-signif(m.EAP,digits = dig)
  m.CI.lower <-signif(m.CI.lower,digits = dig)
  m.CI.upper<-signif(m.CI.upper,digits = dig)

#   if (summary == TRUE) {
#   message("\n (Mean)       m=", m.EAP,", [",m.CI.lower,", ",m.CI.upper,"].\n",sep = "")
# }



   # message("*** Standard Deviation  of noise distribution:\n")

  v.EAP  <- as.data.frame(summary(fit)[[1]])["v","mean"]
  v.CI.lower <- as.data.frame(summary(fit)[[1]])["v","2.5%"]
  v.CI.upper <- as.data.frame(summary(fit)[[1]])["v","97.5%"]
  v.EAP  <-signif(v.EAP,digits = dig)
  v.CI.lower <-signif(v.CI.lower,digits = dig)
  v.CI.upper<-signif(v.CI.upper,digits = dig)

  # if (summary == TRUE) {
  # message("\n (S.D.)       v=", v.EAP,", [",v.CI.lower,", ",v.CI.upper,"].  (not variance) \n",sep = "")
  # message("\n   ** Mean and Standard Deviation (S.D.) of the noise distribution in the Gaussian (binormal) assumption is 0 and 1.\n")
  # }



  # if (summary == TRUE){
  #   print( knitr::kable( data.frame( Parameter = "mean",
  #                                    posterior.mean = m.EAP,
  #                                    lowerCI = m.CI.lower,
  #                                    upperCI=m.CI.upper),
  #                        format = "pandoc"
  #   )
  #   )
  # }




  if (summary == TRUE){
    print( knitr::kable( data.frame( Parameter = c(  "mean" ,"S.D."),
                                     posterior.mean = c(  m.EAP  ,v.EAP),
                                     lowerCI = c(   m.CI.lower ,v.CI.lower),
                                     upperCI=c( m.CI.upper ,v.CI.upper)
                                     ),
                         format = "pandoc"
    )
    )
  }


  if (summary == TRUE) {
    message("
\n* Note that the noise distribution is not the Gaussian of mean 0 and  s.d. 1,
* but we use the differential logarithmic Gaussian, instead.
            ")
  }










  # summary of parameters -----------------comment is not done



  # Hit rate -----

  if (summary == TRUE) {
    message("\n \n \n \n \n")
    message(crayon::silver("  +*+  +*+  +*+                                  -*-  -*-  -*-        \n"))
                   message("  ***  ***  ***            Hit Rate              ***  ***  -*-       \n")
    message(crayon::silver("  +*+  +*+  +*+                                  -*-  -*-  -*-        \n"))
  }



  p.EAP       <- array(0, dim=c(C))
  p.CI.lower  <- array(0, dim=c(C))
  p.CI.upper  <- array(0, dim=c(C))
  lowname     <- array(0, dim=c(C))
  if (summary) {
    message("\n\n\n* p[c] means the hit rate of the binomial distribution with confidence level c.")
  }

    for (cd in 1:C) {
    lowname[cd] <- paste("p[",cd,"]",sep = "")
    p.EAP[cd]  <- as.data.frame(summary(fit)[[1]])[lowname[cd],"mean"]
    p.CI.lower[cd] <- as.data.frame(summary(fit)[[1]])[lowname[cd],"2.5%"]
    p.CI.upper[cd] <- as.data.frame(summary(fit)[[1]])[lowname[cd],"97.5%"]
    p.EAP[cd]  <-signif(p.EAP[cd],digits = dig)
    p.CI.lower[cd] <-signif(p.CI.lower[cd],digits = dig)
    p.CI.upper[cd]<-signif(p.CI.upper[cd],digits = dig)
    # if (summary == TRUE) {
    # message("\n        ",  lowname[cd]  , "=", p.EAP[cd],", [",p.CI.lower[cd],", ",p.CI.upper[cd],"].\n",sep = "")
    # }
    }



  if (summary){print( knitr::kable( data.frame( Parameter = lowname,
                                                        Posterior.Mean = p.EAP,
                                                        lowerCI = p.CI.lower,
                                                        upperCI=p.CI.upper),
                                            format = "pandoc"
  )
  )
  }
  # Hit rate -----

  if (( summary && prototype)||(summary &&!multinomial)) {
    message("\n
* Let h(c) denote the number of hits with confidence level c,
  then the above p[c] means that

               h(c)  ~ Binomial(p'[c], N'[c])

                                    p[c]
              p'[c] :=   ------------------------------------
                         1 - p[C] - p[C-1] - .... - p[c+1] ,


              N'[c] :=   NL - H[C] - H[C-1] - .... - H[c+1] ,


for each c = 1,2,...,", StanS4class@dataList$C, ", where NL denotes the number of lesions and now it is ",StanS4class@dataList$NL,".")
  }


  if (summary&& prototype&&!multinomial) {
    message("\n
* Let h(c) denote the number of hits with confidence level c,
  then the above p[c] means that

               h(c)  ~ Binomial(p[c], NL)

              p  := hit rate of Bernoulli trial

              NL := number of lesions


for each c = 1,2,...,", StanS4class@dataList$C, ", where NL denotes the number of lesions and now it is ",StanS4class@dataList$NL,".")
  }

# hit rate multinomial ----
  if (summary&& multinomial) {
    message("\n
* Let h(c) denote the number of hits with confidence level c,
  then the above p[c] means that

              h'  ~ ",crayon::bgBlack$bold$yellow$underline$italic( "multinomial"),"(p')

              p'  := a simiplex (i.e., sum(p')=1), indicating hit rates of Bernoulli trials


for each c = 1,2,...,", StanS4class@dataList$C, ",

 p' denotes a Bernoulli rates of multinomial distribution.
 h' dentoes an extended vector of the hit vetor, that is,
we add the number of non-detected lesions as the last component in the hit vector.
Namely,")
    p <- extract_EAP_by_array(StanS4class = StanS4class,p)
    p_rev_Extented <- extract_EAP_by_array(StanS4class = StanS4class,p_rev_Extented)

    sss<-"h[1]"
    for (cd in 2:C)  sss<-paste(sss, " + h'[",cd,"]", sep = "")


    # sss <- paste(sss, " = ", h[1])
    # for (cd in 2:C)   sss<-paste(sss, " + ", cat(h[cd]) , sep = "")


for (cd in 1:C) {
  message("The realization of h'[",cd,"] = ", crayon::bgBlack$bold$yellow$underline$italic(   hitExtended[c(cd)]  )," whose Bernoulli rate of multinomia distribution is p'[",cd,"] = ", crayon::bgBlack$bold$yellow$underline$italic(    round(p[c[cd]],digits = 3)  )  )
}
message("The realization of h'[",C+1,"] = ", crayon::bgBlack$bold$yellow$underline$italic(   hitExtended[C+1] ) ," = number of lesions - sum of the number of hits over all ratings
                         = ",NL, " - (", sss ,"). (the number of non-detected lesions)
        whose Bernoulli rate of multinomia distribution is ",  crayon::bgBlack$bold$yellow$underline$italic(  round(p_rev_Extented[C+1],digits = 3) ),
        " where

        where NL denotes the number of lesions,
which is, now NL =  ",StanS4class@dataList$NL,".
        "     )

message("The notation p' denotes a vector of success rate of multinomial distribution
        and now it is")
 for (cd in 1:C) {
   message(" p'[",cd,"] = ",  crayon::bgBlack$bold$yellow$underline$italic(   round(p[c[cd]],digits = 3)  ),","  )
 }
 message(" p'[",C+1,"] = ",  crayon::bgBlack$bold$yellow$underline$italic(    round(p_rev_Extented[C+1],digits = 3)  ),". <- This is not \"success\" rate, we should say the \"failure rate\" of the reader."  )


    }


  if (summary == TRUE) {
    message("\n \n \n \n \n")
    message(crayon::silver("  +*+  +*+  +*+                                  -*-  -*-  -*-        \n"))
                   message("  ***  ***  ***        False Alarm Rate          ***  ***  ***       \n")
    message(crayon::silver("  +*+  +*+  +*+                                  -*-  -*-  -*-        \n"))
  }






  l.EAP       <- array(0, dim=c(C))
  l.CI.lower  <- array(0, dim=c(C))
  l.CI.upper  <- array(0, dim=c(C))
  lowname     <- array(0, dim=c(C))
  if (summary == TRUE) {
    message("\n\n\n* l[c] means the false alarm rate of the Poisson distribution with confidence level c.")
}
      for (cd in 1:C) {
    lowname[cd] <- paste("l[",cd,"]",sep = "")
    l.EAP[cd]  <- as.data.frame(summary(fit)[[1]])[lowname[cd],"mean"]
    l.CI.lower[cd] <- as.data.frame(summary(fit)[[1]])[lowname[cd],"2.5%"]
    l.CI.upper[cd] <- as.data.frame(summary(fit)[[1]])[lowname[cd],"97.5%"]
    l.EAP[cd]  <-signif(l.EAP[cd],digits = dig)
    l.CI.lower[cd] <-signif(l.CI.lower[cd],digits = dig)
    l.CI.upper[cd]<-signif(l.CI.upper[cd],digits = dig)
    # if (summary == TRUE) {
    # message("\n        ",  lowname[cd]  , "=", l.EAP[cd],", [",l.CI.lower[cd],", ",l.CI.upper[cd],"].\n",sep = "")
    # }
      }


  if (summary == TRUE){print( knitr::kable( data.frame( Parameter = lowname,
                                                        Posterior.Mean = l.EAP,
                                                        lowerCI = l.CI.lower,
                                                        upperCI=l.CI.upper),
                                            format = "pandoc"
  )
  )
  }



  if (summary == TRUE) {
    if (StanS4class@studyDesign  =="srsc.per.lesion") {
    message("\n* Let f[c] denote the number of false alarms with confidence level c,
then the above l[c] means that

           f(",StanS4class@dataList$C,") +  f(",StanS4class@dataList$C - 1,") + ...+ f(c) ~ Poisson( l[c]*",crayon::bgBlack$bold$yellow$underline$italic( "NL")," ) \n

or equivalently,

                           f(c) ~ Poisson(  (l[c]-l[c+1])*",crayon::bgBlack$bold$yellow$underline$italic( "NL"),"  ) \n



for each c = 1,2,...,", StanS4class@dataList$C, ", where ",crayon::bgBlack$bold$yellow$underline$italic( "NL")," denotes the number of ",crayon::bgBlack$bold$yellow$underline$italic( "lesions")," and now it is ",StanS4class@dataList$NL,".")
  }# ModifiedPoisson
# False rate -----
    if (StanS4class@studyDesign == "srsc.per.image") {
      message("\n* Let f(c) denote the number of false alarms with confidence level c,
then the above table means that

            f(",StanS4class@dataList$C,") +  f(",StanS4class@dataList$C - 1,") + ...+ f(c) ~ Poisson( l[c]*",crayon::bgBlack$bold$yellow$underline$italic( "NI")," ) \n

or equivalently,

             f(c) ~ Poisson(  ( l[c]-l[c+1] )*",crayon::bgBlack$bold$yellow$underline$italic( "NI"),"  ) \n


for each c = 1,2,...,", StanS4class@dataList$C, ", where ",crayon::bgBlack$bold$yellow$underline$italic( "NI")," denotes the number of ",crayon::bgBlack$bold$yellow$underline$italic( "images")," and now it is ",StanS4class@dataList$NI,".")
    }# ModifiedPoisson
}





  if( PreciseLogLikelihood==TRUE)  loglikelihood.of.model <- as.data.frame(summary(fit)[[1]])["lp__","mean"]
  if( PreciseLogLikelihood==FALSE)  loglikelihood.of.model <-"This model has not use target += formulation. So we do not assign this model the log likelihood, if you desire, please input PreciseLogLikelihood =TRUE when you fit the model."



invisible(list(
  AUC.EAP     =A.EAP,
  AUC.CI.lower = A.CI.lower,
  AUC.CI.upper  =A.CI.upper,

  z.Threshold.EAP        =z.EAP,
  z.Threshold.CI.lower  =z.CI.lower,
  z.Threshold.CI.upper  =z.CI.upper,

  dz.Threshold.EAP        =dz.EAP,
  dz.Threshold.CI.lower  =dz.CI.lower,
  dz.Threshold.CI.upper  =dz.CI.upper,

  mean.of.latent.EAP   =m.EAP,
  mean.of.latent.CI.lower  =m.CI.lower,
  mean.of.latent.CI.upper =m.CI.upper,

  Standard.Deviation.latent.EAP  =v.EAP,
  Standard.Deviation.latent.CI.lower=  v.CI.lower,
  Standard.Deviation.CI.upper = v.CI.upper,

  p.Hitrate.EAP      = p.EAP,
  p.Hitrate.CI.lower = p.CI.lower,
  p.Hitrate.CI.upper = p.CI.upper,


  l.FalseRate.EAP = l.EAP,
  l.FalseRate.CI.lower =l.CI.lower,
  l.FalseRate.CI.upper =l.CI.upper,

  loglikelihood.of.model=loglikelihood.of.model
))

}#format(1111111.111111, scientific = TRUE, digits = 3)
