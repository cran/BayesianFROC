
###################################
##################################
########################

#MRMC


###########
#########
############################


validation.dataset_MRMC <-function(
  replicate.datset =3,

  mean.truth=0.6,
  sd.truth=5.3,
  z.truth =c(-0.8, 0.7, 2.38, 51436.25),
  NL=259,
  NI=57,
  C=length(z.truth)-1,

  # C=5,
  M=5,
  Q=4,

  ModifiedPoisson=F,
  ite =111
){

  # if(ModifiedPoisson==F){ NX <- NI}
  # if(ModifiedPoisson==T){ NX <- NL}


  mu.truth <- array(0,c(M,Q))
  v.truth <- array(0,c(M,Q))

  p.truth <- array(0,c(C,M,Q))
  list.of.dataList <- list()

  for (seed in 1:replicate.datset ) {

    f.inv <- vector()
    h.inv <- vector()
    f <- vector()
    h <- vector()

    l.truth <- -log( stats::pnorm(z.truth))

    for (cd in 1:C) {
      p.truth[cd]<- stats::pnorm((z.truth[cd+1]-mean.truth)/sd.truth) -  stats::pnorm((z.truth[cd]-mean.truth)/sd.truth)
    }

    for (cd in 1:C) {

      if(ModifiedPoisson==F){
        set.seed(seed =  seed); false.alarms <- stats::rpois(n= 1, lambda = (l.truth[cd]-l.truth[cd+1])*NI )
        set.seed(seed =  seed); hits <-   stats::rbinom(n=1,size = NL,prob = p.truth[cd])
        f.inv[cd] <- sum( false.alarms )
        h.inv[cd] <- sum( hits )
      }

      if(ModifiedPoisson==T){
        set.seed(seed =  seed); false.alarms <- stats::rpois(n= 1, lambda = (l.truth[cd]-l.truth[cd+1])*NL)
        set.seed(seed =  seed); hits <-   stats::rbinom(n=1,size = NL,prob = p.truth[cd])
        f.inv[cd] <- sum( false.alarms )
        h.inv[cd] <- sum( hits )
      }

      f[C-cd+1] <- f.inv[cd]
      h[C-cd+1] <- h.inv[cd]

    }

    list.of.dataList[[seed]] <- list(
      NL=NL,
      NI=NI,
      f=f,
      h=h,
      C=C
    )

  }#for seed


  fit <- list()
  validation.of.AUC.EAP <-vector()
  validation.of.AUC.CI.lower <-vector()
  validation.of.AUC.CI.upper <-vector()

  validation.of.z.Threshold.EAP <- list()
  validation.of.z.Threshold.CI.lower <- list()
  validation.of.z.Threshold.CI.upper <- list()

  validation.of.mean.of.latent.EAP <- vector()
  validation.of.mean.of.latent.CI.lower <- vector()
  validation.of.mean.of.latent.CI.upper <- vector()
  validation.of.Standard.Deviation.latent.EAP <- vector()
  validation.of.Standard.Deviation.latent.CI.lower <- vector()
  validation.of.Standard.Deviation.CI.upper <- vector()

  validation.of.p.Hitrate.EAP <- list()
  validation.of.p.Hitrate.CI.lower <- list()
  validation.of.p.Hitrate.CI.upper <- list()
  validation.of.l.FalseRate.EAP <- list()
  validation.of.l.FalseRate.CI.lower <- list()
  validation.of.l.FalseRate.CI.upper <- list()
  validation.of.loglikelihood.of.model <- vector()



  s<-0
  for (seed in 1:replicate.datset ) {

    fit[[seed]] <- fit_Bayesian_FROC(
      dataList=list.of.dataList[[seed]],
      ite = ite,
      new.imaging.device = FALSE,
      summary = FALSE,
      DrawCurve = FALSE
    )



    if ( fit[[seed]]@convergence ==TRUE) {
      s <-s+1
      ssss <- summary_EAP_CI_srsc( fit[[seed]],summary = FALSE)
      validation.of.AUC.EAP[s] <- ssss$AUC.EAP
      validation.of.AUC.CI.lower[s] <-ssss$AUC.CI.lower
      validation.of.AUC.CI.upper[s] <- ssss$AUC.CI.upper

      validation.of.z.Threshold.EAP[[s]]  <- ssss$z.Threshold.EAP
      validation.of.z.Threshold.CI.lower[[s]]  <- ssss$z.Threshold.CI.lower
      validation.of.z.Threshold.CI.upper[[s]]  <- ssss$z.Threshold.CI.upper

      validation.of.mean.of.latent.EAP[s]  <- ssss$mean.of.latent.EAP
      validation.of.mean.of.latent.CI.lower[s]  <- ssss$mean.of.latent.CI.lower
      validation.of.mean.of.latent.CI.upper[s]  <- ssss$mean.of.latent.CI.upper
      validation.of.Standard.Deviation.latent.EAP[s]  <- ssss$Standard.Deviation.latent.EAP
      validation.of.Standard.Deviation.latent.CI.lower[s] <- ssss$Standard.Deviation.latent.CI.lower
      validation.of.Standard.Deviation.CI.upper[s]  <- ssss$Standard.Deviation.CI.upper

      validation.of.p.Hitrate.EAP[[s]]  <- ssss$p.Hitrate.EAP
      validation.of.p.Hitrate.CI.lower[[s]]  <- ssss$p.Hitrate.CI.lower
      validation.of.p.Hitrate.CI.upper[[s]]  <- ssss$p.Hitrate.CI.upper
      validation.of.l.FalseRate.EAP[[s]]  <-      ssss$l.FalseRate.EAP
      validation.of.l.FalseRate.CI.lower[[s]]  <- ssss$l.FalseRate.CI.lower
      validation.of.l.FalseRate.CI.upper[[s]]  <- ssss$l.FalseRate.CI.upper
      validation.of.loglikelihood.of.model[s]  <- ssss$loglikelihood.of.model
    }# if convergence is true

  }


  a.truth <- mean.truth/sd.truth
  b.truth  <- 1/sd.truth
  AUC.truth <- stats::pnorm(a.truth/ sqrt( 1+b.truth^2))

  l.truth <-l.truth[1:C]

  # browser()

  error.of.AUC            <- validation.of.AUC.EAP-AUC.truth
  error.of.z.Threshold.EAP     <-  unlist( validation.of.z.Threshold.EAP) - unlist( rep(z.truth ,
                                                                                        length(unlist( validation.of.z.Threshold.EAP))/length(z.truth)  )
  )
  error.of.mean.of.latent.EAP  <-    validation.of.mean.of.latent.EAP -  mean.truth
  error.of.Standard.Deviation.latent.EAP  <-   validation.of.Standard.Deviation.latent.EAP -sd.truth
  error.of.p.Hitrate.EAP    <-  unlist( validation.of.p.Hitrate.EAP  ) -  unlist( rep(p.truth ,
                                                                                      length(unlist(validation.of.p.Hitrate.EAP))/length(p.truth)  )
  )
  error.of.l.FalseRate.EAP  <-  unlist( validation.of.l.FalseRate.EAP) - unlist( rep(l.truth ,
                                                                                     length(unlist(validation.of.l.FalseRate.EAP))/length(l.truth)  )
  )

  #############
  aaa<-length(    unlist( validation.of.l.FalseRate.EAP)  ) == length( unlist( rep(l.truth ,   length(unlist(validation.of.l.FalseRate.EAP))/length(l.truth)  )))
  bbb<-length(  unlist( validation.of.p.Hitrate.EAP  ) )==   length( unlist( rep(p.truth ,length(unlist(validation.of.p.Hitrate.EAP))/length(p.truth)  )))
  ccc<-length(  unlist( validation.of.z.Threshold.EAP) )== length(   unlist( rep(z.truth ,   length(unlist( validation.of.z.Threshold.EAP))/length(z.truth)  )))
  if(!(aaa&&bbb&&ccc)){return(message("Here is an error."))}
  ###########
  # browser()




  error.of.AUC.normalized     <-  mean(error.of.AUC)/sqrt(stats::var(error.of.AUC))
  error.of.z.Threshold.EAP.normalized     <-  mean(error.of.z.Threshold.EAP)/sqrt(stats::var(error.of.z.Threshold.EAP))
  error.of.mean.of.latent.EAP.normalized  <-  mean(error.of.mean.of.latent.EAP)/sqrt(stats::var(error.of.mean.of.latent.EAP))
  error.of.Standard.Deviation.latent.EAP.normalized  <-  mean(error.of.Standard.Deviation.latent.EAP)/sqrt(stats::var(error.of.Standard.Deviation.latent.EAP))
  error.of.p.Hitrate.EAP.normalized    <-  mean(error.of.p.Hitrate.EAP)/sqrt(stats::var(error.of.p.Hitrate.EAP))
  error.of.l.FalseRate.EAP.normalized  <-  mean(error.of.l.FalseRate.EAP)/sqrt(stats::var(error.of.l.FalseRate.EAP))
  # browser()


  errors.normalized.data.frame <- data.frame(
    names.of.parameter =c(
      "AUC"    ,
      "z.Threshold."        ,
      "Mean.of.latent.Gaussian"      ,
      "Standard.Deviation.latent.Gaussian"     ,
      "p.Hitrate"       ,
      "l.FalseRate."
    ),
    values.of.normalized.errors =c(
      error.of.AUC.normalized    ,
      error.of.z.Threshold.EAP.normalized        ,
      error.of.mean.of.latent.EAP.normalized      ,
      error.of.Standard.Deviation.latent.EAP.normalized     ,
      error.of.p.Hitrate.EAP.normalized       ,
      error.of.l.FalseRate.EAP.normalized
    )
  )
  # browser()
  print(knitr::kable(errors.normalized.data.frame$errors.normalized.data.frame))


  invisible( list(
    fit=fit,
    replicate.datset=replicate.datset,
    ModifiedPoisson=ModifiedPoisson,
    error.of.AUC.normalized=error.of.AUC.normalized,
    mean.truth=mean.truth,
    sd.truth=sd.truth,
    p.truth=p.truth,
    l.truth=l.truth,
    z.truth=z.truth,
    AUC.truth=AUC.truth,
    validation.of.AUC.EAP  =validation.of.AUC.EAP,
    validation.of.AUC.CI.lower =validation.of.AUC.CI.lower,
    validation.of.AUC.CI.upper  =validation.of.AUC.CI.upper,
    validation.of.z.Threshold.EAP = validation.of.z.Threshold.EAP,
    validation.of.z.Threshold.CI.lower = validation.of.z.Threshold.CI.lower,
    validation.of.z.Threshold.CI.upper = validation.of.z.Threshold.CI.upper,
    validation.of.mean.of.latent.EAP = validation.of.mean.of.latent.EAP,
    validation.of.mean.of.latent.CI.lower = validation.of.mean.of.latent.CI.lower,
    validation.of.mean.of.latent.CI.upper = validation.of.mean.of.latent.CI.upper,
    validation.of.Standard.Deviation.latent.EAP = validation.of.Standard.Deviation.latent.EAP,
    validation.of.Standard.Deviation.latent.CI.lower = validation.of.Standard.Deviation.latent.CI.lower,
    validation.of.Standard.Deviation.CI.upper = validation.of.Standard.Deviation.CI.upper,
    validation.of.p.Hitrate.EAP = validation.of.p.Hitrate.EAP,
    validation.of.p.Hitrate.CI.lower = validation.of.p.Hitrate.CI.lower,
    validation.of.p.Hitrate.CI.upper = validation.of.p.Hitrate.CI.upper,
    validation.of.l.FalseRate.EAP = validation.of.l.FalseRate.EAP,
    validation.of.l.FalseRate.CI.lower = validation.of.l.FalseRate.CI.lower,
    validation.of.l.FalseRate.CI.upper = validation.of.l.FalseRate.CI.upper,
    validation.of.loglikelihood.of.model = validation.of.loglikelihood.of.model,



    errors.normalized.data.frame=errors.normalized.data.frame,

    error.of.AUC=error.of.AUC,
    error.of.z.Threshold.EAP    = error.of.z.Threshold.EAP,
    error.of.mean.of.latent.EAP  =error.of.mean.of.latent.EAP,
    error.of.Standard.Deviation.latent.EAP =error.of.Standard.Deviation.latent.EAP,
    error.of.p.Hitrate.EAP   =error.of.p.Hitrate.EAP,
    error.of.l.FalseRate.EAP =error.of.l.FalseRate.EAP,
    # errors.data.frame=errors.data.frame,



    dataList=list.of.dataList
  )
  )


}#function












