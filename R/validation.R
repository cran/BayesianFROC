
#'@title Dataset creator from known distributions
#'@description By specifying the parameters of bi-normal assumptions,
#'( that is, mean and sd of the signal latent Gaussian distribution)
#' the dataset are created from this known distributions.
#' The number of lesions and the number of images corresponds the sample size of each replicated dataset.
#'
#'This function also fit a srsc FROC model for each replicated dataset.
#'
#'@return Return values is,
#'
#'stanfit objects, more precisely some inherited class, for each replicated dataset.
#’Replicated datasets
#’Errors, that is, EAPs minus true values, and the variances of estimators.
#’
#'@param mean.truth This is a parameter of the latent Gaussian assumption for the noise distribution.
#'@param sd.truth This is a parameter of the latent Gaussian assumption for the noise distribution.
#'@param z.truth This is a parameter of the latent Gaussian assumption for the noise distribution.
#'@param NL Number of Lesions.
#'@param NI Number of Images.
#'@param replicate.datset A Number indicate that how many you replicate dataset from user's specified dataset.
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@examples
#' \donttest{
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####

#'#================The first example======================================
#'
#'
#'
#'#   Using the default values, the code run, i.e.:
#'
#'    datasets <- validation.dataset_srsc()
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#================The second example======================================
#'
#'#   If user do not familiar with the values of thresholds, then
#'#   it would be better to use the actual estimated values
#'#    as an example of true parameters. In the following,
#'#     I explain this.
#'
#'# First, get estimators by
#'
#'   fit <- fit_Bayesian_FROC(dataList.Chakra.1,ite = 1111,summary =FALSE,cha=3)
#'
#'#  Secondly, extract the expected a posterior estimators (EAPs) from the object fit
#'
#'   z <- rstan::get_posterior_mean(fit,par=c("z"))[,"mean-all chains"]
#'
#'#  Thirdly we use this z as a true values. If user does not know
#'#   the scale or ordinal values of thersholds, by using fitted values,
#'#     user can use this function to replicate datasets.
#'
#'    datasets <- validation.dataset_srsc(z.truth = z)
#'
#'
#'
#'}# dottest

#' @export
validation.dataset_srsc <-function(
  replicate.datset =3,
  ModifiedPoisson = FALSE,
  mean.truth=0.6,
  sd.truth=5.3,
  z.truth =c(-0.8,0.7,2.38),
  NL=259,
  NI=57,
  ite =1111,
  summary=TRUE
){
  C <- length(z.truth)
  dz.truth <-vector()
  for (cd in 1:C-1) {
    dz.truth[cd] <- z.truth[cd+1]-z.truth[cd]
  }

  if(ModifiedPoisson==F){ NX <- NI}
  if(ModifiedPoisson==T){ NX <- NL}

  p.truth <- vector()
  list.of.dataList <- list()
  vector.of.dataList <- list()
  name.vector.of.dataList <- vector()
  for (cd in 1:C) {
    name.vector.of.dataList[cd] <- paste("f[",C-cd+1,"]",sep = "")
  }
  for (cd in 1:C) {
    name.vector.of.dataList[C+cd] <- paste("h[",C-cd+1,"]",sep = "")
  }
  name.vector.of.dataList[2*C+1] <- "NL"
  name.vector.of.dataList[2*C+2] <- "NI"




  l.truth <- -log( stats::pnorm(z.truth))
  for (seed in 1:replicate.datset ) {

    f.inv <- vector()#these should in for sentence
    h.inv <- vector()
    f <- vector()
    h <- vector()


    for (cd in 1:C) {
      if (cd==C) {p.truth[cd]= 1 -  stats::pnorm((z.truth[cd]-mean.truth)/sd.truth)
      }else{

        p.truth[cd]<- stats::pnorm((z.truth[cd+1]-mean.truth)/sd.truth) -  stats::pnorm((z.truth[cd]-mean.truth)/sd.truth)
      }}




    for (cd in 1:C) {

      if(ModifiedPoisson==F){
        if (cd==C) {
          set.seed(seed =  seed);
          f.inv[cd]  <- stats::rpois(n= 1, lambda = (l.truth[cd]-0)*NI )
        }else{
          set.seed(seed =  seed);
          f.inv[cd]  <- stats::rpois(n= 1, lambda = (l.truth[cd]-l.truth[cd+1])*NI )
        }#else
      }# if  ModifiedPoisson==F




      if(ModifiedPoisson==T){
        if (cd==C) {
          set.seed(seed =  seed);
          f.inv[cd]  <- stats::rpois(n= 1, lambda = (l.truth[cd]-0)*NL )
        }else{
          set.seed(seed =  seed);
          f.inv[cd]  <- stats::rpois(n= 1, lambda = (l.truth[cd]-l.truth[cd+1])*NL)
        }#else      set.seed(seed =  seed); hits <-   stats::rbinom(n=1,size = NL,prob = p.truth[cd])
      }#  if ModifiedPoisson==T

      set.seed(seed =  seed); h.inv[cd] <-   stats::rbinom(n=1,size = NL,prob = p.truth[cd])


      f[C-cd+1] <- f.inv[cd]
      h[C-cd+1] <- h.inv[cd]

    }#  for cd in 1:C



    list.of.dataList[[seed]] <- list(
      NL=NL,
      NI=NI,
      f=f,
      h=h,
      C=C
    )
    a<-append( list.of.dataList[[seed]]$f, list.of.dataList[[seed]]$h);
    b <-append( list.of.dataList[[seed]]$NL, list.of.dataList[[seed]]$NI);
    vector.of.dataList[[seed]] <-append(a,b)
    # browser()

    names(vector.of.dataList[[seed]] )  <-  name.vector.of.dataList
  }#for seed


  #-------------------------------
  #
  #  * Datasets were  created !!
  #
  #  * Next Fitting !!
  #
  #-------------------------------

  fit <- list()
  validation.of.AUC.EAP <-vector()
  validation.of.AUC.CI.lower <-vector()
  validation.of.AUC.CI.upper <-vector()

  validation.of.z.Threshold.EAP <- list()
  validation.of.z.Threshold.CI.lower <- list()
  validation.of.z.Threshold.CI.upper <- list()

  validation.of.dz.Threshold.EAP <- list()
  validation.of.dz.Threshold.CI.lower <- list()
  validation.of.dz.Threshold.CI.upper <- list()

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

  convergent.dataList <- list()
  convergent.dataList.as.dataframe <- data.frame(row.names = name.vector.of.dataList)
  # rownames(convergent.dataList.as.dataframe) <-  name.vector.of.dataList

  s<-0
  for (seed in 1:replicate.datset ) {
    #It seems better to make a indicator and
    #do not print the result of rstan

    fit[[seed]] <- fit_Bayesian_FROC(
      dataList=list.of.dataList[[seed]],
      ite = ite,
      new.imaging.device = FALSE,
      summary = FALSE,
      ModifiedPoisson = ModifiedPoisson,
      DrawCurve = FALSE
    )

    message("\n* The ",seed,"-th fitting \n",sep = "")
    message("\n----------------------------------------\n",sep = "")


    if ( fit[[seed]]@convergence ==TRUE) {
      s <-s+1

      convergent.dataList[[s]] <-fit[[seed]]@dataList

      a<-append( convergent.dataList[[s]]$f, convergent.dataList[[s]]$h);
      b <-append( convergent.dataList[[s]]$NL, convergent.dataList[[s]]$NI);
      convergent.dataList.as.dataframe[,s] <-append(a,b)
      # browser()






      ssss <- summary_EAP_CI_srsc( fit[[seed]],summary = FALSE,dig =11)

      validation.of.AUC.EAP[s] <- ssss$AUC.EAP
      validation.of.AUC.CI.lower[s] <-ssss$AUC.CI.lower
      validation.of.AUC.CI.upper[s] <- ssss$AUC.CI.upper

      validation.of.z.Threshold.EAP[[s]]  <- ssss$z.Threshold.EAP
      validation.of.z.Threshold.CI.lower[[s]]  <- ssss$z.Threshold.CI.lower
      validation.of.z.Threshold.CI.upper[[s]]  <- ssss$z.Threshold.CI.upper

      validation.of.dz.Threshold.EAP[[s]]  <- ssss$dz.Threshold.EAP
      validation.of.dz.Threshold.CI.lower[[s]]  <- ssss$dz.Threshold.CI.lower
      validation.of.dz.Threshold.CI.upper[[s]]  <- ssss$dz.Threshold.CI.upper

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

  }#for seed

  col.names <- vector()
  for (sd in 1:s) {
    col.names[sd] <- paste(sd,"-th model",sep = "")
  }


  colnames(convergent.dataList.as.dataframe) <- col.names

  number.of.convergent.model <-s
  message("\n  ----- Comments for Validation -----\n")
  message("\n* Number of convergent model:",s,"\n")
  message("\n* Convergence rate:", (s/replicate.datset)*100 ,"% \n")
  a.truth <- mean.truth/sd.truth
  b.truth  <- 1/sd.truth
  AUC.truth <- stats::pnorm(a.truth/ sqrt( 1+b.truth^2))

  l.truth <-l.truth[1:C]



  error.of.AUC            <- validation.of.AUC.EAP  -  AUC.truth










  l1 <-validation.of.z.Threshold.EAP
  # l2 <-rep( list(z.truth) , length(unlist( validation.of.z.Threshold.EAP))/length(z.truth)  )
  l2 <-rep( list(z.truth) , number.of.convergent.model)

  error.of.z.Threshold.EAP     <-  Map( `-`,l1  , l2 )
  mean.error.of.z.Threshold.EAP<- rep(NaN, length(error.of.z.Threshold.EAP[[1]]))
  sd.error.of.z.Threshold.EAP<- rep(NaN, length(error.of.z.Threshold.EAP[[1]]))
  name.z.Threshold.EAP<- rep(NaN, length(error.of.z.Threshold.EAP[[1]]))

  for(i.th.column in 1: length(error.of.z.Threshold.EAP[[1]])  ){
    mean.error.of.z.Threshold.EAP[i.th.column]<-mean(as.numeric(lapply(error.of.z.Threshold.EAP,"[",n=i.th.column)))
    # sd.error.of.z.Threshold.EAP[i.th.column]<-stats::sd(as.numeric(lapply(error.of.z.Threshold.EAP,"[",n=i.th.column)))
    sd.error.of.z.Threshold.EAP[i.th.column]<-stats::sd(as.numeric(lapply(validation.of.z.Threshold.EAP,"[",n=i.th.column)))
    name.z.Threshold.EAP[i.th.column]<- paste("z[", i.th.column, "]")

  }
  # browser()



  l1 <-validation.of.dz.Threshold.EAP
  # l2 <-rep( list(z.truth) , length(unlist( validation.of.z.Threshold.EAP))/length(z.truth)  )
  l2 <-rep( list(dz.truth) , number.of.convergent.model)

  error.of.dz.Threshold.EAP     <-  Map( `-`,l1  , l2 )
  mean.error.of.dz.Threshold.EAP<- rep(NaN, length(error.of.dz.Threshold.EAP[[1]]))
  sd.error.of.dz.Threshold.EAP<- rep(NaN, length(error.of.dz.Threshold.EAP[[1]]))
  name.dz.Threshold.EAP<- rep(NaN, length(error.of.dz.Threshold.EAP[[1]]))

  for(i.th.column in 1: length(error.of.dz.Threshold.EAP[[1]])  ){
    mean.error.of.dz.Threshold.EAP[i.th.column]<-mean(as.numeric(lapply(error.of.dz.Threshold.EAP,"[",n=i.th.column)))
    # sd.error.of.z.Threshold.EAP[i.th.column]<-stats::sd(as.numeric(lapply(error.of.z.Threshold.EAP,"[",n=i.th.column)))
    sd.error.of.dz.Threshold.EAP[i.th.column]<-stats::sd(as.numeric(lapply(validation.of.dz.Threshold.EAP,"[",n=i.th.column)))
    name.dz.Threshold.EAP[i.th.column]<- paste("dz[", i.th.column, "]")

  }




  error.of.mean.of.latent.EAP  <-    validation.of.mean.of.latent.EAP -  mean.truth
  error.of.Standard.Deviation.latent.EAP  <-   validation.of.Standard.Deviation.latent.EAP -sd.truth
  #S.E.
  sd.error.of.mean.of.latent.EAP <- stats::sd(validation.of.mean.of.latent.EAP)
  sd.error.of.Standard.Deviation.latent.EAP <-stats::sd(validation.of.mean.of.latent.EAP)
  sd.error.of.AUC <-stats::sd(validation.of.AUC.EAP)

  # sd.error.of.AUC <-stats::sd(error.of.AUC)




  name.Gaussian <- append(name.z.Threshold.EAP, c("mean.of.signal", "sd.of.signal","AUC")   )
  Bias.Gaussian <- append(mean.error.of.z.Threshold.EAP, c(mean(error.of.mean.of.latent.EAP),mean(error.of.Standard.Deviation.latent.EAP),mean(error.of.AUC ) )  )
  Std.Err.Gaussian <- append(sd.error.of.z.Threshold.EAP, c(sd.error.of.mean.of.latent.EAP, sd.error.of.Standard.Deviation.latent.EAP,   sd.error.of.AUC)   )

  name.Gaussian <- append(name.Gaussian, name.dz.Threshold.EAP   )
  Bias.Gaussian <- append(Bias.Gaussian, mean.error.of.dz.Threshold.EAP )
  Std.Err.Gaussian <- append(Std.Err.Gaussian,sd.error.of.dz.Threshold.EAP   )





  if (summary==TRUE) {
    message("\n* This is a model parameter for the latent Gaussian distribution.\n")
    print(knitr::kable( data.frame(Param.Name=name.Gaussian, ############ Here !!
                                   Bias =  Bias.Gaussian,  ########### Here !!
                                   S.E. = Std.Err.Gaussian   )########### Here !!
    ))
  }





  l1 <-validation.of.p.Hitrate.EAP
  l2 <-rep( list(p.truth) , length(unlist( validation.of.p.Hitrate.EAP))/length(p.truth)  )
  error.of.p.Hitrate.EAP     <-  Map( `-`,l1  , l2 )
  mean.error.p.Hitrate.EAP<- rep(NaN, length(error.of.p.Hitrate.EAP[[1]]))
  sd.error.p.Hitrate.EAP<- rep(NaN, length(error.of.p.Hitrate.EAP[[1]]))
  name.p.Hitrate.EAP<- rep(NaN, length(error.of.p.Hitrate.EAP[[1]]))

  for(i.th.column in 1: length(error.of.p.Hitrate.EAP[[1]])  ){
    mean.error.p.Hitrate.EAP[i.th.column]<-mean(as.numeric(lapply(error.of.p.Hitrate.EAP,"[",n=i.th.column)))
    sd.error.p.Hitrate.EAP[i.th.column]<-stats::sd(as.numeric(lapply(error.of.p.Hitrate.EAP,"[",n=i.th.column)))
    name.p.Hitrate.EAP[i.th.column]<- paste("p[", i.th.column, "]")

  }
  if (summary==TRUE) {

    print(knitr::kable( data.frame(Param.Name=name.p.Hitrate.EAP,########### Here !!
                                   Bias =  mean.error.p.Hitrate.EAP,########### Here !!
                                   S.E. = sd.error.p.Hitrate.EAP   )########### Here !!
    )
    )
  }
  # browser()



  l1 <-validation.of.l.FalseRate.EAP
  l2 <-rep( list(l.truth) , length(unlist( validation.of.l.FalseRate.EAP))/length(l.truth)  )
  error.of.l.FalseRate.EAP     <-  Map( `-`,l1  , l2 )
  mean.error.l.FalseRate.EAP<- rep(NaN, length(error.of.l.FalseRate.EAP[[1]]))
  sd.error.l.FalseRate.EAP<- rep(NaN, length(error.of.l.FalseRate.EAP[[1]]))
  name.l.FalseRate.EAP<- rep(NaN, length(error.of.l.FalseRate.EAP[[1]]))
  for(i.th.column in 1: length(error.of.l.FalseRate.EAP[[1]])  ){
    mean.error.l.FalseRate.EAP[i.th.column]<-mean(as.numeric(lapply(error.of.p.Hitrate.EAP,"[",n=i.th.column)))
    sd.error.l.FalseRate.EAP[i.th.column]<-stats::sd(as.numeric(lapply(error.of.p.Hitrate.EAP,"[",n=i.th.column)))
    name.l.FalseRate.EAP[i.th.column]<- paste("lambda[", i.th.column, "]")
  }

  if (summary==TRUE) {

    print(knitr::kable( data.frame(Param.Name=name.l.FalseRate.EAP,########### Here !!
                                   Bias =  mean.error.l.FalseRate.EAP,########### Here !!
                                   S.E. = sd.error.l.FalseRate.EAP   ))   )########### Here !!
    # browser()
  }

  Name.of.all.param <- append(name.Gaussian,
                              append(name.p.Hitrate.EAP,
                                     name.l.FalseRate.EAP)
  )

  Bias.of.all.param <- append(
    Bias.Gaussian,
    append(
      mean.error.p.Hitrate.EAP,
      mean.error.l.FalseRate.EAP)
  )

  S.E.of.all.param <- append(Std.Err.Gaussian,
                             append(
                               sd.error.p.Hitrate.EAP,
                               sd.error.l.FalseRate.EAP)
  )

  results <- data.frame( Name.of.all.param=Name.of.all.param,
                         Bias.of.all.param=Bias.of.all.param,
                         S.E.of.all.param=S.E.of.all.param
  )




  error.of.mean.of.latent.EAP  <-    validation.of.mean.of.latent.EAP -  mean.truth
  error.of.Standard.Deviation.latent.EAP  <-   validation.of.Standard.Deviation.latent.EAP -sd.truth








  invisible( list(
    results=results,
    Name.of.all.param = Name.of.all.param,
    Bias.of.all.param = Bias.of.all.param,
    S.E.of.all.param  = S.E.of.all.param,


    vector.of.dataList=vector.of.dataList,

    convergent.dataList = convergent.dataList,
    convergent.dataList.as.dataframe = convergent.dataList.as.dataframe,



    fit=fit,
    replicate.datset=replicate.datset,
    ModifiedPoisson=ModifiedPoisson,
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




    error.of.AUC=error.of.AUC,
    error.of.z.Threshold.EAP    = error.of.z.Threshold.EAP,
    error.of.mean.of.latent.EAP  =error.of.mean.of.latent.EAP,
    error.of.Standard.Deviation.latent.EAP =error.of.Standard.Deviation.latent.EAP,
    error.of.p.Hitrate.EAP   =error.of.p.Hitrate.EAP,
    error.of.l.FalseRate.EAP =error.of.l.FalseRate.EAP,



    dataList=list.of.dataList
  )
  )


}#function










#'@title Dataset creator from known distributions.
#'@description By specifying the parameters of binormal assumptions,
#' the dataset are created from this known distributions.

#'@param NLvector Vector indicates the number of Lesions.
#@param NIvector  Number of Images.
#'@param ratio   Number of Images is determined by converting \code{ratio} * NLvector to a integer.
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@inheritParams validation.dataset_srsc
#'@examples
#' \donttest{

#'
#'    datasets <-validation.dataset_srsc_for_different_NI_NL(
#'                NLvector = c(100,10000000,1000000000),
#'                ite = 2222
#'                )
#'
#'
#'  # By the following, we can extract only datasets whose
#'  # model has converged.
#'    datasets$convergent.dataList.as.dataframe
#'
#'}# dottest


#' @export


validation.dataset_srsc_for_different_NI_NL <-function(NLvector = c(100,10000,1000000,100000000),
                                                       # NIvector,
                                                       ratio=2,
                                                       replicate.datset =3,
                                                       ModifiedPoisson = FALSE,
                                                       mean.truth=0.6,
                                                       sd.truth=5.3,
                                                       z.truth =c(-0.8,0.7,2.38),
                                                       ite =111
){



  d <- list()
  Bias.of.all.param <- list()
  S.E.of.all.param  <- list()
  Bias.of.all.param.with.NL.NI <- list()
  S.E.of.all.param.with.NL.NI <- list()
  Name.of.all.param.with.NL.NI <- vector()
  col.names <- vector()
  convergent.dataList.as.dataframe <- list()


  col.names[1] <-paste("Name.of.Parameters")
  # col.names[1] <-paste("")
  s <- 0

  for (nl in NLvector) {
    s<- s+1
    ni<- floor(ratio*nl)
    #Create the replicating datasets for different number of lesions
    d[[s]]<-  validation.dataset_srsc(
      replicate.datset =replicate.datset,
      ModifiedPoisson = ModifiedPoisson,
      mean.truth=mean.truth,
      sd.truth=sd.truth,
      z.truth =z.truth,
      NL=ni,
      NI=nl,
      ite =ite,
      summary=FALSE)

    Bias.of.all.param[[s]] <-d[[s]]$Bias.of.all.param
    S.E.of.all.param[[s]]  <-d[[s]]$S.E.of.all.param

    Bias.of.all.param.with.NL.NI[[s]] <- append(append(ni,nl),
                                                Bias.of.all.param[[s]])
    S.E.of.all.param.with.NL.NI[[s]] <- append(append(ni,nl),
                                               S.E.of.all.param[[s]])
    col.names[s+1] <-paste(s,"-th validation",sep = "")

    convergent.dataList.as.dataframe[[s]] <- d[[s]]$convergent.dataList.as.dataframe

  }# for  nl in NLvector

  list.names <- vector()
  for (sd in 1:s) {
    list.names [sd] <- paste(sd,"-th dataset of convergent model ",sep = "")
  }
  names(convergent.dataList.as.dataframe) <- list.names


  Name.of.all.param.with.NL.NI<- append(append("Number of Images",
                                               "Number of Lesions"),
                                        d[[1]]$Name.of.all.param  )

  l <-list(
    Name.of.all.param.with.NL.NI = Name.of.all.param.with.NL.NI,
    Bias.of.all.param.with.NL.NI = Bias.of.all.param.with.NL.NI
  )#list

  options(scipen=100)
  Bias.for.various.NL<-as.data.frame(l)
  names(Bias.for.various.NL) <- col.names
  # browser()
  print(knitr::kable(Bias.for.various.NL)
  )

  invisible(
    list(
      Bias.for.various.NL  =Bias.for.various.NL,

      Name.of.all.param.with.NL.NI = Name.of.all.param.with.NL.NI,
      Bias.of.all.param.with.NL.NI = Bias.of.all.param.with.NL.NI,
      S.E.of.all.param.with.NL.NI  = S.E.of.all.param.with.NL.NI,
      datasets =d,
      convergent.dataList.as.dataframe=convergent.dataList.as.dataframe
    )

  )#invisible


}



# datasets$datasets[[2]]$dataList





#'@title Draw Curves for validation dataset
#'@description  drawing curves.
#'@inheritParams DrawCurves
#'@inheritParams DrawCurves_MRMC_pairwise
#'@inheritParams fit_Bayesian_FROC
#'@param validation.data This is a return value of the function \code{validation.dataset_srsc}.
#'@examples
#'
#' \donttest{

#'
#'     datasets <- validation.dataset_srsc()
#'
#'     validation.draw_srsc(datasets)
#'
#'}# dottest

#' @export
validation.draw_srsc  <-function(validation.data,
                                 mesh.for.drawing.curve=11111,
                                 upper_y=1,
                                 DrawFROCcurve=TRUE
){






  replicate.datset <- as.integer(validation.data$replicate.datset)

  s<-vector()
  for (seed in 1:replicate.datset ) {if (validation.data$fit[[seed]]@convergence == TRUE) {
    s <- append(s, validation.data$fit[[seed]]@metadata$ff )
  }}
  upper_x <-max(s)

  s<-0
  for (seed in 1:replicate.datset ) {
    if (validation.data$fit[[seed]]@convergence == TRUE) {
      s<-s+1
      validation.data$fit[[seed]]@index <-s

      DrawCurves(validation.data$fit[[seed]],
                 new.imaging.device = FALSE,
                 title = FALSE,
                 DrawFROCcurve=DrawFROCcurve,
                 indexCFPCTP = s,
                 upper_x=upper_x,
                 upper_y=upper_y)
    }

    if (validation.data$fit[[seed]]@convergence == FALSE) {
      message("\n* The ",seed,"-th curve are not drawn, since  the model did not converge.\n",sep = "")
    }

  }

  mean.truth <- validation.data$mean.truth
  sd.truth   <- validation.data$sd.truth


  a.truth <- mean.truth/sd.truth
  b.truth  <- 1/sd.truth
  if(validation.data$fit[[1]]@studyDesign ==  "srsc.per.image"){ xlabel =  'mean of cumulative false positives per image'  }
  if(validation.data$fit[[1]]@studyDesign ==  "srsc.per.lesion"){ xlabel =  'mean of cumulative false positives per lesion'  }

  set.seed(1);ll<- stats::rchisq(mesh.for.drawing.curve, 1)
  lll<- 0.99+ll
  x.FROC<-append(ll,lll)
  y.FROC.truth <- 1 - stats::pnorm(  b.truth*stats::qnorm(exp(-x.FROC)) - a.truth  )
  if( missing(upper_y)){  upper_y <- 1.0    }
  suppressWarnings(graphics::par(new=TRUE));
  plot(x.FROC,y.FROC.truth,
       col = 'red',
       cex= 0.1 ,
       xlim = c(0,upper_x ),ylim = c(0,upper_y),
       xlab = xlabel,
       ylab = 'cumulative hit per lesion'
       ,main =""
  );




}#Def of function


