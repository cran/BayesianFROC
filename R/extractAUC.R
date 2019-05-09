#' @title    Extract AUC for each modality from hierarchical Bayesian models.
#'@description    EAPs
#' @inheritParams fit_Bayesian_FROC
#' @export extractAUC
#  devtools::document();help("DrawCurves_MRMC_pairwise")
#'@return The estimates of AUC with respect to modalities.
#'@param StanS4class This is an output of \code{rstan::stan} for a single reader and a single modality.
#' More precisely, this is an object of some inherited class from the S4 class called stanfit in the rstan package.
#'@param dig digits of estimates.
#'
extractAUC <-function(StanS4class,dig=3,summary=TRUE){  if(summary==TRUE){
  fit <- methods::as(StanS4class, "stanfit")
if( length(rstan::get_divergent_iterations(fit))  < 10000  ){
  message(crayon::silver("\n* Note that if your samples from the Hamiltonian MonteCarlo Method is small, then the AUCs is not precise values. You should calculate MCMC samples in suffiecietly large numbers and also carefully check the convergence criterion by R hat statistics. Small MCMC sampling gives you the AUCs which are unreliable. I recommand that the MCMC samples is at least 30000 MCMC samples for reliable estimates."))
}

if(StanS4class@studyDesign =="MRMC")
{  M <-as.integer(StanS4class@dataList$M)

  AUC <- vector()
  A.CI.lower<- vector()
  A.CI.upper<- vector()
  name <- vector()
  nnname <- vector()

   for (md in 1:M) {
   AUC[md] <- signif(  mean(rstan::extract(fit)$A[,md]),   digits = dig)
   name[md] <-md
   nnname[md] <-paste("A[",md,"]",sep = "")
   A.CI.lower[md]  <- signif(  as.data.frame(summary(fit)[[1]])[nnname[md],"2.5%"],   digits = dig)
   A.CI.upper[md]  <- signif(  as.data.frame(summary(fit)[[1]])[nnname[md],"97.5%"],   digits = dig)
   }

  message(crayon::silver( "\n------------------------------"))
  message(crayon::silver( "\n* The following table shows the AUCs for each modality, that is, the area under the AFROC curves."))
  message(crayon::silver( "\n* The following table shows, from the left, modality ID, expected a posterior estimates and upper and lower credible interbals for AUCs."))

   print(knitr::kable(data.frame(Modality=name,
                                AUC.EAP=AUC,
                                AUC.CI.lower=A.CI.lower,
                                AUC.CI.upper=A.CI.upper
                                ) ,  align ="cccc", format="pandoc")
        )

  message("\n------------------------------\n")

  grDevices::dev.new()
  message("In the Figure of AUCs, the two intervals are shown for each modality, i.e., /n")
  print( rstan::plot(methods::as(StanS4class, "stanfit"),par=nnname))

  #  nnname =c(A[1],A[2],....,A[M])


  AUCs <- data.frame(Modality=name,
                     AUC.EAP=AUC,
                     AUC.CI.lower=A.CI.lower,
                     AUC.CI.upper=A.CI.upper )
  invisible(
            list(AUCs=AUCs
                ,nnname=nnname
                )

          )








}#if StanS4class@studyDesign =="MRMC"




  #################################### srsc ################
  if(!StanS4class@studyDesign =="MRMC"){
    AUC <- signif(  as.data.frame(summary(fit)[[1]])["A","mean"],
                    digits = dig
                    )

    A.CI.lower  <- signif(  as.data.frame(summary(fit)[[1]])["A","2.5%"],   digits = dig)
    A.CI.upper  <- signif(  as.data.frame(summary(fit)[[1]])["A","97.5%"],   digits = dig)

    print(knitr::kable(data.frame( AUC=AUC,
                                   lowerCI=A.CI.lower,
                                   upperCI=A.CI.upper )
                       ,  align ="cccc", format="pandoc")  )


    #  grDevices::dev.new()
    # print( rstan::plot(methods::as(StanS4class, "stanfit"),par=c("A")))
    #
    # grDevices::dev.new()
    #  print( rstan::stan_hist(methods::as(StanS4class, "stanfit"),par=c("A")))

     invisible(data.frame( AUC=AUC,
                           A.CI.lower=A.CI.lower,
                           A.CI.upper=A.CI.upper )
               )

  }#if srsc   #################################### srsc ################







}}
