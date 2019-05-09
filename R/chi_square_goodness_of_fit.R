#'@title Get the Chi square values
#'@description Get the Chi square values with indexed by the all possible pair of replicated data and MCMC samples.
#'
#'
#'@details
#'Here, we briefly review how to get the chi square samples.
#'
#'First, Let f(y|s) be a model (likelihood) with future data y and model parameter s. Let p(s|D) be a posterior probability density for given data D. In this situation, the Hamiltonian Monte Carlo method is performed to get the MCMC samples, say s1, s2, s3,...,sN. Alternatively, we get the sequence of models f(y| s1), f(y| s2), f(y| s3),...,f(y| sN). To get the samples y1, y2,...,yN from the posterior probability distribution, we merely draw the y1, y2,...,yN from f(y| s1), f(y| s2), f(y| s3),...,f(y| sN), respectively. That is for all I yi is drawn from the distribution f(y|si).
#'
#'In this manner we get the two sequence of samples, one is from the posterior distribution and one is the posterior predictive distribution. Using these two kind of samples, we can calculate the test statistics as the Bayesian manner. That is, in frequentist method, the test statistics are calculated by the fixed model parameters, such as the maximal likelihood estimators. However, in Bayesian context, the parameter is not deterministic and hence we should calculate test statistics with the posterior measure. To accomplish this task, this package include the function.


#'@inheritParams get_samples_from_Posterior_Predictive_distribution
#'@inheritParams chi_square_goodness_of_fit_from_input_all_param
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@inheritParams validation.dataset_srsc
#'
#' @return the Chi square values with indexed by the all possible pair of replicated data and MCMC samples.
#' @export

#' @examples
#'  \donttest{

# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'# First, fit the model to data. The number of sampling of the Hamiltonian Monte Carlo
#'# methods should be a little number, if user computer has low ability,
#'# since the calculation of the posterior predictive p values is heavy.
#'
#'
#'  fit <- fit_Bayesian_FROC(BayesianFROC::dataList.Chakra.1 ,ite = 1111)
#'
#'
#'
#'#  Second, extract the posterior predictive p value from the output.
#'#  If it is greater than, e.g., 0.05, then your model cannot be said that
#'#   it is not fitted to your data.
#'
#'  output <-  p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit(fit)
#'
#'
#'  output$p.values.for.chisquare
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####

#'# If user has no time, then  plot.replicated.points=FALSE will help you.
#'# By setting FALSE, the replicated data from the posterior predictive
#'# distribution does not draw, and hence the running time of function become shorter.
#'
#'  TPs.FPs <- p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit(fit,
#'                                    plot.replicated.points =  FALSE)
#'
#'
#'#  If user want to use the scatter plots of hits and false alarms from the posterior
#'#  predictive distribution for the submission, then the color plot is not appropriate.
#'#  So, by setting the argument Colour = FALSE, the scatter plot become black and white.
#'#  So, user can use this scatter plot for submission.
#'
#'
#'  p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit(fit,Colour = FALSE)
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'
#'# Since p values are depend on data only, so it is better to show this dependency more
#'# explicitly as follows;
#'
#'    p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit(
#'    fit_Bayesian_FROC(dataList.High)
#'     )
#'
#'
#'
#'
#'        #    Close the graphic device
#'
#'        Close_all_graphic_devices()
#'
#'
#'}# dottest
#'@seealso get_samples_from_Posterior_Predictive_distribution,
#'chi_square_goodness_of_fit_from_input_all_param
#'
p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit <-function(
  StanS4class,
  dig=3,
  Colour=TRUE,
  plot.replicated.points=FALSE

){
  if (StanS4class@studyDesign=="MRMC")return(message("\n* srsc only. \n"))

  if(  StanS4class@studyDesign == "srsc.per.image"){       ModifiedPoisson <-  FALSE   }
  if(  StanS4class@studyDesign == "srsc.per.lesion"){      ModifiedPoisson <-  TRUE    }


  Chi.Square.for.each.MCMC.samples.and.each.replicated.data.from.posterior.predictive.distributions <- vector()
  Chi.Square.for.each.MCMC.samples.for.observed.data <- vector()
  NI <-  StanS4class@dataList$NI
  NL <-  StanS4class@dataList$NL
  f.observed  <-  StanS4class@dataList$f
  h.observed  <-  StanS4class@dataList$h
  C  <-  StanS4class@dataList$C

  fit <-methods::as( StanS4class,"stanfit")

  HitsFalse <-  get_samples_from_Posterior_Predictive_distribution(
    StanS4class=StanS4class,
    Colour=Colour,
    plot.replicated.points=plot.replicated.points
  )

  # MCMC <-length(extract(fit)$lp__)
  MCMC <-  length(  HitsFalse$replicated.false.alarms.from.the.posterior.predictive.distribution )

  f.list <- HitsFalse$replicated.false.alarms.from.the.posterior.predictive.distribution
  h.list <- HitsFalse$replicated.hits.from.the.posterior.predictive.distribution



  p  <- extract(fit, par=c("p"))
  lambda  <- extract(fit, par=c("l"))


  name.of.chisq.vector.replicated <-vector()
  name.of.chisq.vector.observed <-vector()
  name<-vector()


  for (mcmc in 1:MCMC) {

    name.of.chisq.vector.replicated[mcmc] <-paste("the ",mcmc,"-th chi square using replicated data",sep = "")
    name.of.chisq.vector.observed[mcmc] <-paste("the ",mcmc,"-th chi square using observed data",sep = "")
    name[mcmc] <-paste("the ",mcmc,"-th",sep = "")

    Chi.Square.for.each.MCMC.samples.and.each.replicated.data.from.posterior.predictive.distributions[mcmc] <-
      chi_square_goodness_of_fit_from_input_all_param(
        # StanS4class= StanS4class,


        h = h.list[[mcmc]] ,
        f = f.list[[mcmc]],


        p =  p$p[mcmc,], #ecxtract the hits vector for the mcmc - the HMC sample
        lambda= lambda$l[mcmc,],

        NL= NL,
        NI =NI,
        dig=dig,
        ModifiedPoisson = ModifiedPoisson
      )




    Chi.Square.for.each.MCMC.samples.for.observed.data[mcmc]<-
      chi_square_goodness_of_fit_from_input_all_param(
        # StanS4class= StanS4class,


        h = h.observed  ,
        f =  f.observed  ,

        p =  p$p[mcmc,], #ecxtract the hits vector for the mcmc - the HMC sample
        lambda= lambda$l[mcmc,],

        NL= NL,
        NI =NI,
        dig=dig,
        ModifiedPoisson = ModifiedPoisson
      )







  }#for mcmc


  #The following object name is too long, I change the name to obtain the more short name.
  chisq.vector.for.replicated.data <-  Chi.Square.for.each.MCMC.samples.and.each.replicated.data.from.posterior.predictive.distributions

  #The following object name is too long, I change the name to obtain the more short name.
  chisq.vector.for.observed.data <- Chi.Square.for.each.MCMC.samples.for.observed.data

  #Count that the chi square for replicated data is greater than the chi square for observed data.
  indicator <- table( chisq.vector.for.replicated.data > chisq.vector.for.observed.data)[2]

  # Mante Carlo Integral

  p.values.for.chisquare  <- indicator/MCMC

  if (all(!chisq.vector.for.replicated.data >=chisq.vector.for.observed.data)) {
    p.values.for.chisquare <- 0
  }


  if (all(chisq.vector.for.replicated.data <=chisq.vector.for.observed.data)) {
    p.values.for.chisquare <- 1
  }

  d <- data.frame(name =name,
                  chisq.vector.for.observed.data=chisq.vector.for.observed.data,
                  chisq.vector.for.replicated.data=chisq.vector.for.replicated.data,
                  replication.vs.observed= chisq.vector.for.replicated.data > chisq.vector.for.observed.data

  )
  print(knitr::kable(d))
  message("\n*  Note that the posterior predictive p value is a rate of TRUE in the right column in the above table. \n\n")

  names(chisq.vector.for.replicated.data) <- name.of.chisq.vector.replicated

  names(chisq.vector.for.observed.data) <- name.of.chisq.vector.observed


  names(p.values.for.chisquare)<-" The p value of the posterior predictive measure for the chi square discrepancy."

  print(p.values.for.chisquare)

  invisible(list(
    # Chi.Square.for.each.MCMC.samples.and.each.replicated.data.from.posterior.predictive.distributions=Chi.Square.for.each.MCMC.samples.and.each.replicated.data.from.posterior.predictive.distributions,
    Chi.Square.for.each.MCMC.samples.and.each.replicated.data.from.posterior.predictive.distributions = chisq.vector.for.replicated.data,
    Chi.Square.for.each.MCMC.samples.for.observed.data  = chisq.vector.for.observed.data,
    p.values.for.chisquare = p.values.for.chisquare
  )
  )



}#function




#'@title  The Goodness of Fit (Chi Square) Calculator
#'@description Chi square goodness of fit statistics for each MCMC sample with a fixed dataset.
#'
#'
#'
#'    Our data is 2C categories, that is,
#'
#'
#'  the number of hits        :h[1], h[2], h[3],...,h[C] and
#'
#'  the number of false alarms: f[1],f[2], f[3],...,f[C].
#'
#'
#'    Our model has C+2 parameters, that is,
#'
#'    the thresholds of the bi normal assumption z[1],z[2],z[3],...,z[C] and
#'
#'    the mean and standard deviation of the signal distribution.
#'
#'
#'  So, the degree of freedom of this statistics is calculated by
#'
#'
#'             2C-(C+2)-1 =C -3.
#'
#'This differ from Chakraborty's  result C-2. Why ?

#'
#'
#'
#'
#'@details To calculate the chi square test statistics, the two quantities are needed, that is, data and parameter. In the classical (frequentists) chi square values, as the estimates of parameter, for example, MLE (maximal likelihood estimator) is chosen. In Bayesian sense, the parameter can be taken for all MCMC iterations, that is, parameter is not deterministic and we consider it is a random variable or samples from the posterior distribution. And such samples are obtained in the Hamiltonian Monte Carlo Simulation with the author's  Bayesian Model. Thus we can calculate chi square values with MCMC samples.
#'@return Chi squares for each MCMC samples. So, the return values is a vector whose length is number of MCMC iterations minus the warming up period. Of course if MCMC is not only one chain, then taking the summation over all chains.
#'@param f The number of false alarms. The reason why we include this variable is to substitute the false alarms from the posterior predictive distributions.
#'In famous Gelman's book, he explain how to use the test statistics in the Bayesian context. In this context I need to substitute the replication data from the posterior predictive distributions.

#'@param h The number of hits. The reason why we include this variable is to substitute the hits from the posterior predictive distributions.
#'In famous Gelman's book, he explain how to use the test statistics in the Bayesian context. In this context I need to substitute the replication data from the posterior predictive distributions.


#'@param p Hit rate. A vector whose length is number of confidence levels.
#'@param lambda False alarm rate. A vector whose length is number of confidence levels.
#'@param NI Number of Images
#'@param NL Number of Lesions
#'
#'
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@return A number !! Not list nor dataframe nor vector !! Only A number represent the chi square for your input data.
#'@examples
#'\donttest{


# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'
#'#  Get the MCMC samples from a dataset.
#'
#'        fit <- fit_Bayesian_FROC(BayesianFROC::dataList.Chakra.1,
#'                            ite = 1111,
#'                            summary =FALSE,
#'                            cha = 2)
#'
#'#   The chi square discrepancies are calculated
#'#   by the following code by using each MCMC samples as a parameter.#
#'
#'
#'   NI          <-  fit@dataList$NI
#'   NL          <-  fit@dataList$NL
#'   f.observed  <-  fit@dataList$f
#'   h.observed  <-  fit@dataList$h
#'   C           <-  fit@dataList$C
#'   p <-  rstan::get_posterior_mean(fit, par=c("p"))
#'   lambda <- rstan::get_posterior_mean(fit, par=c("l"))
#'
#'
#'          Chi.Square <-   chi_square_goodness_of_fit_from_input_all_param(
#'
#'                           h   =   h.observed,
#'                           f   =   f.observed,
#'                           p   =   p,
#'                       lambda  =   lambda,
#'                           NL  =   NL,
#'                           NI  =   NI
#'                                )
#'
#'#  Get posterior mean of the chi square discrepancy.
#'
#'                     Chi.Square
#'
#' # Calculate the p-value for the posterior mean of the chi square discrepancy.
#'
#'                      stats::pchisq(Chi.Square,df=1)
#'
#'
#'
#'
#'
#'
#'
#'}# dottest
#'
#' @export

chi_square_goodness_of_fit_from_input_all_param <- function(
  # StanS4class,
  h,
  f,
  p,
  lambda,
  NL ,
  NI ,
  ModifiedPoisson=FALSE,
  dig=3
){

  # if(  StanS4class@studyDesign == "srsc.per.image"){       ModifiedPoisson <-  FALSE   }
  # if(  StanS4class@studyDesign == "srsc.per.lesion"){      ModifiedPoisson <-  TRUE    }


  C <-length(h)

  chisquare <- vector()

  ss <- array(0,dim = c(C))
  tt <- array(0,dim = c(C))

  if (ModifiedPoisson == FALSE) {

    for(cd in 1:C){
      ss[cd]<-(h[C+1-cd]-NL*p[cd])^2/(NL*p[cd])
      if(!cd==C)tt[cd]<-(f[C+1-cd]-NI*(lambda[cd]-lambda[cd+1]))^2/(NI*(lambda[cd]-lambda[cd+1]))
      if(cd==C)  tt[cd]<-(f[C+1-cd]-NI*(lambda[cd]- 0))^2/(NI*(lambda[cd]- 0))
    }

  }

  if (ModifiedPoisson == TRUE) {

    for(cd in 1:C){
      ss[cd]<-(h[C+1-cd]-NL*p[cd])^2/(NL*p[cd])
      if(!cd==C)tt[cd]<-(f[C+1-cd]-NL*(lambda[cd]-lambda[cd+1]))^2/(NL*(lambda[cd]-lambda[cd+1]))
      if(cd==C)  tt[cd]<-(f[C+1-cd]-NL*(lambda[cd]- 0))^2/(NL*(lambda[cd]- 0))
    }

  }

  chi.Square.for.each.MCMC.samples <- sum(ss)+sum(tt)
  chi.Square.for.each.MCMC.samples <- signif(chi.Square.for.each.MCMC.samples, digits = dig)
  return(chi.Square.for.each.MCMC.samples)
}









#'@title  The Goodness of Fit (Chi Square) Calculator
#'@description Chi square goodness of fit statistics for each MCMC sample with a fixed dataset.
#'
#'
#'
#'    Our data is 2C categories, that is,
#'
#'
#'  the number of hits        :h[1], h[2], h[3],...,h[C] and
#'
#'  the number of false alarms: f[1],f[2], f[3],...,f[C].
#'
#'
#'    Our model has C+2 parameters, that is,
#'
#'    the thresholds of the bi normal assumption z[1],z[2],z[3],...,z[C] and
#'
#'    the mean and standard deviation of the signal distribution.
#'
#'
#'  So, the degree of freedom of this statistics is calculated by
#'
#'
#'             2C-(C+2)-1 =C -3.
#'
#'This differ from Chakraborty's  result C-2. Why ?

#'
#'
#'
#'
#'@details To calculate the chi square test statistics, the two quantities are needed, that is, data and parameter. In the classical chi square values, as the parameter, for example, MLE(maximal likelihood estimator) is chosen. In Bayesian sense, the parameter can be taken for all MCMC iterations, that is, parameter is not deterministic and we consider it is a random variable or samples from the posterior distribution. And such samples are obtained in the Hamiltonian Monte Carlo Simulation with the author's  Bayesian Model. Thus we can calculate chi square values with MCMC samples.
#'@return Chi squares for each MCMC samples. So, the return values is a vector whose length is number of MCMC iterations minus the warming up period. Of course if MCMC is not only one chain, then taking the summation over all chains.
#'@param StanS4class An object of the inherited class of the S4 class stanfit.
#'That is, the stanfitExtended S4 class defined in this package.
#'@param f The number of false alarms. The reason why we include this variable is to substitute the false alarms from the posterior predictive distributions.
#'In famous Gelman's book, he explain how to use the test statistics in the Bayesian context. In this context I need to substitute the replication data from the posterior predictive distributions.

#'@param h The number of hits. The reason why we include this variable is to substitute the hits from the posterior predictive distributions.
#'In famous Gelman's book, he explain how to use the test statistics in the Bayesian context. In this context I need to substitute the replication data from the posterior predictive distributions.

#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@examples
#'
#' \donttest{

# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'
#'#  Get the MCMC samples from a dataset.
#'
#'        fit <- fit_Bayesian_FROC(BayesianFROC::dataList.Chakra.1,
#'                            ite = 1111,
#'                            summary =FALSE,
#'                            cha = 2)
#'
#'#   The chi square discrepancies are calculated by the following code
#'
#'          Chi.Square.for.each.MCMC.samples   <-   chi_square_goodness_of_fit(fit)
#'
#'#  Get posterior mean of the chi square discrepancy.
#'
#'                     m<-   mean(Chi.Square.for.each.MCMC.samples)
#'
#' # Calculate the p-value for the posterior mean of the chi square discrepancy.
#'
#'                      stats::pchisq(m,df=1)
#'
#'
#'
#'
#'
#'
#'
#'}# dottest

#'
#' @export

chi_square_goodness_of_fit <- function(StanS4class,dig=3,h,f){
  C <-StanS4class@dataList$C

  if (StanS4class@studyDesign=="MRMC") {
    return(message("Now, MRMC not be allowed"))
  }

  #This hits and false positives should be changed to the simultaion
  # from the posterior predictive distributions
  if (missing(h)==TRUE)  {
    message("Since hits are missing, we use the data which is used to estimates the model parameter to run this function")
    h <-StanS4class@dataList$h
  }

  if (missing(f)==TRUE){
    message("Since false alarms are missing, we use the data which is used to estimates the model parameter to run this function")
    f <-StanS4class@dataList$f
  }

  NL <-StanS4class@dataList$NL
  NI <-StanS4class@dataList$NI


  fit <-methods::as(StanS4class, "stanfit")

  MCMC <- length(
    rstan::get_divergent_iterations(fit)
  ) # = cha*(ite-war)

  # MCMC=(ite-war)*cha
  chisquare <- vector()

  #--------- chi ^2 -----------Start
  p<-rstan::extract(fit)$p
  lambda<-rstan::extract(fit)$l


  ss <- array(0,dim = c(MCMC,C))
  tt <- array(0,dim = c(MCMC,C))
  for(cd in 1:C){
    ss[,cd]<-(h[C+1-cd]-NL*p[,cd])^2/(NL*p[,cd])
    if(!cd==C)tt[,cd]<-(f[C+1-cd]-NI*(lambda[,cd]-lambda[,cd+1]))^2/(NI*(lambda[,cd]-lambda[,cd+1]))
    if(cd==C)  tt[,cd]<-(f[C+1-cd]-NI*(lambda[,cd]- 0))^2/(NI*(lambda[,cd]- 0))

  }
  chi.Square.for.each.MCMC.samples <- apply(ss, 1,sum)+apply(ss,1, sum)
  chi.Square.for.each.MCMC.samples <- signif(chi.Square.for.each.MCMC.samples, digits = dig)
  return(chi.Square.for.each.MCMC.samples)
}








#'@title Get Samples from the Predictive Posterior Distribution (PPD).
#'@description
#'Get samples from the posterior predictive distribution.
#'@details
#'This methods to draw from the PPD is described in Gelman book, Bayesian Data Analysis. The aim of this function is to evaluate the chi square test statistics as a Bayesian sense. According to Gelman book, the chi square test need the samples from the PPD. So, we use this function to accomplish this task.
#'@inheritParams validation.dataset_srsc
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@param plot.replicated.points TRUE or FALSE. If true, then plot replicated points (hits, false alarms) by the scatter plot. This process will takes a long times. So if user has no time, then \code{FALSE} will help you.
#'
#' @return A list of datalists from the posterior predictive distribution
#' @export
#'
#' @examples
#'  \donttest{

# #####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'
#'
#'
#'  fit <- fit_Bayesian_FROC(
#'   ite  = 1111,
#'    summary = FALSE ,
#'   dataList = BayesianFROC::dataList.Chakra.1 )
#'
#'
#'
#'
#'
#'#=======  The first example ======================================================
#'  TPs.FPs <- get_samples_from_Posterior_Predictive_distribution(fit)
#'
#'
#'#=======  The Second Example: Short cut    ===========================================
#'# If user has no time, then  plot.replicated.points=FALSE will help you.
#'# By setting FALSE, the replicated data from the posterior predictive
#'# distribution does not draw, and hence the running time of function become shorter.
#'
#'  TPs.FPs <- get_samples_from_Posterior_Predictive_distribution(fit,
#'                                    plot.replicated.points =  FALSE)
#'
#'
#'
#'
#'

#' #      Close the graphic device to avoid errors in R CMD check.
#'
#'      grDevices::dev.new();plot(stats::runif(100),stats::runif(100))
#'

#'
#'
#'#================The third example:  From Hand made data to fitting  ==========

#'#  If user want to use the scatter plots of hits and false alarms from the posterior
#'#  predictive distribution for the submission, then the color plot is not appropriate.
#'#  So, by setting the argument Colour = FALSE, the scatter plot become black and white.
#'#  So, user can use this scatter plot for submission.
#'
#'
#'  get_samples_from_Posterior_Predictive_distribution(fit,Colour = FALSE)
#'
#'   g <-get_samples_from_Posterior_Predictive_distribution(fit)
#'
#'   x <- g$CFP
#'
#'   y <- g$CTP
#'
#'
#'  plot(   hexbin::hexbin(unlist(x),unlist(y))   )
#'
#'
#'
#' #      Close the graphic device to avoid errors in R CMD check.
#'
#'          Close_all_graphic_devices()

#'}# dottest

#'
#'
get_samples_from_Posterior_Predictive_distribution <-
  function(StanS4class,


           new.imaging.device=TRUE,
           upper_x,upper_y,
           Colour=TRUE,
           plot.replicated.points=TRUE
  ){
    main <-"Replicated FROC data from the posterior predictive distribuion."


    if(  StanS4class@studyDesign == "srsc.per.image"){       ModifiedPoisson <-  FALSE   }
    if(  StanS4class@studyDesign == "srsc.per.lesion"){      ModifiedPoisson <-  TRUE    }



    if (StanS4class@studyDesign=="MRMC")return(message("\n* srsc only. \n"))
    C <-StanS4class@dataList$C
    NL <-StanS4class@dataList$NL
    NI <-StanS4class@dataList$NI
    if(   StanS4class@studyDesign == "srsc.per.image"){  xlab<- "Replicated cumulative false alarms per image"   }
    if(   StanS4class@studyDesign == "srsc.per.lesion"){  xlab<- "Replicated cumulative false alarms per lesion"   }
    ylab<- "Replicated cumulative hits per lesion"

    fit <-methods::as(StanS4class, "stanfit")
    p<-rstan::extract(fit)$p
    z<-rstan::extract(fit)$z
    sd<-rstan::extract(fit)$v
    m<-rstan::extract(fit)$m

    lambda<-rstan::extract(fit)$l
    MCMC <- length( rstan::get_divergent_iterations(fit) ) # = cha*(ite-war)
    preCFPCTP <-list()
    CFP  <-list()
    CTP  <-list()

    list.of.dataList <- list()
    replicated.false.alarms.from.the.posterior.predictive.distribution  <- list()
    replicated.hits.from.the.posterior.predictive.distribution  <- list()

    for (mcmc in 1:MCMC) {
      list.of.dataList[[mcmc]] <-  hits_false_alarms_creator_from_thresholds(
        replicate.datset =1,
        ModifiedPoisson = ModifiedPoisson,
        mean.truth=m[mcmc], #not mu !!
        sd.truth=sd[mcmc],
        z.truth =z[mcmc,],
        NL=NL,
        NI=NI,
        summary=summary
      )

      replicated.false.alarms.from.the.posterior.predictive.distribution [[mcmc]] <- list.of.dataList[[mcmc]][[1]]$f
      replicated.hits.from.the.posterior.predictive.distribution [[mcmc]] <- list.of.dataList[[mcmc]][[1]]$h
      preCFPCTP<-c(
        cumsum( replicated.false.alarms.from.the.posterior.predictive.distribution [[mcmc]]),
        cumsum(  replicated.hits.from.the.posterior.predictive.distribution [[mcmc]])
      )

      if (ModifiedPoisson==TRUE) {


        CFP[[mcmc]] <-  cumsum( replicated.false.alarms.from.the.posterior.predictive.distribution [[mcmc]])/NL
        CTP[[mcmc]] <-   cumsum(  replicated.hits.from.the.posterior.predictive.distribution [[mcmc]])/NL

        CFP[[mcmc]] <-  give_name_srsc_CFP_CTP_vector( CFP[[mcmc]], CFP.or.CTP = "CFP",ModifiedPoisson = TRUE)
        CTP[[mcmc]] <-  give_name_srsc_CFP_CTP_vector( CTP[[mcmc]], CFP.or.CTP = "CTP",ModifiedPoisson = TRUE)



      }

      if (ModifiedPoisson==FALSE) {


        CFP[[mcmc]] <- cumsum( replicated.false.alarms.from.the.posterior.predictive.distribution [[mcmc]])/NI
        CTP[[mcmc]]  <- cumsum(  replicated.hits.from.the.posterior.predictive.distribution [[mcmc]])/NL


        CFP[[mcmc]] <-  give_name_srsc_CFP_CTP_vector( CFP[[mcmc]], CFP.or.CTP = "CFP",ModifiedPoisson = FALSE)
        CTP[[mcmc]] <-  give_name_srsc_CFP_CTP_vector( CTP[[mcmc]], CFP.or.CTP = "CTP",ModifiedPoisson = FALSE)
      }

      if (ModifiedPoisson==FALSE)    xlab = 'Replicated false positives per images from the posterior predictive distribution.'
      if (ModifiedPoisson==TRUE)    xlab = 'Replicated false positives per nodule from the posterior predictive distribution.'

      # suppressWarnings(graphics::par(new=TRUE));
      # plot(   CFP[[mcmc]],
      #         CTP[[mcmc]] ,
      #           pch =paste(mcmc),# paste(40) is dysplayed by 4 !! NOT 40 !! Caution !!
      #           xlim = c(0,3),
      #           ylim = c(0,1),
      #
      #           xlab = xlab,
      #           ylab = 'Replicated cumulative hit per nodule from the posterior predictive distribution.'
      #
      #           )
      #

    }#for mcmc


    black <- function(Colour) {


      if ( Colour == TRUE) {
        graphics::par(bg= "gray12", #"gray27",#"gray40",#"black",# ,
                      fg="gray",
                      col.lab="bisque2" ,#"bisque" ,#  "antiquewhite1",
                      col.axis="bisque2" ,##"bisque" "antiquewhite1",
                      col.main="bisque2" ,
                      cex.lab=1.5,
                      cex.axis=1.3
        )
      }


    }#function







    if (plot.replicated.points==TRUE) {

      message("\n*   Iteration for drawing a scatter plot of replicating data drawing from the posterior predictive distribution.\n\n")
      if (  new.imaging.device==TRUE) {  grDevices::dev.new()     }
      black(Colour)
      if(  missing(upper_x ))  upper_x <-max(unlist(CFP))
      if(  missing(upper_y ))    upper_y <-max(unlist(CTP))

      if ( Colour == TRUE) {
        graphics::par(bg= "gray12", #"gray27",#"gray40",#"black",# ,
                      fg="gray",
                      col.lab="bisque2" ,#"bisque" ,#  "antiquewhite1",
                      col.axis="bisque2" ,##"bisque" "antiquewhite1",
                      col.main="bisque2" ,
                      cex.lab=1.5,
                      cex.axis=1.3
        )
      }

      sss <-0 #dummy for process indicator
      message("/")# adjust for process indicating
      for (mcmc in 1:MCMC) {  # To plot the confidence level wise plot



        Divisor <-100  # for process indicating
        if(MCMC<100){ Divisor <- 1 }# for process indicating
        if(mcmc %% round(MCMC/Divisor)==0){# for process indicating
          sss <- sss +1# for process indicating

          if(sss%%10==0){  message("  [", sss,"% ] \n")}# for process indicating
          if(!sss==100){message("/")}# for process indicating
        }# for process indicating



        mat <- cbind( CFP[[mcmc]],# To plot the confidence level wise plot
                      CTP[[mcmc]])# To plot the confidence level wise plot
        for(cd in 1:dim(mat)[1]){# To plot the confidence level wise plot
          suppressWarnings(graphics::par(new=TRUE));
          plot(   mat[cd,1],# To plot the confidence level wise plot
                  mat[cd,2] ,# To plot the confidence level wise plot
                  pch =paste(cd),
                  cex=0.9,# Size of scatter plot
                  xlim = c(0,upper_x ),
                  ylim = c(0,upper_y),
                  main =main,
                  sub  ="Number is the confidence level.",
                  xlab = xlab,
                  ylab = 'Replicated cumulative hit per nodule from the posterior predictive distribution.'

          )}
      }


      message("/  [100 % ]    Finish !!   \n")# for process indicator

    }#if  plot.replicated.points==TRUE







    #Making a dataset to draw a counter plot
    x <-unlist(CFP)
    names(x)<-"CFPs from the posterior predictive distribution"
    y<- unlist(CTP)
    names(y)<-"CTPs from the posterior predictive distribution"
    group <-  rep( 1:C, length(CFP) )


    if (Colour ==TRUE) {

      grDevices::dev.new()
      black(Colour)
      plot(x,y, pch=20, cex=0.8, col=grDevices::rainbow(C+6)[group], xlab=xlab, ylab=ylab, main =main)


      grDevices::dev.new()
      black(Colour)
      plot(x,y, pch=20, col=grDevices::rainbow(C+6, alpha=0.2)[group], cex=0.8, xlab=xlab, ylab=ylab, main =main)
      car::dataEllipse(x,y,factor(group), levels=c(0.70,0.85,0.95),lwd=0.1,
                       plot.points=FALSE, col=grDevices::rainbow(C+6), group.labels=NA, center.pch=FALSE)

      grDevices::dev.new()
      black(Colour)

      plot(x,y,pch=NA, xlab=xlab, ylab=ylab, main =main)
      car::dataEllipse(x,y,factor(group), levels=seq(0.11,0.99,0.02),lwd=1,
                       plot.points=FALSE, col=grDevices::rainbow(C+6), group.labels=NA,
                       center.pch=FALSE, fill=TRUE, fill.alpha=0.05, lty=0)

      grDevices::dev.new()
      black(Colour)

      plot(x,y,pch=NA, xlab=xlab, ylab=ylab, main =main)
      car::dataEllipse(x,y,factor(group), levels=c(seq(0.15,0.95,0.2), 0.995),
                       plot.points=FALSE, col=grDevices::rainbow(C+6), group.labels=NA,
                       center.pch=FALSE, fill=TRUE, fill.alpha=0.15, lty=1, lwd=1)

    }# if Colour ==TRUE


    if (Colour ==FALSE) {

      # plot(hexbin::hexbin(x,y))



      all.black <- rep(1,C)#This is used to specify the color , all black. 1 means black.


      grDevices::dev.new()
      black(Colour)
      plot(x,y, pch=group, cex=0.5, col=all.black, xlab=xlab, ylab=ylab, main =main)


      grDevices::dev.new()
      black(Colour)
      plot(x,y, pch=20, col=all.black[group], cex=0.8, xlab=xlab, ylab=ylab, main =main)
      car::dataEllipse(x,y,factor(group), levels=c(0.70,0.85,0.95),lwd=0.1,
                       plot.points=FALSE, col=all.black, group.labels=NA, center.pch=FALSE)

      grDevices::dev.new()
      black(Colour)

      plot(x,y,pch=NA, xlab=xlab, ylab=ylab, main =main)
      car::dataEllipse(x,y,factor(group), levels=seq(0.11,0.99,0.02),lwd=1,
                       plot.points=FALSE, col=all.black, group.labels=NA,
                       center.pch=FALSE, fill=TRUE, fill.alpha=0.05, lty=0)

      grDevices::dev.new()
      black(Colour)

      plot(x,y,pch=NA, xlab=xlab, ylab=ylab, main =main)
      car::dataEllipse(x,y,factor(group), levels=c(seq(0.15,0.95,0.2), 0.995),
                       plot.points=FALSE, col=all.black, group.labels=NA,
                       center.pch=FALSE, fill=TRUE, fill.alpha=0.15, lty=1, lwd=1)


    }#if









    invisible(list(
      replicated.false.alarms.from.the.posterior.predictive.distribution=replicated.false.alarms.from.the.posterior.predictive.distribution,
      replicated.hits.from.the.posterior.predictive.distribution=replicated.hits.from.the.posterior.predictive.distribution,
      CFP=CFP,#Cumulative False Positives
      CTP=CTP, #Cumulatiove True Positives

      x=x, #This is an unlist of CFP
      y=y,# This is an unlist of CTP
      group=group # grouping for x,y to indicate the confidence levels

    )

    )


  }#function











#'@title Hits and False Alarms Creator
#'
#'@description From the parameter of the bi-normal assumptions, hits and false alarms are generated.
#'@details
#'From the fixed parameters of bi-normal assumptions, we replicate data, that is, we draw the data from the distributions whose parameters are known. Especially, we interest the hits and false alarms since the number of images, lesions and confidence level is same for all replications. So, it is sufficient to check the hits and false alarms.
#'@inheritParams validation.dataset_srsc
#'@param  initial.seed Replicated datasets are created using a continuous sequence of seeds and its initial seed is specified by this argument. For example, if you choose initial.seed =12300, then the replicated datasets are created from using  the sequence of seeds: 12301,12302,12303,12304,…
#' @return Datasets Including Hits and False Alarms
#' @export
#'
#' @examples
#'
#'  \donttest{

#'#================The first example======================================
#'#      Replication of Data from Fixed ( specified) Parameters.
#'
#'  a <- hits_false_alarms_creator_from_thresholds(replicate.datset = 1)
#'
#'#  Extract the first replicated dataset:
#'
#'  a[[1]]$NL
#'  a[[1]]$NI
#'  a[[1]]$f
#'  a[[1]]$h
#'  a[[1]]$C
#'
#'
#'#================The second example======================================
#'#      Replication of Data from Fixed ( specified) Parameters.
#'
#'  b <- hits_false_alarms_creator_from_thresholds(replicate.datset = 2)
#'
#'
#'#  Extract the first replicated dataset:
#'
#'  b[[1]]$NL
#'  b[[1]]$NI
#'  b[[1]]$f
#'  b[[1]]$h
#'  b[[1]]$C
#'
#'
#'#  Extract the second replicated dataset:
#'
#'  b[[2]]$NL
#'  b[[2]]$NI
#'  b[[2]]$f
#'  b[[2]]$h
#'  b[[2]]$C
#'
#'
#'#================The Third example======================================
#'#      Replication of Data from Fixed ( specified) Parameters.
#'
#'
#'  c <- hits_false_alarms_creator_from_thresholds(replicate.datset = 3)
#'
#'
#'#  Extract the first replicated dataset:
#'
#'  c[[1]]$NL
#'  c[[1]]$NI
#'  c[[1]]$f
#'  c[[1]]$h
#'  c[[1]]$C
#'
#'
#'#  Extract the second replicated dataset:
#'
#'  c[[2]]$NL
#'  c[[2]]$NI
#'  c[[2]]$f
#'  c[[2]]$h
#'  c[[2]]$C
#'
#'#  Extract the third replicated dataset:
#'
#'  c[[3]]$NL
#'  c[[3]]$NI
#'  c[[3]]$f
#'  c[[3]]$h
#'  c[[3]]$C
#'
#'
#'
#'}# dottest

#'
hits_false_alarms_creator_from_thresholds <-function(
  replicate.datset =3,
  ModifiedPoisson = FALSE,
  mean.truth=0.6,
  sd.truth=5.3,
  z.truth =c(-0.8,0.7,2.38), #<  <  <
  NL=259,
  NI=57,
  summary=TRUE,
  initial.seed =12345

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
  # name.vector.of.dataList <- vector()
  # for (cd in 1:C) {
  #   name.vector.of.dataList[cd] <- paste("f[",C-cd+1,"]",sep = "")
  # }
  # for (cd in 1:C) {
  #   name.vector.of.dataList[C+cd] <- paste("h[",C-cd+1,"]",sep = "")
  # }
  # name.vector.of.dataList[2*C+1] <- "NL"
  # name.vector.of.dataList[2*C+2] <- "NI"



  #
  ################################
  #
  #  ######   #########
  #  #        #        #
  #  ######   #      #
  #  #        ######
  #  #        #     #
  #  #        #      # #
  #  false Rate
  #################################


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
          set.seed(seed = initial.seed + seed);
          f.inv[cd]  <- stats::rpois(n= 1, lambda = (l.truth[cd]-0)*NI )
        }else{
          set.seed(seed = initial.seed + seed);
          f.inv[cd]  <- stats::rpois(n= 1, lambda = (l.truth[cd]-l.truth[cd+1])*NI )
        }#else
      }# if  ModifiedPoisson==F




      if(ModifiedPoisson==T){
        if (cd==C) {
          set.seed(seed = initial.seed + seed);
          f.inv[cd]  <- stats::rpois(n= 1, lambda = (l.truth[cd]-0)*NL )
        }else{
          set.seed(seed = initial.seed + seed);
          f.inv[cd]  <- stats::rpois(n= 1, lambda = (l.truth[cd]-l.truth[cd+1])*NL)
        }#else      set.seed(seed =  seed); hits <-   stats::rbinom(n=1,size = NL,prob = p.truth[cd])
      }#  if ModifiedPoisson==T

      set.seed(seed = initial.seed + seed); h.inv[cd] <-   stats::rbinom(n=1,size = NL,prob = p.truth[cd])


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

    list.of.dataList[[seed]] <-give_name_srsc_data(list.of.dataList[[seed]] )
    # a<-append( list.of.dataList[[seed]]$f, list.of.dataList[[seed]]$h);
    # b <-append( list.of.dataList[[seed]]$NL, list.of.dataList[[seed]]$NI);
    # vector.of.dataList[[seed]] <-append(a,b)
    # # browser()
    #
    # names(vector.of.dataList[[seed]] )  <-  name.vector.of.dataList
  }#for seed


  #-------------------------------
  #
  #  * Datasets were  created !!


  return(list.of.dataList)

}#function
