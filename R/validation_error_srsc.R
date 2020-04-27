#'@title Error between a give parameter and estimates for the parameters
#'@description Let us denote a  model parameter by \eqn{\theta_0}
#' \eqn{N_I} by a number of
#' images  and number
#'  of lesions  by \eqn{N_L} which are specified
#'   by user as the variables of the function.
#'
#' \describe{
#' \item{ \strong{(I)} Replicates models for \eqn{D_1,D_2,...,D_k,...,D_K}.        }{          }
#' \item{ Draw a dataset \eqn{D_k}  from a likelihood (model), namely              }{ \eqn{D_k ~ likelihood(|\theta_0)}.                                                                  }
#' \item{ Draw a MCMC samples  \eqn{\{ \theta_i (D_k)\}} from a posterior, namely  }{ \eqn{ \theta _i ~ \pi(|D_k)}.                                                                       }
#' \item{ Calculate  a posterior mean,  namely                                     }{ \eqn{ \bar{\theta}(D_k) := \sum_i \theta_i(D_k) }.                                                  }
#' \item{ Calculates error for  \eqn{D_k}                                          }{ \eqn{\epsilon_k}:=Trueth - posterior mean estimates of  \eqn{D_k} =  \eqn{|\theta_0   - \bar{\theta}(D_k)|} (or  =  \eqn{\theta_0   - \bar{\theta}(D_k)}, accordinly by the user specified \code{absolute.errors} ).                       }
#' \item{ \strong{(II)} Calculates mean of errors                                  }{ mean of errors  \eqn{ \bar{\epsilon}(\theta_0,N_I,N_L)}=  \eqn{ \frac{1}{K} \sum     \epsilon_k }   }
#' }
#'
#'  Running this function, we can see that the error  \eqn{ \bar{\epsilon}(\theta_0,N_I,N_L)} decreases
#'  monotonically as a given number of images \eqn{N_I} or a given number of lesions \eqn{N_L} increases.
#'
#'  Also, the scale of error also will be found. Thus this function can show how our estimates are correct.
#' Scale of error differs for each componenet of model parameters.
#'
#'
#'
#' Revised 2019  August 28
#'
#'
#'
#'
#'
#'
#'
#'@return Return values is,
#'
#' \describe{
#' \item{ Stanfit objects           }{  for each Replicated datasets   }
#' \item{ Errors                    }{ EAPs minus true values, in the above notations, it is \eqn{ \bar{\epsilon}(\theta_0,N_I,N_L)}    }
#' \item{  Variances of estimators. }{ This calculates the vaiance of posterior means over all replicated datasets   }
#' }
#'








#'@param mean.truth This is a parameter
#'of the latent Gaussian assumption
#'for the noise distribution.
#'@param sd.truth This is a parameter of the latent
#' Gaussian assumption for the noise distribution.
#'@param z.truth This is a parameter of the
#' latent Gaussian assumption for the noise distribution.
#'@param NL Number of Lesions.
#'@param NI Number of Images.
#'@param replicate.datset A Number indicate
#'that how many you replicate dataset
#' from user's specified dataset.
#'@param serial.number An positive integer
#'or Character. This is for programming perspective.
#' The author use this to print the serial
#'  numbre of validation. This will be used
#'  in the validation function.
#'@param base_size An numeric for size of object, this is for the package developer.
#'@param absolute.errors A logical specifying whether  mean of errors is defined by
#'
#' \describe{
#' \item{     \code{TRUE}                           }{ \eqn{ \bar{\epsilon}(\theta_0,N_I,N_L)}=  \eqn{ \frac{1}{K} \sum     | \epsilon_k | }   }
#' \item{     \code{FALSE}                          }{ \eqn{ \bar{\epsilon}(\theta_0,N_I,N_L)}=  \eqn{ \frac{1}{K} \sum       \epsilon_k   }   }
#'  }
#'
#'
#'
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@examples
#' \dontrun{
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####

#'#===========================    The first example  ======================================
#'
#'
#'#   It is sufficient to run the function with default variable
#'
#'    datasets <- validation.dataset_srsc()
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#=============================  The second example ======================================
#'
#'#   If user do not familiar with the values of thresholds, then
#'#   it would be better to use the actual estimated values
#'#    as an example of true parameters. In the following,
#'#     I explain this.
#'
#'# First, to get estimates, we run the following:
#'
#'   fit <- fit_Bayesian_FROC(dataList.Chakra.1,ite = 1111,summary =FALSE,cha=3)
#'
#'
#'
#'
#'
#'
#'#  Secondly, extract the expected a posterior estimators (EAPs) from the object fit
#'
#'
#'   z <- rstan::get_posterior_mean(fit,par=c("z"))[,"mean-all chains"]
#'
#'
#'
#'
#'
#'#  Thirdly we use this z as a true values.
#'
#'
#'    datasets <- validation.dataset_srsc(z.truth = z)
#'
#'
#'
#'#========================================================================================
#'#            1)             extract replicated fitted model object
#'#========================================================================================
#'
#'
#'     # Replicates models
#'
#'     a <- validation.dataset_srsc(replicate.datset = 3,ite = 111)
#'
#'
#'
#'     # Check convergence, in the above MCMC iterations = 111 which is too small to get
#'     # a convergence MCMC chain, and thus the following example will the example
#'     # of a non-convergent model in the r hat criteria.
#'
#'     ConfirmConvergence( a$fit[[3]])
#'
#'
#'     # Check trace plot to confirm whether MCMC chain do converge or not.
#'
#'     stan_trace( a$fit[[3]],pars = "A")
#'
#'
#'    # Check p value

#'     ppp( a$fit[[3]])
#'
#'
#'
#'     # In the above example, the posterior predictive p value is enough large,
#'     # but the model did not converge in R that criteria, which will cause
#'     # that the model does not fit to data. However p value is said
#'     # we can not reject the null hypothesis that the model does fit.
#'     # The author think this contradiction cause that the
#'     # number of MCMC iterations are too small which leads us to incorrect
#'     # Monte Carlo integral for p value. Thu p value is not correct.
#'     # Calculation of p value relies on the law of large number and thus
#'     # to obtain reliable posterior predictive p value, we need enough large
#'     # MCMC samples. 2019 August 29
#'
#'
#'
#'
#'
#'
#'                                           # Revised in 2019 August 29
#'
#'
#'
#'
#'
#'#========================================================================================
#'#            1)            Histogram of error of postrior means for replicated datasets
#'#========================================================================================
#'#'
#'
#'   a<-   validation.dataset_srsc(replicate.datset = 100)
#'   hist(a$error.of.AUC,breaks = 111)
#'   hist(a$error.of.AUC,breaks = 30)
#'
#'
#'
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                             absolute.errors = FALSE generates negative biases
#'#========================================================================================
#'
#'
#'  validation.dataset_srsc(absolute.errors = FALSE)
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                             absolute.errors = TRUE dose not generate negative biases
#'#========================================================================================
#'
#'
#'  validation.dataset_srsc(absolute.errors = TRUE)
#'
#'
#'}# dontrun

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
  cha =1,
  summary=TRUE,
  serial.number=1,
  base_size=0,
  absolute.errors = TRUE
  # ,  convergence.model.only = FALSE
){
  C <- length(z.truth)
  c<-C:1
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


# here is hitrate modified -----
    deno <-vector()
    hit_rate.truth <- vector()
    deno[C-1] = 1-p.truth[C];
    for(cd in 3:C){ deno[c[cd]] = deno[c[cd-1]]-p.truth[c[cd-1]];  }
    hit_rate.truth[C] = p.truth[C];
    for(cd in 1:C-1) hit_rate.truth[cd] = p.truth[cd]/deno[cd];








    s <-0# 2019 Sept 30. This is a key idea
    c <-C:1
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

                  # set.seed(seed =  seed); h.inv[cd] <-   stats::rbinom(n=1,
                  #                                                      size = NL-s, # 2019 Sept 30. This is a key idea    # In order to avoid the sum of hits may be greater than NL
                  #                                                      prob = p.truth[cd])
                  # s <- s + h.inv[cd] # 2019 Sept 30. This is a key idea    # In order to avoid the sum of hits may be greater than NL
                  #
                  #
      set.seed(seed =  seed); h[cd] <-   stats::rbinom(n=1,
                                                           size = NL-s, # 2019 Sept 30. This is a key idea    # In order to avoid the sum of hits may be greater than NL
                                                           prob = hit_rate.truth[c[cd]])
      s <- s + h[cd] # 2019 Sept 30. This is a key idea    # In order to avoid the sum of hits may be greater than NL






                  f[C-cd+1] <- f.inv[cd]
                  # h[C-cd+1] <- h.inv[cd]

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


  #Here
  #
  #  * Datasets were  created !!
  #
  #  * Next Fitting !!
  #
  #Here

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

  # base_size <-0

  for (seed in 1:replicate.datset ) {
    #It seems better to make a indicator and
    #do not print the result of rstan
    # Here fitting  ------

    fit[[seed]]   <- fit_Bayesian_FROC(
      dataList=list.of.dataList[[seed]],
      ite = ite,
      cha = cha,
      new.imaging.device = FALSE,
      summary = FALSE,
      ModifiedPoisson = ModifiedPoisson,
      DrawCurve = FALSE
    )


    base_size <- base_size + size_of_return_value(fit[[seed]] ,is_return_value = FALSE,summary = FALSE)
# here col of size ----
    print(size_of_return_value(fit[[seed]] ,is_return_value = FALSE, base_size =base_size,col=TRUE ))


# view data ----
    print(viewdata(list.of.dataList[[seed]]))

    message("\n----------------------------------------\n",sep = "")
    message("\n* ",serial.number, " -th validation")
    message("\n* The (",seed," / ", replicate.datset, " ) -th fitting finished.\n",sep = "")
    message("\n----------------------------------------\n",sep = "")

    # Here   ------

    if ( fit[[seed]]@convergence ==TRUE) {
      s <-s+1

      convergent.dataList[[s]] <-fit[[seed]]@dataList

      a<-append( convergent.dataList[[s]]$f, convergent.dataList[[s]]$h);
      b <-append( convergent.dataList[[s]]$NL, convergent.dataList[[s]]$NI);
      convergent.dataList.as.dataframe[,s] <-append(a,b)
      # browser()





#ssss -----
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

  number.of.convergent.model <-s#Here
  message("\n  ----- Comments for Validation -----\n")
  message("\n* Number of all replicated models:",replicate.datset,"\n")
  message("\n* Number of convergent models:",s,"\n")
  message("\n* Convergence rate:", round(   (s/replicate.datset)*100 , digits = 2),"% \n")
  a.truth <- mean.truth/sd.truth
  b.truth  <- 1/sd.truth
  AUC.truth <- stats::pnorm(a.truth/ sqrt( 1+b.truth^2))

  l.truth <-l.truth[1:C]


  # here   error.of.AUC------
                        error.of.AUC <- validation.of.AUC.EAP  -  AUC.truth
  if (absolute.errors)  error.of.AUC <- abs(validation.of.AUC.EAP  -  AUC.truth)






  l1 <-validation.of.z.Threshold.EAP
  # l2 <-rep( list(z.truth) , length(unlist( validation.of.z.Threshold.EAP))/length(z.truth)  )
  l2 <-rep( list(z.truth) , number.of.convergent.model)
  # here   error.of.z.Threshold.EAP------
  # browser()

  error.of.z.Threshold.EAP     <-  Map( `-`,l1  , l2 )
  if (absolute.errors)   error.of.z.Threshold.EAP     <-   lapply( Map( `-`,l1  , l2 ),abs)

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
# error ----
  # browser()
                         error.of.dz.Threshold.EAP     <-  Map( `-`,l1  , l2 )
  if (absolute.errors)   error.of.dz.Threshold.EAP     <-   lapply( Map( `-`,l1  , l2 ),abs)

  mean.error.of.dz.Threshold.EAP<- rep(NaN, length(error.of.dz.Threshold.EAP[[1]]))
  sd.error.of.dz.Threshold.EAP<- rep(NaN, length(error.of.dz.Threshold.EAP[[1]]))
  name.dz.Threshold.EAP<- rep(NaN, length(error.of.dz.Threshold.EAP[[1]]))

  for(i.th.column in 1: length(error.of.dz.Threshold.EAP[[1]])  ){
    mean.error.of.dz.Threshold.EAP[i.th.column]<-mean(as.numeric(lapply(error.of.dz.Threshold.EAP,"[",n=i.th.column)))
    # sd.error.of.z.Threshold.EAP[i.th.column]<-stats::sd(as.numeric(lapply(error.of.z.Threshold.EAP,"[",n=i.th.column)))
    sd.error.of.dz.Threshold.EAP[i.th.column]<-stats::sd(as.numeric(lapply(validation.of.dz.Threshold.EAP,"[",n=i.th.column)))
    name.dz.Threshold.EAP[i.th.column]<- paste("dz[", i.th.column, "]")

  }



  # error ----
# browser()
  if (absolute.errors)   error.of.mean.of.latent.EAP  <-  abs(  validation.of.mean.of.latent.EAP -  mean.truth)
                         error.of.mean.of.latent.EAP  <-    validation.of.mean.of.latent.EAP -  mean.truth

   if (absolute.errors)  error.of.Standard.Deviation.latent.EAP  <-abs(   validation.of.Standard.Deviation.latent.EAP -sd.truth)
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
  if (absolute.errors)   error.of.p.Hitrate.EAP     <-  lapply( Map( `-`,l1  , l2 ),abs)

  mean.error.p.Hitrate.EAP<- rep(NaN, length(error.of.p.Hitrate.EAP[[1]]))
  sd.error.p.Hitrate.EAP<- rep(NaN, length(error.of.p.Hitrate.EAP[[1]]))
  name.p.Hitrate.EAP<- rep(NaN, length(error.of.p.Hitrate.EAP[[1]]))

  for(i.th.column in 1: length(error.of.p.Hitrate.EAP[[1]])  ){
    # mean ----

    mean.error.p.Hitrate.EAP[i.th.column] <- mean(as.numeric(lapply(error.of.p.Hitrate.EAP,"[",n=i.th.column)))
    sd.error.p.Hitrate.EAP[i.th.column]   <- stats::sd(as.numeric(lapply(error.of.p.Hitrate.EAP,"[",n=i.th.column)))
    name.p.Hitrate.EAP[i.th.column]       <- paste("p[", i.th.column, "]")

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
  # error ----
                         error.of.l.FalseRate.EAP <-  Map( `-`,l1  , l2 )
  if (absolute.errors)   error.of.l.FalseRate.EAP     <-  lapply( Map( `-`,l1  , l2 ),abs)

  # mean ----
  mean.error.l.FalseRate.EAP<- rep(NaN, length(error.of.l.FalseRate.EAP[[1]]))
  sd.error.l.FalseRate.EAP<- rep(NaN, length(error.of.l.FalseRate.EAP[[1]]))
  name.l.FalseRate.EAP<- rep(NaN, length(error.of.l.FalseRate.EAP[[1]]))
  for(i.th.column in 1: length(error.of.l.FalseRate.EAP[[1]])  ){
    # mean ----

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
  # error ----

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



  # error ----

  error.of.mean.of.latent.EAP             <-   validation.of.mean.of.latent.EAP -  mean.truth
  error.of.Standard.Deviation.latent.EAP  <-   validation.of.Standard.Deviation.latent.EAP -sd.truth








 return_value<- list(
    results=results,
    Name.of.all.param = Name.of.all.param,
    Bias.of.all.param = Bias.of.all.param,
    S.E.of.all.param  = S.E.of.all.param,


    vector.of.dataList=vector.of.dataList,

    convergent.dataList = convergent.dataList,
    convergent.dataList.as.dataframe = convergent.dataList.as.dataframe,

############################################################# Convergence number and all (non conv and conv) number
    number.of.convergent.model =number.of.convergent.model,
    replicate.datset=replicate.datset,
#Here

    fit=fit,
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



# here errors ------
    error.of.AUC=error.of.AUC,
    error.of.z.Threshold.EAP    = error.of.z.Threshold.EAP,
    error.of.mean.of.latent.EAP  =error.of.mean.of.latent.EAP,
    error.of.Standard.Deviation.latent.EAP =error.of.Standard.Deviation.latent.EAP,
    error.of.p.Hitrate.EAP   =error.of.p.Hitrate.EAP,
    error.of.l.FalseRate.EAP =error.of.l.FalseRate.EAP,



    dataList=list.of.dataList
  )


 invisible( return_value  )


}#function










#'@title Validation via replicated datasets from a model at a given model parameter
#'@description
#'  Print for a given true parameter,
#'   a errors of estimates from replicated dataset.
#'
#'   Also print a standard error which is the variance  of estimates.
#'
#'
#'
#' Suppose that \eqn{\theta_0} is a given true model parameter with a given number of images \eqn{N_I} and a given number of lesions \eqn{N_L}, specified by user.
#' \describe{
#' \item{  \strong{(I)}  }{}
#' \item{  \strong{(I.1)}   Synthesize a collection of dataset \eqn{D_k} (\eqn{k=1,2,...,K})  from a likelihood (model) at a given parameter \eqn{\theta_0}, namely  }{    \eqn{D_k  \sim}  likelihood( \eqn{\theta_0}).                                                                                 }
#' \item{   \strong{(I.2)}  Replicates \eqn{K}   models fitted to each dataset  \eqn{D_k} (\eqn{k=1,2,...,K}), namely, draw  MCMC samples  \eqn{\{ \theta_i (D_k);i=1,...,I\}} from each posterior of the dataset  \eqn{D_k}, namely         }{   \eqn{ \theta _i(D_k)} ~ \eqn{ \pi(|D_k)}.                                                                                       }
#' \item{   \strong{(I.3)}  Calculate  posterior means for the set of data \eqn{D_k} (\eqn{k=1,2,...,K}),  namely                                                     }{    \eqn{ \bar{\theta}(D_k) := \frac{1}{I} \sum_i \theta_i(D_k) }.                                                     }
#' \item{ \strong{(I.4)} Calculates error for each dataset \eqn{D_k}                                                 }{ \eqn{\epsilon_k}:=Trueth - estimates =  \eqn{\theta_0   - \bar{\theta}(D_k)}.                                          }
#' \item{ \strong{(II)} Calculates mean of errors over all datasets \eqn{D_k} (\eqn{k=1,2,...,K})                                            }{ mean of errors  \eqn{ \bar{\epsilon}(\theta_0,N_I,N_L)}=  \eqn{ \frac{1}{K} \sum     \epsilon_k }.                    }
#' \item{  NOTE                                                                                                      }{ We note that if a fitted model does not converge,( namely R hat is far from one), then it is omiited from this calculation.}
#' \item{ \strong{(III) } Calculates mean of errors for various number of lesions and images                         }{ mean of errors  \eqn{ \bar{\epsilon}(\theta_0,N_I,N_L)}                                                               }
#' }
#'
#' For example, if  \eqn{(N_I^1,N_L^1),(N_I^2,N_L^2),(N_I^3,N_L^3),...,(N_I^m,N_L^m)}, then
#' \eqn{ \bar{\epsilon}(\theta_0,N_I^1,N_L^1)},
#' \eqn{ \bar{\epsilon}(\theta_0,N_I^2,N_L^2)},
#' \eqn{ \bar{\epsilon}(\theta_0,N_I^3,N_L^3)},...,
#' \eqn{ \bar{\epsilon}(\theta_0,N_I^m,N_L^m)} are calculated.
#'
#' To obtain precise error,
#' The number of replicated fitted models (denoted by \eqn{K}) should be large enough.
#' If \eqn{K} is small, then it causes a bias.
#' \eqn{K} = \code{replicate.datset}: a variable of the function \code{error_srsc}.
#'
#'
#'
#'
#'
#'
#'
#'
#describe

#'
#'
#'  Running this function, we can see that the error
#'   \eqn{ \bar{\epsilon}(\theta_0,N_I,N_L)} decreases
#'  monotonically as a given number of images \eqn{N_I}
#'   or a given number of lesions \eqn{N_L} increases.
#'
#'  Also, the scale of error also will be found. Thus this function can show how our estimates are correct.
#' Scale of error differs for each componenet of model parameters.
#'
#'
#'
#' Revised 2019  August 28
#'
#'
#'
#' @details
#'
#'
#' In Bayesian inference,
#' if sample size is large,
#'  then posterior tends to the Dirac measure.
#'  So, the error and variance of estimates
#'  should be tends to zero as sample size tends to infinity.
#'
#'  This function check this phenomenen.
#'
#'  If model has problem, then it contains some non-decreasing vias
#'  with respect to sample size.
#'
#'   Revised 2019  Nov 1

#'
#'
#'
#' Provides a reliability of our posterior mean estimates.
#' Using this function, we can
#' find what digit makes sence.
#'
#' In the real world, the data for modality
#'  comparison or observer performan evaluation is
#' 100 images or 200 images. In such scale data,
#'  any estimate of AUC will contain error at most 0.0113....
#' So, the value of AUC should round in 0.XXX
#' and not 0.XXXX or 0.XXXXX or more. Since
#' error is 0.00113... and hence 4 digit
#'  or more digit is meaningless. In such manner,
#' we can analyize the errors.
#'
#'We note that if we increase the
#'number of images or lesinons,
#' the errors decrease.
#'
#' For example, if we  use 20000 images in FROC trial,
#'  then the error of AUC will be 0.0005... and thus,
#' and so on. Thus large number of images gives
#' us more reliable AUC. However the radiologist
#' cannot read such large (20000) images.
#'
#'  Thus, the error will be 0.00113...
#'
#'
#'   If the number of images are given before hand and moreover if we obtains the estimates,
#'   then we can run this function using these two, we can find the estimated errors by simulation.
#'   Of course, the esimates is not the truth, but roughly speaking, if we assume that
#'   the estimates is not so far from truth, and the error analysis is rigid with respect to
#'   changing the truth, then we can say using estimates as truth, the result of this error analysis can be regarded as an
#'   actual error.
#'
#'
#' I want to go home. Unfortunatly, my house is ...
#'
#'
#'
#'@return Replicated datasets, estimates,
#' errors,...etc I made this program 1 years ago?
#'  and now I forget ... the precise return values.
#'When I see today, 2019 August. It retains too many return
#'values to explain all of them.
#@param ----
#'@param NLvector A vector of positive integers,
#' indicating a collection of numbers of Lesions.
#@param NIvector  Number of Images.
#'@param ratio  A positive \strong{\emph{rational}} number,
#' with which Number of Images is determined by the formula:
#'  (number of images) = \code{ratio} times (numbser of lesions).
#' Note that in calculation, it  rounds   \code{ratio * NLvector } to an integer.
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@inheritParams validation.dataset_srsc
#'@examples
#' \dontrun{
#'#========================================================================================
#'#            0)            0-th example
#'#========================================================================================
#'
#'
#'    datasets <-error_srsc(
#'                NLvector = c(100,10000,1000000),
#'                ite = 2222
#'                )
#'
#'
#'  # By the following, we can extract only datasets whose
#'  # model has converged.
#'    datasets$convergent.dataList.as.dataframe
#'
#'
#'
#'
#'#========================================================================================
#'#            1)            1-st example
#'#========================================================================================
#'# Long width  is required in  R console.
#'
#'
#'
#' datasets <-error_srsc(NLvector = c(
#'   50L,
#'   111L,
#'   11111L
#'   ),
#'   # NIvector,
#'   ratio=2,
#'   replicate.datset =3,
#'   ModifiedPoisson = FALSE,
#'   mean.truth=0.6,
#'   sd.truth=5.3,
#'   z.truth =c(-0.8,0.7,2.38),
#'   ite =2222
#' )
#'
#'
#'
#'#========================================================================================
#'#            2)             Plot the error of AUC with respect to  NI
#'#========================================================================================

#'
#'
#' a <-error_srsc(NLvector = c(
#'   33L,
#'   50L,
#'   111L,
#'   11111L
#'   ),
#'   # NIvector,
#'   ratio=2,
#'   replicate.datset =3,
#'   ModifiedPoisson = FALSE,
#'   mean.truth=0.6,
#'   sd.truth=5.3,
#'   z.truth =c(-0.8,0.7,2.38),
#'   ite =2222
#' )
#'
#'
#'
#'

#'
#'
#'       aa <- a$Bias.for.various.NL
#'
#'
#'       error.of.AUC <-  aa[8,]
#'       y <- subset(aa[8,], select  = 2:length(aa[8,]))
#'       y <- as.numeric(y)
#'       y <- abs(y)
#'       upper_y <- max(y)
#'       lower_y <- min(y)
#'
#'       x <- 1:length(y)
#'
#'
#'
#'       plot(x,y, ylim=c(lower_y, upper_y))
#'
#'#  From this plot, we cannot see whether the error has decreased or not.
#'#  Thus, we replot with the log y-axis, the we will see that the error
#'#  has decreased with respect to number of images and lesions.
#'
#'       ggplot(data.frame(x=x,y=y), aes(x = x, y = y)) +
#'            geom_line() +
#'            geom_point() +
#'            scale_y_log10()
#'
#'# Revised 2019 Sept 25
#'
#'
#' # General print of log scale
#' df<-data.frame(x=c(10,100,1000,10,100,1000),
#'                y=c(1100,220000,33000000,1300,240000,36000000),
#'                group=c("1","1","1","2","2","2")
#' )
#'
#' ggplot2::ggplot(df, aes(x = x, y = y, shape = group)) +
#'   ggplot2::geom_line(position = position_dodge(0.2)) +           # Dodge lines by 0.2
#'   ggplot2::geom_point(position = position_dodge(0.2), size = 4)+  # Dodge points by 0.2
#'   ggplot2::scale_y_log10()+
#'   ggplot2::scale_x_log10()

#'
#'
#'#========================================================================================
#'#    2)   Add other param into plot plain of the error of AUC with respect to  NI
#'#========================================================================================
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' a <-error_srsc(NLvector = c(
#'   111L,
#'   11111L
#'   ),
#'   # NIvector,
#'   ratio=2,
#'   replicate.datset =3,
#'   ModifiedPoisson = FALSE,
#'   mean.truth=0.6,
#'   sd.truth=5.3,
#'   z.truth =c(-0.8,0.7,2.38),
#'   ite =2222
#' )
#'       aa <- a$Bias.for.various.NL
#'
#'
#'       error.of.AUC <-  aa[8,]
#'       y1 <- subset(aa[8,], select  = 2:length(aa[8,]))
#'       y1 <- as.numeric(y1)
#'       y1 <- abs(y1)

#'
#'       LLL <-length(y1)
#'
#'       y2 <- subset(aa[7,], select  = 2:length(aa[7,]))
#'       y2 <- as.numeric(y2)
#'       y2 <- abs(y2)

#'
#'       y <- c(y1,y2)
#'
#'
#'       upper_y <- max(y)
#'       lower_y <- min(y)
#'
#'     group <- rep(seq(1,2,1),1 , each=LLL)
#'     x <-  rep(seq(1,LLL,1),2 , each=1)
#'     group <-  as.character(group)
#'    df <-  data.frame(x=x,y=y,group=group)
#'
#'
#'                 ggplot2::ggplot(df, aes(x = x, y = y, shape = group)) +
#' ggplot2::geom_line(position = position_dodge(0.2)) +           # Dodge lines by 0.2
#'   ggplot2::geom_point(position = position_dodge(0.2), size = 4)+  # Dodge points by 0.2
#'            ggplot2::scale_y_log10()
#'          # ggplot2::scale_x_log10()
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'#========================================================================================
#'#          Confidence level = 4
#'#========================================================================================
#'
#'
#'
#'
#'
#' datasets <-error_srsc(NLvector = c(
#'   111L,
#'   11111L
#'   ),
#'   # NIvector,
#'   ratio=2,
#'   replicate.datset =3,
#'   ModifiedPoisson = FALSE,
#'   mean.truth=-0.22,
#'   sd.truth=5.72,
#'   z.truth =c(-0.46,-0.20,0.30,1.16),
#'   ite =2222
#' )
#'
#'
#'
#'
#'
#'  error_srsc_variance_visualization(datasets)
#'
#'#  The parameter of model is 7 in which the ggplot2 fails with the following warning:
#'
#'
#'# The shape palette can deal with a maximum of 6 discrete values because more than 6
#'# becomes difficult to
#'# discriminate; you have 7. Consider specifying shapes manually if you must have them.
#'
#'
#'
#'
#'}# dontrun


#' @export

# OUt put can be made by TeX source.
error_srsc <-function(NLvector = c(
  100L,
  10000L,
  1000000L
  ),
  # NIvector,
  ratio=2,# NUmber of image is not so important, since when we consider ModifiedPoisson = TRUE
  replicate.datset =3,
  ModifiedPoisson = FALSE,
  mean.truth=0.6,
  sd.truth=5.3,
  z.truth =c(-0.8,0.7,2.38),
  ite =2222,
  cha =1


){
 C <- length(z.truth)


  d <- list()
  Bias.of.all.param <- list()
  S.E.of.all.param  <- list()
  Bias.of.all.param.with.NL.NI <- list()
  S.E.of.all.param.with.NL.NI <- list()
  Name.of.all.param.with.NL.NI <- vector()

  col.names_for_SE <- vector()
  col.names_for_SE[1] <-paste("Variance of estimates")


  col.names <- vector()
  col.names[1] <-paste("Error of estimates")

  convergent.dataList.as.dataframe <- list()

  number.of.convergent.model<- vector()
  number.of.replicate.datset<- vector()

  # col.names[1] <-paste("")
  s <- 0
  base_size <- 0
  for (nl in NLvector) {
    s<- s+1
    ni<- floor(ratio*nl)
    #Create the replicating datasets for different number of lesions
    # validation.dataset_srsc -----
    d[[s]]<-  validation.dataset_srsc(
      replicate.datset =replicate.datset,
      ModifiedPoisson = ModifiedPoisson,
      mean.truth=mean.truth,
      sd.truth=sd.truth,
      z.truth =z.truth,
      NL=ni,
      NI=nl,
      ite =ite,
      cha =cha,

      base_size = base_size ,

      summary=FALSE,
      serial.number= paste( "The number of Lesion = ",nl, " and The number of images",ni, ". \n\n* The (" , s, "/", length(NLvector), ")")
      )
    base_size <- base_size  +    size_of_return_value(d[[s]], is_return_value  = FALSE,base_size = base_size,summary = FALSE)


    # Here ------
    number.of.convergent.model[s] <- d[[s]]$number.of.convergent.model#Here
    number.of.replicate.datset[s] <- d[[s]]$replicate.datset#Here

    Bias.of.all.param[[s]] <-d[[s]]$Bias.of.all.param
    S.E.of.all.param[[s]]  <-d[[s]]$S.E.of.all.param

    Bias.of.all.param.with.NL.NI[[s]] <- append(append(append(append(ni,nl),
                                                               Bias.of.all.param[[s]]),
                                                       number.of.convergent.model[s]),
                                                       number.of.replicate.datset[s])


    S.E.of.all.param.with.NL.NI[[s]]  <- append(append(append(append(ni,nl),# 2019 Sept 6
                                                                S.E.of.all.param[[s]]),# 2019 Sept 6
                                                       number.of.convergent.model[s]),# 2019 Sept 6
                                                       number.of.replicate.datset[s])# 2019 Sept 6




    col.names[s+1] <-paste(s,"-th validation",sep = "")
    if(s==1)    col.names[s+1] <-paste(s,"-st validation",sep = "")
    if(s==2)    col.names[s+1] <-paste(s,"-nd validation",sep = "")
    if(s==3)    col.names[s+1] <-paste(s,"-rd validation",sep = "")

    col.names_for_SE[s+1] <-paste(s,"-th validation",sep = "")# 2019 Sept 6
    if(s==1)    col.names_for_SE[s+1] <-paste(s,"-st validation",sep = "")# 2019 Sept 6
    if(s==2)    col.names_for_SE[s+1] <-paste(s,"-nd validation",sep = "")# 2019 Sept 6
    if(s==3)    col.names_for_SE[s+1] <-paste(s,"-rd validation",sep = "")# 2019 Sept 6




    convergent.dataList.as.dataframe[[s]] <- d[[s]]$convergent.dataList.as.dataframe

  }# for  nl in NLvector

  list.names <- vector()
  for (sd in 1:s) {
    list.names [sd] <- paste(sd,"-th dataset of convergent model ",sep = "")
  }
  names(convergent.dataList.as.dataframe) <- list.names


  # Name.of.all.param.with.NL.NI<- append(append("Number of Images",
  #                                              "Number of Lesions"),
  #                                       d[[1]]$Name.of.all.param  )

  Name.of.all.param.with.NL.NI<- append( append(  append(append("Number of Images",
                                                                "Number of Lesions"),
                                                         d[[1]]$Name.of.all.param  ),

                                                  "number of convergent models"),#Here
                                         "number of replicated datsets")#Here
  # browser()

  l <-list(
    Name.of.all.param.with.NL.NI = Name.of.all.param.with.NL.NI,
    Bias.of.all.param.with.NL.NI = Bias.of.all.param.with.NL.NI
  )#list


  ll <-list(# 2019 Sept 6
    Name.of.all.param.with.NL.NI = Name.of.all.param.with.NL.NI,# 2019 Sept 6
    S.E.of.all.param.with.NL.NI = S.E.of.all.param.with.NL.NI# 2019 Sept 6
  )#list# 2019 Sept 6


  # browser()
  options(scipen=100)

  #Here, list l is converted, and each component of list become column vector!! This is very good!
  Bias.for.various.NL<-as.data.frame(l)
  S.E.for.various.NL<-as.data.frame(ll)# 2019 Sept 6




  names(Bias.for.various.NL) <- col.names
  names(S.E.for.various.NL)<- col.names_for_SE # 2019 Sept 6

  message(
    "
==================================================================

* Each column is independent.

* Each row means the mean of the difference of [ truth  - estimate ]

* If each number is small, then it means that each error is small.

* In my model, the most important quantity is  a parameter of AUC,
  whose error is the most important.


          ")

  message("\n* Variance of estimates, (Standard Error) \n")# 2019 Sept 6
  print(knitr::kable(S.E.for.various.NL) )# 2019 Sept 6


  message("\n* Error between estimates and specified true parameter \n")
  print(knitr::kable(Bias.for.various.NL) )





  return_value <-  list(
    Bias.for.various.NL  =Bias.for.various.NL,
    S.E.for.various.NL=S.E.for.various.NL,

    Name.of.all.param.with.NL.NI = Name.of.all.param.with.NL.NI,

    Bias.of.all.param.with.NL.NI = Bias.of.all.param.with.NL.NI,
    S.E.of.all.param.with.NL.NI  = S.E.of.all.param.with.NL.NI,
    C=C,
    length.of.NLvector = length(  NLvector),
    NLvector=NLvector,

    ratio=ratio,# NUmber of image is not so important, since when we consider ModifiedPoisson = TRUE
    replicate.datset =replicate.datset,
    ModifiedPoisson = ModifiedPoisson,
    mean.truth=mean.truth,
    sd.truth=sd.truth,
    z.truth =z.truth,
    Number.of.MCMC.iteration =ite,

    number.of.convergent.model =number.of.convergent.model,#Here
    number.of.replicate.datset =number.of.replicate.datset, #Here


    datasets =d,
    convergent.dataList.as.dataframe=convergent.dataList.as.dataframe

     )





  size_of_return_value(
    return_value

  )





  invisible(
    return_value

  )#invisible


}



# datasets$datasets[[2]]$dataList






















#' @title Visualization for Error of Estimator
#' @description
#' The function plot the graph of errors with respect to sample sizes.
#'
#'
#'\strong{\emph{Error plot}}
#'\describe{
#'\item{ \strong{\emph{x-axis}}    }{ Sample sizes  }
#'\item{ \strong{\emph{y-axis}}    }{ Error for each parameter   }
#'}
#'
#' @param return.value.of_error_srsc A return value
#'  of the function \code{\link{error_srsc}()}.
#' @param log_scale_x.axis A logical,
#' whether x axis is log scale or not.
#' @return A long format dataframe of
#'  error and its parameter name
#' @export
#' @seealso \link{error_srsc_variance_visualization}
#'
#' @examples
#'
#' # General plot
#'
#' df <- data.frame(x=runif(100),y=runif(100),g= as.factor(rep(1:5,10)))
#'
#' ggplot(df, aes(x = x, y = y, shape = g)) +
#'   geom_point(size = 3) +
#'   scale_shape_manual(values = c(1,2,3,4,5,6,7,8,9))
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' df <- data.frame(x=runif(100),y=runif(100),g= as.factor(rep(1:25,4)))
#'
#'   # Use slightly larger points and use custom values for the shape scale
#'
#'
#' ggplot(df, aes(x = x, y = y, shape = g)) +
#'   geom_point(size = 3) +
#'   scale_shape_manual(values = c(1,2,3,4,5,6,7,8,9,10,
#'                                 11,12,13,14,15,16,17,18,19,20,21,22,23,24,25))
#'
#' \dontrun{
#'  a <- error_srsc()
#'
#'  error_srsc_error_visualization(a)
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#              In case of C = 4, arbitrary C is available.
#'#========================================================================================
#'
#'   a <-error_srsc(NLvector = c(
#' 100,
#' 10000,
#' 1000000
#' ),
#' ratio=2,
#' replicate.datset =2,
#' ModifiedPoisson = FALSE,
#' mean.truth=0.6,
#' sd.truth=5.3,
#' z.truth =c(-0.8,0.7,2.38,3), # Here we use the C=4
#' ite =500
#' )
#'
#' error_srsc_error_visualization(a)
#' error_srsc_variance_visualization(a)
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#              In case of C = 7, arbitrary C is available.
#'#========================================================================================
#'
#'
#'
#'
#'
#'
#'
#' #'
#' a <-error_srsc(NLvector = c(
#'   100,
#'   10000,
#'   100000
#' ),
#' ratio=2,
#' replicate.datset =2,
#' ModifiedPoisson = FALSE,
#' mean.truth=0.6,
#' sd.truth=5.3,
#' z.truth =c(-0.8,0.7,2.38,3,3.4,3.6,3.8), # Here we use the C=7
#' ite =500
#' )
#'
#' error_srsc_error_visualization(a)
#' error_srsc_variance_visualization(a)
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' }
#'

error_srsc_error_visualization <- function(return.value.of_error_srsc,
                                  log_scale_x.axis = TRUE

                                  ){

  a<-return.value.of_error_srsc
  aa <- a$Bias.for.various.NL
  NLvector <- a$NLvector
  length.of.NLvector <- a$length.of.NLvector
  C <- a$C
  LLL <-length.of.NLvector

  y.list <- list()
  group.list <-list()
  for (cd in 1:(C+3)) {
    y.list[[cd]] <- vector()
    y.list[[cd]] <- abs(
        as.numeric(
          subset(aa[cd+2,], select  = 2:length(aa[cd+2,]))
                  )
                  )

    # group.list[[cd]] <- subset(aa[,1], subset  = 2:(C+2),select  =1 )
}
    for (cd in 1:C) {
      group.list[[cd]] <- vector()

        for (nnn in 1:LLL) {
      group.list[[cd]][nnn] <- paste("z[",cd,"]")
    }
    }

  group.list[[C+1]] <- vector()
  group.list[[C+2]] <- vector()
  group.list[[C+3]] <- vector()

  for (nnn in 1:LLL) {
  group.list[[C+1]][nnn] <-  "mean.of.signal"
  group.list[[C+2]][nnn] <- "sd.of.signal"
  group.list[[C+3]][nnn] <- "AUC"

  }


  y <- unlist(y.list)
  group   <- unlist(group.list)
  # upper_y <- max(y)
  # lower_y <- min(y)

  # group  <- rep(seq(1,LLL,1),1 , each=C+2)
  # group <-  as.character(group)

  if(log_scale_x.axis==FALSE) x <-   rep(seq(1,LLL,1),C+3 , each=1)
  if(log_scale_x.axis==TRUE)  x <-   rep(NLvector    ,C+3 , each=1)



  # browser()
  # browser()
  # browser()

  df <-  data.frame(x=x,y=y,group=group)

  # browser()


  if (log_scale_x.axis==TRUE) {
    print(
      ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, shape = group)) +
    ggplot2::geom_line(position = ggplot2::position_dodge(0.2)) +           # Dodge lines by 0.2
    ggplot2::geom_point(position = ggplot2::position_dodge(0.2), size = 4)+  # Dodge points by 0.2
      ggplot2::scale_shape_manual(values = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25))+ # This is a valiation of dots in the plot, and important in case of  the number of confidence level is greater than 4


    ggplot2::scale_y_log10()+
    ggplot2::scale_x_log10()
    )
  }

   if (log_scale_x.axis==FALSE) {
     print(   ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, shape = group)) +
      ggplot2::geom_line(position = ggplot2::position_dodge(0.2)) +           # Dodge lines by 0.2
      ggplot2::geom_point(position = ggplot2::position_dodge(0.2), size = 4)+  # Dodge points by 0.2
        ggplot2::scale_shape_manual(values = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25))+ # This is a valiation of dots in the plot, and important in case of  the number of confidence level is greater than 4
        ggplot2::scale_y_log10()
      #+ ggplot2::scale_x_log10()
     )
  }

# browser()
}
























#' @title Visualization Of variance Analysis
#'
#' @param return.value.of_error_srsc A return value
#' of the function \code{\link{error_srsc}()}.
#' @param log_scale_x.axis A logical, whether x axis is log scale.
#' @return A long format dataframe of error and its parameter name
#' @export
#'
#' @examples
#' \dontrun{
#'  a <- error_srsc()
#'
#'  error_srsc_variance_visualization(a)
#'
#'
#'
#'   a <- error_srsc(replicate.datset = 10)
#'   error_srsc_variance_visualization(a)
#'
#' }
#'


error_srsc_variance_visualization <- function(return.value.of_error_srsc,
                                              log_scale_x.axis = TRUE

){

  a<-return.value.of_error_srsc
  aa <- a$S.E.for.various.NL
  NLvector <- a$NLvector
  length.of.NLvector <- a$length.of.NLvector
  C <- a$C
  LLL <-length.of.NLvector

  y.list <- list()
  group.list <-list()
  for (cd in 1:(C+3)) {
    y.list[[cd]] <- vector()
    y.list[[cd]] <- abs(
      as.numeric(
        subset(aa[cd+2,], select  = 2:length(aa[cd+2,]))
      )
    )

    # group.list[[cd]] <- subset(aa[,1], subset  = 2:(C+2),select  =1 )
  }
  for (cd in 1:C) {
    group.list[[cd]] <- vector()

    for (nnn in 1:LLL) {
      group.list[[cd]][nnn] <- paste("z[",cd,"]")
    }
  }

  group.list[[C+1]] <- vector()
  group.list[[C+2]] <- vector()
  group.list[[C+3]] <- vector()

  for (nnn in 1:LLL) {
    group.list[[C+1]][nnn] <-  "mean.of.signal"
    group.list[[C+2]][nnn] <- "sd.of.signal"
    group.list[[C+3]][nnn] <- "AUC"

  }


  y <- unlist(y.list)
  group   <- unlist(group.list)
  # upper_y <- max(y)
  # lower_y <- min(y)

  # group  <- rep(seq(1,LLL,1),1 , each=C+2)
  # group <-  as.character(group)

  if(log_scale_x.axis==FALSE) x <-   rep(seq(1,LLL,1),C+3 , each=1)
  if(log_scale_x.axis==TRUE)  x <-   rep(NLvector    ,C+3 , each=1)



  # browser()
  # browser()
  # browser()

  df <-  data.frame(x=x,y=y,group=group)

  # browser()


  if (log_scale_x.axis==TRUE) {
    print(
      ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, shape = group)) +
        ggplot2::geom_line(position = ggplot2::position_dodge(0.2)) +           # Dodge lines by 0.2
        ggplot2::geom_point(position = ggplot2::position_dodge(0.2), size = 4)+  # Dodge points by 0.2
        ggplot2::scale_shape_manual(values = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25))+ # This is a valiation of dots in the plot, and important in case of  the number of confidence level is greater than 4
        ggplot2::scale_y_log10()+
        ggplot2::scale_x_log10()
    )
  }

  if (log_scale_x.axis==FALSE) {
    print(   ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, shape = group)) +
               ggplot2::geom_line(position = ggplot2::position_dodge(0.2)) +           # Dodge lines by 0.2
               ggplot2::geom_point(position = ggplot2::position_dodge(0.2), size = 4)+  # Dodge points by 0.2
               ggplot2::scale_shape_manual(values = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25))+ # This is a valiation of dots in the plot, and important in case of  the number of confidence level is greater than 4
               ggplot2::scale_y_log10()
             #+ ggplot2::scale_x_log10()
    )
  }

  # browser()
}


























#'@title Draw Curves for validation dataset
#'@description  drawing curves.
#'
#'
#' \strong{Red curve} indicates an FROC curve of truth parameter.
#'
#' \strong{Other curves} are drawn using replicated estimates.
#'
#'
#'@inheritParams DrawCurves
#'@inheritParams DrawCurves_MRMC_pairwise
#'@inheritParams fit_Bayesian_FROC
#'@param validation.data This is a return value of
#' the function \code{validation.dataset_srsc}.
#'
#'@return \code{NULL}
#'
#'
#'@examples
#'
#' \dontrun{
#'#--------------------------------------------------------------------------------------
#'#                         1)       Draw the curve for each replicated dataset
#'#--------------------------------------------------------------------------------------
#'
#'     datasets <- validation.dataset_srsc()
#'
#'     validation.draw_srsc(datasets)
#'
#'
#'
#'
#'#--------------------------------------------------------------------------------------
#'#                         1)       Draw the curve for each replicated dataset
#'#--------------------------------------------------------------------------------------
#'
#'
#'
#'               datasets <- validation.dataset_srsc(replicate.datset = 5)
#'
#'
#'                                  validation.draw_srsc(datasets)
#'
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
  upper_x <-max(s) ### Nice! I was great 2019 September 1

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
                 upper_y=upper_y,
                 # cex=0.04
      )
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


