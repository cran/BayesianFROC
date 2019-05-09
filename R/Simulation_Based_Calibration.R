



#' @title Draw One Sample from Prior
#'
#' @param sd Standard deviation of priors. Very large number.
#' @param C No. of Confidence level
#' @param seed.for.drawing.a.prior.sample seed
#'
#' @return w, v, m, dz, z
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#'      Draw.a.prior.sample <- Draw_a_prior_sample()
#'
#'}# dottest
#'
Draw_a_prior_sample <- function(
  sd=5,
  C=5,
  seed.for.drawing.a.prior.sample=1111){
  set.seed(seed.for.drawing.a.prior.sample)

  m <- stats::rnorm(1,mean = 1,sd =sd/4) ##################################

   v<-numeric()
   v<-0
  repeat {
    if (v <= 0 )   v <- stats::rnorm(1,mean = 5,sd =sd/4) ###############################
    else        break
  }


   w   <- stats::rnorm(1,mean = -0.8,sd =sd/8) ######################

   dz <- vector()
   name.dz <- vector()
   name.z <- vector()

   for (cd in 1:C) {
     dz[cd] <- -1
     name.dz[cd] <-paste("dz[",cd,"]",sep = "")
     name.z[cd] <-paste("z[",cd,"]",sep = "")

     repeat {
       if ( dz[cd] < 0 )    dz[cd] <- stats::rnorm(1,mean = 0.5,sd =sd/5) ##############
       else        break
     }
   }


   z <- vector()
   z[1] <- w
   for (cd in 1:C) {
     z[cd + 1] <- z[cd]+dz[cd]
   }

   names(dz) <- name.dz
   names(z) <- name.z

   Draw.a.prior.sample <- list(
     w=w,
     v=v,
     m=m,
     dz=dz[1:C-1],
     z=z[1:C]

   )




   return(
     Draw.a.prior.sample
     )


}
















#' @title Draw a simulated dataset from model distributions with specified parameters from priors
#'
#' @inheritParams  Draw_a_simulated_data_set_and_Draw_posterior_samples
#'
#' @return One simulated dataset
#' @export
#'
#' @examples
#'
#'\donttest{
#'    one.dataList  <-  Draw_a_simulated_data_set()
#'
 #'}# dottest

Draw_a_simulated_data_set<- function(
  sd=5,
  C=5,
  seed.for.drawing.a.prior.sample=1111,
  fun = stats::var,
  # Draw.a.prior.sample,
  NI=259,
  NL=259,
  initial.seed.for.drawing.a.data=1234,
  ModifiedPoisson=FALSE,
  ite = 1111

){

  Draw.a.prior.sample <-  Draw_a_prior_sample(
    sd=sd,
    C=C,
    seed.for.drawing.a.prior.sample=seed.for.drawing.a.prior.sample
  )

  w <- Draw.a.prior.sample$w
  m <- Draw.a.prior.sample$m
  v <- Draw.a.prior.sample$v
  dz<- Draw.a.prior.sample$dz
  z <- Draw.a.prior.sample$z


  prior.samples.vector <- c(w,dz,m,v)
  prior.samples.sended.by.fun <- fun(c(w,dz,m,v)   )


  l <- -log( stats::pnorm(z))
  lambda <- vector()

  for (cd in 1:C) {
    if(ModifiedPoisson==F){
      if (cd==C) {
        lambda[cd] <- (l[cd]-0)*NI
      }else{
        lambda[cd] <- (l[cd]-l[cd+1])*NI
      }#else      set.seed(seed =  seed); hits <-   stats::rbinom(n=1,size = NL,prob = p.truth[cd])
    }#  if ModifiedPoisson==T

    if(ModifiedPoisson==T){
      if (cd==C) {
        lambda[cd] <- (l[cd]-0)*NL
      }else{
        lambda[cd] <- (l[cd]-l[cd+1])*NL
      }#else      set.seed(seed =  seed); hits <-   stats::rbinom(n=1,size = NL,prob = p.truth[cd])
    }#  if ModifiedPoisson==T

  }
  for (cd in 1:C) {
    names(lambda)[cd] <- paste("l[",cd,"]")
  }

  C <- length(dz) +1








  one.dataList <- hits_false_alarms_creator_from_thresholds(
    replicate.datset =1,
    ModifiedPoisson = FALSE,
    mean.truth=m,
    sd.truth=v,
    z.truth =z, #<  <  <
    NL=NL,
    NI=NI,
    summary=TRUE,
    initial.seed = initial.seed.for.drawing.a.data
  )


  # Convert from list constructed from one dataframe to dataframe
  one.dataList <- one.dataList[[1]]
  draw.CFP.CTP.from.dataList(one.dataList,ModifiedPoisson =ModifiedPoisson)
  return(one.dataList)

}#function










#' @title Draw a dataset and MCMC samples
#'@description Draw a dataset and MCMC samples
#' @return Draw.a.prior.sample The Return value of \code{Draw_a_prior_sample}
#' @param NI No. of images
#' @param NL No. of Lesions
#'@inheritParams hits_false_alarms_creator_from_thresholds
#'@seealso  hits_false_alarms_creator_from_thresholds

#' @param sd Standard Deviation of priors
#' @param fun An one dimensional real valued function defined on the parameter space. This is used in the definition of the rank statistics. Generally speaking, the element of the parameter space is a vector, so the function should be defined on vectors. In my model parameter is mean, standard deviation, C thresholds of the latent Gaussian, so this function should be defined on the C+2 dimensional Euclidean space.

#' @param C No. of Confidence levels
#' @param seed.for.drawing.a.prior.sample seed
#'@inheritParams fit_Bayesian_FROC

#' @param initial.seed.for.drawing.a.data  seed
#'
#' @return A dataList and an object of the stanfit S4 class with respect to the dataList
#' @export
#'
#' @examples
#' \donttest{
 #'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####

#' #  Draw a curve for various seeds and various number of confidence levels.
#' #  Changing the seed, we can draw a parameter from priors and using this sample,
#' #  we can draw the datasets from our model whose parameters are
#' #  the priors samples.
#'
#'
#'#     1. draw a prior sample,
#'#     2. draw a data from the model at the prior sample drawn in step 1,
#'#     3. draw a posterior sample for the data drawn in step 2.
#'
#'
#'
#'  Draw_a_simulated_data_set_and_Draw_posterior_samples(
#'  seed.for.drawing.a.prior.sample = 1234,
#'  C=8)
#'
#'    Draw_a_simulated_data_set_and_Draw_posterior_samples(
#'  seed.for.drawing.a.prior.sample = 12345,
#'  C=7)
#'
#'     Draw_a_simulated_data_set_and_Draw_posterior_samples(
#'  seed.for.drawing.a.prior.sample = 123456,
#'  C=6)
#'
#'
#'     Draw_a_simulated_data_set_and_Draw_posterior_samples(
#'  seed.for.drawing.a.prior.sample = 1234567,
#'  C=5)
#'
#'
#'}# dottest
#'

Draw_a_simulated_data_set_and_Draw_posterior_samples <- function(
  sd=5,
  C=5,
  seed.for.drawing.a.prior.sample=1111,
 fun = stats::var,
   # Draw.a.prior.sample,
   NI=259,
   NL=259,
  initial.seed.for.drawing.a.data=1234,
  ModifiedPoisson=FALSE,
 PreciseLogLikelihood = TRUE,
 ite = 1111,
 DrawCurve = FALSE

  ){

  Draw.a.prior.sample <-  Draw_a_prior_sample(
    sd=sd,
    C=C,
    seed.for.drawing.a.prior.sample=seed.for.drawing.a.prior.sample
  )

  w <- Draw.a.prior.sample$w
  m <- Draw.a.prior.sample$m
  v <- Draw.a.prior.sample$v
  dz<- Draw.a.prior.sample$dz
  z <- Draw.a.prior.sample$z


  prior.samples.vector <- c(w,dz,m,v)
  prior.samples.sended.by.fun <- fun(c(w,dz,m,v)   )


  l <- -log( stats::pnorm(z))
  lambda <- vector()

  for (cd in 1:C) {
    if(ModifiedPoisson==F){
        if (cd==C) {
          lambda[cd] <- (l[cd]-0)*NI
        }else{
          lambda[cd] <- (l[cd]-l[cd+1])*NI
        }#else      set.seed(seed =  seed); hits <-   stats::rbinom(n=1,size = NL,prob = p.truth[cd])
      }#  if ModifiedPoisson==T

    if(ModifiedPoisson==T){
      if (cd==C) {
        lambda[cd] <- (l[cd]-0)*NL
      }else{
        lambda[cd] <- (l[cd]-l[cd+1])*NL
      }#else      set.seed(seed =  seed); hits <-   stats::rbinom(n=1,size = NL,prob = p.truth[cd])
    }#  if ModifiedPoisson==T

}
  for (cd in 1:C) {
    names(lambda)[cd] <- paste("l[",cd,"]")
  }

  C <- length(dz) +1








  one.dataList <- hits_false_alarms_creator_from_thresholds(
    replicate.datset =1,
    ModifiedPoisson = FALSE,
    mean.truth=m,
    sd.truth=v,
    z.truth =z, #<  <  <
    NL=NL,
    NI=NI,
    summary=TRUE,
    initial.seed = initial.seed.for.drawing.a.data
  )


  # Convert from list constructed from one dataframe to dataframe
  one.dataList <- one.dataList[[1]]



 fit <- fit_Bayesian_FROC( ite  = ite,
                           summary = FALSE ,
                           dataList = one.dataList,
                           cha=1,
                           DrawCurve = DrawCurve,
                           PreciseLogLikelihood = TRUE

                           )

 w  <-  extract(fit)$w
 L  <- length(w)  #length of MCMC chains not contains warming up period

 z.list  <-  list()
 dz.list <- list()
 m.list  <- list()
 v.list  <- list()
 w.list  <- list()

 for (jjj in 1:L) {

 z.list[[jjj]]    <-  extract(fit)$z[jjj,]
 dz.list[[jjj]]   <- extract(fit)$dz[jjj,]
 m.list[[jjj]]    <- extract(fit)$m[jjj]
 v.list[[jjj]]    <- extract(fit)$v[jjj]
 w.list[[jjj]]    <- extract(fit)$w[jjj]

 }#for


MCMC.samples <-  Map( c, w.list, dz.list, m.list, v.list  )


MCMC.samples.sended.by.fun  <- lapply( MCMC.samples, fun )
MCMC.samples.sended.by.fun  <- unlist(MCMC.samples.sended.by.fun)

rank.statistics <- rank_statistics_with_two_parameters(
                        MCMC.samples.sended.by.fun,
                        prior.samples.sended.by.fun
                        )



# w.EAP  <- extract_EAP_by_array(fit,w)
# dz.EAP <- extract_EAP_by_array(fit,dz)
# z.EAP  <- extract_EAP_by_array(fit,z)
# m.EAP  <- extract_EAP_by_array(fit,m)
# v.EAP  <- extract_EAP_by_array(fit,v)

# list.of.param.vectors <- list(
#   w.EAP  = w.EAP,
#   dz.EAP = dz.EAP,
#   m.EAP  = m.EAP,
#   v.EAP  = v.EAP
# )
 return.value <-  list(
   # list.of.param.vectors = list.of.param.vectors,

   lambda=lambda,
   Draw.a.prior.sample=Draw.a.prior.sample,
   one.dataList =one.dataList
   ,fit=fit,

   # w.EAP  = w.EAP,
   # dz.EAP = dz.EAP,
   # z.EAP  = z.EAP,
   # m.EAP  = m.EAP,
   # v.EAP  = v.EAP,

   z.list    =  z.list,
   dz.list   =  dz.list,
   m.list    =  m.list,
   v.list    =  v.list,
   w.list    =  w.list,

   MCMC.samples = MCMC.samples,
   MCMC.samples.sended.by.fun  =MCMC.samples.sended.by.fun,

   prior.samples.vector=prior.samples.vector,
   prior.samples.sended.by.fun = prior.samples.sended.by.fun,

   rank.statistics =rank.statistics
 )

 size_of_return_value( object=return.value)

 # browser()
 invisible( return.value )


}#function   Draw_a_simulated_data_set_and_Draw_posterior_samples

















#' @title Draw a histogram of the rank statistics
#'@description  To validate that the MCMC procedure is correct or not, we show the  histogram of rank statistics. If the resulting histogram is uniformly distributed, then we can conclude that the MCMC sampling is correct. If the histogram is far from uniformity, then the MCMC sampling or specification of priors is not correct or not appropriate.
#' @param N samples size of the rank statistics.

#' @param initial.seed.for.drawing.a.rank.statistics seed

#' @param initial.seed.for.drawing.a.data seed
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams Draw_a_simulated_data_set_and_Draw_posterior_samples

#' @return samples of rank statistics
#' @export
#'
#' @examples
#'\dontrun{
#'   g <-Simulation_Based_Calibration_histogram(N=2,ite = 2222)
#'
#'   graphics::hist(g$rank.statistics)
#'
#'
#'
#'
#'   g <- Simulation_Based_Calibration_histogram(
#'   NI=1111111,
#'   NL=1111111,
#'   # N =100 would be better  more than N =10
#'   # But this is only example, we take very small N
#'   N=10,
#'   ite=3333,
#'   sd=1,
#'   initial.seed.for.drawing.a.rank.statistics = 123456789,
#'   DrawCurve = T
#'   )
#'
#'
#'
#'    g <- Simulation_Based_Calibration_histogram(
#'    NI=1111111,
#'    NL=1111111,
#'   # N =100 would be better  more than N =10
#'   # But this is only example, we take very small N
#'    N=10,
#'    ite=3333,
#'    sd=1,initial.seed.for.drawing.a.rank.statistics = 123456789,
#'    DrawCurve = T,
#'    C=11)
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#=======      The Second Example:       =================================================
#'
#' # If you want to see the replicated data, then the following code is available.
#' # In the following, I extract the dataset which is very small rank statistics, e.g.
#' # less than 10. And draw the CFP and CTP for observation of dataset.
#'
#'
#'
#'
#' gggg <- Simulation_Based_Calibration_histogram(
#' NI=1111111,
#' NL=1111111,
#' N=22,
#' ite=2222)
#'
#'
#'  a <- gggg$rank.statistics<10
#'
#'  aa <- the_row_number_of_logical_vector(a)
#'
#'
#'   draw.CFP.CTP.from.dataList(gggg$fit.list[[  aa[1]  ]]@dataList)
#'
#'
#'}#\dontrun
Simulation_Based_Calibration_histogram <-function(
    N =3,
    sd=5,
    C=5,
    initial.seed.for.drawing.a.rank.statistics=1234567,
    fun = stats::var,
    # Draw.a.prior.sample,
    NI=259,
    NL=259,
    initial.seed.for.drawing.a.data=1234,
    ModifiedPoisson=FALSE,
    ite = 1111,
    DrawCurve = FALSE

  ){

  index.of.nonconvergent <- vector()
  fit.list <- list()
  rank.statistics <- vector()


s<-0
aaa<-initial.seed.for.drawing.a.rank.statistics
bbb<-initial.seed.for.drawing.a.rank.statistics +N-1
  for(seed in aaa:bbb){
s<-s+1
message("\n* The ",s,"-th fitting \n")
   a <- Draw_a_simulated_data_set_and_Draw_posterior_samples(
    sd=sd,
    C=C,
    seed.for.drawing.a.prior.sample =  seed*3+123,
    fun = fun,
    # Draw.a.prior.sample,
    NI=NI,
    NL=NL,
    initial.seed.for.drawing.a.data = seed*2+1053,
    ModifiedPoisson=ModifiedPoisson,
    ite = ite,
    DrawCurve = DrawCurve

  )
# browser()
   fit.list[[s]] <- a$fit
   rank.statistics[s] <- a$rank.statistics
   index.of.nonconvergent[s] <- a$fit@convergence
  }# for seed

# dark_theme(type = 1)
# graphics::hist( rank.statistics)


message(crayon::silver(
"\n* We do not draw the histogram for rank statistics. The reason is that if the error Error in plot.new() : figure margins too large
occur then the raturn value vanish. So, to avoid such a case, we do not implement the histogram, please draw by your hand !! \n  "
)
)

message("\n* return value has rank statistics, so please execute the following code: ")
message("\n\n    BayesianFROC::dark_theme(type = 1) #This is used for the rayout of histogram \n     ")
message("\n    graphics::hist( return.value$rank.statistics)   #This code draw the histogram of rank statistics \n     ")


# graphics::hist( rank.statistics)


return.value <- list(
  fit.list = fit.list,
  rank.statistics = rank.statistics,
  index.of.nonconvergent=index.of.nonconvergent
)

size_of_return_value(object = return.value)
invisible(return.value)
   }# Simulation_Based_Calibration_histogram


#' @title Plot the pairs of CFPs and CTPs
#'@description  It plot the emipirical FROC curves (not depicted the line).
#'@inheritParams fit_Bayesian_FROC

#'
#' @return CFPs and CTPs
#' @export
#'
#' @examples
#'
#'   draw.CFP.CTP.from.dataList(dataList.Chakra.1)
#'
draw.CFP.CTP.from.dataList <- function(dataList,ModifiedPoisson =FALSE,new.imaging.device=TRUE){

  NI <- dataList$NI
  NL <- dataList$NL



   if (ModifiedPoisson==FALSE)
{     NX <-NI
  xlab <- "CFP per image"
}

  if (ModifiedPoisson==TRUE)
  {     NX <-NL
  xlab <- "CFP per lesion"
  }

  h <- dataList$h
  f <- dataList$f

  #The order is ignored in this calculation
  CTP <- cumsum(h)/NL
  CFP <- cumsum(f)/NX

  CTP <- c(0,CTP)
  CFP <- c(0,CFP)

  if (new.imaging.device==TRUE) {
    grDevices::dev.new()
  }


  graphics::par(bg= "gray12", #"gray27",#"gray40",#"black",# ,
                fg="gray",
                col.lab="bisque2" ,#"bisque" ,#  "antiquewhite1",
                col.axis="bisque2" ,##"bisque" "antiquewhite1",
                col.main="bisque2" ,
                cex.lab=1.5,
                cex.axis=1.3
  )

  graphics::plot(CFP,CTP,xlab=xlab,
                 type="b",
                 col="royalblue3",
                 pch=15,
                 cex=2,
                 lwd=2,
                 lty=1
                 )

  graphics::title("Empirical FROC")


  return(
    list(
    CFP=CFP,
    CTP=CTP
    )
  )

}





#' @title Rank Statistics
#'
#' @param values.of.f.at.one.MCMC.samples The value of f at a vector whose components are constructed by the all parameters at one MCMC sample.
#' @param values.of.f.at.a.sample.from.priors The value of f at a vector of model parameters from the prior distribution.
#'
#' @return The value of the Rank Statistics
#'
#' @export
#'
#' @examples
#'
#'  \donttest{

# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#======== The first example   ==========================================
#'
#'   rank_statistics_with_two_parameters(c(1,2,3,4,5),4)
#'
#'#=======  The Second Example  ==========================================
#'
#'        a <- Draw_a_simulated_data_set_and_Draw_posterior_samples()
#'
#'     rank_statistics_with_two_parameters(
#'          a$MCMC.samples.sended.by.fun,
#'          a$prior.samples.sended.by.fun
#'          )
#'
#'
#'}# dottest

#'
rank_statistics_with_two_parameters <- function(values.of.f.at.one.MCMC.samples, values.of.f.at.a.sample.from.priors){

  L <- length( values.of.f.at.one.MCMC.samples  )

  s <- 0
  for (i in 1:L) {
    if (values.of.f.at.one.MCMC.samples[i]  <   values.of.f.at.a.sample.from.priors) {
      s <-s+1
    }#if

  }


  return(s)


}#function

# rank_statistics_with_one_parmater
#
# rank_statistics_with_replicated_param_from_prior
