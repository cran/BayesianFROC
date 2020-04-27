
#'@title Extract Etimates Preserving Array Format.
#'@description Extract posterior mean extimates (\strong{EAP})  by array format.
#'@details
#'If an estimate is an array,
#'then this function extract  estimated parameters preserving an array format.
#' The \code{rstan} also has such function,
#' i.e., \strong{\emph{\code{rstan::\link[rstan]{get_posterior_mean}}()}}.
#'  However this function does not extract paramter as an array but coerce to the class matrix.

#'@inheritParams validation.dataset_srsc
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@param  name.of.parameter An parameter name (given as a character string, should not surround by "").
#'The name of parameter which user want to extract.
#'Parameters are contained in the parameter block of each Stan file in the path: inst/extdata.

#'
#' @return A list of datalists from the posterior predictive distribution
#' @export
#'
#' @examples
#'  \dontrun{
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#=================================The first example: MRMC case ========================
#'#========================================================================================
#'#             MRMC case: Extract a estimates from fitted model objects
#'#========================================================================================
#'
#'
#'# Make a fitted model object of class stanfitExtended
#'# which is inherited from the S4class stanfit.
#'# The following example, fitted model is the hierarchical Bayesian FROC model
#'# which is used to compare modality.
#'
#'  fit <- fit_Bayesian_FROC( ite  = 1111 ,
#'                            summary = FALSE   ,
#'                            dataList = dataList.Chakra.Web.orderd,
#'                            cha=1
#'                             )
#'
#'#  Extract one dimensional array "z = z[]",
#'
#'                   z   <- extract_EAP_by_array(
#'                                                fit,  # The above fitted model object
#'                                                z     # One of the parameter in "fit"
#'                                                )
#'
#'
#'
#'#  Extract two dimensional array "AA = AA[ , ]",
#'
#'                   AA  <- extract_EAP_by_array(
#'                                               fit,
#'                                               AA
#'                                               )
#'
#'
#'#  Extract three dimensional array "ppp = ppp[ , , ]",
#'
#'                   ppp <- extract_EAP_by_array(fit,ppp)
#'
#'

#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#================= The second example: singler reader and single modality ==============
#'#========================================================================================
#'#             srsc case: Extract a estimates from fitted model objects
#'#========================================================================================
#'
#'
#' #   Of course, for the case of srsc, it is also available.
#' #   We shall show the case of srsc in which case the parameters are not array,
#' #   but in such a case we can extract estimates preserving its format such as vector.
#'
#'  fit <- fit_Bayesian_FROC( ite  = 1111 ,
#'                            summary = FALSE   ,
#'                            dataList = dataList.Chakra.1,
#'                            cha=2
#'                             )
#'
#'#  To extract the posterior mean for parameter "A" representing AUC, we run the following;
#'
#'
#'           A <- extract_EAP_by_array(
#'                                     fit,
#'                                      A
#'                                      )
#'
#'
#'
#'
#'#  To extract the posterior mean for parameter "z" indicating decision thresholds;
#'
#'
#'           z <- extract_EAP_by_array(
#'                                      fit,
#'                                      z
#'                                      )
#'
#'
#'
#' # 2019.05.21 Revised.
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#              name.of.parameter surrounded by double quote is also available
#'#========================================================================================
#'
#'
#'#      Let fit be the above fitted model object.
#'#      Then the following two codes are same.
#'
#'
#'
#'                               extract_EAP_by_array( fit, "A" )
#'
#'                               extract_EAP_by_array( fit,  A  )
#'
#'
#'# Unfortunately, the later case sometimes cause the R CMD check error which said
#'# that no visible binding, since object A is not defined.
#'# For example, if we use the later in the functiton: metadata_to_DrawCurve_MRMC
#'# Then R command said some NOTE that
#'
#'# > checking R code for possible problems ... NOTE
#'# metadata_to_DrawCurve_MRMC: no visible binding for global variable 'A'
#'# Undefined global functions or variables: A
#'
#'# Revised 2019 Oct 19
#'
#'
#'
#'
#'
#'# I am not sure, does this package development make me happy?
#'# Back pain being due to an abnormality in my immune system, which is caused
#'# my exposure to surfactants or latex (not LaTeX).
#'

#'}# Revised 2019 Jun 19

#'

extract_EAP_by_array <-function(StanS4class,
                                name.of.parameter
){

  # This detect the length of array
  if (class("name.of.parameter")=="character"){
    name.of.parameter <-substitute(name.of.parameter)
  }
    fit <- methods::as(StanS4class, "stanfit")
  extract.expression.dim <- paste( "length(dim(extract(fit,par=c(name.of.parameter ))[[1]]))-1" ,sep = "")
  foo.dim <- parse(text = extract.expression.dim)
  dim<- eval(foo.dim)
  # dim means e.g.,
  # real z[5 ] --dim=1
  # real z[4,5]---dim = 2
  # real z[5,6,7]---dim = 3

  if (dim==0) {
    extract.expression <- paste( "mean (extract(fit)$",name.of.parameter, ",)" ,sep = "")
  }


  if (dim==1) {
    extract.expression <- paste( "apply(extract(fit)$",name.of.parameter, ", MARGIN = 2, mean)" ,sep = "")
  }


  if (dim==2){
    extract.expression <- paste( "apply(extract(fit)$",name.of.parameter, ", MARGIN = c(2,3), mean)" ,sep = "")
  }

  if (dim==3){
    extract.expression <- paste( "apply(extract(fit)$",name.of.parameter, ", MARGIN = c(2,3,4), mean)" ,sep = "")
  }

  if (dim==4){
    extract.expression <- paste( "apply(extract(fit)$",name.of.parameter, ", MARGIN = c(2,3,4,5), mean)" ,sep = "")
  }

  foo <- parse(text = extract.expression )
  e<- eval(foo)

  return(e)
}



# n <-array(NA,dim = c(5,2,3))
# for (cd in 1:5) {
#   for (md in 1:2) {
#     for (qd in 1:3) {
#       n[cd,md,qd] <- paste("[",cd,",",md, ",", qd , "]",sep = ""  )
#
#
#   }
#   }
# }

















#'@title MRMC: Extract  Estimates of a vector from stanfitExtended object
#'
#'@description We extract the EAPs and CIs from the stanfitExtended S4 class which is an
#'inherited class of the stanfit S4 class.
#'
#'@details To validate our model has no bias,
#'that is comparison of true parameters of distributions and EAPs,
#'we have to extract the estimates from the stanfitExtended object.
#' And this function do it.

#'@return EAPs, CI.
#'@param StanS4class An S4 object of the class \strong{\emph{\code{\link[rstan]{stanfit}}}}. No need that it is the S4 class \strong{\code{ \link{stanfitExtended}}}.
#'@param parameter.name  character vector. E.g., use as "aaa" . for names of parameter described in the parameter block of stan file.
#'@param dimension.of.parameter If parameter \code{aaa} is vector,
#'i.e.,\code{aaa[1],aaa[2],...aaa[6]} then  \code{dimension.of.parameter = 6}
#'@seealso \code{\link{extract_estimates_MRMC}}


#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@examples
#'
#' \dontrun{

# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####

#'# First we create the following fitted model object of  class stanfitExtend.
#'
#'
#'    fit <- fit_Bayesian_FROC(
#'                        BayesianFROC::dataList.Chakra.Web.orderd, # data
#'                               ite = 1111,                        # MCMC iteration
#'                               summary = FALSE                    # vervose
#'    )
#'
#'
#'
#'# Second, to extract the EAPs of the parameter z,
#'# we also have to specify the dimension of vector z as follows.
#'
#'
#'        extract_EAP_CI(
#'
#'                  fit,  #  The above fitted model object
#'                  "z",  #  The parameter name described in parameter block of stan file
#'                   5    #  The dimension of vector z
#'                       )
#'
#'
#'
#'# One more example: to extract the EAPs of the parameter dz,
#'# we also have to specify its dimension of vector dz as follows.
#'
#'           list.of.dz <-extract_EAP_CI(fit,"dz",4)
#'
#'# One more example: to extract the EAPs of the parameter w,
#'#     we also have to specify its dimension of vector w as follows.
#'
#'            list.w  <-extract_EAP_CI(fit,"w",1)
#'
#'
#'# Note that this function can extract only parameter of "vector" and not "array" !!
#'# To extract such array please use "extract_estimates_MRMC()"
#'# which extract all parameters from a hierarchical Bayesian model
#'# estimated from user data. So, this function is no longer meaningless,
#'# and I will delete this.
#'
#'
#'# I forgot where I use this function
#'# 2019.05.21 Revised.
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#           the following gives convergence seed 2019 Oct 12
#'#========================================================================================
#'#'
#'
#'f <- fit_Bayesian_FROC( ite  = 1111,  cha = 1, summary = TRUE, dataList = ddd ,see = 123456)
#'   z <- extract_EAP_CI(f,"z",f@dataList$C )$z.EAP
#'   #usethis::use_data(z)
#'   #usethis package cannot be to use since it is not declared in NAMESPACE.
#'
#'
#'   dz <- extract_EAP_CI(f,"dz",f@dataList$C-1 )$dz.EAP
#'   #usethis::use_data(dz)
#'   #usethis package cannot be to use since it is not declared in NAMESPACE.



#'}# dottest

#'
#'
#'
#' @export
#'

extract_EAP_CI <-function(StanS4class,
                          parameter.name,
                          dimension.of.parameter,
                          dig=5,
                          summary=TRUE
){

  fit <- methods::as(StanS4class, "stanfit")

  C <-dimension.of.parameter

  if (C==1){

    EAP    <- signif(  as.data.frame(summary(fit)[[1]])[parameter.name,"mean"],   digits = dig)
    CI.lower <- signif(  as.data.frame(summary(fit)[[1]])[parameter.name,"2.5%"],   digits = dig)
    CI.upper <- signif(  as.data.frame(summary(fit)[[1]])[parameter.name,"97.5%"],   digits = dig)

    l<-list(
      name=parameter.name,
      EAP=EAP,
      CI.lower=CI.lower,
      CI.upper=CI.upper)

    names(l) <- c(  "parameter.name",
                    paste(parameter.name,".EAP",sep = ""),
                    paste(parameter.name,".CI.lower",sep = ""),
                    paste(parameter.name,".CI.upper",sep = "")
    )


    d <- as.data.frame(l)
    if (summary==TRUE) print(knitr::kable(d, format = "pandoc"))

    return(l)


  }else

    name <- paste(parameter.name,".name",sep = "")
  EAP  <- paste(parameter.name,".EAP",sep = "")
  CI.lower <- paste(parameter.name,".CI.lower",sep = "")
  CI.upper <- paste(parameter.name,".CI.upper",sep = "")

  assign(name, vector())
  assign(EAP, vector())
  assign(CI.lower, vector())
  assign(CI.upper, vector())

  for (cd in 1:C) {
    name[cd] <- paste(parameter.name,"[",cd,"]", sep = "")

    EAP[cd]  <- signif(  as.data.frame(summary(fit)[[1]])[name[cd],"mean"],   digits = dig)
    CI.lower[cd]<- signif(  as.data.frame(summary(fit)[[1]])[name[cd],"2.5%"],   digits = dig)
    CI.upper[cd]<- signif(  as.data.frame(summary(fit)[[1]])[name[cd],"97.5%"],   digits = dig)
  }

  l<-list(
    name = name,
    EAP  = as.numeric(EAP),
    CI.lower=as.numeric(CI.lower),
    CI.upper=as.numeric(CI.upper)
  )

  names(l) <- c(
    paste(parameter.name,".name",sep = ""),
    paste(parameter.name,".EAP",sep = ""),
    paste(parameter.name,".CI.lower",sep = ""),
    paste(parameter.name,".CI.upper",sep = "")
  )


  d <- as.data.frame(l)
  if (summary==TRUE) print(knitr::kable(d, format = "pandoc"))



  return(l)

}














#'@title MRMC: Extract All Posterior Mean Estimates from stanfitExtended object
#'@description Extract Posterior Mean estimates, preserving its format, such as array, vector. From MRMC models, it extract the EAPs and CIs.
#'@details To validate our model has no bias,
#'that is comparison of true parameters of distributions and EAPs,
#'we have to extract the estimates from the stanfitExtended object.
#' And this function do it.
#'@return EAPs, CIs which preserving its format, such as array, vector.
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@seealso extract_EAP_CI() is used  in the function \code{extract_estimates_MRMC()}.
#'@examples
#'
#'\dontrun{
#'
#'  fit <- fit_Bayesian_FROC(
#'               BayesianFROC::dataList.Chakra.Web.orderd,
#'               summary = FALSE,
#'               ite=111)
#'
#'  EAPs <- extract_estimates_MRMC(fit)
#'
#'}# dottest

#'
#' @export



extract_estimates_MRMC <- function(StanS4class,dig=3){
  C <-StanS4class@dataList$C
  M <-StanS4class@dataList$M
  Q <-StanS4class@dataList$Q
  fit <- methods::as(StanS4class, "stanfit")


  #------  z --------- Start
  name.z <- vector()
  z.EAP <- vector()
  z.CI.lower<- vector()
  z.CI.upper<- vector()
  for (cd in 1:C) {
    name.z[cd] <- paste("z[",cd,"]", sep = "")
    z.EAP[cd]  <- signif(  as.data.frame(summary(fit)[[1]])[name.z[cd],"mean"],   digits = dig)
    z.CI.lower[cd]<- signif(  as.data.frame(summary(fit)[[1]])[name.z[cd],"2.5%"],   digits = dig)
    z.CI.upper[cd]<- signif(  as.data.frame(summary(fit)[[1]])[name.z[cd],"97.5%"],   digits = dig)
  }
  #------  z --------- Fin


  #-- Note that dz is C-1 and not C !!
  #------  dz --------- Start
  dz.list <-   extract_EAP_CI(StanS4class =  fit, parameter.name = "dz",
                              dimension.of.parameter = C-1,#Not C !! But C-1 !!
                              dig = dig, summary = FALSE)
  name.dz <- dz.list$dz.name
  dz.EAP <- dz.list$dz.EAP
  dz.CI.lower <- dz.list$dz.CI.lower
  dz.CI.upper <- dz.list$dz.CI.upper
  #------  dz --------- Fin




  # ------- mu  ------ Start
  name.mu      <- array(0,dim = c(M,Q))
  mu.EAP       <- array(0,dim = c(M,Q))
  mu.CI.lower  <- array(0,dim = c(M,Q))
  mu.CI.upper  <- array(0,dim = c(M,Q))
  for (md in 1:M) {
    for (qd in 1:Q) {
      name.mu[md,qd] <- paste("mu[",md,",",qd,"]", sep = "")
      mu.EAP[md,qd]  <-  signif(  as.data.frame(summary(fit)[[1]])[name.mu[md,qd],"mean"],   digits = dig)
      mu.CI.lower[md,qd]<- signif(  as.data.frame(summary(fit)[[1]])[name.mu[md,qd],"2.5%"],   digits = dig)
      mu.CI.upper[md,qd]<- signif(  as.data.frame(summary(fit)[[1]])[name.mu[md,qd],"97.5%"],   digits = dig)
    }# for cd
  }# for md
  # ------- mu  ------ Fin





  # ------- v  ------ Start
  name.v <- array(0,dim = c(M,Q))
  v.EAP <- array(0,dim = c(M,Q))
  v.CI.lower<- array(0,dim = c(M,Q))
  v.CI.upper<- array(0,dim = c(M,Q))
  for (md in 1:M) {
    for (qd in 1:Q) {
      name.v[md,qd] <- paste("v[",md,",",qd,"]", sep = "")
      v.EAP[md,qd]  <-  signif(  as.data.frame(summary(fit)[[1]])[name.v[md,qd],"mean"],   digits = dig)
      v.CI.lower[md,qd]<- signif(  as.data.frame(summary(fit)[[1]])[name.v[md,qd],"2.5%"],   digits = dig)
      v.CI.upper[md,qd]<- signif(  as.data.frame(summary(fit)[[1]])[name.v[md,qd],"2.5%"],   digits = dig)
    }# for cd
  }# for md
  # ------- v  ------ Fin



  # ------- ppp  ------ Start
  name.ppp <- array(0,dim = c(C,M,Q))
  ppp.EAP  <- array(0,dim = c(C,M,Q))
  ppp.CI.lower <- array(0,dim = c(C,M,Q))
  ppp.CI.upper <- array(0,dim = c(C,M,Q))
  for (cd in 1:C) {
    for (md in 1:M) {
      for (qd in 1:Q) {
        name.ppp[cd,md,qd] <- paste("ppp[",cd,",",md,",",qd,"]", sep = "")
        ppp.EAP[cd,md,qd]  <-  signif(  as.data.frame(summary(fit)[[1]])[name.ppp[cd,md,qd],"mean"],   digits = dig)
        ppp.CI.lower[cd,md,qd]<- signif(  as.data.frame(summary(fit)[[1]])[name.ppp[cd,md,qd],"2.5%"],   digits = dig)
        ppp.CI.upper[cd,md,qd]<- signif(  as.data.frame(summary(fit)[[1]])[name.ppp[cd,md,qd],"2.5%"],   digits = dig)
      }# for qd
    }# for md
  }# for cd
  # ------- ppp  ------ Fin
  w.list <-   extract_EAP_CI(StanS4class =  fit, parameter.name = "w",dimension.of.parameter = 1,dig = dig, summary = FALSE)
  name.w <- w.list$w.name
  w.EAP <- w.list$w.EAP
  w.CI.lower <- w.list$w.CI.lower
  w.CI.upper <- w.list$w.CI.upper



  dl.list <-   extract_EAP_CI(StanS4class =  fit, parameter.name = "dl",dimension.of.parameter = C,dig = dig, summary = FALSE)
  name.dl <- dl.list$dl.name
  dl.EAP <- dl.list$dl.EAP
  dl.CI.lower <- dl.list$dl.CI.lower
  dl.CI.upper <- dl.list$dl.CI.upper

  l.list <-   extract_EAP_CI(StanS4class =  fit, parameter.name = "l",dimension.of.parameter = C,dig = dig, summary = FALSE)
  name.l <- l.list$l.name
  l.EAP <- l.list$l.EAP
  l.CI.lower <- l.list$l.CI.lower
  l.CI.upper <- l.list$l.CI.upper

  A.list <-   extract_EAP_CI(StanS4class =  fit, parameter.name = "A",dimension.of.parameter = M,dig = dig, summary = FALSE)
  name.A <- A.list$A.name
  A.EAP <- A.list$A.EAP
  A.CI.lower <- A.list$A.CI.lower
  A.CI.upper <- A.list$A.CI.upper


  # ------- AA  ------ Start
  name.AA <- array(0,dim = c(M,Q))
  AA.EAP <- array(0,dim = c(M,Q))
  AA.CI.lower<- array(0,dim = c(M,Q))
  AA.CI.upper<- array(0,dim = c(M,Q))
  for (md in 1:M) {
    for (qd in 1:Q) {
      name.AA[md,qd] <- paste("AA[",md,",",qd,"]", sep = "")
      AA.EAP[md,qd]  <-  signif(  as.data.frame(summary(fit)[[1]])[name.AA[md,qd],"mean"],   digits = dig)
      AA.CI.lower[md,qd]<- signif(  as.data.frame(summary(fit)[[1]])[name.AA[md,qd],"2.5%"],   digits = dig)
      AA.CI.upper[md,qd]<- signif(  as.data.frame(summary(fit)[[1]])[name.AA[md,qd],"2.5%"],   digits = dig)
    }# for cd
  }# for md
  # ------- AA  ------ Fin





  # ------- aa  ------ Start
  name.aa <- array(0,dim = c(M,Q))
  aa.EAP <- array(0,dim = c(M,Q))
  aa.CI.lower<- array(0,dim = c(M,Q))
  aa.CI.upper<- array(0,dim = c(M,Q))
  for (md in 1:M) {
    for (qd in 1:Q) {
      name.aa[md,qd] <- paste("aa[",md,",",qd,"]", sep = "")
      aa.EAP[md,qd]  <-  signif(  as.data.frame(summary(fit)[[1]])[name.aa[md,qd],"mean"],   digits = dig)
      aa.CI.lower[md,qd]<- signif(  as.data.frame(summary(fit)[[1]])[name.aa[md,qd],"2.5%"],   digits = dig)
      aa.CI.upper[md,qd]<- signif(  as.data.frame(summary(fit)[[1]])[name.aa[md,qd],"2.5%"],   digits = dig)
    }# for cd
  }# for md
  # ------- aa  ------ Fin


  # ------- bb  ------ Start
  name.bb <- array(0,dim = c(M,Q))
  bb.EAP <- array(0,dim = c(M,Q))
  bb.CI.lower<- array(0,dim = c(M,Q))
  bb.CI.upper<- array(0,dim = c(M,Q))
  for (md in 1:M) {
    for (qd in 1:Q) {
      name.bb[md,qd] <- paste("bb[",md,",",qd,"]", sep = "")
      bb.EAP[md,qd]  <-  signif(  as.data.frame(summary(fit)[[1]])[name.bb[md,qd],"mean"],   digits = dig)
      bb.CI.lower[md,qd]<- signif(  as.data.frame(summary(fit)[[1]])[name.bb[md,qd],"2.5%"],   digits = dig)
      bb.CI.upper[md,qd]<- signif(  as.data.frame(summary(fit)[[1]])[name.bb[md,qd],"2.5%"],   digits = dig)
    }# for cd
  }# for md
  # ------- bb  ------ Fin



  return(
    list(
      w.EAP   =   w.EAP,
      dz.EAP  =  dz.EAP,
      z.EAP   =   z.EAP,
      mu.EAP  =  mu.EAP,
      v.EAP   =   v.EAP,
      ppp.EAP = ppp.EAP,
      l.EAP   =   l.EAP,
      dl.EAP  =  dl.EAP,
      A.EAP   =   A.EAP,
      AA.EAP  =   AA.EAP,
      aa.EAP  =   aa.EAP,
      bb.EAP  =   bb.EAP,

      w.CI.upper   =   w.CI.upper,
      dz.CI.upper  =  dz.CI.upper,
      z.CI.upper   =   z.CI.upper,
      mu.CI.upper  =  mu.CI.upper,
      v.CI.upper   =   v.CI.upper,
      ppp.CI.upper = ppp.CI.upper,
      l.CI.upper   =   l.CI.upper,
      dl.CI.upper  =  dl.CI.upper,
      A.CI.upper   =   A.CI.upper,
      AA.CI.upper  =   AA.CI.upper,
      aa.CI.upper  =   aa.CI.upper,
      bb.CI.upper  =   bb.CI.upper,

      w.CI.lower   =   w.CI.lower,
      dz.CI.lower  =  dz.CI.lower,
      z.CI.lower   =   z.CI.lower,
      mu.CI.lower  =  mu.CI.lower,
      v.CI.lower   =   v.CI.lower,
      ppp.CI.lower = ppp.CI.lower,
      l.CI.lower   =   l.CI.lower,
      dl.CI.lower  =  dl.CI.lower,
      A.CI.lower   =   A.CI.lower,
      AA.CI.lower  =   AA.CI.lower,
      aa.CI.lower  =   aa.CI.lower,
      bb.CI.lower  =   bb.CI.lower

    ))
}











#' @title    Extract AUC
#'@description     Extract AUC for both srsc and MRMC data.
#' @inheritParams fit_Bayesian_FROC
#' @export extractAUC
#  devtools::document();help("extractAUC")
#'@return The estimates of AUC with respect to modalities.
#'It also retains the name vector, nnname =c(A[1],A[2],....,A[M])
#' @inheritParams DrawCurves_MRMC_pairwise
#'@inheritParams DrawCurves
#'@param print_CI_of_AUC Logical, if \code{TRUE} then Credible intervals of AUCs for each modality are plotted.

#'@param dig digits of estimates.
#'
extractAUC <-function(StanS4class,
                      dig=3,
                      summary=TRUE,
                      new.imaging.device=TRUE,
                      print_CI_of_AUC = TRUE


                      ){  if(summary==TRUE){
  fit <- methods::as(StanS4class, "stanfit")
  # if( length(rstan::get_divergent_iterations(fit))  < 10000  ){
  #   message(crayon::silver("\n* Note that if your samples from the Hamiltonian MonteCarlo Method is small, then the AUCs is not precise values. You should calculate MCMC samples in suffiecietly large numbers and also carefully check the convergence criterion by R hat statistics. Small MCMC sampling gives you the AUCs which are unreliable. The author recommand that the MCMC samples is more than 30000 MCMC samples to obtain more reliable estimates."))
  # }

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

  if(print_CI_of_AUC)message("\n------------------------------\n")

  if(print_CI_of_AUC)message("In the Figure of AUCs, the two intervals are shown for each modality, i.e., ")


  #plot interval
  if(new.imaging.device&&print_CI_of_AUC) grDevices::dev.new()
  if(print_CI_of_AUC) print( rstan::plot(methods::as(StanS4class, "stanfit"),par=nnname))

  # if(print_CI_of_AUC) print(  stan_hist(f,pars = nnname ) )
  #  nnname =c(A[1],A[2],....,A[M])


  AUCs <- data.frame(Modality=name,
                     AUC.EAP=AUC,
                     AUC.CI.lower=A.CI.lower,
                     AUC.CI.upper=A.CI.upper )
  return(
    list(AUCs=AUCs
         ,nnname=nnname  #  nnname =c(A[1],A[2],....,A[M])
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

