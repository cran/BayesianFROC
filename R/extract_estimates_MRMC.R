

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
#'\donttest{
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
