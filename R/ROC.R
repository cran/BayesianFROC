

#' @title Empirical ROC curve
#'
#' @param Number_of_cases Number_of_cases
#' @param Number_of_non_cases Number_of_non_cases
#' @param frequencies_of_cases frequencies_of_cases
#' @param frequencies_of_non_cases frequencies_of_non_cases
#' @inheritParams fit_Bayesian_FROC
#'
#' @details Suppose that there is a \eqn{K} categories and data are drawn
#' from two multinomial distributions of size \eqn{n,m}.
#'
#'  \deqn{h_1,h_2,...,h_K, \Sigma h_i = n,}
#'
#'  \deqn{f_1,f_2,...,f_K,\Sigma f_i = m.}
#'
#'  Then this plots the cumulative sums.
#'
#'
#'
#'
#'
#'
# @return
#' @export
#'
# @examples
#'
plot_ROC_empirical_curves <- function(
  Number_of_cases = 100,
  Number_of_non_cases = 100,
  frequencies_of_non_cases   = stats::rmultinom(1, size = Number_of_cases, prob = c(0.1, 0.2, 0.3, 0.5)) ,
  frequencies_of_cases  =stats::rmultinom(1, size = Number_of_non_cases, prob = c(0.4, 0.3, 0.2, 0.1)),
  new.imaging.device=FALSE


  ){
  dark_theme()

  NL <- Number_of_cases
  NI <- Number_of_non_cases
  h<-frequencies_of_cases
  f<-frequencies_of_non_cases
  # # roc <- dataList.Chakra.1
  # # roc$NI <- roc$NL
  # # NI <- roc$NI
  # # NL <- roc$NL
  # #
  # # h <- roc$h
  # # f <- roc$f
  #
  # h1 <- c(h,NL-sum(h))
  # f1 <- c(f,NI-sum(f))
  # FPF <- cumsum(f1)/NI
  # TPF <- cumsum(h1)/NL
  # # FPF <- c(0,FPF,1)
  # # TPF <- c(0,TPF,1)
  # plot(FPF,TPF, type = "l"
  #      # , xlim=c(0,1),ylim=c(0,1)
  #      )
  #
  # suppressWarnings(graphics::par(new=TRUE));
  # plot(c(0,1),c(0,1), type = "l",xlab ="",ylab=""
  #      , xlim=c(0,1),ylim=c(0,1)
  #      )
  #
  #
  # h <- c(NL-sum(h),h)
  # f <- c(NI-sum(f),f)
  FPF <- cumsum(f)/NI
  TPF <- cumsum(h)/NL
  FPF <- c(0,FPF)
  TPF <- c(0,TPF)
  if(new.imaging.device)  suppressWarnings(graphics::par(new=TRUE));
  plot(FPF,TPF, type = "l"
       , xlim=c(0,1),ylim=c(0,1)
  )
  dark_theme(4)

  suppressWarnings(graphics::par(new=TRUE));
  plot(c(0,1),c(0,1), type = "l",xlab ="",ylab=""
       , xlim=c(0,1),ylim=c(0,1),lty ="dashed"
  )

}
























#' @title Synthesize ROC data
#'
#' @param Number_of_cases Number_of_cases
#' @param Number_of_non_cases Number_of_non_cases
#' @param prob_case  prob_case
#' @param prob_non_case prob_non_case
#'
#' @return A list, indicatin ROC data
#' @export
#'
# @examples
ROC_data_creator <- function(
  Number_of_cases = 100,
  Number_of_non_cases = 100,
  prob_case = c(0.1,0.2,0.3,0.5),
  prob_non_case = c(0.4,0.3,0.2,0.1)


){

  list(

    Number_of_cases = Number_of_cases,
    Number_of_non_cases = Number_of_non_cases,
    frequencies_of_non_cases   = as.vector( stats::rmultinom(1, size = Number_of_cases,    prob =prob_case   ) ) ,
    frequencies_of_cases  = as.vector(stats::rmultinom(1, size = Number_of_non_cases,  prob = prob_non_case  ) ),
    C = length(prob_case)
  )


}





#' @title Synthesize ROC data
#'
#' @param Number_of_cases Number_of_cases
#' @param Number_of_non_cases Number_of_non_cases
#' @param prob_case  prob_case
#' @param prob_non_case prob_non_case
#' @param name A logical, whether name is given or not.
#' @param seed An integer, indicating seed
#'
#' @return A list, indicatin ROC data
#'
#'
#' @examples
#'
#'   d<-ROC_data_creator2()
#'
#' \dontrun{
#' f<-fit_srsc_ROC(d,ite  = 111, summary = FALSE,  cha = 1,)
#' rstan::check_hmc_diagnostics(f)
#' rstan::traceplot(f)
#' draw_ROC_Curve_from_fitted_model(f)
#'}
#'
#'
#'
#'
#' d<-ROC_data_creator2(
#'   Number_of_cases = 100,
#'   Number_of_non_cases = 100,
#'   prob_case = c(0.1,0.2,0.3,0.5,0.8,0.9),
#'   prob_non_case = c(0.4,0.3,0.2,0.1,0.1,0.1)
#'   )

#' \dontrun{
#' f<-fit_srsc_ROC(d,ite  = 111, summary = FALSE,  cha = 1,)
#' rstan::check_hmc_diagnostics(f)
#' rstan::traceplot(f)
#' draw_ROC_Curve_from_fitted_model(f)
#'}
#'
#'
#'
#'
#'
#'
#'
#'
#' @export ROC_data_creator2

#'
ROC_data_creator2 <- function(
  Number_of_cases = 100,
  Number_of_non_cases = 100,
  prob_case = c(0.1,0.2,0.3,0.5),
  prob_non_case = c(0.4,0.3,0.2,0.1),
  name = FALSE,
  seed = NA


){

 if(!is.na(seed)) set.seed(seed)

  NL <- Number_of_cases
  NI <- Number_of_non_cases
  f   <- as.vector( stats::rmultinom(1, size = Number_of_cases,    prob =prob_case   ) )
  h  <- as.vector(stats::rmultinom(1, size = Number_of_non_cases,  prob = prob_non_case  ) )
  C <- length(prob_case)



  names(NL) <- "the Number of cases"
  names(NI) <- "the Number of non-cases"
  names(f) <- rep("the Number of false alarms", times = length(f))
  names(h) <- rep("the Number of hits", times = length(f))
  names(C) <- "the Number of categories"

list(

  NL = NL,
  NI = NI,
  f   =f,
  h  = h,
  C = C
)




}


#
# roc  <- function(dataList) {
#
#
#   d<-dataList
#   sum(d$f)
#   d$NI <- 100
#   d$h<-c(23,53,123)
#   # d$h<-c(123,53,23)
#
#   f <- fit_an_ROC_model_to(d)
#
#   draw_ROC_Curve(f)
# }#fun



#' Title
#'
#' @param a a
#' @param b b
#' @param x x
#'
#' @return x, y
#' @export
#'
# @examples
ROC_curve  <- function(a=0.2,b=0.2,x) {
  # x<- Phi(t)
  y<- Phi(b*Phi_inv(x)-a)
  y<-1- Phi(b*Phi_inv(1-x)-a)
  y<- Phi(b*Phi_inv(x)+a)

}

#' Title
#'
#' @param a a
#' @param b b
# @param x
#' @inheritParams fit_Bayesian_FROC
#' @inheritParams DrawCurves
# @return
#' @export
#'
# @examples
draw_ROC_Curve  <-  function(StanS4class,a=0.2,b=0.2,dataList) {

  dark_theme()

  # utils::globalVariables("x")

  x<-1# To avoid the NOTE in R CMD check which said that no visible binding for global  variable 'x'  Undefined global functions or variables:  x



  if (missing(StanS4class)) {
    graphics::curve(1- Phi(b*Phi_inv(1-x)-a) )
    graphics::curve( ROC_curve(a,b,x) )
  }

  if(!missing(dataList)&&missing(StanS4class)){
    NI<-dataList$NI
    NL<-dataList$NL
    metadata<-metadata_srsc_per_image(dataList,ModifiedPoisson = F)
    falsealarmExtended<- metadata$falsealarmExtended
    hitExtended  <- metadata$hitExtended
    FPF <- cumsum(falsealarmExtended)/NI
    TPF <- cumsum(hitExtended)/NL
    FPF<-c(0,FPF)
    TPF<-c(0,TPF)
    # FPF <- cumsum(rev(falsealarmExtended))
    # TPF <- cumsum(rev(hitExtended))
    suppressWarnings(graphics::par(new=TRUE));plot(FPF,TPF, cex =6, xlim=c(0,1), ylim=c(0,1))
  }



  if(!missing(StanS4class)){

    if(!missing(dataList))  message("We ignore the variable \"dataList\", and use a dataset which was used to be fitted a model, instead.")

    f<-StanS4class
    dataList <-f@dataList
    a<-extract_EAP_by_array(f,name.of.parameter ="a")
    b<-extract_EAP_by_array(f,name.of.parameter ="b")

    NI<-dataList$NI
    NL<-dataList$NL
    dark_theme()
    graphics::curve(1- Phi(b*Phi_inv(1-x)-a) )
    graphics::curve( ROC_curve(a,b,x) )
    metadata<-metadata_srsc_per_image(dataList,ModifiedPoisson = F)
    falsealarmExtended<- metadata$falsealarmExtended
    hitExtended  <- metadata$hitExtended
    FPF <- cumsum(falsealarmExtended)/NI
    TPF <- cumsum(hitExtended)/NL
    FPF<-c(0,FPF)
    TPF<-c(0,TPF)
    # FPF <- cumsum(rev(falsealarmExtended))
    # TPF <- cumsum(rev(hitExtended))
    suppressWarnings(graphics::par(new=TRUE));plot(FPF,TPF, cex =6, xlim=c(0,1), ylim=c(0,1))

  }






}



#' Title
#'
#' @inheritParams fit_Bayesian_FROC
#' @inheritParams DrawCurves
#' @param plot_empirical_curves A logical, if it is true, then the empirical curve is drawn in the same plane.

# @return
#' @export
#'
# @examples
draw_ROC_Curve_from_fitted_model  <-  function(StanS4class,
                                               plot_empirical_curves=FALSE) {


  f<-StanS4class
  dataList <-f@dataList




  a<-extract_EAP_by_array(f,name.of.parameter ="a")
  b<-extract_EAP_by_array(f,name.of.parameter ="b")

  NI<-dataList$NI
  NL<-dataList$NL
  dark_theme()
  x<-1# To avoid the NOTE in R CMD check which said that no visible binding for global  variable 'x'  Undefined global functions or variables:  x
  graphics::curve(1- Phi(b*Phi_inv(1-x)-a) )
  graphics::curve( ROC_curve(a,b,x),xlab ="",ylab="",main="ROC curve" )
  metadata<-metadata_srsc_per_image(dataList,ModifiedPoisson = F)
  falsealarmExtended<- metadata$falsealarmExtended
  hitExtended  <- metadata$hitExtended
  FPF <- cumsum(falsealarmExtended)/NI
  TPF <- cumsum(hitExtended)/NL
  FPF<-c(0,FPF)
  TPF<-c(0,TPF)
  # FPF <- cumsum(rev(falsealarmExtended))
  # TPF <- cumsum(rev(hitExtended))
  dark_theme(4)

  suppressWarnings(graphics::par(new=TRUE));plot(FPF,TPF, cex =6, xlim=c(0,1), ylim=c(0,1),xlab ="",ylab="")
  suppressWarnings(graphics::par(new=TRUE));plot(FPF,TPF, cex =2, xlim=c(0,1), ylim=c(0,1),xlab ="FPF",ylab="TPF")
  suppressWarnings(graphics::par(new=TRUE));plot(c(0,1),c(0,1), type = "l",xlab ="",ylab="", xlim=c(0,1),ylim=c(0,1),lty ="dashed")
   if(plot_empirical_curves) {
     # dark_theme(2)

     plot_ROC_empirical_curves(
    Number_of_cases =  f@dataList$NL,
    Number_of_non_cases =  f@dataList$NI,
    frequencies_of_non_cases   =  f@dataList$f,
    frequencies_of_cases  =f@dataList$h,
    new.imaging.device=plot_empirical_curves
     )

     }

  }









#' @title  fit a model to data in the case of
#' A Single reader and A Single modality (srsc).
#'@description  Build a \emph{fitted model object}  in case of  \strong{single reader
#'and single modality} data \code{dataList}. FPF is \strong{per image}.
#'@details Revised 2019.Jun. 17
#'@param dataList.Name This is not for user, but the author for this package development.
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves_MRMC_pairwise
#'@param dataList A list, to be fitted a model.
#'For example, in case of a single reader and a single modality,
#'it consists of  \code{f, h, NL, NI, C}.
#'The detail of these dataset,
#' see the example data-sets.
#'Note that the maximal number of confidence level,
#' denoted by  \code{C}, are included,
#' however,
#' should not include its each confidence level in \code{dataList}
#'
#' @param multinomial A logical, if \code{TRUE} then model is the most classical one using multinomial distribution.
#'@return An S4 object of class \code{stanfitExtended},
#'which is an inherited S4 class from \code{stanfit}.
#'
#'To change the S4 class, use
#'
#'@examples
#' \dontrun{
#'#First, prepare the example data from this package.
#'
#'
#'
#'           dat  <- get(data("dataList.Chakra.1"))
#'
#'
#'
#'
#'#Second, fit a model to data named "dat"
#'
#'
#'
#'
#'
#'            fit <-  fit_srsc(dat)
#'
#'
#'
#'
#'
#'
#'
#' #      Close the graphic device to avoid errors in R CMD check.
#'
#'          Close_all_graphic_devices()
#'
#'
#'
#'}# dottest
#' @export
#'
#'
#___________________------

fit_srsc_ROC <- function(
  dataList,
  prior = -1,
  new.imaging.device=TRUE,
  dataList.Name = "",
  ModifiedPoisson = FALSE,
  model_reparametrized =FALSE,
  verbose = FALSE,
  type_to_be_passed_into_plot ="l",
  multinomial = FALSE,

  DrawCurve = TRUE,
  PreciseLogLikelihood = TRUE,
  Drawcol = TRUE,
  # make.csv.file.to.draw.curve=FALSE, #Closed at 2021 25 October,
  mesh.for.drawing.curve=10000,
  summary =TRUE,
  DrawFROCcurve=TRUE,
  DrawAFROCcurve=FALSE,
  DrawCFPCTP=TRUE,
  cha = 4,
  ite = 3000,
  dig = 5,
  war = floor(ite/5),
  see = 1234,

  prototype = FALSE,

  # prior ----

  ww=-0.81,
  www =0.001,
  mm=0.65,
  mmm=0.001,
  vv=5.31,
  vvv=0.001,
  zz= 1.55,
  zzz=0.001,

  # zz= 2.55,
  ...

){

  if(ModifiedPoisson==TRUE) NX <- dataList$NL
  if(ModifiedPoisson==FALSE)NX <-dataList$NI
  if(summary==TRUE) {  viewdata(dataList )}

  data <- metadata_srsc_per_image(dataList,ModifiedPoisson)



  #
  #   ww=-0.81;
  #   www =0.001;
  #   mm=0.65;
  #   mmm=0.001;
  #   vv=5.31;
  #   vvv=0.001;
  #   zz= 1.55;
  #   zzz=0.001;



  # data ----

  data <- c(data,
            prior=prior,
            PreciseLogLikelihood=PreciseLogLikelihood,
            ModifiedPoisson=ModifiedPoisson,
            # prior ----
            ww=ww,
            www =www,
            mm=mm,
            mmm=mmm,
            vv=vv,
            vvv=vvv,
            zz= zz,
            zzz=zzz
  )
  C <- as.integer(data$C)
  f <- data$f
  h <- data$h
  NI <- data$NI
  NL <- data$NL

  ff <- data$ff

  hh <- data$hh
  # scr <- "Model_srsc_per_image.stan";
  #base::system.file is not go well
  # model ----

  scr <-  system.file("extdata", "Model_ROC.stan", package="BayesianFROC")
  scrr <-  system.file("extdata", "Model_ROC.rds", package="BayesianFROC")

  scr <-  system.file("extdata", "Model_ROC2.stan", package="BayesianFROC")
  scrr <-  system.file("extdata", "Model_ROC2.rds", package="BayesianFROC")



  # scr <-  system.file("extdata", "Model_srscVer2.stan",             package="BayesianFROC")
  # scrr <-  system.file("extdata", "Model_srscVer2.rds",             package="BayesianFROC")
  #
  # if(prototype)  {
  #   scr <-  system.file("extdata", "Model_srsc_prototype.stan",   package="BayesianFROC")
  #   scrr <-  system.file("extdata", "Model_srsc_prototype.rds",   package="BayesianFROC")
  # }
  # if(multinomial){
  #   scr <-  system.file("extdata", "Model_srsc_multinomial.stan", package="BayesianFROC")
  #   scrr <-  system.file("extdata", "Model_srsc_multinomial.rds", package="BayesianFROC")
  # }
  # # if (model_reparametrized)  scr <-  system.file("extdata", "Model_srsc_reparametrized.stan", package="BayesianFROC")
  # if (model_reparametrized){
  #   scr <-  system.file("extdata", "Model_srsc_prior.stan", package="BayesianFROC")
  #   scrr <-  system.file("extdata", "Model_srsc_prior.rds", package="BayesianFROC")
  # }
  initial <-c("m"=1,"v"=5,"w"=0,"dz"=1/2)
  #
  # rstan::rstan_options(auto_write = TRUE)
  if(scrr=="")message("Now, ahem, the model is being compiled. Oh my gosh! It tooks a few minuites, wait ..., I love you...")
  if(!(scrr==""))message("Already, ahem, the model has been compiled. But...dang it!")

  scr <- rstan::stan_model(scr)# add

  # fit____sampling -----

  if (summary==FALSE) {


    invisible(utils::capture.output(
      fit  <-  rstan::sampling(
        object  = scr,
        data    = data,
        verbose = FALSE,
        seed    = see,
        chains  = cha,
        warmup  = war,
        iter    = ite,
        control = list(
          adapt_delta   = 0.9999999,
          max_treedepth = 15),
        init    = initial,...
      )
    ))
  }#if


  if (summary==TRUE) {

    fit  <-  rstan::sampling(
      object  = scr,
      data    = data,
      verbose = FALSE,
      seed    = see,
      chains  = cha,
      warmup  = war,
      iter    = ite,
      control = list(
        adapt_delta = 0.9999999,
        max_treedepth = 15),
      init = initial,...
    )

  }#if




  rstan::check_hmc_diagnostics(fit)
  cat("\nMax R hat: \n")
  message(  paste( R_hat_max(fit) , crayon::silver(" achieved by the param \"",name_of_param_whose_Rhat_is_maximal(fit), "\"")  ,sep = "")  )
  check_rhat(fit)


  convergence <- ConfirmConvergence(fit,summary = summary)
  if(convergence ==FALSE)message("\n* So, model is unreliable!!\n")

  # fit.new.class <- methods::as(fit,"stanfitExtended")
  # fit.new.class@metadata <-data
  # fit.new.class@dataList <-dataList
  # # slot -------------------
  # if(!ModifiedPoisson)  fit.new.class@studyDesign <-  "srsc.per.image"
  # if(ModifiedPoisson)  fit.new.class@studyDesign <-  "srsc.per.lesion"
  #
  # # if(PreciseLogLikelihood==TRUE) {fit.new.class@WAIC <- waic(fit,dig,summary=FALSE)}
  # fit.new.class@convergence    <-  convergence
  # # fit.new.class@chisquare <- chisquare
  # fit.new.class@PreciseLogLikelihood    <-  PreciseLogLikelihood
  # fit.new.class@prototype    <-  prototype



  # return(fit.new.class)
  # }
  # if(convergence ==TRUE){   if(summary==TRUE) message(crayon::silver("\n* We do not stop, since model converged.\n"))}

  if(summary==TRUE) {print(fit )}
  if(summary==FALSE) {  message(crayon::silver("\n* summary = TRUE for more details.\n"))}



  if(PreciseLogLikelihood == FALSE  ){
    if(summary==TRUE) message(crayon::silver("\n* WAIC was not caluculated, since log likelihood is not a precise value."))
  }else{
    if(PreciseLogLikelihood == TRUE  ){
      if(summary==TRUE)  message(crayon::silver("\n* WAIC was caluculated,\n since log likelihoodis is a precise value, i.e., the traget += statement are used in the stan file."))
      waic <-waic(fit,dig,summary=verbose)
    } else{
      print("* PreciseLogLikelihood is allowed only two choice; TRUE or FALSE.")
    }}

  MCMC=(ite-war)*cha
  # EAP_a <-  array(0, dim=c(  MCMC))
  # EAP_b <-  array(0, dim=c(  MCMC))
  # EAP_a <- 0
  # EAP_b <- 0
  # s<-0
  # t<-0
  # for(mc in 1:MCMC){
  #   s<-  EAP_a
  #   EAP_a <-  s+ a[mc]
  #   t<-  EAP_b
  #   EAP_b <-  t+ b[mc]
  # }
  # EAP_a<-EAP_a/MCMC
  # EAP_b<-EAP_b/MCMC

  # MCMC=(ite-war)*cha
  # #--------- chi ^2 -----------Start
  # p<-rstan::extract(fit)$p
  # lchi<-rstan::extract(fit)$l
  # EAP_p <-  array(0, dim=c(   C))
  # EAP_l <-  array(0, dim=c(   C+1))
  #
  # s <-  array(0, dim=c(   C))
  # t <-  array(0, dim=c(   C+1))
  # for(mc in 1:MCMC){
  #   for(cd in 1:C){
  #     s[ cd]<-  EAP_p[ cd]
  #
  #     EAP_p[ cd] <-  s[ cd]+ p[mc,cd]
  #   }
  #   for(cd in 0:C){
  #     t[ cd]<-  EAP_l[ cd]
  #     EAP_l[ cd] <-  t[ cd]+ lchi[mc,cd]
  #   }
  # }
  # EAP_p<-EAP_p/MCMC
  # EAP_l<-EAP_l/MCMC
  #
  # ss<-vector()
  # tt<-vector()
  # for(cd in 1:C){
  #
  #   ss[cd]<-(h[C+1-cd]-NL*EAP_p[cd])^2/(NL*EAP_p[cd])
  #   tt[cd]<-(f[C+1-cd]-NI*(EAP_l[cd]-EAP_l[cd+1]))^2/(NI*(EAP_l[cd]-EAP_l[cd+1]))
  #
  # }
  # chisquare <- sum(ss)+sum(tt)
  ###

  e <-extract_EAP_CI(fit,"l",dataList$C ,summary = FALSE)
  lambda <- e$l.EAP

  e <-extract_EAP_CI(fit,"p",dataList$C ,summary = FALSE)
  p <- e$p.EAP
  chisquare <-   chi_square_goodness_of_fit_from_input_all_param(

    h   =   h,
    f   =   f,
    p   =   p,
    lambda  =   lambda,
    NL  =   NL,
    NI  =   NI,
    ModifiedPoisson =  ModifiedPoisson
  )
  chisquare <- signif(chisquare,digits = dig)
  #Draw the  AFROC curve-----
  set.seed(1);ll<- stats::rchisq(mesh.for.drawing.curve, 1)
  lll<- 0.99+ll

  # for near 0 and 1, FROC cure are parse, if usual points, so we generate points with very large weights for 0 and 1
  l0<-pracma::logspace(-0.5, -222, mesh.for.drawing.curve)
  l2<-pracma::linspace(0, 1.5, mesh.for.drawing.curve)
  l3<-pracma::logspace(0,3, mesh.for.drawing.curve)


  l4<-append(l0,l2)
  la<-append(l4,l3)

  lb<-append(ll,lll)

  l <- append(la,lb)
  l<-sort(l, method = "shell", index.return = FALSE)

  x<- 1-exp(-l) #AFROC
  y <-  array(0, dim=c(length(x)))



  a<-rstan::extract(fit)$a
  b<-rstan::extract(fit)$b
  EAP_a<-mean(a)
  EAP_b<-mean(b)


  for(i in 1:length(x)) y[i]<-1-stats::pnorm(EAP_b*stats::qnorm(exp(-l[i]))-EAP_a)


  # if(DrawCurve == FALSE  ||DrawCurve == FALSE){
  #   if(summary==TRUE)  message(crayon::silver(" \n We de not draw the FROC and AFROC curves. \n"))
  # }






  #     if(summary ==TRUE){
  #   message("\n--------------------------------------------------  \n")
  #   message("* The goodness of fit chi-square statistic is equal to ",  signif(chisquare,digits = dig),". \n")
  #   message("\n* The representation of goodness of fit chi-square statistic is given in the Chakraborty's paper; Med Phys. 1989 Jul-Aug;16(4):561-8. Maximum likelihood analysis of free-response receiver operating characteristic (FROC) data. Chakraborty DP. It is also given in the author's paper.\n")
  #   message("--------------------------------------------------  \n")
  #   message("\n The expected a posterior estimate of the area under the FROC curve: \n \n ")
  #   pasteAUC <- paste("AUC =  ",  signif(summary(fit)$summary["A","mean"],digits = dig), " \n" )
  #   message(pasteAUC)
  #   message("\n The 95% Credible Interval of AUC := [ lower bound, upper bound] is the following:\n \n ")
  #   message("The 95%CI = [",signif( summary(fit)$summary["A","2.5%"],digits = dig), ",",signif(summary(fit)$summary["A","97.5%"],digits = dig), "]." )
  #   message("\n--------------------------------------------------  \n")
  # }




  fit.new.class <- methods::as(fit,"stanfitExtended")
  fit.new.class@metadata <-data
  fit.new.class@dataList <-dataList
  # slot -------------------
  if(!ModifiedPoisson)  fit.new.class@studyDesign <-  "srsc.per.image"
  if( ModifiedPoisson)  fit.new.class@studyDesign <-  "srsc.per.lesion"
  if(PreciseLogLikelihood==TRUE) {fit.new.class@WAIC <- waic(fit,dig,summary=FALSE)}
  fit.new.class@convergence    <-  convergence
  fit.new.class@chisquare <- chisquare

  #slot ----

  fit.new.class@PreciseLogLikelihood    <-  PreciseLogLikelihood
  fit.new.class@ModifiedPoisson   <- ModifiedPoisson
  fit.new.class@prototype    <-  prototype
  fit.new.class@multinomial    <-  multinomial


  # e <- rstan::extract(fit)
  ee<-extract_EAP_CI(StanS4class = fit,parameter.name = "p_value_logicals",dimension.of.parameter = 1,summary = FALSE )
  p.value <- ee$p_value_logicals.EAP
  fit.new.class@posterior_predictive_pvalue_for_chi_square_goodness_of_fit    <-  p.value




  if(verbose){# print estimates -----
    summary_EAP_CI_srsc(
      StanS4class=fit.new.class,
      dig=dig
    )
  }#if
  extractAUC(
    StanS4class=fit.new.class,
    # summary=summary,
    summary =   verbose,
    dig=dig
  )


  drawdata <- data.frame(x.AFROC =x,
                         y.AFROC=y,
                         x.FROC= l,
                         y.FROC=y )

  #Closed at 2021 25 October, because installation of pkg xlsx is difficult. (rJava is also difficult)
  #The code will work fine but, to reduce dependencies, I have to omit the following from here by #.

  # if(make.csv.file.to.draw.curve==TRUE){
  #   message("\n\n* Please wait ... now we launch two scv files to draw your FROC curve and cumulative hits and false alarms")
  #   #Launch the Draw data---START
  #   xlsx::write.xlsx (drawdata, paste(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/DrawData.xlsx", sep = ""),  col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE)
  #   message("* A DrawData.csv are created in your desktop. \n* Using this csv file, you can draw the FROC and AFROC curves by scatter plot.")
  #   drawTPFP <- data.frame(NumberOfCumulativeFalsePositives =ff,
  #                          NumberOfCumulativeTurePositives=hh)
  #   xlsx::write.xlsx (drawTPFP, paste(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"/DrawPoints.xlsx",  sep = ""),col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE)
  #   message("\n* A DrawPoints.csv are created in your desktop. \n")
  #   message("\n* Using this csv file you can plot cumlative false positives and cumulative true positives by scatter plot.")
  # }
  #Closed at 2021 25 October,




  #Launch the Draw data---STOP
  fit.new.class@plotdata <-drawdata



  if(!sum(rstan::get_divergent_iterations(fit))==0){

    message("\n* Divergence:",    sum(rstan::get_divergent_iterations(fit)) )
    message("\n* Iterations:",    length(rstan::get_divergent_iterations(fit)) )# = cha*(ite-war)
    message("\n* Rate: ", 100*sum(rstan::get_divergent_iterations(fit))/length(rstan::get_divergent_iterations(fit)),"% \n")
  }


  fit.new.class@Divergences      <- sum(rstan::get_divergent_iterations(fit))
  fit.new.class@MCMC.Iterations       <- length(rstan::get_divergent_iterations(fit))
  fit.new.class@Divergence.rate  <- 100*sum(rstan::get_divergent_iterations(fit))/length(rstan::get_divergent_iterations(fit))
  if ( dataList.Name==""   ) dataList.Name <-  deparse(substitute(dataList))
  fit.new.class@dataList.Name <- dataList.Name

  # here ----

  if(DrawCurve   )DrawCurves(fit.new.class, Colour = TRUE, new.imaging.device = T,type_to_be_passed_into_plot=type_to_be_passed_into_plot)
  if(summary  ){size_of_return_value(summary=summary,object =  fit.new.class); print(format(utils::object.size(fit.new.class), units = "auto"))}
  invisible(fit.new.class)

}



#
# ll<- stats::rchisq(100, 1)
# lll<- 0.99+ll
# l<-append(ll,lll)
#
# x<- 1-exp(-l)
# y <- 1-stats::pnorm(0.3*stats::qnorm( exp(-l ) )-0.5)
#
# x <- c(0, x, 1)
# y <- c(0, y, 1)
#
# plot(x,y,xlim=c(0,1),ylim=c(0,1))




