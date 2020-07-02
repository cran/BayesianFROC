#' @title  Fit and Draw the FROC models (curves) version2.
#'@description  Fit and Draw the FROC models (curves).
#'This model is aimed to draw a free-response ROC curves for multiple readers and a single modality.
#'@inheritParams DrawCurves_MRMC_pairwise
#'@inheritParams fit_Bayesian_FROC
#'
#'
# sum(exp(rstan::extract(stan.MRMC)$lp__))/(10000-3000)
# devtools::use_package("base")# this will cause error, do not run!!
# devtools::use_package("rstan")
# devtools::use_package("knitr")
# devtools::use_package("readxl")
# devtools::use_package("openxlsx")
# devtools::use_package("xlsx")
#'@seealso
#' \strong{Example data:}
#'
#'
#'BayesianFROC::dataList.one.modality
#'
#'This dataset is a single modality dataset with multiple readers.

# @importFrom base system.file
# devtools::document();help("fit_MRMC_versionTWO") # Confirm reflection
#'@examples
#'\dontrun{
#'
#'#================The first example======================================
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####


#'#(1)First, we prepare the data from this package.
#'
#'     dat  <- BayesianFROC::dataList.one.modality
#'
#'
#'
#'#(2)Second, we run fit_Bayesian_FROC() in which the rstan::stan() is implemented.
#'#with data named "dat"  and the author's Bayesian model.
#'
#'
#'  #        fit <-  fit_MRMC_versionTWO(dat,see = 12,ite=111)
#'
#'  #      It needs a lot of memory and so, in this example we take the small iteration,
#'  #      i.e., ite =2222. However if user execute this, then the ite =30000 is recommended
#'  #      for getting reliable estimates.
#'
#'
#'
#'
#'
#'
#'
#'#Note that we change the seed from default to 12 to get a convergence model.
#'#If users enconter the convergence issues,
#'#then please consider changing the seed like this example.
#'
#'#The resulting FROC curve means the summarizing curve over all readers
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#================The Second example======================================

#'#(1)First, we prepare the data from this package.
#'
#'         dat  <- BayesianFROC::dataList.Chakra.Web
#'
#'
#'
#'#(2)Second, we run fit_Bayesian_FROC() in which the rstan::stan() is implemented.
#'#with data named "dat"  and the author's Bayesian model.
#'
#'
#'   #       fit <-  fit_MRMC_versionTWO(dataList.Chakra.Web ,ite=111)
#'

#'#The resulting FROC curve means the summarizing curve over all readers
#'
#'  #      It needs a lot of memory and so, in this example we take the small iteration,
#'  #      i.e., ite =2222. However if user execute this, then the ite =30000 is recommended
#'  #      for getting reliable estimates.
#'
#'
#'
#'
#'
#'
#'
#'#'#      Close the graphic device to avoid errors in R CMD check.
#'
#'if (!grDevices::dev.cur()>=2) {
#'
#'     for (i in 1:grDevices::dev.cur()-1) {message("The",i,"-th graphic device is omitted.")
#'     grDevices::dev.off()
#'      }
#'      }
#'
#'
#'      }#\dontrun








#' @export fit_MRMC_versionTWO
#' @param version 2 or 3

fit_MRMC_versionTWO<- function(
  dataList,
  DrawFROCcurve = TRUE,
  DrawCFPCTP=TRUE,
  version = 2,
  mesh.for.drawing.curve=10000,
  significantLevel = 0.7,

  cha = 1,
  war = floor(ite/5),
  ite = 10000,
  dig = 5,
  see = 1234569)
{
  viewdata(dataList ) # I do not know, but this code is the availble only in the last part.




  if(version == 2  ){
    scr <- system.file("extdata", "Model_Hiera_versionTWO.stan", package="BayesianFROC")
  }else{
    if(version == 3  ){
    scr <-  system.file("extdata", "Model_Hiera_versionTHREE.stan", package="BayesianFROC")
    } else{
      print("version is allowed only two choice; 2 or 3")
    }}

  data <-metadata_to_fit_MRMC(dataList)



  m<-data$m   ;S<-data$S;  NL<-data$NL;c<-data$c;q<-data$q;
  h<-data$h; f<-data$f;
  hh<-data$hh; hhN<-data$hhN;
  ff<-data$ff;ffN<-data$ffN;
  harray<-data$harray;    farray<-data$farray;
  hharray<-data$hharray;    ffarray<-data$ffarray;
  hharrayN<-data$hharrayN;    ffarrayN<-data$ffarrayN;

  C<-as.integer(data$C)
  M<-as.integer(data$M)
  N<-as.integer(data$N)
  Q<-as.integer(data$Q)


  ll<- stats::rchisq(mesh.for.drawing.curve, 1)
  lll<- 0.99+stats::rchisq(mesh.for.drawing.curve, 1)
  l<-append(ll,lll) #This name l is duplicated!! CAUTION!!
  x<-list(x=l,mesh=mesh.for.drawing.curve)
  ext.data <- c( data,x )

  rstan_options(auto_write = TRUE)
  fit  <- stan(file=scr, model_name=scr, data=ext.data, verbose = TRUE,
               seed=see, chains=cha, warmup=war,
               iter=ite, control = list(adapt_delta = 0.9999999,
                                        max_treedepth = 15)
  )


  convergence <- ConfirmConvergence(fit)
  if(convergence ==FALSE){message("\n* So, model has no mean, we have to finish a calculation !!\n")
    return(fit)}
  if(convergence ==TRUE){message("\n* We continue the procedure, since model cannot be said not converged.\n")}




  message("---------- Useage of the return value-------------------------  \n")
  message("\n * Using this return value which is S4 class generated by rstan::stan and another function in this package, you can draw FROC and AFROC curves.   \n")
  message("\n * Using this return value, you can apply functions in the package rstan, e.g., rstan::traceplot().   \n")
  message("\n-----------------------------------------  \n")

  message(" \n* Now, curve are drawing ...  \n")



  if(  DrawFROCcurve == TRUE|| DrawCFPCTP==TRUE){grDevices::dev.new()}
  if( !( DrawFROCcurve == TRUE|| DrawCFPCTP==TRUE) ){message("\n* We do not draw anything according to your input.\n")}





  #Draw the  AFROC curve~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  MCMC=(ite-war)*cha  #Total Samples By MCMC

  x<- 1-exp(-l)   #AFROC-Curve

  EAP_a <- rstan::get_posterior_mean(fit,par=c("a"))
  EAP_a <- apply(EAP_a, 1, mean) # mean over chains

  EAP_b <- rstan::get_posterior_mean(fit,par=c("b"))
  EAP_b <- apply(EAP_b, 1, mean) # mean over chains



  #####################################
 y<-  array(0, dim=c(length(l),  M)) #
  for(md in 1:M){
    # message("|")#Processsssssss
      y[ ,md]<-1-stats::pnorm(EAP_b[md] *stats::qnorm(exp(-l ))-EAP_a[md] )#  y[ld,md]<-1-stats::pnorm(EAP_b[md] *stats::qnorm(exp(-l[ld]))-EAP_a[md] )
    # message(paste("", ceiling(round(md/M,2)*100/2 +50),"% \n"))#Processsssssss
  }

  graphics::par(bg= "gray12", #"gray27",#"gray40",#"black",# ,
                fg="gray",
                col.lab="bisque2" ,#"bisque" ,#  "antiquewhite1",
                col.axis="bisque2" ,##"bisque" "antiquewhite1",
                col.main="bisque2" ,
                cex.lab=1.5,
                cex.axis=1.3
  );
  Colour1 <-  array(0, dim=c( 20)) #array(0, dim=c( M))
  Colour2 <-  array(0, dim=c( M)) #
  Colour1[1]<-"antiquewhite1" # "gray0"  #"orange3"
  Colour1[2]<-"brown1"  #"orchid"
  Colour1[3]<-"dodgerblue1" #"coral1" #"deeppink4"  #"firebrick4"
  Colour1[4]<-"orange2"  #"aquamarine1"  #"darkcyan"
  Colour1[5]<-"yellowgreen" #"blue4" #"deeppink4"  #" cyan4 " #"mediumvioletred" # "green4"##"darkgoldenrod4"
  Colour1[6]<-"khaki1"#"darkolivegreen"
  Colour1[7]<-"darkorange4"
  Colour1[8]<-"slateblue4"
  for (cc in 9:20) {
    Colour1[cc] <- as.character(cc);
  };
  upper_x <-max(ffarrayN)
  upper_y <- max(hharrayN)
  lower_y <- min(hharrayN)
  if(DrawFROCcurve==TRUE){
    message("* Process of drawing FROC curve \n")#Processsssssss

    for(md in 1:M){
      cat("|")#Processsssssss

      #FROC
      graphics::par(new = TRUE); plot(
        l,y[,md],
        col =Colour1[md],
        bg="gray",
        fg="gray",
        xlab = 'mean of false positives per nodule',
        ylab = 'cumulative hit per nodule',
        cex= 0.1,
        xlim = c(0,upper_x ),
        ylim = c(lower_y,upper_y)


      );
      message(paste("", ceiling(round(md/M,2)*100/2 +50),"% \n"))#Processsssssss

    }#for md

  }






  if(DrawCFPCTP==TRUE){
    #CFP-CTP points
    for(md in 1:M){for(qd in 1:Q){
      if( !M ==1){

        graphics::par(new=T);plot(
          ffarrayN[,md,qd],hharrayN[,md,qd],
          xlim = c(0,upper_x ),
          ylim = c(lower_y,upper_y),
          bg="gray",
          fg="gray",
          col =Colour1[md],
          pch =paste(md),
          cex=1,# Size of Dots
          xlab = '', ylab = ''
          ,main = 'Each number of Scatter plots denotes modality ID'
        )
      }#if

      if(  M ==1){

        graphics::par(new=T);plot(
          ffarrayN[,md,qd],hharrayN[,md,qd],
          xlim = c(0,upper_x ),
          ylim = c(lower_y,upper_y),
          bg="gray",
          fg="gray",
          col =Colour1[qd],
          pch =paste(qd),
          cex=1,# Size of Dots
          xlab = '', ylab = ''
          ,main = 'Each number of Scatter plots denotes reader ID'
        )
      }#if M ==1

    }#for qd
    }#for md
  } #DrawCFPCTP==TRUE











  a<-rstan::extract(fit)$a #samples of "a" by MCMC
  b<-rstan::extract(fit)$b #samples of "b" by MCMC

  ####################
  yyy.pre <-  array(0, dim=c(length(l),MCMC,  M)) #
  var.yy <-  array(0, dim=c(length(l),  M)) #
  message("\n* Process for calculation of y coordinates of FROC curve\n")#Processsssssss
cat("/")#Adjust
    sss <-0 #dummy for process indicator
    Divisor <-100
    if(length(l)<100){ Divisor <- 1 }
    for (ld in 1:length(l)) {
      if(ld %% round(length(l)/Divisor)==0){
        sss <- sss +1

        if(sss%%10==0){  message("/  [", sss,"% ] \n")}
        if(!sss==100){cat("/")}
      }
      yyy.pre[ld,,]<- 1 - stats::pnorm(  b  *stats::qnorm(exp(-l[ld])) -a   )
    }


  yyy <-  aperm(  yyy.pre, c(2,1,3))   #zzz[1,2,3] =  zzz.pre[2,1,3]
  var.yy <- apply(yyy, c(2,3), stats::var)# var.yy[ld,md] <-stats::var(yyy[,ld,md])



      y.lower  <- y - var.yy
      y.hight  <- y  + var.yy

   for (md in 1:M) {

    graphics::par(new = TRUE);plot(l,y.lower[,md], cex= 0.05 ,
                                   col = grDevices::gray(0.4),
                                   xlim = c(0,upper_x ),
                                   ylim = c(lower_y,upper_y),
                                   xlab = '', ylab = ''
    )
    graphics::par(new = TRUE);plot(l,y.hight[,md], cex= 0.05 ,
                                   col = grDevices::gray(0.4),
                                   xlim = c(0,upper_x ),
                                   ylim = c(lower_y,upper_y),
                                   xlab = '', ylab = ''
    )
  }
  #credible curve##############################
  #credible curve########  fin    ############
  #credible curve##############################






  fit.new.class <- methods::as(fit,"stanfitExtended")
  fit.new.class@metadata <-data
  fit.new.class@dataList <-dataList
  fit.new.class@studyDesign <-  "MRMC"

  fit.new.class@ModifiedPoisson  <-  TRUE



  if(M==1){message("\n* The modality comparison procedure is omitted, since your data has only one modality.\n")}
  if(!M==1){
    # summary_AUC_comparison_MRMC_with_crayon(
    #    StanS4class=fit.new.class,
    #   significantLevel=significantLevel,
    #   dig=dig
    # )

    summarize_MRMC(fit.new.class)

  }# if(!M==1)




rstan::check_hmc_diagnostics( fit  )

  invisible(fit.new.class)

}
























#' @title  Draw FROC curves which means credible interval.
#'@inheritParams DrawCurves_MRMC_pairwise
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams plotFROC
#'@param StanS4class.fit_MRMC_versionTWO A return value of \code{fit_MRMC_versionTWO}.
#'@description Plot FROC curves based on two parameters a and b.

#' @export
Credible_Interval_for_curve <-function(dataList,
                                       StanS4class.fit_MRMC_versionTWO,
                                       mesh.for.drawing.curve=10000,
                                       upper_x=upper_x,
                                       upper_y=upper_y,
                                       lower_y=lower_y


){
  message("\n  Please wait... for credible curves...  \n")

  fit <- StanS4class.fit_MRMC_versionTWO

  if(missing(upper_x)||missing(lower_y)||missing(upper_y)){
    data <- metadata_to_fit_MRMC(dataList)
    hharrayN<-data$hharrayN;    ffarrayN<-data$ffarrayN;

    upper_x <-max(ffarrayN)
    upper_y <- max(hharrayN)
    lower_y <- min(hharrayN)
  }




  #### Credible curves ##############################
  M <-dataList$M
  Q <-dataList$Q

  ll <-     stats::rchisq(mesh.for.drawing.curve, 1)
  lll<- 0.99+stats::rchisq(mesh.for.drawing.curve, 1)
  l<-append(ll,lll) #This name l is duplicated!! CAUTION!!

  war <- fit@sim$warmup
  cha <- fit@sim$chains
  ite <- fit@sim$iter

  MCMC=(ite-war)*cha  #Total Samples By MCMC

  a<-rstan::extract(fit)$a #samples of a by MCMC
  b<-rstan::extract(fit)$b #samples of b by MCMC
  yyy <-  array(0, dim=c(MCMC,length(l),  M)) #
  var.yy <-  array(0, dim=c(length(l),  M)) #

  #pb <- txtProgressBar(min = 1, max = length(l), style = 3)
  for (md in 1:M) {
    for (ld in 1:length(l)) {     #utils::setTxtProgressBar(pb, md-1+ld)
      yyy[,ld,md]<- 1 - stats::pnorm(  b[,md] *stats::qnorm(exp(-l[ld])) -a[,md]  )
      var.yy[ld,md] <-stats::var(yyy[,ld,md]) # here is a code to help understanding -----
    }
  }


  y <-  array(0, dim=c(length(l),  M)) #
  y.lower <-  array(0, dim=c(length(l),  M)) #
  y.hight <-  array(0, dim=c(length(l),  M)) #

  EAP_a <-  array(0, dim=c( M)) #
  EAP_b <-  array(0, dim=c( M)) #


  for(md in 1:M){
    EAP_a[md] <- 0
    EAP_b[md] <- 0
    s<-0
    t<-0

    for(mc in 1:MCMC){ #EAP
      s<-  EAP_a[md]
      EAP_a[md] <-  s+ a[mc,md]
      t<-  EAP_b[md]
      EAP_b[md] <-  t+ b[mc,md]
    }
    EAP_a[md] <-EAP_a[md] /MCMC  #EAP of a
    EAP_b[md] <-EAP_b[md] /MCMC  #EAP of b
  }



  for(md in 1:M){
    for(ld in 1:length(l)){
      y[ld,md]<-1-stats::pnorm(EAP_b[md] *stats::qnorm(exp(-l[ld]))-EAP_a[md] )

      y.lower[ld,md] <- y[ld,md] - var.yy[ld,md]# here is a code to help understanding -----
      y.hight[ld,md] <- y[ld,md] + var.yy[ld,md]# here is a code to help understanding -----
    }}




  # invisible(list(x.axis.FROC = l, y.lower=y.lower,y.hight=y.hight ))

  for (md in 1:M) {


    graphics::par(new = TRUE);plot(l,y.lower[,md], cex= 0.1 ,
                                   col = grDevices::gray(0.8),
                                   xlim = c(0,upper_x ),
                                   ylim = c(lower_y,upper_y),
                                   xlab = '', ylab = ''

    )


    graphics::par(new = TRUE);plot(l,y.hight[,md], cex= 0.1 ,
                                   col = grDevices::gray(0.8),
                                   xlim = c(0,upper_x ),
                                   ylim = c(lower_y,upper_y),
                                   xlab = '', ylab = ''

    )



  }#  for (md in 1:M)
}


