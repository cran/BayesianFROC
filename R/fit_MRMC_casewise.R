
#' @title  Fit and Draw the FROC models (curves)
#'@inheritParams fit_Bayesian_FROC
#'@description  Fit and Draw the FROC models (curves).
#'@inheritParams fit_srsc

# devtools::use_package("base")# this will cause error, do not run!!
# @importFrom base system.file
# devtools::document();help("fit_MRMC") # Confirm reflection
#' @export
#'
# ________________ ----------------

fit_MRMC_casewise<- function(
  dataList,
  DrawCurve = FALSE,
  type_to_be_passed_into_plot="p",

  verbose = TRUE,
  print_CI_of_AUC = TRUE,
  PreciseLogLikelihood = FALSE,
  summary =TRUE,
  dataList.Name = "",
  prior=1,
  ModifiedPoisson=TRUE,
  mesh.for.drawing.curve=10000,
  significantLevel = 0.7,
  cha = 1,
  war = floor(ite/5),
  ite = 10000,
  dig = 3,
  see = 1234569,
  Null.Hypothesis=FALSE,
  prototype = FALSE,
  model_reparametrized =FALSE,
  Model_MRMC_non_hierarchical = TRUE,

  ww=-0.81,
  www =1,
  mm=0.65,
  mmm=1,
  vv=5.31,
  vvv=1,
  zz= 1.55,
  zzz=1,
  ...

){

  M <- dataList$M

  # if ( is.null(dataList$m)||is.null(dataList$q)||is.null(dataList$c)) {
  #   return(message("Dataformat error.  Your data may be not multiple reader and multiple case,
  #                  but a single reader and a singlr modality case."))
  # }
  #
  #
  # if(summary)viewdata_MRMC_casewise(dataList ) # I do not know, but this code is the availble only in the last part.
  #
  #
  # # model -----
  # if(Null.Hypothesis==FALSE){
  #   if(PreciseLogLikelihood == FALSE  ){
  #     # scr <- "Model_MRMC.stan";
  #     if(!M==1)  scr <- system.file("extdata", "Model_MRMC.stan", package="BayesianFROC")
  #     if (prototype) scr <- system.file("extdata", "Model_MRMC_prototype.stan", package="BayesianFROC")
  #   }else{
  #     if(PreciseLogLikelihood == TRUE  ){
  #       # scr <- "Model_Hiera_TargetFormulation.stan";
  #       if(!M==1)  scr <- system.file("extdata", "Model_MRMC.stan", package="BayesianFROC")
  #       if (prototype) scr <- system.file("extdata", "Model_MRMC_prototype.stan", package="BayesianFROC")
  #     } else{
  #       print("PreciseLogLikelihood is allowed only two choice; TRUE or FALSE.")
  #     }}
  # }#Null.Hypothesis
  #
  # if(Null.Hypothesis) {
  #   scr <- system.file("extdata", "null_hier.stan", package="BayesianFROC")
  #   scrr <- system.file("extdata", "null_hier.rds",  package="BayesianFROC")
  # }
  #
  # if (model_reparametrized) {
  #   scr <-  system.file("extdata", "Model_MRMC_reparametrized.stan", package="BayesianFROC")
  #   scrr <-  system.file("extdata", "Model_MRMC_reparametrized.rds",  package="BayesianFROC")
  # }
  #
  # if (Model_MRMC_non_hierarchical) {
  #   scr <-  system.file("extdata", "Model_MRMC_non_hierarchical.stan", package="BayesianFROC")
  #   scrr <-  system.file("extdata", "Model_MRMC_non_hierarchical.rds", package="BayesianFROC")
  # }
  #
  # if(M==1) {
  #   scr <- system.file("extdata", "Model_Hiera_OneModalityMultipleReader_TargetFormulation.stan", package="BayesianFROC")
  #   scrr <- system.file("extdata", "Model_Hiera_OneModalityMultipleReader_TargetFormulation.rds",  package="BayesianFROC")
  # }

  scr <-  system.file("extdata",  "Model_MRMC_Multinomial_casewise.stan", package="BayesianFROC") #2020 Nov 23
  scrr <-  system.file("extdata", "Model_MRMC_Multinomial_casewise.rds",  package="BayesianFROC") #2020 Nov 23


  # # 2020 dec 17
  # scr <-  system.file("extdata",  "Model_MRMC_Multinomial_ordered_explanatory.stan", package="BayesianFROC") #2020 Nov 23
  # scrr <-  system.file("extdata", "Model_MRMC_Multinomial_ordered_explanatory.rds",  package="BayesianFROC") #2020 Nov 23










  data <-metadata_to_fit_MRMC_casewise(dataList,ModifiedPoisson)


  # data ----
  data <- c(data,
            prior = prior,
            PreciseLogLikelihood=PreciseLogLikelihood,
            ww  = ww,
            www = www,
            mm  = mm,
            mmm = mmm,
            vv  = vv,
            vvv = vvv,
            zz  = zz,
            zzz = zzz,
            prototype=prototype
  )


  m<-data$m   ;S<-data$S;  NL<-data$NL;NI<-data$NI;c<-data$c;q<-data$q;
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

  NI_deseased <- data$NI_deseased


  if (summary==FALSE)message("\n* Now, the Hamiltonian Monte Carlo simulation is running...")
  # if (summary==FALSE)message(crayon::silver( "\n* To print the procedure, set  summary = TRUE. "))


  rstan_options(auto_write = TRUE)

  if(scrr=="")message("Now, the Stan file is being compiled and it tooks a few minuites, wait ...")
  if(!(scrr==""))message("Already, the Stan file has been compiled. But...Darn it!")

  scr <- rstan::stan_model(scr)# add

  # initial <- initial_values_specification_for_stan_in_case_of_MRMC(dataList)

  # fit____sampling -----
  # if (summary==FALSE) {

  init_fun <- function(...) list(
        w=array(0,c(NI_deseased,M,Q)),
     dz =array(1,c(C-1,NI_deseased,M,Q)),


     modalityID_dummy_array_slop_mu =array(1,c(NI_deseased,M,Q)),
     readerID_dummy_array_slop_mu =array(1,c(NI_deseased,M,Q)),
     caseID_dummy_array_slop_mu =array(1,c(NI_deseased,M,Q)),

       ground_v =1,

       modalityID_dummy_array_slop_v =array(1,c(NI_deseased,M,Q)),
       readerID_dummy_array_slop_v =array(1,c(NI_deseased,M,Q)),
       caseID_dummy_array_slop_v =array(1,c(NI_deseased,M,Q))

  )

    invisible(utils::capture.output(
      fit  <-  rstan::sampling(
        init = init_fun,
        object= scr, data=data,  verbose = FALSE,
        seed=see, chains=cha, warmup=war, iter=ite
        , control = list(adapt_delta = 0.9999999,
                         max_treedepth = 15),...
        # ,init = initial
      )
    ))
  # }#if


  # if (summary==TRUE) {
  #   # fit____sampling -----
  #   fit  <-  rstan::sampling(
  #     object= scr, data=data,  verbose = FALSE,
  #     seed=see, chains=cha, warmup=war, iter=ite
  #     , control = list(adapt_delta = 0.9999999,
  #                      max_treedepth = 15),...#,init = initial
  #   )
  #
  # }#if


  #   fit  <- stan(file=scr, model_name=scr, data=data, verbose = FALSE,
  #                seed=see, chains=cha, warmup=war,
  #                iter=ite, control = list(adapt_delta = 0.9999999,
  #                                         max_treedepth = 15)
  # )

  # rstan::check_hmc_diagnostics(fit)

  convergence <- ConfirmConvergence(fit)
  #   if(convergence ==FALSE){
  #
  #     message("\n
  # * So, model has no mean, we have to finish a calculation !!
  # * Changing seed may help, that is, variable [see = 1] or [see = 123] or [see = 12345] or ...
  #
  #             \n")
  #     return(fit)}
  #   if(convergence ==TRUE){ if(summary==TRUE)         message(crayon::silver( "\n* We do not stop, since model cannot be said not converged.\n"))
  #                          }

  # if(summary==TRUE) {
  #   # message("---------- Useage of the return value-------------------------  \n")
  #   message(crayon::silver( "\n * Using the return value which is S4 class generated by rstan::stan, you can draw FROC and AFROC curves.   \n"))
  #   message(crayon::silver( "\n * Using this return value, you can apply functions in the package rstan, e.g., rstan::traceplot().   \n"))
  #   if(PreciseLogLikelihood == FALSE  ){
  #     message(crayon::silver( "\n* WAIC did not caluculated, since log likelihood is not a precise value.\n"))
  #   }else{
  #     if(PreciseLogLikelihood == TRUE  ){
  #       message(crayon::silver( "\n* WAIC was caluculated, since log likelihoodis is a precise value, i.e., the traget += statement are used in the stan file.\n"))
  #       waic(fit)
  #     } else{
  #       print("PreciseLogLikelihood is allowed only two choice; TRUE or FALSE.")
  #     }}
  # }# if summary ==TRUE


  fit.new.class <- methods::as(fit,"stanfitExtended")
  fit.new.class@metadata <-data
  fit.new.class@dataList <-dataList
  fit.new.class@studyDesign <-  "MRMC"
  fit.new.class@PreciseLogLikelihood    <-  PreciseLogLikelihood
  fit.new.class@ModifiedPoisson    <- ModifiedPoisson
  if(PreciseLogLikelihood==TRUE) {fit.new.class@WAIC <- waic(fit,dig,summary=FALSE)}
  fit.new.class@convergence    <-  convergence
  # fit.new.class@plotdataMRMC  <-  metadata_to_DrawCurve_MRMC(fit.new.class,mesh.for.drawing.curve = mesh.for.drawing.curve)
  fit.new.class@prototype    <-  prototype

  if ( dataList.Name==""   ) dataList.Name <-  deparse(substitute(dataList))
  fit.new.class@dataList.Name <- dataList.Name


  # p value 2020 Nov 24 ----
  e <- extract(fit)
  p.value <- mean(e$p_value_logicals)
  fit.new.class@posterior_predictive_pvalue_for_chi_square_goodness_of_fit <- p.value



  # is(fit,"stanfit")
  # getClass("stanfit")


  #Change the S4 object (fit) from old class to new class


  # AUC -----
  # # if(!M==1) {
  # extractAUC(
  #   StanS4class=fit.new.class,
  #   summary=summary,
  #   dig=dig,
  #   print_CI_of_AUC = print_CI_of_AUC
  # )
  # # }
  # if(summary ==FALSE){ message(crayon::silver( "\n* To see results, summary=TRUE \n"))}
  #

  # check_rhat(fit)


  # rstan::check_hmc_diagnostics(fit)
  # cat("\nMax R hat: \n")
  # message(  paste( R_hat_max(fit) , crayon::silver(" achieved by the param \"",name_of_param_whose_Rhat_is_maximal(fit), "\"")  ,sep = "")  )
  # if(summary){size_of_return_value(summary=summary,object =  fit.new.class); print(format(utils::object.size(fit.new.class), units = "auto"))}
  # if(summary&&(!M==1)&&!model_reparametrized)summarize_MRMC(fit.new.class,dig=dig)#fit@ModifiedPoisson is used in this function
  # if(summary&&(!M==1)) sortAUC(fit.new.class)


  # if(DrawCurve == TRUE  ){
  #
  #   message(crayon::silver( "* Now, we draw the FROC and AFROC curves. \n"))
  #   message(crayon::silver( "* Please wait ... . \n"))
  #
  #
  #   # DrawCurves(    modalityID  = 1:fit.new.class@dataList$M,
  #   #                readerID    = 1:fit.new.class@dataList$Q,
  #   #                StanS4class =   fit.new.class,
  #   #                summary = FALSE,
  #   #                type_to_be_passed_into_plot=type_to_be_passed_into_plot
  #   #
  #   # )
  #
  # }
  invisible(fit.new.class)




}



