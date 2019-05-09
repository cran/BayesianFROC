#' @title  Print summary for AUC comparisons for MRMC without color
#'
#'
#'@description  This is print the results of AUC comparison for MRMC data.
#'@inheritParams DrawCurves_MRMC_pairwise
#'@inheritParams fit_Bayesian_FROC
#' @export summary_AUC_comparison_MRMC_without_crayon
summary_AUC_comparison_MRMC_without_crayon <- function(
   StanS4class,
  significantLevel=0.8,
  dig=3
){
  fit <-StanS4class
  M<-fit@dataList$M
  fit <- methods::as(StanS4class, "stanfit")

   message("************************************************************** \n")
  message("** Comparison of modalities by two characteristics!!        ** \n")
  message("**                                                          ** \n")
  message("** 1) Probabilities of one AUC is greater than another AUC  ** \n")
  message("**                                                          ** \n")
  message("** 2) Difference Estimates of AUC by Expected A Posterior   ** \n")
  message("**                                                          ** \n")
  message("************************************************************** \n")

  explanation.Probb <-vector()
  Probb <-vector()
  s<-0
  for(md in 1:M){for(mmd in 1:M){if(md <mmd){
  s<-s+1
    message("\n * The expected a posterior probability that the AUC of modality (", md,") is greater than or equal to the AUC of ( ", mmd, " )is equal to (",signif(mean(extract(fit)$Prob_diff_A[,md,mmd]),digits = dig), ").\n")
    explanation.Probb[s] <-paste("* The expected a posterior probability that the AUC of modality (", md,") is greater than or equal to the AUC of ( ", mmd, " ) =")
    Probb[s] <-signif(mean(extract(fit)$Prob_diff_A[,md,mmd]),digits = dig)

    }}}
  message("\n* Summarizing the above: ")
  explanation.prob <-vector()
  prob <-vector()
  ss <-0
  for(md in 1:M){for(mmd in 1:M){if(md <mmd){
    ss<- ss+1
    explanation.prob[ss] <- paste(" * Prob( AUC of modality", md," > AUC of modality", mmd, " ) = ")
    prob[ss] <- signif(mean(extract(fit)$Prob_diff_A[,md,mmd]),digits = dig)

    message("* Prob( AUC of", md," > AUC of ", mmd, " ) = ",signif(mean(extract(fit)$Prob_diff_A[,md,mmd]),digits = dig), ".\n")



    }}}


  for(md in 1:M){for(mmd in 1:M){if(md <mmd){
    message("\n * The expected a posterior estimate of the AUC of modality (", md,") minus the AUC of modality ( ", mmd, " )is equal to (",signif(mean(extract(fit)$diff_A[,md,mmd]),digits = dig), ").\n")
  }}}
  message("\n* Summarizing the above: ")
  message("\n*  AUC of modality   -  AUC of modality   =  EAP-estimate  (Probability), where Probaility =  The probability of the event that one AUC is greater than another AUC.")
  for(md in 1:M){for(mmd in 1:M){if(!md==mmd){
    message("\n *  AUC of", md,"  -  AUC of ", mmd, "  = ",signif(mean(extract(fit)$diff_A[,md,mmd]),digits = dig),"(",signif(mean(extract(fit)$Prob_diff_A[,md,mmd]),digits = dig), ") .\n")
  }}}



   message("************************************************************** \n")
  message("**       Siginificant Result                                ** \n")
  message("**                                                          ** \n")
  message("**       Siginificant Level =   ",significantLevel,"          \n")
  message("**                                                          ** \n")
  message("************************************************************** \n")


  explanation.Diff <-vector()
  Diff <-vector()

  message("\n* Extract high probabilities with significant level =", significantLevel,"from the above: ")
  ssss<-0
  for(md in 1:M){for(mmd in 1:M){if(!md==mmd &&mean(extract(fit)$Prob_diff_A[,md,mmd])>significantLevel){
    ssss<-ssss+1
    explanation.Diff[ssss] <- paste("*  AUC of", md,"  -  AUC of ", mmd, "  = ")
    Diff[ssss]<-signif(mean(extract(fit)$diff_A[,md,mmd]),digits = dig)
    message("\n *  AUC of", md,"  -  AUC of ", mmd, "  = ",signif(mean(extract(fit)$diff_A[,md,mmd]),digits = dig),"(",signif(mean(extract(fit)$Prob_diff_A[,md,mmd]),digits = dig), " > Significant Level =",significantLevel,") .\n")
  }}}

  if(ssss==0){ return(message("\n* There is no modality pairs such that the difference is significant.\n"))}

  message("That is:")
  for(md in 1:M){for(mmd in 1:M){if(!md==mmd &&mean(extract(fit)$Prob_diff_A[,md,mmd])>significantLevel){
    message("\n *  The difference of the AUC of modality", md,"  -  the AUC of modality", mmd, "  is equal to ",signif(mean(extract(fit)$diff_A[,md,mmd]),digits = dig)," by Expected a posterior estimate and the probability that the AUC of",md," is greater than the AUC of" ,mmd, "is equal to",signif(mean(extract(fit)$Prob_diff_A[,md,mmd]),digits = dig), ".\n")
  }}}

 invisible( list(
                     Long.Expected.A.Posterior.Probability =data.frame( explanation.Probb=explanation.Probb, Probb=Probb  ) ,
                  Short.Expected.A.Posterior.Probability=data.frame(    explanation.prob=explanation.prob,prob=prob ),
            Short.Expected.A.Posterior.Diff=data.frame(    explanation.Diff=explanation.Diff,Diff=Diff ))


            )



 }# function




