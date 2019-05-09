#' @title  Print summary for AUC comparisons for MRMC hier with color
#'
#'
#'@description  This is print the results of AUC comparison for MRMC data.
#'@inheritParams DrawCurves_MRMC_pairwise
#'@inheritParams fit_Bayesian_FROC
#' @export summary_AUC_comparison_MRMC_with_crayon
summary_AUC_comparison_MRMC_with_crayon <- function(
   StanS4class,
  significantLevel=0.8,
  dig=3
){
  fit <-StanS4class
  M<-fit@dataList$M

  fit <- methods::as(StanS4class, "stanfit")





     `%c+%` <- utils::getFromNamespace("%+%", "crayon") # changed to not break other things
     cyan <- utils::getFromNamespace("cyan", "crayon")
     blue <- utils::getFromNamespace("blue", "crayon")
     red <- utils::getFromNamespace("red", "crayon")
     yellow <- utils::getFromNamespace("yellow", "crayon")

     bold <- utils::getFromNamespace("bold", "crayon")
     blurred <- utils::getFromNamespace("blurred", "crayon")







   message("************************************************************** \n")
  message("** Comparison of modalities by two characteristics!!        ** \n")
  message("**                                                          ** \n")
  message("** 1)", crayon::cyan("Probabilities") ,"of one AUC is greater than another AUC  ** \n")
  message("**                                                          ** \n")
  message("** 2)", crayon::cyan(" Difference") ," of AUC by Expected A Posterior           ** \n")
  message("**                                                          ** \n")
  message("************************************************************** \n")

  for(md in 1:M){for(mmd in 1:M){if(md <mmd){
    # if Prob >= signi.level then Colour
    if(mean(extract(fit)$Prob_diff_A[,md,mmd]) >= significantLevel){
    message( "\n * The expected a posterior probability that the AUC of modality (",  crayon::cyan( md),") is greater than or equal to the AUC of ( ", crayon::cyan( mmd), " )is equal to (",crayon::cyan(signif(mean(extract(fit)$Prob_diff_A[,md,mmd]),digits = dig)), ").\n")
    }#if
    # if Prob >= signi.level then Black and White
    if(mean(extract(fit)$Prob_diff_A[,md,mmd]) < significantLevel){
      message(crayon::silver("\n * The expected a posterior probability that the AUC of modality (",  crayon::cyan( md),") is greater than or equal to the AUC of ( ",crayon::cyan( mmd), " )is equal to (",crayon::cyan(signif(mean(extract(fit)$Prob_diff_A[,md,mmd]),digits = dig)), ").\n"))
    }#if
      }#if
    }}# for md mmd








  for(md in 1:M){for(mmd in 1:M){if(md <mmd){
    if(mean(extract(fit)$Prob_diff_A[,md,mmd]) >= significantLevel){
      message( "\n * The expected a posterior estimate of the AUC of modality (", md,") minus the AUC of modality ( ", mmd, " )is equal to (",signif(mean(extract(fit)$diff_A[,md,mmd]),digits = dig), ").\n")
    }#if
    # if Prob >= signi.level then Black and White
    if(mean(extract(fit)$Prob_diff_A[,md,mmd]) < significantLevel){
      message(crayon::silver("\n * The expected a posterior estimate of the AUC of modality (", md,") minus the AUC of modality ( ", mmd, " )is equal to (",signif(mean(extract(fit)$diff_A[,md,mmd]),digits = dig), ").\n"))
    }#if

  }}}
  message("\n* Summarizing the above: ")
  message("\n*  AUC of modality   -  AUC of modality   =  EAP-estimate  [ Probaility =  The probability of the event that one AUC is greater than another AUC.]")
  for(md in 1:M){for(mmd in 1:M){if(!md==mmd){


    if(mean(extract(fit)$Prob_diff_A[,md,mmd]) >= significantLevel){
      message("\n *  AUC of", crayon::cyan(md),"  -  AUC of ", crayon::cyan(mmd), "  = ",crayon::cyan(signif(mean(extract(fit)$diff_A[,md,mmd]),digits = dig)),".")
      message( "   [", crayon::cyan(signif(mean(extract(fit)$Prob_diff_A[,md,mmd]),digits = dig))," = Prob( AUC of", crayon::cyan(md)," > AUC of ", crayon::cyan(mmd), " )  ]].\n")
       }#if
    # if Prob >= signi.level then Black and White
    if(mean(extract(fit)$Prob_diff_A[,md,mmd]) < significantLevel){
      message(crayon::silver("\n *  AUC of", crayon::cyan(md),"  -  AUC of ", crayon::cyan(mmd), "  = ",crayon::cyan(signif(mean(extract(fit)$diff_A[,md,mmd]),digits = dig))))
      message(crayon::silver( "   [[", crayon::cyan(signif(mean(extract(fit)$Prob_diff_A[,md,mmd]),digits = dig))," = Prob( AUC of", crayon::cyan(md)," > AUC of ", crayon::cyan(mmd), " )  ]].\n"))

      }#if


  }}}



  message("************************************************************** \n")
  message("**       Siginificant Result                                ** \n")
  message("**                                                          ** \n")
  message("**       Siginificant Level =   ",crayon::cyan(significantLevel),"          \n")
  message("**                                                          ** \n")
  message("************************************************************** \n")





  message("\n* Extract high probabilities with significant level =", significantLevel,"from the above: ")
  ssss<-0
  for(md in 1:M){for(mmd in 1:M){if(!md==mmd &&mean(extract(fit)$Prob_diff_A[,md,mmd])>significantLevel){
    ssss<-ssss+1
    message("\n *  AUC of", md,"  -  AUC of ", mmd, "  = ",crayon::yellow(signif(mean(extract(fit)$diff_A[,md,mmd]),digits = dig)),"(",crayon::cyan(signif(mean(extract(fit)$Prob_diff_A[,md,mmd]),digits = dig)), " > Significant Level =",significantLevel,") .\n")
  }}}


  ############ Table ##################
  tableA <- array(0, dim=c(ssss))
  tableB <- array(0, dim=c(ssss))
  tableC <- array(0, dim=c(ssss))
  tableD <- array(0, dim=c(ssss))
  tttt <-0
  for(md in 1:M){for(mmd in 1:M){if(!md==mmd &&mean(extract(fit)$Prob_diff_A[,md,mmd])>significantLevel){
  tttt <-tttt+1
    tableA[tttt]  <- md
    tableB[tttt]  <- mmd
    # tableC[tttt] <-crayon::yellow(signif(mean(extract(fit)$diff_A[,md,mmd]),digits = dig))
    # tableD[tttt] <- crayon::cyan(signif(mean(extract(fit)$Prob_diff_A[,md,mmd]),digits = dig))
    tableC[tttt] <- signif(mean(extract(fit)$diff_A[,md,mmd]),digits = dig)
    tableD[tttt] <- signif(mean(extract(fit)$Prob_diff_A[,md,mmd]),digits = dig)
  }}}
  table <-data.frame(modality_A=tableA, modality_B=tableB,   AUC.A_minus_AUC.B   =tableC, Prob_AUC_of_A_is_greater_than_AUC_of_B =tableD)
  # browser()

  print( knitr::kable(table, format = "pandoc"))

  ############ #########################################

  if(ssss==0){ return(message("\n* There is no modality pairs such that the difference is significant.\n"))}


  message("That is:")
  for(md in 1:M){for(mmd in 1:M){if(!md==mmd &&mean(extract(fit)$Prob_diff_A[,md,mmd])>significantLevel){
    message("\n *  The difference of the AUC of modality", md,"  -  the AUC of modality", mmd, "  is equal to ",crayon::yellow(signif(mean(extract(fit)$diff_A[,md,mmd]),digits = dig))," by Expected a posterior estimate and the probability that the AUC of",md," is greater than the AUC of" ,mmd, "is equal to",crayon::cyan(signif(mean(extract(fit)$Prob_diff_A[,md,mmd]),digits = dig)), ".\n")
  }}}


}# function
