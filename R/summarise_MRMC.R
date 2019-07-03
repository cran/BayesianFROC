






#' @title Summarize the estimates for MRMC case
#'
#'@inheritParams DrawCurves
#'@inheritParams fit_Bayesian_FROC

#' @return Nothing
#' @export
#'
#' @examples
#' \donttest{

#'    fit <- fit_Bayesian_FROC(
#'            dataList.Chakra.Web.orderd,
#'            ite = 1111,
#'            summary =FALSE
#'            )
#'
#'    summarize_MRMC(fit)
#'
#'
#'}# dottest

summarize_MRMC <- function(
  StanS4class,dig=3
 ){

  fit <- methods::as(StanS4class,"stanfit")
  e <- extract(fit)
  M <- StanS4class@dataList$M
  Prob <- vector()
  Diff <- vector()

  name <- vector()
  name.first.modality   <- vector()
  name.second.modality  <- vector()


  s<-0
  for (md in 1:M) {
    for (mmd in md:M) {
      if (!md==mmd) {

      s<- s+1
    Prob[s]  <-         mean( e$A[,md] > e$A[,mmd]  )
    Diff[s]  <-         mean( e$A[,md] - e$A[,mmd]  )

    name[s] <- paste( "Probablity of the event that the AUC of the ", md,"-th Modality is greater than that of the", mmd, "-th modality.")
    name.first.modality[s] <- paste(md)
    name.second.modality[s] <- paste(mmd)
      }#if md==mmd

    } }


  message("\n\n\n ---- Summarizing Tables ----")
  message(crayon::silver("\n* Probablity of the event that the AUC of the  first column Modality is greater than that of the second column modality."))
  message(crayon::silver("\n* For example, the first row in the first table implies that Probablity of the event that the AUC of the  first  Modality is greater than that of the second modality is equal to ", Prob[1], " with the posterior mean of the difference of the AUC of the first column minus that of second column is", Diff[1],".\n"))

   print(knitr::kable(
             data.frame(first.modality=name.first.modality,
                        second.modality=name.second.modality,
                        Prob=Prob,
                        Diff=Diff),format = "pandoc",digits = dig
               )
  )
  print(knitr::kable(
    data.frame(first.modality=name.second.modality,
               second.modality=name.first.modality,
               Prob=1-Prob,
               Diff=-Diff),format = "pandoc",digits = dig
  )
  )

  message("\n* Chi square goodness of fit statitic (posterior mean): ",mean(Chi_square_goodness_of_fit_in_case_of_MRMC_Posterior_Mean(StanS4class,summary=FALSE)$chi.square)    )


  names(Prob) <- name

  }
