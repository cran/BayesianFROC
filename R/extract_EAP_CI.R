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
#' \donttest{

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
#'#----------------------------------------------------------------------------------------
#'#           the following gives convergence seed 2019 Oct 12
#'#----------------------------------------------------------------------------------------
#'#'
#'
#'f <- fit_Bayesian_FROC( ite  = 1111,  cha = 1, summary = T, dataList = ddd ,see = 123456)
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
