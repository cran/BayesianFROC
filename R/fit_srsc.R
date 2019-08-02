#' @title  fit a model to data in the case of
#' Single reader and Single modality (srsc).
#'
#'@description  Build a fitted model object to data in case of  single reader
#'and single modality data \code{dataList}.
#'@details Revised 2019.Jun. 17
#'@param dataList.Name This is not for user, but the author for this package development.
#'@inheritParams fit_Bayesian_FROC
#'@param dataList it should include  \code{f, h, NL, NI, C}.
#'The detail of these dataset, please see the endowed datasets.
#'Note that the maximal number of confidence level, denoted by  \code{C}, are included,
#' however,
#' should not include its each confidence level in \code{dataList}
#'@author Issei Tsunoda

#'@return An S4 object of class \code{stanfitExtended}.
#'
#'@examples
#' \donttest{

#'#First, prepare the example data from this package.
#'
#'
#'
#'         dat  <- get(data("dataList.Chakra.1"))
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
#' #Using the object  "fit", apply the functions in the "rstan" package,
#' # To do so,  change the class  to stanfit, as follows;
#'
#'
#'         fit <- as(fit, "stanfit")
#'
#' #   e.g.,
#'
#'           rstan::stan_trace(fit)
#'           rstan::stan_hist(fit)
#'           rstan::stan_rhat(fit)
#'
#'}#donttest

# devtools::document();help("fit_srsc") # Confirm reflection
# devtools::use_data(dataList.high.ability)
#' @export fit_srsc
#'@inheritParams DrawCurves_MRMC_pairwise

fit_srsc <- function(
  dataList,
  new.imaging.device=TRUE,
  dataList.Name = "",
  DrawCurve = T,
  ModifiedPoisson = FALSE,
  PreciseLogLikelihood = FALSE,
  Drawcol = TRUE,
  summary =TRUE,
  DrawFROCcurve=TRUE,
  DrawAFROCcurve=FALSE,
  DrawCFPCTP=TRUE,
  mesh.for.drawing.curve=10000,
  make.csv.file.to.draw.curve=FALSE,
  cha = 4,
  ite = 3000,
  dig = 5,
  war = floor(ite/5),
  see = 1234){



  if ( dataList.Name==""   ) dataList.Name <-  deparse(substitute(dataList))




  if (  ModifiedPoisson == FALSE  ) {
    fit_srsc_per_image_test(
      dataList = dataList,
      DrawCurve = DrawCurve,
      dataList.Name = dataList.Name,

      PreciseLogLikelihood = PreciseLogLikelihood,
      mesh.for.drawing.curve=mesh.for.drawing.curve,
      make.csv.file.to.draw.curve=make.csv.file.to.draw.curve,
      Drawcol = Drawcol,
      summary =summary,

      DrawFROCcurve=DrawFROCcurve,
      DrawAFROCcurve=DrawAFROCcurve,
      DrawCFPCTP=DrawCFPCTP,
      cha =  cha,
      ite  = ite,
      dig  = dig,
      war  = war,
      see  = see)




  } else
    if (  ModifiedPoisson == TRUE  ) {


      fit_srsc_per_lesion(
        dataList  = dataList,
        DrawCurve  =  DrawCurve,
        dataList.Name = dataList.Name,

        PreciseLogLikelihood  = PreciseLogLikelihood,
        mesh.for.drawing.curve=mesh.for.drawing.curve,
        make.csv.file.to.draw.curve=make.csv.file.to.draw.curve,
        DrawFROCcurve=DrawFROCcurve,
        DrawAFROCcurve=DrawAFROCcurve,
        DrawCFPCTP=DrawCFPCTP,
        Drawcol   = Drawcol,
        summary =summary,

        cha  =  cha,
        ite  =  ite,
        dig   =dig ,
        war   = war,
        see  = see )


    } else

      message("Format error occurs in your data")



}
