#' @title  Build a table of FROC data
#'
#'
#'@description
#' Create a tabular representation
#' of FROC data from FROC data object.


#'@return Nothing
#'
#'In order to confirm your data,
#' please view table before fitting.
#'Confidence level vector are created
#'in my program regardless of user's
#'confidence level vectors.


#'@author Issei Tsunoda

#'@param dataList
#'------------------------------------------------------
#'
#'
#'   A Single reader and A single modality (SRSC) case.
#'
#'
#'------------------------------------------------------
#'
#'In a single reader and a single modality case,
#' it should include  \code{f, h, NL, NI, C}.
#'
#' For example data,
#' see the datasets
#' endowed with this package.
#'
#'
#'
#'
#'
#'
#'
#'

#'\strong{\emph{ data Format:}}
#'
#'  \emph{            A single reader and a single modality case   }
#'
#'------------------------------------------------------------------------------------------------------

#' \tabular{rccc}{
#' \code{NI=63,NL=124}   \tab \strong{ confidence level } \tab \strong{ No. of false alarms} \tab \strong{No. of hits}  \cr
#'  In R console ->      \tab \code{ c} \tab   \code{f }  \tab   \code{h}  \cr
#'   -----------------------\tab ----------------------- \tab ----------------------------- \tab ------------- \cr
#' \emph{definitely} present  \tab  \code{c[1] = }5 \tab \code{f[1] = }\eqn{F_5} = 1 \tab  \code{h[1] = }\eqn{H_5} = 41 \cr
#'  \emph{probably} present   \tab  \code{c[2] = }4 \tab \code{f[2] = }\eqn{F_4} = 2 \tab  \code{h[2] = }\eqn{H_4} = 22 \cr
#'  equivocal                 \tab  \code{c[3] = }3 \tab \code{f[3] = }\eqn{F_3} = 5 \tab  \code{h[3] = }\eqn{H_3} = 14  \cr
#'  subtle                    \tab  \code{c[4] = }2 \tab \code{f[4] = }\eqn{F_2} = 11 \tab \code{h[4] = }\eqn{H_2} = 8  \cr
#'  \emph{very} subtle        \tab  \code{c[5] = }1 \tab \code{f[5] = }\eqn{F_1} = 13 \tab \code{h[5] = }\eqn{H_1} = 1  \cr
#'  }
#'
#'---------------------------------------------------------------------------------------------------
#'
#'
#'
#'----------------------------------------------------------------------------
#'
#'   Multiple readers and multiple modalities case, i.e., MRMC case
#'
#'
#'----------------------------------------------------------------------------
#'
#'
#'In  multiple readers and multiple modalities case, i.e., MRMC case,
#' it should include  \code{m,q,c,h,f,NL,C,M,Q} which means the followings:
#'
#'\code{C } means the highest number of confidence level, this is a scalar.
#'
#'\code{M } means the number of modalities
#'
#'\code{Q } means the number of readers.
#'
#'\code{c } means the confidence level vector. This vector must be made by \code{rep(rep(C:1), M*Q)}.
#'
#'
#'\code{m } means the modality ID vector.
#'
#'\code{q } means the reader ID vector.
#'
#'
#'\code{h } means the number of hits vector.
#'
#'\code{f } means the number of false alarm vector.
#'
#'\code{NL } means the Total number of lesions for all images, this is a scalar.
#'

#'The detail of these dataset, please see the endowed datasets.
#'Note that the maximal number of confidence level, denoted by  \code{C}, are included,
#' however,
#'its each confidence level vector also created in the program by \code{C}. So, to confirm
#'your false positives and hits are correctly correspond
#'to confidence levels,
#'you should confirm the orders by the function \code{viewdata_MRMC}.


#' @param summary Logical: \code{TRUE} or \code{FALSE}.
#'  If true then results are printed,
#'  if FALSE this function do nothing.

#'@param head.only Logical: \code{TRUE} or \code{FALSE}.
#'Whether it prints  data of head part only (\code{TRUE})
#' or entire (\code{FALSE}). If \code{TRUE},
#'  only head part are shown. Default is \code{FALSE}
#'


#'
#'@examples
#' \donttest{

#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####

#'# The first example, we prepare the data in this package.
#'
#'
#'              dat  <- get(data("dataList.Chakra.1"))
#'
#'              viewdata(dat)
#'
#'
#'# The second examle, we consider a dataset of multiple readers and multiple modalities
#'
#'
#'
#'              dat <-  get(data("dataList.Chakra.Web"))
#'
#'              viewdata(dat)
#'
#'
#'}# dottest
#'
#'

#'
# @importFrom   gridExtra tableGrob
# @importFrom  grid grid.draw
#' @importFrom  knitr kable
#' @export viewdata

# devtools::document();help("viewdata") # Confirm reflection

# devtools::use_package("rstan")






viewdata <- function(dataList,summary=TRUE,head.only=FALSE){



  # message(" This function named viewdata refer your datalist's column of modality ID, if such column does not exsit then this function consider your data are a single reader and a signle modality case, and in the other cases, this function regards your data as multiple reader and multiple case.\n")

  if ( length(dataList[["m"]])==0  ) {# the reader =0 occur even the case of MRMC

    # message("\n-------------------------------------\n")
    # message("\n* Your data:         ",deparse(substitute(dataList)) ," \n \n")
    message("\n* Number of Lesions: ",  crayon::bgBlack$bold$italic$underline$yellow(dataList$NL)   )
    message("\n* Number of Images : ",  crayon::bgBlack$bold$italic$underline$yellow(dataList$NI) ,"\n")
    # viewdata_srsc(dataList,summary)

    tryCatch(
      {   viewdata_srsc(dataList,summary)  },
  error =function(e){message("Should confirm whether data format is correct. ")}
  )


    message(
      crayon::bold$italic$underline$silver(
        "\n\n* Higher number of confidence level indicates reader's higher confidence. In your case, the number "
        ),
        crayon::bgBlack$bold$italic$underline$yellow(dataList$C),
      crayon::bold$italic$underline$silver(
       " is the most high confidence level, i.e., we may say that confidence level "
      ),
      crayon::bgBlack$bold$italic$underline$yellow(dataList$C,
   crayon::bold$italic$underline$silver(
      " means that \"definitely lesion is present \" " )
   )
   )


  } else if (length(dataList[["m"]]) >= 1) {
    # message("\n-------------------------------------\n")
    # message("\n* Your data:         ",deparse(substitute(dataList)) ," \n \n")
    message("\n* Number of Lesions: ",crayon::bgBlack$bold$italic$underline$yellow(dataList$NL) )
    # if (!is.null(dataList$NI))  message("\n* Number of Images : ",crayon::bgBlack$bold$italic$underline$yellow(dataList$NI) ,". (This is not used for estimates).\n")
    if (!is.null(dataList$NI))  message("\n* Number of Images : ",crayon::bgBlack$bold$italic$underline$yellow(dataList$NI) ,".\n")
    # if (is.null(dataList$NI))  message(  crayon::bold$italic$underline$silver("\n* Number of Images is not assigned, not required to esimate. \n") )

     # viewdata_MRMC(dataList,summary)
     tryCatch(
       {   viewdata_MRMC(dataList,summary,head.only=head.only)   },
       error =function(e){message("Should confirm whether data format is correct. ")}
     )


     } else

      message("Verify the  column vector of modality ID is named by  m  exactly in your data.")



}










#' @title  Build a table of data in the case of
#' A Single reader and A Single modality (srsc)
#'
#'
#'@description  In order to confirm your data, please view table.
#'my program makes new column of confidence levels which are used in
#'my program. So, it is possible that your order of confidence level and
#'Program's order of confidence level are inverse.
#'This function's result table are the one which are used in program.


#'@param dataList it should include  \code{f, h, NL, NI, C}.
#'The detail of these dataset, please see the endowed datasets.
#'Note that the maximal number of confidence  level, denoted by  \code{C}, are included,
#' however,
#'its each confidence level should not included  your data. So, to confirm
#'your false positives and hits are correctly correspondence
#'to confidence levels,
#'user should confirm the orders by the function.

#'
#'@examples
#'\donttest{
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#   First, we prepare an example FROC data "dataList.Chakra.1" in this package.
#'# Note that this data should be formed as a single reader and a single modality.
#'# If data are  multiple readers and multiple modalities, i.e.,MRMC-data,
#'# then another function named viewdataMRMC is available for MRMC-data.
#'
#'              dat  <- get(data("dataList.Chakra.1"))
#'
#'
#'
#'
#'# Show data named "dat";
#'
#'
#'              viewdata_srsc(dat)
#'
#'
#'
#'#The Reason why the author made this \code{viewdata_srsc} is
#'#the code does not refer your confidence level.
#'#More precisely, my program made the column vector of confidence levels
#'#from the its highest number,
#'#so, it may be occur the interpretaion of code for hits and false alarm
#'#are inverse order compared with your data.
#'
#'
#'}# dottest
#'
#' @importFrom knitr  kable
#'@param summary TRUE or FALSE, if true then results are printed, if FALSE this function do nothing.

# devtools::document();help("viewdata_srsc") # Confirm reflection

# devtools::use_package("rstan")



# devtools::use_data(dataList.high.ability)
#' @export viewdata_srsc
#'@inheritParams fit_Bayesian_FROC
viewdata_srsc <-function(dataList,summary=TRUE){
  C<-dataList$C
  if (summary==TRUE) {

    # the output of kable is disturbed from the name of vector.
    names(dataList$f)<-NULL
    names(dataList$h)<-NULL









    if (C==3) {
      rating <- c(  "Obviouly present", "Relatively obvious" ,"Subtle"  )

      data  <- data.frame(
        . = rating,
        Confidence.Level=rep(C:1),
        False.Positives=dataList$f,
        True.Positives=dataList$h
      )


      print( knitr::kable(data, format = "pandoc"))




      ####       kableExtra  are used from here
      #  `%>%` <- utils::getFromNamespace("%>%", "magrittr")
      #
      # print(
      #    knitr::kable(data) %>%
      #    kableExtra::kable_styling("striped", full_width = F) %>%
      #    kableExtra::column_spec(3:4, bold = T)
      #    )

      # print(
      #   knitr::kable(data) %>%
      #   kable_styling("striped", full_width = F) %>%
      #   row_spec(0, angle = -45)
      # )



      ####       kableExtra  are used up to here

      # https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html



      # print(    data %>%
      #       dplyr::mutate_if(is.numeric, function(x) {
      #         cell_spec(x, bold = T,
      #                   color = spec_color(x, end = 0.9),
      #                   font_size = spec_font_size(x))
      #       }) %>%
      #       dplyr::mutate(Confidence.Level = cell_spec(
      #         Confidence.Level, color = "white", bold = T,
      #         background = spec_color(1:length(Confidence.Level), end = 0.9, option = "A", direction = -1)
      #       )) %>%
      #       knitr::kable(escape = F, align = "c") %>%
      #       kableExtra::kable_styling(c("striped", "condensed"), full_width = F)
      #
      # )













    }#if C=3













    if (C==4) {
      rating <- c(  "Obviously lesion", "Relatively obvious" ,"Subtle", "Very subtle"   )

      data  <- data.frame(
        . = rating,
        Confidence.Level=rep(C:1),
        False.Positives=dataList$f,
        True.Positives=dataList$h
      )


      print( knitr::kable(data, format = "pandoc"))

    }#if C=4








    if (C==5) {
      rating <- c(  "Obviously lesion", "Relatively obvious" ,"Subtle", "Very subtle" ,  "Extremely subtle"    )

      data  <- data.frame(
        . = rating,
        Confidence.Level=rep(C:1),
        False.Positives=dataList$f,
        True.Positives=dataList$h
      )


      print( knitr::kable(data, format = "pandoc"))

    }#if C=5


    if (C==6) {
      rating <- c(  "Obviously lesion", "Relatively obvious",  "equivocal"  ,"Subtle", "Very subtle" ,  "Extremely subtle"    )

      data  <- data.frame(
        . = rating,
        Confidence.Level=rep(C:1),
        False.Positives=dataList$f,
        True.Positives=dataList$h
      )


      print( knitr::kable(data, format = "pandoc"))

    }#if C=6

    if (! C==3 && !C==4 && !C ==5 && !C==6) {


      data  <- data.frame(
        Confidence.Level=rep(C:1),
        False.Positives=dataList$f,
        True.Positives=dataList$h
      )


      print( knitr::kable(data, format = "pandoc"))


    }

  }
}














#' @title  View MRMC data
#'
#'
#'@description  Build a table for data \code{dataList}.

#'@param dataList it should include  \code{m,q,c,h,f,NL,C,M,Q} which means from the right
#'
#'\code{m } means the modality ID vector
#'
#'\code{q } means the reader ID vector
#'
#'\code{c } means the confidence level
#'
#'\code{h } means the number of hits
#'
#'\code{f } means the number of false alarm
#'
#'\code{NL } means the Total number of lesions for all images
#'
#'\code{C } means the highest number of confidence level
#'
#'\code{M } means the number of modalities
#'
#'\code{Q } means the number of readers.
#'
#'The detail of these dataset, please see the endowed datasets.
#'Note that the maximal number of confidence  level, denoted by  \code{C}, are included,
#' however,
#'its each confidence level should not included  your data. So, to confirm
#'your false positives and hits are correctly correspondence
#'to confidence levels,
#'you should confirm the orders by the function \code{viewdata_MRMC}.
#'@param summary TRUE or FALSE, if true then results are printed, if FALSE this function do nothing.
#'@param head.only Logical: \code{TRUE} of \code{FALSE}. Whether  head part or entire. If \code{TRUE}, only head part are shown. Default is \code{FALSE}

#' @importFrom knitr  kable

# devtools::document();help("viewdata_MRMC") # Confirm reflection
#' @export viewdata_MRMC

viewdata_MRMC <-function(dataList,summary=TRUE, head.only =FALSE){
  if (summary==TRUE) {

    C<-dataList$C
    M<-dataList$M
    Q<-dataList$Q
    NI<-dataList$NI
    NL<-dataList$NL

    data  <- data.frame(ModalityID=dataList$m,
                        ReaderID=dataList$q,
                        Confidence.Level=rep(rep(C:1), M*Q),
                        False.Positives=dataList$f,
                        True.Positives=dataList$h)


    message("
 -----  Interpretation of the Table ------

*   The first row shows that the number of hits ",crayon::bgBlack$bold$italic$underline$yellow(dataList$h[1]),"  and the number of false alarms  ",crayon::bgBlack$bold$italic$underline$yellow(dataList$f[1])," with the ",crayon::bgBlack$bold$italic$underline$yellow(dataList$q[1]),"-th reader under the imaging method ID ",crayon::bgBlack$bold$italic$underline$yellow(dataList$m[1])," with his confidence level ",crayon::bgBlack$bold$italic$underline$yellow(dataList$c[1]),".")


    if (head.only == FALSE)   print( knitr::kable(data, format = "pandoc",caption = paste("\n       Number of lesions:",NL,"\n       Number of images :", NI)))
    if (head.only == TRUE)  { print( knitr::kable( utils::head(data,n=2*C), format = "pandoc",caption = paste("\n       Number of lesions:",NL,"\n       Number of images :", NI)))
      message("\n* We show the head part of data, i.e., first ", 2*C," rows  are shown. \n")
      message("\n* To show all rows of data, use viewdata(dataList = ", crayon::bgBlack$cyan("Your data")   ,", head.only = ", crayon::bgBlack$cyan("FALSE")   ,")\n")

    }
  }# if (summary=TRUE)

}
