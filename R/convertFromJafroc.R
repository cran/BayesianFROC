#' @title   Convert from \code{.xlsx} file of \strong{\emph{Jafroc}}  into \R object
#'
#'@description  Convert an Excel file whose extension is \strong{.xlsx} of Chakraborty's Jafroc formulation to
#'   an \R object representing FROC data to which we will apply functions in this package such as  \code{\link{fit_Bayesian_FROC}()}.
#'
#'Convert
#'\describe{
#'
#'\item{from}{ .xlsx  file of Jafroc  }
#'\item{into}{ \R object  }

#'}
#'
#'
#'
#'


#'@param  No.of.Modalities Total number of modalities used your data.
#'@param No.of.readers Total number of readers who are participants.
#'@param No.of.confidence.levels The number of confidence levels.
#'
#'
#'@references Bayesian Models for Free-response Receiver Operating Characteristic Analysis
#'
#'@author Issei Tsunoda
#'
#'@details
#' The return values include the data list which are directly available to the main function \code{fit_Bayesian_FROC}.
#' So, if user has data of Jafroc, then by running this function, user immediately can fit the author's Bayesian FROC model to the resulting \R object.
#'
#' The Jafroc software's format includes suspicious locations of readers and true locations.
#' Such data is \emph{redundant} for our Bayesian statistical models.
#' So, we reduce the information of data to the number of
#' false positives and number of hits for each confidence levels by this function.
#'
#' \strong{minor comment or regret}
#' The author said the Jafroc data is redundant, but I should say more informative, and it cause limitation of our model.
#' So, our model start to fit a model to this reduced data from Jafroc. So, redunction will cause the non accuracy evaluation of observer performance.
#' The future research I should start the Jafroc formulation to build model not the \R object of this function.
#'
#'
#'@format The \code{.xlsx} file of Jafroc must include three sheets named by \strong{\emph{TP, FP, Truth}} (other names never be permitted !!)
#'
#'
#'\strong{\emph{-----------------------------------  TP ------------------------------------------}}
#'
#'
#'A sheet named \strong{\emph{TP}}  includes five columns named from the right hand side:
#'
#'\strong{\emph{ReaderID,	ModalityID,	CaseID,	LesionID,	TP_Rating.}}
#'
#' 1) Note that the above word CaseID means the Image ID vectors indicating the ID of radiographs.
#'    That is "case = image = radiograph".
#' 2) Note that the first row of \code{.xlsx} sheet devote for the names as follows:
#'
#'\strong{An Example of a sheet named \emph{TP} in a \emph{\code{.xlsx}} file for the \emph{Jafroc} software}
#'
#' \tabular{ccccc}{
#'   ReaderID \tab   ModalityID   \tab   CaseID \tab   LesionID \tab   TP_Rating.\cr
#'   -------------------\tab-------------------\tab-------------------\tab-------------------\tab------------------\cr
#'   1 \tab 1 \tab 1 \tab  1\tab 5\cr
#'   1 \tab 2 \tab 2  \tab 1\tab 4\cr
#'   1 \tab 3 \tab 4  \tab 1\tab 5\cr
#'   1 \tab 1 \tab 8  \tab 1\tab 3\cr
#'   1 \tab 2 \tab 8  \tab 2\tab 4\cr
#'   1 \tab 3 \tab 9  \tab 1\tab 4\cr
#'   1 \tab 1 \tab 9  \tab 2\tab 3\cr
#'   1 \tab 2 \tab 9  \tab 3\tab 5\cr
#'   1 \tab 3 \tab 11 \tab 1\tab 3\cr
#'   2 \tab 1 \tab 1 \tab  1\tab 4\cr
#'   2 \tab 2 \tab 4  \tab 1\tab 4\cr
#'   2 \tab 3 \tab 5  \tab 1\tab 4\cr
#'   2 \tab 1 \tab 8  \tab 1\tab 1\cr
#'   2 \tab 2 \tab 8  \tab 2\tab 2\cr
#'   2 \tab 3 \tab 8  \tab 3\tab 2\cr
#'   2 \tab 1 \tab 10  \tab 1\tab 3\cr
#'   2 \tab 2 \tab 10 \tab 2\tab 2\cr
#'   2 \tab 3 \tab 11  \tab 1\tab 2\cr
#'
#'   : \tab : \tab :  \tab :\tab :\cr
#'   : \tab : \tab :  \tab :\tab :\cr    }
#'
#'
#'\strong{\emph{-----------------------------------  FP ------------------------------------------}}
#'
#'
#'

#' A sheet named \strong{FP}  includes four columns named from the right hand side: \strong{ReaderID,	ModalityID,	CaseID,	FP_Rating}
#'\strong{An Example of a sheet named FP in a \code{.xlsx} file for the Jafroc software}

#'
#' \tabular{cccc}{
#'   ReaderID \tab   ModalityID   \tab   CaseID  \tab   FP_Rating.\cr
#' -------------------\tab-------------------\tab-------------------\tab------------------\cr

#'   1 \tab 1 \tab 1 \tab   2\cr
#'   1 \tab 2 \tab 2  \tab  1\cr
#'   1 \tab 3 \tab 3  \tab  1\cr
#'   1 \tab 1 \tab 5  \tab  2\cr
#'   1 \tab 2 \tab 7 \tab   1\cr
#'   1 \tab 3 \tab 7  \tab   2\cr
#'   1 \tab 1 \tab 9  \tab   3\cr
#'   1 \tab 2 \tab 9  \tab   4\cr
#'   1 \tab 3 \tab 10 \tab   1\cr
#'   2 \tab 1 \tab 1 \tab    2\cr
#'   2 \tab 2 \tab 2  \tab   3\cr
#'   2 \tab 3 \tab 3  \tab   4\cr
#'   2 \tab 1 \tab 8  \tab   1\cr
#'   2 \tab 2 \tab 9  \tab  1\cr
#'   2 \tab 3 \tab 11  \tab   1\cr
#'   2 \tab 1 \tab 14  \tab  1\cr
#'   2 \tab 2 \tab 15 \tab   1\cr
#'   2 \tab 3 \tab 21  \tab   2\cr
#'   : \tab :  \tab :\tab :\cr
#'   : \tab :  \tab :\tab :\cr
#'      }
#'
#'
#'
#'
#'\strong{\emph{-----------------------------------  Truth ------------------------------------------}}

#'
#'A sheet named \strong{Truth }  includes three columns named from the right hand side:\strong{CaseID,	LesionID,	Weight} .
#'
#'\strong{An Example of a sheet named Truth in a \code{.xlsx} file for the Jafroc software}

#' \tabular{ccc}{
#'   CaseID \tab   LesionID   \tab     Weight\cr
#'   -------------------\tab-------------------\tab------------------\cr

#'   1 \tab 1 \tab    0.3333...\cr
#'   1 \tab 2 \tab    0.3333...\cr
#'   1 \tab 3 \tab    0.3333...\cr
#'   2 \tab 1 \tab    0.5\cr
#'   2 \tab 2 \tab    0.5\cr
#'   3 \tab 1 \tab    1\cr
#'   4 \tab 1 \tab    0.25\cr
#'   4 \tab 2 \tab    0.25\cr
#'   4 \tab 3 \tab    0.25\cr
#'   4 \tab 4 \tab    0.25\cr
#'   5 \tab 1 \tab    0.5\cr
#'   5 \tab 2 \tab    0.5\cr
#'   6 \tab 1 \tab    0.3333...\cr
#'   6 \tab 2 \tab    0.3333...\cr
#'   6 \tab 3 \tab    0.3333...\cr
#'   7 \tab 1 \tab    0.3333...\cr
#'   7 \tab 2 \tab    0.3333...\cr
#'   7 \tab 3 \tab    0.3333...\cr
#'   8 \tab 1 \tab    0.25\cr
#'   8 \tab 2 \tab    0.25\cr
#'   8 \tab 3 \tab    0.25\cr
#'   8 \tab 4 \tab    0.25\cr
#'   :  \tab :\tab :\cr
#'   :  \tab :\tab :\cr
#'   }
#'
#'
#'
#'
#'
#'
#' Never change from these column names in any \code{.xlsx} file.
#'
#'
#'Note that the weght are used for each images influence a same effect on the esimates.
#'If we consider the truth without weight, then the images including many targets (lesions) has very strong effect on the estimates.
#'To avoid such bias, Jafroc uses weight. However, in this package, we do not use the information of weight.
#'Since the theory of the author of this package did not consider such weight. In the future I have to include the notion of weight.
#'Jafroc use the notion fo figure of metric as non parametric manner.
#'So, it seems difficult to include it in the Bayesian model, since generally speaking, Bayesian methodology is parametric.
#'
#'@import readxl
#'
#'@seealso Rjafroc


#'@examples
#'
#' \donttest{

# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'
#'
#' # Aim
#'
#' # step 0) Prepare Jafroc .xlsx file being contained in this package
#' # step 1) Convert the .xlxs file obtained in step 0)
#' # step 2) Fit model to the data object obtained in step 1)
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' # ------------------------ step 0)   -------------------------------------------------
#'
#' # By an xlsx file named JAFROC_data.xlsx in the director "inst/extdata" of this package,
#' # we can reconstruct it  as follows:(If someone can obtain the Excel file
#' # from the path BayesianFROC/inst/extdata/JAFROC_data.xlsx, then the following code
#' # is not required to run. If searching bother you, then run the R script to obtain the
#' # Excel file.)
#' # I do not know how to users refer the JAFROC_data.xlsx in this package,
#' # so I provide it by making the same xlsx file as the JAFROC_data.xlsx.
#'
#'
#' # Note that JAFROC_data.xlsx cannot remove,
#' # if it is removed, then devtools::run_examples() make an error.
#'
#' Truth <- readxl::read_excel(system.file("extdata",
#'  "JAFROC_data.xlsx",
#'   package="BayesianFROC"),
#'    sheet = "Truth")
#' View(Truth)
#'
#' TP <- readxl::read_excel( system.file("extdata",
#'                                       "JAFROC_data.xlsx",
#'                                        package="BayesianFROC"),
#'                            sheet = "TP")
#' View(TP)
#'
#' FP <- readxl::read_excel( system.file("extdata",
#'                                       "JAFROC_data.xlsx",
#'                                         package="BayesianFROC"),
#'                           sheet = "FP")
#' View(FP)
#'
#'
#'
#'
#'
#'sample <- list(TP=TP,FP=FP,Truth=Truth)
#'openxlsx::write.xlsx(sample,"JafrocDatasetExample.xlsx")
#'
#'
#'
#'
#'# Now, we get excel file named "JafrocDatasetExample.xlsx", which is same as
#'# the JAFROC_data.xlsx.
#'# whose format is available in the Jafroc software developed by Chakraborty.
#'# If you use your data, your data must has same format of "JafrocDatasetExample.xlsx".
#'# Note that other excel data must comply with the above format.
#'
#'# Note that if you have proper format excel file for our package,
#'# this process does not need.
#'
#'
#'
#'
#'#    (0) From the above, we obtain  "JafrocDatasetExample.xlsx"
#'#    which is the multiple reader and multiple modality dataset
#'#    for Jfroc analysis which is NOT implemented in our package,
#'#     but Chakraborty's software called Jafroc or the R package Rjafroc.
#'
#'
#'
#'
#'
#'# ------------------------ step 1)   -------------------------------------------------
#'
#'# (1) Using "JafrocDatasetExample.xlsx" as an example excel file,
#'# we run the function to convert the excel file from Jafroc format
#'# to our format:
#'
#'
#'
#'      dataList <- convertFromJafroc(
#'                                   No.of.Modalities =5,
#'                                   No.of.readers    =4,
#'                                   No.of.confidence.levels = 5
#'                                     )
#'
#'
#'
#'# In the variable, there is no xlsx file, since it is selected by interactive manner.
#'# So, please select the xlsx file obtained in step 0).
#'
#'# ------------------------ step 2)   -------------------------------------------------

#'
#'#  (2)   Now, we obtain a data list as the return value.
#'#        Using this list, we run the function "fit_Bayesian_FROC":
#'
#'
#'
#'           fit  <-  fit_Bayesian_FROC(dataList )
#'
#'
#'            } # dontrun


#'
# @importFrom openxlsx write.xlsx
# @importFrom xlsx     write.xlsx
# @importFrom openxlsx  write.xlsx
# @importFrom xlsx read.xlsx
# @import readxl
#transaaa(xlsxfilename,5,4,5)
# devtools::document();help("convertFromJafroc")

#' @export convertFromJafroc
#'@inheritParams fit_Bayesian_FROC
convertFromJafroc <- function(
  No.of.Modalities,
  No.of.readers,
  No.of.confidence.levels){


  M <- No.of.Modalities
  Q <- No.of.readers
  C <- No.of.confidence.levels




  ########### PASS ########## START
  message("* Please select a xlsx file to calculate its number of false alarms and true positives for each confidenve level.")
  message("* The lowest confidence level should be 1, not zero.")

  tcltk::tkmessageBox(message="
 * Select a Jafroc file whose extesion is .xlsx to convert it into an R object.

 * The return.value is used to calculate FROC bayesian model in this package by running the following code:

                      fit_Bayesian_FROC(return.value)")
  path<-tcltk::tclvalue(tcltk::tkgetOpenFile())
  if (!nchar(path)){
    tcltk::tkmessageBox(message="No file was selected! Exit.")
    return(message("Suspend a process."))
  }else{
    xlsxfilename <- basename(path)

    tcltk::tkmessageBox(message=paste("* The selected file name  was:",xlsxfilename,".
                                      * More precisely, its path is the follwing:", path, "."))
  }

  # path
  xlsxfilename <- basename(path)
  directoryname  <- dirname(path)
  ########### PASS ########## Fin







  message("\n \n \n \n Wait...")
  fileTP<-xlsx::read.xlsx(path, sheetIndex ="TP")
  message("\n  Wait...")
  fileFP<-xlsx::read.xlsx(path, sheetIndex ="FP")
  message("\n   Wait... \n\n\n")
  fileTruth<-xlsx::read.xlsx(path, sheetIndex ="Truth")


  # head(fileTP )
  # head(fileFP )
  # head(fileTruth)
  # names(fileTP)
  # names(fileFP )
  # names(fileTruth)
  #
  #  C <- 5
  #  M<-5
  #  Q<-4
  # M <-max(max(fileTP$ModalityID),max( fileFP$ModalityID))
  # M <-as.integer (max(  max( fileFP$ModalityID)))
  # Q <-as.integer (max(  max( fileTP$ReaderID  )))

  SRSC <-array(0,list(M,Q,C))
  hhh <-array(0,list(M,Q,C))
  fff <-array(0,list(M,Q,C))
  for (iii in 1:nrow(fileTP)) {
    hhh[  fileTP$ModalityID[iii]  , fileTP$ReaderID[iii]  ,fileTP$TP_Rating[iii]   ] <- 1+  hhh[  fileTP$ModalityID[iii]  , fileTP$ReaderID[iii]  ,fileTP$TP_Rating[iii]   ]
  }
  for (iii in 1:nrow(fileFP)) {
    fff[  fileFP$ModalityID[iii]  , fileFP$ReaderID[iii]  ,fileFP$FP_Rating[iii]   ] <- 1+  fff[  fileFP$ModalityID[iii]  , fileFP$ReaderID[iii]  ,fileFP$FP_Rating[iii]   ]
  }


  ######################################################

  m<- rep(1:M, each=C*Q)
  m
  q <-  rep(seq(1,Q,1), M, each=C)
  q
  c <-rep(rep(C:1), M*Q)
  c

  f <-array(0,list(M*Q*C))
  f
  h <-array(0,list(M*Q*C))
  h
  #
  # length(m)
  # length(q)
  # length(c)
  # length(h)
  # length(f)
  for (mmm in 1:M) {

    for (qqq in 1:Q) {
      for (ccc in 1:C) {
        h[C+1-ccc+(mmm-1)*C*Q+(qqq-1)*C] <-hhh[mmm,qqq,ccc]
        f[C+1-ccc+(mmm-1)*C*Q+(qqq-1)*C] <-fff[mmm,qqq,ccc]

      }}}

  # length(m)
  # length(q)
  # length(c)
  # length(h)
  # length(f)

  yourdata <- data.frame(ModalityID=m,
                         ReaderID=q,
                         Confidence_lebels=c,
                         True_Positives=h,
                         False_Positives=f)







  # yourdata
  # nrow(yourdata)
  # nrow(yourdata)==M*Q*C
  message("\n--------------------------------------------------  \n\n")
  message("* An excel file is made in the directory (",directoryname,"), in which the number of false positives and true positives are calcultated for your data (",xlsxfilename ,"). \n \n ")
  xlsx::write.xlsx(yourdata, file=paste(directoryname,"/converted_",xlsxfilename,sep=""), sheetName="TPsFPs", row.names=F)
  #xlsx::write.xlsx(yourdata, file=paste(directoryname,"/trans... This / is important!! This is it in path !!


  # data.transformed <<- read.xlsx2(paste("transformed",xlsxfilename), 1, stringsAsFactors=FALSE)
  # message("2) A dataframe is made and named 'dataframe.transformed'.   \n")
  # message("--------------------------------------------------  \n")


  NL <- nrow(fileTruth)- nrow(subset(fileTruth ,fileTruth$LesionID ==0) )
  dataList <- list(m=m,q=q,c=c,h=h,f=f, NL=NL, C=C,M=M,Q=Q)
  # assign("dataList", dataList, envir=globalenv())

  message("* The retrun value is a list which is available for model fitting by the fit_Bayesian_FROC().   \n\n")

  message("--------------------------------------------------  \n")

  return(dataList)
  #
  # dataList.Chakra.Web$m==dataList.transformed$m
  # dataList.Chakra.Web$q==dataList.transformed$q
  # dataList.Chakra.Web$c==dataList.transformed$c
  # dataList.Chakra.Web$h==dataList.transformed$h
  # dataList.Chakra.Web$f==dataList.transformed$f
  # dataList.Chakra.Web$NL==dataList.transformed$NL
  # dataList.Chakra.Web$C==dataList.transformed$C
  # dataList.Chakra.Web$M==dataList.transformed$M
  # dataList.Chakra.Web$Q==dataList.transformed$Q






  #
  # m <- data.transformed$ModalityID
  # q <- data.transformed$ReaderID
  # c <- data.transformed$Confidence_lebels
  # h <- data.transformed$True_Positives
  # f <- data.transformed$False_Positives

  #
  #
  # length(m)
  # length(q)
  # length(c)
  # length(h)
  # length(f)
  # N <-M*Q*C
  #
  #
  # m <-data$m
  # q <-data$q
  # c <-data$c
  # h <-data$h
  # f <-data$f
  # NL<-data$NL
  # C<-data$C
  # M<-data$M
  # Q<-data$Q
  #

  }
