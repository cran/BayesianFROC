% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/document_dataset_srsc.R
\docType{data}
\name{data.nonconverge.srsc}
\alias{data.nonconverge.srsc}
\title{\strong{Non-Convergent} Data: Single reader and Single modality}
\format{
A list consists of two integer vectors  \code{f, h} and three integers \code{NL, NI, C}.


\describe{
\item{ \code{f}  }{Non-negative integer vector  specifying  number of false alarms   associated with  each confidence level. The first component corresponding to the highest confidence level.}
\item{ \code{h}  }{Non-negative integer vector  specifying  number  of Hits  associated with  each confidence level. The first component corresponding to the highest confidence level.}
\item{ \code{NL} }{A positive integer, representing  Number of Lesions.}
\item{ \code{NI} }{A positive integer, representing  Number of Images. }
\item{ \code{C}  }{A positive integer, representing  Number of Confidence level. }
}




\strong{\emph{Contents:}}

 \emph{            A single reader and single modality case   }




------------------------------------------------------------------------------------------------------
\tabular{rccc}{
\code{NI=57,NL=269}   \tab \strong{ confidence level } \tab \strong{ No. of false alarms} \tab \strong{No. of hits}  \cr
 In R console ->      \tab \code{ c} \tab   \code{f }  \tab   \code{h}  \cr
  -----------------------\tab ----------------------- \tab ----------------------------- \tab ------------- \cr
\emph{\strong{definitely present}}  \tab  3 \tab 99 \tab     88\cr
 probably present                   \tab  2 \tab 0 \tab    0\cr
 questionable                       \tab  1 \tab 0 \tab   0\cr
 }

---------------------------------------------------------------------------------------------------


*  \emph{false alarms} = False Positives = FP

*  \emph{hits} = True Positives = TP

Note that  in FROC data, the confidence level means present (deseased, positive) case only. Since each reader marks their suspicous location only and it generate the hits and false alarms for his confidenc level representing that lesion is present.
In the absent case, reader does not mark any locations and hence, the absent cofidence level does not relate this dataset.


Note that the first column of confidence level vector \code{c } should not be specified. If specified, will be ignored , since it is created by \code{  c <-c(rep(C:1))} automatically in the program and it does not refer from user input data even if it is specified explicitly, where \code{C} is the highest number of confidence levels.
So you should check the compatibility of your data and the program's generating new confidence  level vector by
a table which can be displayed by the function \code{\link{viewdata}()}.


Note that The format for the above example data must be made by the following forms:

\code{ dat <- list(       }

\code{            h = c(99, 0, 0 ),   }

\code{            f = c( 88, 0, 0),    }

\code{            NL = 111,     }

\code{            NI = 111,    }

\code{            C = 3)         }

This object \code{dat} can be passed to the function \code{\link{fit_Bayesian_FROC}()}  as the following manner \code{fit_Bayesian_FROC(dat)}.
}
\description{
A list, representing \strong{non-convergent} FROC data (which does not converge in the sence of R hat) of hits and false alarms. This is used to build a non-hierarchical FROC model.
}
\details{
Note that the maximal number of confidence level,
denoted by  \code{C}, are included,
however,  confidence level vector \code{c } should not be specified.
 If specified, will be ignored ,
 since it is created by \code{  c <-c(rep(C:1))} in the program
 and it does not refer from user input data,
 where \code{C} is the highest number of confidence levels.
Should write down your hits and
false alarms vector so that it is compatible with this automatically created  vector \code{c}.
}
\seealso{
\code{\link{dataList.Chakra.1.with.explantation}}
}
