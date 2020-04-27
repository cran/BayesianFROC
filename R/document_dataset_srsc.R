



#' @title Data: A Single Reader and A Single Modality
#' @description A list, representing an FROC dataset consisting of hits, false alarms, number of lesions, number of images. We fit a FROC model to the data.
#' @details
#'
#' Note that the maximal number of confidence level,
#' denoted by  \code{C}, are included,
#' however,  confidence level vector \code{c } should not be specified.
#'  If specified, will be ignored ,
#'  since it is created by \code{  c <-c(rep(C:1))} in the program
#'  and it does not refer from user input data,
#'  where \code{C} is the highest number of confidence levels.
#'Should write down your hits and
#'false alarms vector so that it is compatible with this automatically created  vector \code{c}.
#'
#'
#'
#'
#'
#'This data appeared in Chakraborty's paper (1988).
#'  This dataset is same as \code{dataList.Chakra.1}.
#'  The difference between two dataset is only explanations for vectors.
#'  That is I attached the name for each vector by \code{names()}.
#'  I hope it help user for understanding what it is.
#'
#'
#'
#' @format
#'  A list consists of two integer vectors  \code{f, h} and three integers \code{NL, NI, C}.
#'

#'

#' \describe{
#' \item{ \code{f}  }{Non-negative integer vector  specifying  number of false alarms   associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{h}  }{Non-negative integer vector  specifying  number  of Hits  associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{NL} }{A positive integer, representing  Number of Lesions.}
#' \item{ \code{NI} }{A positive integer, representing  Number of Images. }
#' \item{ \code{C}  }{A positive integer, representing  Number of Confidence level. }
#' }
#'
#'
#'

#'
#'\strong{\emph{ Contents:}}
#'
#'  \emph{            A single reader and a single modality case   }
#'
#'
#'
#'
#'------------------------------------------------------------------------------------------------------

#' \tabular{rccc}{
#' \code{NI=57,NL=259}   \tab \strong{ confidence level } \tab \strong{ No. of false alarms} \tab \strong{No. of hits}  \cr
#'  In R console ->      \tab \code{ c} \tab   \code{f }  \tab   \code{h}  \cr
#'   -----------------------\tab ----------------------- \tab ----------------------------- \tab ------------- \cr
#' \emph{\strong{definitely present}}  \tab  3 \tab 1 \tab     97\cr
#'  probably present                   \tab  2 \tab 14 \tab    32\cr
#'  questionable                       \tab  1 \tab 74 \tab   31\cr
#'  }
#'
#'---------------------------------------------------------------------------------------------------
#'
#'
#'*  \emph{false alarms} = False Positives = FP
#'
#'*  \emph{hits} = True Positives = TP
#'
#'Note that  in FROC data, the confidence level means present (deseased, positive) case only. Since each reader marks their suspicous location only and it generate the hits and false alarms for his confidenc level representing that lesion is present.
#'In the absent case, reader dose not mark any locations and hence, the absent cofidence level does not relate this dataset.
#'
#'
#' Note that the first column of confidence level vector \code{c } should not be specified. If specified, will be ignored , since it is created by \code{  c <-c(rep(C:1))} automatically in the program and it does not refer from user input data even if it is specified explicitly, where \code{C} is the highest number of confidence levels.
#'So you should check the compatibility of your data and the program's generating new confidence  level vector by
#'a table which can be displayed by the function \code{\link{viewdata}()}.
#'
#'
#'Note that The format for the above example data must be made by the following forms:
#'
#' \code{ dat <- list(       }
#'
#'\code{            h = c(97,   32,  31 ),   }
#'
#' \code{            f = c(1 ,  14, 74 ),    }
#'
#' \code{            NL = 259,     }
#'
#' \code{            NI = 57,    }
#'
#' \code{            C = 3)         }
#'
#' This object \code{dat} can be passed to the function \code{\link{fit_Bayesian_FROC}()}  as the following manner \code{fit_Bayesian_FROC(dat)}.

#'
#'
#'
# dataList.Chakra.1.with.explantation ----
#' @name dataList.Chakra.1.with.explantation
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#'
#' @references Maximum likelihood analysis of free-response receiver operating characteristic (FROC) data, Dev P. Chakraborty.
#' @source Maximum likelihood analysis of free-response receiver operating characteristic (FROC) data, Dev P. Chakraborty.
# devtools::document();help(dataList.Chakra.1)
NULL



































#' @title Data: A Single Reader and A Single Modality
#' @description A list, representing FROC data consisting of hits, false alarms, number of lesions, number of images. We fit a FROC model to the data.
#' @details
#'
#' Note that the maximal number of confidence level,
#' denoted by  \code{C}, are included,
#' however,  confidence level vector \code{c } should not be specified.
#'  If specified, will be ignored ,
#'  since it is created by \code{  c <-c(rep(C:1))} in the program
#'  and it does not refer from user input data,
#'  where \code{C} is the highest number of confidence levels.
#'Should write down your hits and
#'false alarms vector so that it is compatible with this automatically created  vector \code{c}.
#'
#'
#'
#'
#'
#'This data appeared in Chakraborty's paper (1988).

#'
#'
#'
#' @format
#'  A list consists of two integer vectors  \code{f, h} and three integers \code{NL, NI, C}.
#'

#'

#' \describe{
#' \item{ \code{f}  }{Non-negative integer vector  specifying  number of false alarms   associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{h}  }{Non-negative integer vector  specifying  number  of Hits  associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{NL} }{A positive integer, representing  Number of Lesions.}
#' \item{ \code{NI} }{A positive integer, representing  Number of Images. }
#' \item{ \code{C}  }{A positive integer, representing  Number of Confidence level. }
#' }
#'
#'
#'

#'
#'\strong{\emph{Contents:}}
#'
#'
#'  \emph{            A single reader and a single modality case   }
#'
#'
#'
#'
#'------------------------------------------------------------------------------------------------------

#' \tabular{rccc}{
#' \code{NI=57,NL=259}   \tab \strong{ confidence level } \tab \strong{ No. of false alarms} \tab \strong{No. of hits}  \cr
#'  In R console ->      \tab \code{ c} \tab   \code{f }  \tab   \code{h}  \cr
#'   -----------------------\tab ----------------------- \tab ----------------------------- \tab ------------- \cr
#' \emph{\strong{definitely present}}  \tab  3 \tab 1 \tab     97\cr
#'  probably present                   \tab  2 \tab 14 \tab    32\cr
#'  questionable                       \tab  1 \tab 74 \tab   31\cr
#'  }
#'
#'---------------------------------------------------------------------------------------------------
#'
#'
#'*  \emph{false alarms} = False Positives = FP
#'
#'*  \emph{hits} = True Positives = TP
#'
#'Note that  in FROC data, the confidence level means present (deseased, positive) case only. Since each reader marks their suspicous location only and it generate the hits and false alarms for his confidenc level representing that lesion is present.
#'In the absent case, reader dose not mark any locations and hence, the absent cofidence level does not relate this dataset.
#'
#'
#' Note that the first column of confidence level vector \code{c } should not be specified. If specified, will be ignored , since it is created by \code{  c <-c(rep(C:1))} automatically in the program and it does not refer from user input data even if it is specified explicitly, where \code{C} is the highest number of confidence levels.
#'So you should check the compatibility of your data and the program's generating new confidence  level vector by
#'a table which can be displayed by the function \code{\link{viewdata}()}.
#'
#'
#'Note that The format for the above example data must be made by the following forms:
#'
#' \code{ dat <- list(       }
#'
#'\code{            h = c(97,   32,  31 ),   }
#'
#' \code{            f = c(1 ,  14, 74 ),    }
#'
#' \code{            NL = 259,     }
#'
#' \code{            NI = 57,    }
#'
#' \code{            C = 3)         }
#'
#' This object \code{dat} can be passed to the function \code{\link{fit_Bayesian_FROC}()}  as the following manner \code{fit_Bayesian_FROC(dat)}.

#'
#'
#'
# dataList.Chakra.1 ----
#' @name dataList.Chakra.1
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#' @seealso \code{\link{dataList.Chakra.1.with.explantation}}
#' @references Maximum likelihood analysis of free-response receiver operating characteristic (FROC) data, Dev P. Chakraborty.
#'
# devtools::document();help(dataList.Chakra.1)
NULL


#' @title Data: A Single Reader and A Single Modality
#' @description A list, representing FROC data consisting of hits, false alarms, number of lesions, number of images. We fit a FROC model to the data.
#' @details
#'
#' Note that the maximal number of confidence level,
#' denoted by  \code{C}, are included,
#' however,  confidence level vector \code{c } should not be specified.
#'  If specified, will be ignored ,
#'  since it is created by \code{  c <-c(rep(C:1))} in the program
#'  and it does not refer from user input data,
#'  where \code{C} is the highest number of confidence levels.
#'Should write down your hits and
#'false alarms vector so that it is compatible with this automatically created  vector \code{c}.
#'
#'
#'
#'
#'
#'This data appeared in Chakraborty's paper (1988).

#'
#'
#' @format
#'  A list consists of two integer vectors  \code{f, h} and three integers \code{NL, NI, C}.
#'

#'

#' \describe{
#' \item{ \code{f}  }{Non-negative integer vector  specifying  number of false alarms   associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{h}  }{Non-negative integer vector  specifying  number  of Hits  associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{NL} }{A positive integer, representing  Number of Lesions.}
#' \item{ \code{NI} }{A positive integer, representing  Number of Images. }
#' \item{ \code{C}  }{A positive integer, representing  Number of Confidence level. }
#' }
#'
#'
#'

#'
#'\strong{\emph{Contents:}}
#'
#'  \emph{            A single reader and a single modality case   }
#'
#'
#'
#'
#'------------------------------------------------------------------------------------------------------

#' \tabular{rccc}{
#' \code{NI=57,NL=269}   \tab \strong{ confidence level } \tab \strong{ No. of false alarms} \tab \strong{No. of hits}  \cr
#'  In R console ->      \tab \code{ c} \tab   \code{f }  \tab   \code{h}  \cr
#'   -----------------------\tab ----------------------- \tab ----------------------------- \tab ------------- \cr
#' \emph{\strong{definitely present}}  \tab  3 \tab 4 \tab     122\cr
#'  probably present                   \tab  2 \tab 13 \tab    31\cr
#'  questionable                       \tab  1 \tab 44 \tab   20\cr
#'  }
#'
#'---------------------------------------------------------------------------------------------------
#'
#'
#'*  \emph{false alarms} = False Positives = FP
#'
#'*  \emph{hits} = True Positives = TP
#'
#'Note that  in FROC data, the confidence level means present (deseased, positive) case only. Since each reader marks their suspicous location only and it generate the hits and false alarms for his confidenc level representing that lesion is present.
#'In the absent case, reader dose not mark any locations and hence, the absent cofidence level does not relate this dataset.
#'
#'
#' Note that the first column of confidence level vector \code{c } should not be specified. If specified, will be ignored , since it is created by \code{  c <-c(rep(C:1))} automatically in the program and it does not refer from user input data even if it is specified explicitly, where \code{C} is the highest number of confidence levels.
#'So you should check the compatibility of your data and the program's generating new confidence  level vector by
#'a table which can be displayed by the function \code{\link{viewdata}()}.
#'
#'
#'Note that The format for the above example data must be made by the following forms:
#'
#' \code{ dat <- list(       }
#'
#'\code{            h = c(122,  31,  20 ),   }
#'
#' \code{            f = c( 4, 13, 44 ),    }
#'
#' \code{            NL = 269,     }
#'
#' \code{            NI = 57,    }
#'
#' \code{            C = 3)         }
#'
#' This object \code{dat} can be passed to the function \code{\link{fit_Bayesian_FROC}()}  as the following manner \code{fit_Bayesian_FROC(dat)}.

#'
#'
#'
# dataList.Chakra.2 ----
#' @name dataList.Chakra.2
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#' @seealso \code{\link{dataList.Chakra.1.with.explantation}}

#' @references Maximum likelihood analysis of free-response  receiver operating characteristic (FROC) data, Dev P. Chakraborty.
#'
# devtools::document();help(dataList.Chakra.1)
NULL
















#' @title Data: A Single Reader and A Single Modality
#' @description A list, representing FROC data consisting of hits, false alarms, number of lesions, number of images. We fit a FROC model to the data.
#' @details
#'
#' Note that the maximal number of confidence level,
#' denoted by  \code{C}, are included,
#' however,  confidence level vector \code{c } should not be specified.
#'  If specified, will be ignored ,
#'  since it is created by \code{  c <-c(rep(C:1))} in the program
#'  and it does not refer from user input data,
#'  where \code{C} is the highest number of confidence levels.
#'Should write down your hits and
#'false alarms vector so that it is compatible with this automatically created  vector \code{c}.
#'
#'
#'
#'
#'
#'This data appeared in Chakraborty's paper (1988).

#'
#'
#'
#' @format
#'  A list consists of two integer vectors  \code{f, h} and three integers \code{NL, NI, C}.
#'

#'

#' \describe{
#' \item{ \code{f}  }{Non-negative integer vector  specifying  number of false alarms   associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{h}  }{Non-negative integer vector  specifying  number  of Hits  associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{NL} }{A positive integer, representing  Number of Lesions.}
#' \item{ \code{NI} }{A positive integer, representing  Number of Images. }
#' \item{ \code{C}  }{A positive integer, representing  Number of Confidence level. }
#' }
#'
#'
#'

#'
#'\strong{\emph{Contents:}}
#'
#'  \emph{            A single reader and a single modality case   }
#'
#'
#'
#'
#'------------------------------------------------------------------------------------------------------

#' \tabular{rccc}{
#' \code{NI=57,NL=269}   \tab \strong{ confidence level } \tab \strong{ No. of false alarms} \tab \strong{No. of hits}  \cr
#'  In R console ->      \tab \code{ c} \tab   \code{f }  \tab   \code{h}  \cr
#'   -----------------------\tab ----------------------- \tab ----------------------------- \tab ------------- \cr
#' \emph{\strong{definitely present}}  \tab  3 \tab 2 \tab     96\cr
#'  probably present                   \tab  2 \tab 16 \tab    39\cr
#'  questionable                       \tab  1 \tab 48 \tab   13\cr
#'  }
#'
#'---------------------------------------------------------------------------------------------------
#'
#'
#'*  \emph{false alarms} = False Positives = FP
#'
#'*  \emph{hits} = True Positives = TP
#'
#'Note that  in FROC data, the confidence level means present (deseased, positive) case only. Since each reader marks their suspicous location only and it generate the hits and false alarms for his confidenc level representing that lesion is present.
#'In the absent case, reader dose not mark any locations and hence, the absent cofidence level does not relate this dataset.
#'
#'
#' Note that the first column of confidence level vector \code{c } should not be specified. If specified, will be ignored , since it is created by \code{  c <-c(rep(C:1))} automatically in the program and it does not refer from user input data even if it is specified explicitly, where \code{C} is the highest number of confidence levels.
#'So you should check the compatibility of your data and the program's generating new confidence  level vector by
#'a table which can be displayed by the function \code{\link{viewdata}()}.
#'
#'
#'Note that The format for the above example data must be made by the following forms:
#'
#' \code{ dat <- list(       }
#'
#'\code{            h = c(96, 39, 13 ),   }
#'
#' \code{            f = c( 2, 16, 48),    }
#'
#' \code{            NL = 269,     }
#'
#' \code{            NI = 57,    }
#'
#' \code{            C = 3)         }
#'
#' This object \code{dat} can be passed to the function \code{\link{fit_Bayesian_FROC}()}  as the following manner \code{fit_Bayesian_FROC(dat)}.

#'
#'
# dataList.Chakra.3 ----
#' @name dataList.Chakra.3
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#' @seealso \code{\link{dataList.Chakra.1.with.explantation}}

#' @references Maximum likelihood analysis of free-response  receiver operating characteristic (FROC) data, Dev P. Chakraborty.
#'
# devtools::document();help(dataList.Chakra.1)
NULL
















#' @title Data: A Single Reader and A Single Modality
#' @description A list, representing FROC data consisting of hits, false alarms, number of lesions, number of images. We fit a FROC model to the data.
#' @details
#'
#' Note that the maximal number of confidence level,
#' denoted by  \code{C}, are included,
#' however,  confidence level vector \code{c } should not be specified.
#'  If specified, will be ignored ,
#'  since it is created by \code{  c <-c(rep(C:1))} in the program
#'  and it does not refer from user input data,
#'  where \code{C} is the highest number of confidence levels.
#'Should write down your hits and
#'false alarms vector so that it is compatible with this automatically created  vector \code{c}.
#'
#'
#'
#'
#'
#'This data appeared in Chakraborty's paper (1988).

#'
#'
#'
#' @format
#'  A list consists of two integer vectors  \code{f, h} and three integers \code{NL, NI, C}.
#'

#'

#' \describe{
#' \item{ \code{f}  }{Non-negative integer vector  specifying  number of false alarms   associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{h}  }{Non-negative integer vector  specifying  number  of Hits  associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{NL} }{A positive integer, representing  Number of Lesions.}
#' \item{ \code{NI} }{A positive integer, representing  Number of Images. }
#' \item{ \code{C}  }{A positive integer, representing  Number of Confidence level. }
#' }
#'
#'
#'

#'
#'\strong{\emph{Contents:}}
#'
#'  \emph{            A single reader and a single modality case   }
#'
#'
#'
#'
#'------------------------------------------------------------------------------------------------------

#' \tabular{rccc}{
#' \code{NI=50,NL=397}   \tab \strong{ confidence level } \tab \strong{ No. of false alarms} \tab \strong{No. of hits}  \cr
#'  In R console ->      \tab \code{ c} \tab   \code{f }  \tab   \code{h}  \cr
#'   -----------------------\tab ----------------------- \tab ----------------------------- \tab ------------- \cr
#' \emph{\strong{definitely present}}  \tab  4 \tab 8 \tab     160\cr
#'  probably present                   \tab  3 \tab 16 \tab    25\cr
#'  subtle                             \tab  2 \tab 18 \tab   15\cr
#'  very subtle                        \tab  1 \tab 13 \tab   7\cr
#'  }
#'
#'---------------------------------------------------------------------------------------------------
#'
#'
#'*  \emph{false alarms} = False Positives = FP
#'
#'*  \emph{hits} = True Positives = TP
#'
#'Note that  in FROC data, the confidence level means present (deseased, positive) case only. Since each reader marks their suspicous location only and it generate the hits and false alarms for his confidenc level representing that lesion is present.
#'In the absent case, reader dose not mark any locations and hence, the absent cofidence level does not relate this dataset.
#'
#'
#' Note that the first column of confidence level vector \code{c } should not be specified. If specified, will be ignored , since it is created by \code{  c <-c(rep(C:1))} automatically in the program and it does not refer from user input data even if it is specified explicitly, where \code{C} is the highest number of confidence levels.
#'So you should check the compatibility of your data and the program's generating new confidence  level vector by
#'a table which can be displayed by the function \code{\link{viewdata}()}.
#'
#'
#'Note that The format for the above example data must be made by the following forms:
#'
#' \code{ dat <- list(       }
#'
#'\code{            h = c(160,  25,  15,   7),   }
#'
#' \code{            f = c( 8, 16, 18, 13),    }
#'
#' \code{            NL = 397,     }
#'
#' \code{            NI = 50,    }
#'
#' \code{            C = 4)         }
#'
#' This object \code{dat} can be passed to the function \code{\link{fit_Bayesian_FROC}()}  as the following manner \code{fit_Bayesian_FROC(dat)}.

#'
# dataList.Chakra.4 ----
#' @name dataList.Chakra.4
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#' @seealso \code{\link{dataList.Chakra.1.with.explantation}}

#' @references Maximum likelihood analysis of free-response  receiver operating characteristic (FROC) data, Dev P. Chakraborty.
#'
# devtools::document();help(dataList.Chakra.1)
NULL






#' @title Data: A Single Reader and A Single Modality
#' @description A list, representing FROC data. This is used to build a hierarchical FROC model. This data is same as dataList.Chakra.1.
#' @details This data appeared in Chakraborty's paper (1988)
# data.SingleReaderSingleModality ----
#' @name data.SingleReaderSingleModality
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#' @seealso \code{\link{dataList.Chakra.1.with.explantation}}

#' @references Maximum likelihood analysis of free-response  receiver operating characteristic (FROC) data, Dev P. Chakraborty.
#'
# devtools::document();help(dataList.Chakra.1)
NULL

#' @title Data: A Single Reader and A Single Modality
#' @description A list, representing FROC data. This is used to build a hierarchical FROC model. This data is same as dataList.Chakra.1.
#' @details This data-set is fictitious.
# dataList.high.ability ----
#' @name dataList.high.ability
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#' @seealso \code{\link{dataList.Chakra.1.with.explantation}}

#' @references Maximum likelihood analysis of free-response  receiver operating characteristic (FROC) data, Dev P. Chakraborty.
#'
# devtools::document();help(dataList.Chakra.1)
NULL



#' @title Data: A Single Reader and A Single Modality
#' @description A list, representing FROC data. This is used to build a hierarchical FROC model. This data is same as dataList.Chakra.1.
#' @details This data-set is fictitious.
# dataList.low.ability ----
#' @name dataList.low.ability
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#' @seealso \code{\link{dataList.Chakra.1.with.explantation}}

#' @references Maximum likelihood analysis of free-response  receiver operating characteristic (FROC) data, Dev P. Chakraborty.
#'

# devtools::document();help(dataList.Chakra.1)
NULL








#' @title Data: A Single Reader and A Single Modality
#' @description A list, representing FROC data. This is used to build a hierarchical FROC model. This data is exactly same as dataList.Chakra.1.
#' @details This data is same as \code{\link{dataList.Chakra.1.with.explantation}}.
#'  The author name it  \code{d} for the sake of simplicity, that is, it is easy to write, because only one character!!
#'
#'
#'
#'
# d ----
#' @name d
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#' @seealso \code{\link{dataList.Chakra.1.with.explantation}} which is exactly same in this data \code{d}.

#' @references Maximum likelihood analysis of free-response  receiver operating characteristic (FROC) data, Dev P. Chakraborty.
#'
# devtools::document();help(dataList.Chakra.1)
NULL































#' @title Data: Single reader and Single modality
#' @description A list, representing FROC data. This is used to build a hierarchical FROC model. This data is same as dataList.Chakra.1.
#' @details This data-set is fictitious.
# dataList.High ----
#' @name dataList.High
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#' @seealso \code{\link{dataList.Chakra.1.with.explantation}}

#' @references Maximum likelihood analysis of free-response  receiver operating characteristic (FROC) data, Dev P. Chakraborty.
#'
#  @keywords Single reader and Single modality data. Non-hierarchical FROC data.
# devtools::document();help(dataList.Chakra.1)
NULL


#' @title Data: Single reader and Single modality
#' @description A list, representing FROC data to which we fit a  FROC model. This data is same as dataList.Chakra.1.
#' @details This data-set is fictitious.
# dataList.Low -----
#' @name dataList.Low
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#' @seealso \code{\link{dataList.Chakra.1.with.explantation}}

#' @references Maximum likelihood analysis of free-response  receiver operating characteristic (FROC) data, Dev P. Chakraborty.
#'
# devtools::document();help(dataList.Chakra.1)
NULL






















#' @title Data: Single reader and Single modality
#' @description A list, representing FROC data consisting
#'  of hits, false alarms, number of lesions, number of images,
#'   to which we fit a FROC model.
#' @details
#'
#' This data-set is very bad  fitting. Even if the MCMC sampling is very good,
#'  however,
#' the FPF and TPF are not on the FROC curve.
#'
#'
#'
#'
#'
#' Note that the maximal number of confidence level,
#' denoted by  \code{C}, are included,
#' however,  confidence level vector \code{c } should not be specified.
#'  If specified, will be ignored ,
#'  since it is created by \code{  c <-c(rep(C:1))} in the program
#'  and it does not refer from user input data,
#'  where \code{C} is the highest number of confidence levels.
#'Should write down your hits and
#'false alarms vector so that it is compatible with this automatically created  vector \code{c}.
#'
#'
#'
#'
#'
#'
#'
#'
#' @format
#'  A list consists of two integer vectors  \code{f, h} and three integers \code{NL, NI, C}.
#'

#'

#' \describe{
#' \item{ \code{f}  }{Non-negative integer vector  specifying  number of false alarms associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{h}  }{Non-negative integer vector  specifying  number  of Hits  associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{NL} }{A positive integer, representing  Number of Lesions.}
#' \item{ \code{NI} }{A positive integer, representing  Number of Images. }
#' \item{ \code{C}  }{A positive integer, representing  Number of Confidence level. }
#' }
#'
#'
#'

#'
#'\strong{\emph{Contents:}}
#'
#'  \emph{            A single reader and single modality case   }
#'
#'
#'
#'
#'------------------------------------------------------------------------------------------------------

#' \tabular{rccc}{
#' \code{NI=57,NL=259}   \tab \strong{ confidence level } \tab \strong{ No. of false alarms} \tab \strong{No. of hits}  \cr
#'  In R console ->      \tab \code{ c} \tab   \code{f }  \tab   \code{h}  \cr
#'   -----------------------\tab ----------------------- \tab ----------------------------- \tab ------------- \cr
#' \emph{\strong{definitely present}}  \tab  4 \tab 11 \tab     11\cr
#'  probably present                   \tab  3 \tab 1 \tab    97\cr
#'  subtle                             \tab  2 \tab 14 \tab   32\cr
#'  very subtle                        \tab  1 \tab 74 \tab   31\cr
#'  }
#'
#'---------------------------------------------------------------------------------------------------
#'
#'
#'*  \emph{false alarms} = False Positives = FP
#'
#'*  \emph{hits} = True Positives = TP
#'
#'Note that  in FROC data, the confidence level means present (deseased, positive) case only. Since each reader marks their suspicous location only and it generate the hits and false alarms for his confidenc level representing that lesion is present.
#'In the absent case, reader dose not mark any locations and hence, the absent cofidence level does not relate this dataset.
#'
#'
#' Note that the first column of confidence level vector \code{c } should not be specified. If specified, will be ignored , since it is created by \code{  c <-c(rep(C:1))} automatically in the program and it does not refer from user input data even if it is specified explicitly, where \code{C} is the highest number of confidence levels.
#'So you should check the compatibility of your data and the program's generating new confidence  level vector by
#'a table which can be displayed by the function \code{\link{viewdata}()}.
#'
#'
#'Note that The format for the above example data must be made by the following forms:
#'
#' \code{ dat <- list(       }
#'
#'\code{            h = c(11,  97,  32,   31),   }
#'
#' \code{            f = c( 11, 1, 14, 74),    }
#'
#' \code{            NL = 259,     }
#'
#' \code{            NI = 57,    }
#'
#' \code{            C = 4)         }
#'
#' This object \code{dat} can be passed to the function \code{\link{fit_Bayesian_FROC}()}  as the following manner \code{fit_Bayesian_FROC(dat)}.

#'
# data.bad.fit ----
#' @name data.bad.fit
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#' @seealso \code{\link{viewdata}()}, which shows your data confortably by \code{knitr::kable()}.

#' @references I love you.
#'
#  @keywords Single reader and Single modality data. Non-hierarchical FROC data.
# devtools::document();help(dataList.Chakra.1)
NULL


































#' @title \strong{Non-Convergent} Data: Single reader and Single modality
#' @description A list, representing \strong{non-convergent} FROC data (which does not converge in the sence of R hat) of hits and false alarms. This is used to build a non-hierarchical FROC model.
#' @details
#'
#' Note that the maximal number of confidence level,
#' denoted by  \code{C}, are included,
#' however,  confidence level vector \code{c } should not be specified.
#'  If specified, will be ignored ,
#'  since it is created by \code{  c <-c(rep(C:1))} in the program
#'  and it does not refer from user input data,
#'  where \code{C} is the highest number of confidence levels.
#'Should write down your hits and
#'false alarms vector so that it is compatible with this automatically created  vector \code{c}.
#'
#'
#'
#'
#'

#'
#'
#'
#' @format
#'  A list consists of two integer vectors  \code{f, h} and three integers \code{NL, NI, C}.
#'

#'

#' \describe{
#' \item{ \code{f}  }{Non-negative integer vector  specifying  number of false alarms   associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{h}  }{Non-negative integer vector  specifying  number  of Hits  associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{NL} }{A positive integer, representing  Number of Lesions.}
#' \item{ \code{NI} }{A positive integer, representing  Number of Images. }
#' \item{ \code{C}  }{A positive integer, representing  Number of Confidence level. }
#' }
#'
#'
#'

#'
#'\strong{\emph{Contents:}}
#'
#'  \emph{            A single reader and single modality case   }
#'
#'
#'
#'
#'------------------------------------------------------------------------------------------------------

#' \tabular{rccc}{
#' \code{NI=57,NL=269}   \tab \strong{ confidence level } \tab \strong{ No. of false alarms} \tab \strong{No. of hits}  \cr
#'  In R console ->      \tab \code{ c} \tab   \code{f }  \tab   \code{h}  \cr
#'   -----------------------\tab ----------------------- \tab ----------------------------- \tab ------------- \cr
#' \emph{\strong{definitely present}}  \tab  3 \tab 99 \tab     88\cr
#'  probably present                   \tab  2 \tab 0 \tab    0\cr
#'  questionable                       \tab  1 \tab 0 \tab   0\cr
#'  }
#'
#'---------------------------------------------------------------------------------------------------
#'
#'
#'*  \emph{false alarms} = False Positives = FP
#'
#'*  \emph{hits} = True Positives = TP
#'
#'Note that  in FROC data, the confidence level means present (deseased, positive) case only. Since each reader marks their suspicous location only and it generate the hits and false alarms for his confidenc level representing that lesion is present.
#'In the absent case, reader dose not mark any locations and hence, the absent cofidence level does not relate this dataset.
#'
#'
#' Note that the first column of confidence level vector \code{c } should not be specified. If specified, will be ignored , since it is created by \code{  c <-c(rep(C:1))} automatically in the program and it does not refer from user input data even if it is specified explicitly, where \code{C} is the highest number of confidence levels.
#'So you should check the compatibility of your data and the program's generating new confidence  level vector by
#'a table which can be displayed by the function \code{\link{viewdata}()}.
#'
#'
#'Note that The format for the above example data must be made by the following forms:
#'
#' \code{ dat <- list(       }
#'
#'\code{            h = c(99, 0, 0 ),   }
#'
#' \code{            f = c( 88, 0, 0),    }
#'
#' \code{            NL = 111,     }
#'
#' \code{            NI = 111,    }
#'
#' \code{            C = 3)         }
#'
#' This object \code{dat} can be passed to the function \code{\link{fit_Bayesian_FROC}()}  as the following manner \code{fit_Bayesian_FROC(dat)}.

#'
# data.nonconverge.srsc -----
#'
#' @name data.nonconverge.srsc
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#' @seealso \code{\link{dataList.Chakra.1.with.explantation}}
#'
# devtools::document();help(dataList.Chakra.1)
NULL



























#' @title An FROC Dataset with \strong{\emph{Divergent Transitions}} in case of A Single reader and A Single modality
#' @description A list, representing an  FROC dataset with \strong{divergent transitions }.
#'
#' Note that the maximal number of confidence level,
#' denoted by  \code{C}, are included,
#' however,  confidence level vector \code{c } should not be specified.
#'  If specified, will be ignored ,
#'  since it is created by \code{  c <-c(rep(C:1))} in the program
#'  and it does not refer from user input data,
#'  where \code{C} is the highest number of confidence levels.
#'Should write down your hits and
#'false alarms vector so that it is compatible with this automatically created  vector \code{c}.
#'
#'
#'
#'
#'

#'
#'
#'
#' @format
#'  A list consists of the following
#'   integer vectors  \code{f, h} and integers \code{NL, NI, C}.
#'

#'

#' \describe{
#' \item{ \code{f}  }{Non-negative integer vector  specifying  number of false alarms   associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{h}  }{Non-negative integer vector  specifying  number  of Hits  associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{NL} }{A positive integer, representing  Number of Lesions.}
#' \item{ \code{NI} }{A positive integer, representing  Number of Images. }
#' \item{ \code{C}  }{A positive integer, representing  Number of Confidence level. }
#' }
#'
#'
#'

#'
#'\strong{\emph{Contents:}}
#'
#'  \emph{ A single reader and single modality case   }
#'
#'
#'
#'
#'------------------------------------------------------------------------------------------------------

#' \tabular{rccc}{
#' \code{NI=57,NL=269}   \tab \strong{ confidence level } \tab \strong{ No. of false alarms} \tab \strong{No. of hits}  \cr
#'  In R console ->      \tab \code{ c} \tab   \code{f }  \tab   \code{h}  \cr
#'   -----------------------\tab ----------------------- \tab ----------------------------- \tab ------------- \cr
#' \emph{\strong{definitely present}}  \tab  3 \tab 0 \tab     21\cr
#'  probably present                   \tab  2 \tab 7 \tab    4\cr
#'  questionable                       \tab  1 \tab 36 \tab   3\cr
#'  }
#'
#'---------------------------------------------------------------------------------------------------
#'
#'
#'*  \emph{false alarms} = False Positives = FP
#'
#'*  \emph{hits} = True Positives = TP
#'
#'Note that  in FROC data, the confidence level means present (deseased, positive) case only. Since each reader marks their suspicous location only and it generate the hits and false alarms for his confidenc level representing that lesion is present.
#'In the absent case, reader dose not mark any locations and hence, the absent cofidence level does not relate this dataset.
#'
#'
#' Note that the first column of confidence level vector \code{c } should not be specified. If specified, will be ignored , since it is created by \code{  c <-c(rep(C:1))} automatically in the program and it does not refer from user input data even if it is specified explicitly, where \code{C} is the highest number of confidence levels.
#'So you should check the compatibility of your data and the program's generating new confidence  level vector by
#'a table which can be displayed by the function \code{\link{viewdata}()}.
#'
#'
#'Note that The format for the above example data must be made by the following forms:
#'
#'
#' \code{ dat <- list(                      }
#'
#' \code{   c=c(3,2,1),    #Confidence level}
#'
#' \code{   h=c(21,4,3), #Number of hits for each confidence level}
#'
#'  \code{  f=c(0,7,36),  #Number of false alarms for each confidence level}
#'
#' \code{   NL=60,       #Number of lesions}
#'
#'  \code{  NI=30,        #Number of images}
#'
#' \code{   C=3)          #Number of confidence level    }
#'
#' This \R object \code{dat} can be passed to the function \code{\link{fit_Bayesian_FROC}()}  as the following manner \code{fit_Bayesian_FROC(dat)}.

#'
#'
# dataList.divergent.transition.in.case.of.srsc -----
#' @name dataList.divergent.transition.in.case.of.srsc
#' @docType data
#'
#'
#'
#'@examples
#' \dontrun{
#'#========================================================================================
#'#  Change the zero cell to 1,
#'#  then The number of divergent transitions are significantly decrease
#'#  Thus, the divergent transtions is not rigid.
#'#========================================================================================
#'
#' data   <- dataList.divergent.transition.in.case.of.srsc
#' data$f <- c(1,7,36)
#' f      <- fit_Bayesian_FROC( ite  = 1111,  cha = 1, summary = TRUE, dataList = data )
#'
#'
#' }#dontrun
#'
#'
# devtools::document();help(dataList.Chakra.1)
NULL





# df<-data.frame(ddd$m,ddd$q,ddd$c,ddd$f,ddd$h)
tabular <- function(df, ...) {
  stopifnot(is.data.frame(df))

  align <- function(x) if (is.numeric(x)) "r" else "l"
  col_align <- vapply(df, align, character(1))

  cols <- lapply(df, format, ...)
  contents <- do.call("paste",
                      c(cols, list(sep = " \\tab ", collapse = "\\cr\n  ")))

  paste("\\tabular{", paste(col_align, collapse = ""), "}{\n  ",
        contents, "\n}\n", sep = "")
}
