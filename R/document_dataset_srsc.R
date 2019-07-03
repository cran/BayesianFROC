



#' @title  Single reader and Single modality data
#' @description A list, representing FROC data of hits and false alarms. This is used to build a non-hierarchical FROC model.
#' @details
#'
#' Note that the maximal number of confidence level,
#' denoted by  \code{C}, are included,
#' however,  confidence level vector \code{c } should not be specified.
#'  If specified, will be ignored ,
#'  since it is created by \code{  c <-c(rep(C:1))} in the program
#'  and do not refer from user input data,
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
#'  This data list contains the following integer vectors  \code{f, h} and integers \code{NL, NI, C}.
#'

#'

#' \describe{
#' \item{ \code{f}  }{Non-negative integer vector  specifying  number of False Alarms   associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{h}  }{Non-negative integer vector  specifying  number  of Hits  associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{NL} }{A positive integer, representing  Number of Lesions.}
#' \item{ \code{NI} }{A positive integer, representing  Number of Images. }
#' \item{ \code{C}  }{A positive integer, representing  Number of Confidence level. }
#' }
#'
#'
#'

#'
#'\strong{\emph{ Example data:}}
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
#' Note that the first column of confidence level vector \code{c } should not be specified. If specified, will be ignored , since it is created by \code{  c <-c(rep(C:1))} automatically in the program and do not refer from user input data even if it is specified explicitly, where \code{C} is the highest number of confidence levels.
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
#'And using this object \code{dat}, we can apply \code{\link{fit_Bayesian_FROC}()} such as \code{fit_Bayesian_FROC(dat)}.

#'
#'
#'
#'
#' @name dataList.Chakra.1.with.explantation
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#'
#' @references Maximum likelihood analysis of free-response receiver operating characteristic (FROC) data, Dev P. Chakraborty.
#' @source Maximum likelihood analysis of free-response receiver operating characteristic (FROC) data, Dev P. Chakraborty.
 # devtools::document();help(dataList.Chakra.1)
NULL





























#' @title  Hit Rate data
#' @description For the default value of a variable of a function.
#' @details Hit Rate data of some MRMC data to use as a default value
#' of the function \code{ hits_creator_from_rate}.
#' This is an array obtained from estimates of some data contained in this package.
#' To simulate a replication of dataset, the default values should be used from an actual values.
#'  Thus the author prepare this data.

#' @name p_truth
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#' @seealso \code{hits_creator_from_rate}
#'
NULL


#' @title  Mean data of MRMC
#' @description For the default value of a variable of a function.
#' @details Mean Rate data of some MRMC data to use as a default value
#' of the function \code{ hits_creator_from_rate}.
#' This is an array obtained from estimates of some data contained in this package.
#' To simulate a replication of dataset, the default values should be used from an actual values.
#'  Thus the author prepare this data.

#' @name mu_truth
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#' @seealso \code{hits_creator_from_rate}
#'
NULL



#' @title  Standard Deviation data of MRMC
#' @description For the default value of a variable of a function.
#' @details Standard Deviation Rate data of some MRMC data to use as a default value
#' of the function \code{ hits_creator_from_rate}.
#' This is an array obtained from estimates of some data contained in this package.
#' To simulate a replication of dataset, the default values should be used from an actual values.
#'  Thus the author prepare this data.
#' @name v_truth
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#' @seealso \code{hits_creator_from_rate}
#'
NULL

#' @title  Threshold data of MRMC
#' @description For the default value of a variable of a function.
#' @details Threshold Rate data of some MRMC data to use as a default value
#' of the function \code{ hits_creator_from_rate}.
#' This is an array obtained from estimates of some data contained in this package.
#' To simulate a replication of dataset, the default values should be used from an actual values.
#'  Thus the author prepare this data.
#' @name z_truth
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#' @seealso \code{hits_creator_from_rate}
#'
NULL
















#' @title  Single reader and Single modality data
#' @description A list, representing FROC data of hits and false alarms. This is used to build a non-hierarchical FROC model.
#' @details
#'
#' Note that the maximal number of confidence level,
#' denoted by  \code{C}, are included,
#' however,  confidence level vector \code{c } should not be specified.
#'  If specified, will be ignored ,
#'  since it is created by \code{  c <-c(rep(C:1))} in the program
#'  and do not refer from user input data,
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
#'  This data list contains the following integer vectors  \code{f, h} and integers \code{NL, NI, C}.
#'

#'

#' \describe{
#' \item{ \code{f}  }{Non-negative integer vector  specifying  number of False Alarms   associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{h}  }{Non-negative integer vector  specifying  number  of Hits  associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{NL} }{A positive integer, representing  Number of Lesions.}
#' \item{ \code{NI} }{A positive integer, representing  Number of Images. }
#' \item{ \code{C}  }{A positive integer, representing  Number of Confidence level. }
#' }
#'
#'
#'

#'
#'\strong{\emph{ Example data:}}
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
#' Note that the first column of confidence level vector \code{c } should not be specified. If specified, will be ignored , since it is created by \code{  c <-c(rep(C:1))} automatically in the program and do not refer from user input data even if it is specified explicitly, where \code{C} is the highest number of confidence levels.
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
#'And using this object \code{dat}, we can apply \code{\link{fit_Bayesian_FROC}()} such as \code{fit_Bayesian_FROC(dat)}.

#'
#'
#'
#'
#' @name dataList.Chakra.1
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#' @seealso \code{\link{dataList.Chakra.1.with.explantation}}
#' @references Maximum likelihood analysis of free-response receiver operating characteristic (FROC) data, Dev P. Chakraborty.
#'
#  @keywords Single reader and Single modality data. Non-hierarchical FROC data.
# devtools::document();help(dataList.Chakra.1)
NULL


#' @title  Single reader and Single modality data
#' @description A list, representing FROC data of hits and false alarms. This is used to build a non-hierarchical FROC model.
#' @details
#'
#' Note that the maximal number of confidence level,
#' denoted by  \code{C}, are included,
#' however,  confidence level vector \code{c } should not be specified.
#'  If specified, will be ignored ,
#'  since it is created by \code{  c <-c(rep(C:1))} in the program
#'  and do not refer from user input data,
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
#'  This data list contains the following integer vectors  \code{f, h} and integers \code{NL, NI, C}.
#'

#'

#' \describe{
#' \item{ \code{f}  }{Non-negative integer vector  specifying  number of False Alarms   associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{h}  }{Non-negative integer vector  specifying  number  of Hits  associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{NL} }{A positive integer, representing  Number of Lesions.}
#' \item{ \code{NI} }{A positive integer, representing  Number of Images. }
#' \item{ \code{C}  }{A positive integer, representing  Number of Confidence level. }
#' }
#'
#'
#'

#'
#'\strong{\emph{ Example data:}}
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
#' Note that the first column of confidence level vector \code{c } should not be specified. If specified, will be ignored , since it is created by \code{  c <-c(rep(C:1))} automatically in the program and do not refer from user input data even if it is specified explicitly, where \code{C} is the highest number of confidence levels.
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
#'And using this object \code{dat}, we can apply \code{\link{fit_Bayesian_FROC}()} such as \code{fit_Bayesian_FROC(dat)}.

#'
#'
#'
#'
#' @name dataList.Chakra.2
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#' @seealso \code{\link{dataList.Chakra.1.with.explantation}}

#' @references Maximum likelihood analysis of free-response  receiver operating characteristic (FROC) data, Dev P. Chakraborty.
#'
 # devtools::document();help(dataList.Chakra.1)
NULL
















#' @title  Single reader and Single modality data
#' @description A list, representing FROC data of hits and false alarms. This is used to build a non-hierarchical FROC model.
#' @details
#'
#' Note that the maximal number of confidence level,
#' denoted by  \code{C}, are included,
#' however,  confidence level vector \code{c } should not be specified.
#'  If specified, will be ignored ,
#'  since it is created by \code{  c <-c(rep(C:1))} in the program
#'  and do not refer from user input data,
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
#'  This data list contains the following integer vectors  \code{f, h} and integers \code{NL, NI, C}.
#'

#'

#' \describe{
#' \item{ \code{f}  }{Non-negative integer vector  specifying  number of False Alarms   associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{h}  }{Non-negative integer vector  specifying  number  of Hits  associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{NL} }{A positive integer, representing  Number of Lesions.}
#' \item{ \code{NI} }{A positive integer, representing  Number of Images. }
#' \item{ \code{C}  }{A positive integer, representing  Number of Confidence level. }
#' }
#'
#'
#'

#'
#'\strong{\emph{ Example data:}}
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
#' Note that the first column of confidence level vector \code{c } should not be specified. If specified, will be ignored , since it is created by \code{  c <-c(rep(C:1))} automatically in the program and do not refer from user input data even if it is specified explicitly, where \code{C} is the highest number of confidence levels.
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
#'And using this object \code{dat}, we can apply \code{\link{fit_Bayesian_FROC}()} such as \code{fit_Bayesian_FROC(dat)}.

#'
#'
#'
#' @name dataList.Chakra.3
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#' @seealso \code{\link{dataList.Chakra.1.with.explantation}}

#' @references Maximum likelihood analysis of free-response  receiver operating characteristic (FROC) data, Dev P. Chakraborty.
#'
 # devtools::document();help(dataList.Chakra.1)
NULL
















#' @title  Single reader and Single modality data
#' @description A list, representing FROC data of hits and false alarms. This is used to build a non-hierarchical FROC model.
#' @details
#'
#' Note that the maximal number of confidence level,
#' denoted by  \code{C}, are included,
#' however,  confidence level vector \code{c } should not be specified.
#'  If specified, will be ignored ,
#'  since it is created by \code{  c <-c(rep(C:1))} in the program
#'  and do not refer from user input data,
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
#'  This data list contains the following integer vectors  \code{f, h} and integers \code{NL, NI, C}.
#'

#'

#' \describe{
#' \item{ \code{f}  }{Non-negative integer vector  specifying  number of False Alarms   associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{h}  }{Non-negative integer vector  specifying  number  of Hits  associated with  each confidence level. The first component corresponding to the highest confidence level.}
#' \item{ \code{NL} }{A positive integer, representing  Number of Lesions.}
#' \item{ \code{NI} }{A positive integer, representing  Number of Images. }
#' \item{ \code{C}  }{A positive integer, representing  Number of Confidence level. }
#' }
#'
#'
#'

#'
#'\strong{\emph{ Example data:}}
#'
#'  \emph{            A single reader and single modality case   }
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
#' Note that the first column of confidence level vector \code{c } should not be specified. If specified, will be ignored , since it is created by \code{  c <-c(rep(C:1))} automatically in the program and do not refer from user input data even if it is specified explicitly, where \code{C} is the highest number of confidence levels.
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
#'And using this object \code{dat}, we can apply \code{\link{fit_Bayesian_FROC}()} such as \code{fit_Bayesian_FROC(dat)}.

#'
#'
#' @name dataList.Chakra.4
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#' @seealso \code{\link{dataList.Chakra.1.with.explantation}}

#' @references Maximum likelihood analysis of free-response  receiver operating characteristic (FROC) data, Dev P. Chakraborty.
#'
#  @keywords Single reader and Single modality data. Non-hierarchical FROC data.
# devtools::document();help(dataList.Chakra.1)
NULL






#' @title  Single reader and Single modality data
#' @description A list, representing FROC data. This is used to build a hierarchical FROC model. This data is same as dataList.Chakra.1.
#' @details This data appeared in Chakraborty's paper (1988)
#' @name data.SingleReaderSingleModality
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#' @seealso \code{\link{dataList.Chakra.1.with.explantation}}

#' @references Maximum likelihood analysis of free-response  receiver operating characteristic (FROC) data, Dev P. Chakraborty.
#'
 # devtools::document();help(dataList.Chakra.1)
NULL

#' @title  Single reader and Single modality data
#' @description A list, representing FROC data. This is used to build a hierarchical FROC model. This data is same as dataList.Chakra.1.
#' @details This data is fictitious.
#' @name dataList.high.ability
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#' @seealso \code{\link{dataList.Chakra.1.with.explantation}}

#' @references Maximum likelihood analysis of free-response  receiver operating characteristic (FROC) data, Dev P. Chakraborty.
#'
#  @keywords Single reader and Single modality data. Non-hierarchical FROC data.
# devtools::document();help(dataList.Chakra.1)
NULL



#' @title  Single reader and Single modality data
#' @description A list, representing FROC data. This is used to build a hierarchical FROC model. This data is same as dataList.Chakra.1.
#' @details This data is fictitious.
#' @name dataList.low.ability
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#' @seealso \code{\link{dataList.Chakra.1.with.explantation}}

#' @references Maximum likelihood analysis of free-response  receiver operating characteristic (FROC) data, Dev P. Chakraborty.
#'
#  @keywords Single reader and Single modality data. Non-hierarchical FROC data.
# devtools::document();help(dataList.Chakra.1)
NULL








#' @title  Single reader and Single modality data
#' @description A list, representing FROC data. This is used to build a hierarchical FROC model. This data is same as dataList.Chakra.1.
#' @details This data is same as \code{\link{dataList.Chakra.1.with.explantation}}.
#'  The author name it  \code{d} for the sake of simplicity.
#'
#' @name d
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#' @seealso \code{\link{dataList.Chakra.1.with.explantation}}

#' @references Maximum likelihood analysis of free-response  receiver operating characteristic (FROC) data, Dev P. Chakraborty.
#'
# devtools::document();help(dataList.Chakra.1)
NULL































#' @title  Single reader and Single modality data
#' @description A list, representing FROC data. This is used to build a hierarchical FROC model. This data is same as dataList.Chakra.1.
#' @details This data is fictitious.
#' @name dataList.High
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#' @seealso \code{\link{dataList.Chakra.1.with.explantation}}

#' @references Maximum likelihood analysis of free-response  receiver operating characteristic (FROC) data, Dev P. Chakraborty.
#'
#  @keywords Single reader and Single modality data. Non-hierarchical FROC data.
# devtools::document();help(dataList.Chakra.1)
NULL


#' @title  Single reader and Single modality data
#' @description A list, representing FROC data. This is used to build a hierarchical FROC model. This data is same as dataList.Chakra.1.
#' @details This data is fictitious.
#' @name dataList.Low
#' @docType data
#' @author Issei Tsunoda \email{tsunoda.issei1111@gmail.com }
#' @seealso \code{\link{dataList.Chakra.1.with.explantation}}

#' @references Maximum likelihood analysis of free-response  receiver operating characteristic (FROC) data, Dev P. Chakraborty.
#'
 # devtools::document();help(dataList.Chakra.1)
NULL

