#' @title Creats vectors: \code{m,q,c}  from integers: \code{M,Q,C}
#' @description
#'  Makes \code{m,q,c} vectors
#'  from a collection of three integers \code{M,Q,C},
#'  where three vectors \code{m,q,c}
#'  denotes modality ID, reader ID, confidence level,
#'  respectively.
#'

#'
#'
#'@details
#'
#'  My research is not supported any found, I am completely independent
#'  and only my own or my parents are supported my research.
#'  No internet, poor condition, I made this. I must go on untill jounal accepts my manuscripts.
#'
#'  I am not happy to spent with FROC analysis, since it is not my interest.
#'  I want to research pure mathematics. I do not want to waste a time.
#'  I do not want to waste a time in hospital or plurigo nodularis.
#'  When I become happy? This program helps me? With great pain at 2019 Sept.
#'  2019 Sept. 8
#'
#' @param M A positive integer, representing modality ID
#' @param Q A positive integer, representing reader ID
#' @param C A positive integer, representing confidence level
#' @param NI A positive integer, indicating the number of cases, i.e., images
#'
#' @return A data-frame, including three vectors, which are named \code{m,q,c}  representing modality ID and reader ID and confidence level, respectively.
#'
#' For example, the resulting object of \code{a <- m_q_c_vector_from_M_Q_C(2,3,4)} is given by
#'
#' \code{  > a}

#'
#'
#' \tabular{rrr}{
#'   \code{ m }\tab \code{ q} \tab  \code{c} \cr
#'   -------- \tab ------ \tab ---------\cr

#'   1 \tab 1 \tab 4\cr
#'   1 \tab 1 \tab 3\cr
#'   1 \tab 1 \tab 2\cr
#'   1 \tab 1 \tab 1\cr
#'   1 \tab 2 \tab 4\cr
#'   1 \tab 2 \tab 3\cr
#'   1 \tab 2 \tab 2\cr
#'   1 \tab 2 \tab 1\cr
#'   1 \tab 3 \tab 4\cr
#'   1 \tab 3 \tab 3\cr
#'   1 \tab 3 \tab 2\cr
#'   1 \tab 3 \tab 1\cr
#'   2 \tab 1 \tab 4\cr
#'   2 \tab 1 \tab 3\cr
#'   2 \tab 1 \tab 2\cr
#'   2 \tab 1 \tab 1\cr
#'   2 \tab 2 \tab 4\cr
#'   2 \tab 2 \tab 3\cr
#'   2 \tab 2 \tab 2\cr
#'   2 \tab 2 \tab 1\cr
#'   2 \tab 3 \tab 4\cr
#'   2 \tab 3 \tab 3\cr
#'   2 \tab 3 \tab 2\cr
#'   2 \tab 3 \tab 1
#' }
#'
#'
#'
#'
#' @export
# @aliases m_c_q_vector_from_M_C_Q
#' @examples
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                     Create a ID vectors
#'#========================================================================================
#'
#'   a <- caseID_m_q_c_vector_from_NI_M_Q_C(2,2,3,4)
#'
#'   a$caseID
#'   a$m
#'   a$q
#'   a$c
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                      validation of this function
#'#========================================================================================
#'#'
#'
#'
#'             a   <- caseID_m_q_c_vector_from_NI_M_Q_C(2,2,3,4)
#'
#'             a$caseID
#'             a$m == dd$m
#'             a$c == dd$c
#'             a$q == dd$q
#'
#'
#'
#'
caseID_m_q_c_vector_from_NI_M_Q_C <-function(NI,M,Q,C){





  m<-  rep(1:M, each=C*Q)
  q <-  rep(seq(1,Q,1), M, each=C)
  c <-rep(rep(C:1), M*Q)

  mm<-rep(m,NI)
  qq<-rep(q,NI)
  cc<-rep(c,NI)
  caseID <- rep(1:NI,each=C*M*Q)

  DF<-data.frame(caseID = caseID,m=mm,q=qq,c=cc)

  print.data.frame(DF,row.names = F)
  invisible( DF)

}
