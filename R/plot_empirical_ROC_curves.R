

#' @title Empirical ROC curve
#'
#' @param Number_of_cases Number_of_cases
#' @param Number_of_non_cases Number_of_non_cases
#' @param frequencies_of_cases frequencies_of_cases
#' @param frequencies_of_non_cases frequencies_of_non_cases
#'
#' @details Suppose that there is a \eqn{K} categories and data are drawn
#' from two multinomial distributions of size \eqn{n,m}.
#'
#'  \deqn{h_1,h_2,...,h_K, \Sigma h_i = n,}
#'
#'  \deqn{f_1,f_2,...,f_K,\Sigma f_i = m.}
#'
#'  Then this plots the cumulative sums.
#'
#'
#'
#'
#'
#'
# @return
#' @export
#'
# @examples
#'
plot_empirical_ROC_curves <- function(
    Number_of_cases = 100,
    Number_of_non_cases = 100,
    frequencies_of_non_cases   = stats::rmultinom(1, size = Number_of_cases, prob = c(0.1,0.2,0.3,0,5)) ,
    frequencies_of_cases  =stats::rmultinom(1, size = Number_of_non_cases, prob = c(0.4,0.3,0.2,0,1))
){
dark_theme()

NL <- Number_of_cases
NI <- Number_of_non_cases
h<-frequencies_of_cases
f<-frequencies_of_non_cases
# # roc <- dataList.Chakra.1
# # roc$NI <- roc$NL
# # NI <- roc$NI
# # NL <- roc$NL
# #
# # h <- roc$h
# # f <- roc$f
#
# h1 <- c(h,NL-sum(h))
# f1 <- c(f,NI-sum(f))
# FPF <- cumsum(f1)/NI
# TPF <- cumsum(h1)/NL
# # FPF <- c(0,FPF,1)
# # TPF <- c(0,TPF,1)
# plot(FPF,TPF, type = "l"
#      # , xlim=c(0,1),ylim=c(0,1)
#      )
#
# suppressWarnings(graphics::par(new=TRUE));
# plot(c(0,1),c(0,1), type = "l",xlab ="",ylab=""
#      , xlim=c(0,1),ylim=c(0,1)
#      )
#
#
# h <- c(NL-sum(h),h)
# f <- c(NI-sum(f),f)
FPF <- cumsum(f)/NI
TPF <- cumsum(h)/NL
FPF <- c(0,FPF)
TPF <- c(0,TPF)
plot(FPF,TPF, type = "l"
          , xlim=c(0,1),ylim=c(0,1)
)

suppressWarnings(graphics::par(new=TRUE));
plot(c(0,1),c(0,1), type = "l",xlab ="",ylab=""
          , xlim=c(0,1),ylim=c(0,1),lty ="dashed"
)

}
























#' @title Synthesize ROC data
#'
#' @param Number_of_cases Number_of_cases
#' @param Number_of_non_cases Number_of_non_cases
#' @param prob_case  prob_case
#' @param prob_non_case prob_non_case
#'
#' @return A list, indicatin ROC data
#' @export
#'
# @examples
ROC_data_creator <- function(
  Number_of_cases = 100,
  Number_of_non_cases = 100,
  prob_case = c(0.1,0.2,0.3,0,5),
  prob_non_case = c(0.4,0.3,0.2,0,1)


  ){

  list(

  Number_of_cases = Number_of_cases,
  Number_of_non_cases = Number_of_non_cases,
  frequencies_of_non_cases   = as.vector( stats::rmultinom(1, size = Number_of_cases,    prob =prob_case   ) ) ,
  frequencies_of_cases  = as.vector(stats::rmultinom(1, size = Number_of_non_cases,  prob = prob_non_case  ) )

  )


  }



