


#' @title Who should be inspected?
#' @description
#' Even if a diagnosis test with respect to "all" said
#' that it is positive, however the result cannot be correct in high probability.
#' If we test no suspicous people, then it reduce our resource of diagnosis test
#' and when some suspicous people needs the test, we cannot do the test.
#'
#' So, the diagnosis test should be done for the suspicous people only. Not should be done
#' for all people including no suspicous people. The medical resource is finite, we should use it
#' for more optimal way.
#'
#'
#'
#'
#' @param N The number of population, including diseased and non-diseased people
#' @param n The number of diseased population
#' @param se  Sensitivity of a diagnostic test
#' @param sp Specificity of a diagnostic test
#'
#' @return Probability which is between 0 and 1. I you want to get percent,
#'  then it is 100 times the return value.
#'
#' \deqn{Prob(Truth = diseased | Diagnosis = Positive) = \frac{Se\times n}{Se \times n + (N-n)\times(1-sp)}   }
#'
#' @details
#'
#'
#'
#'--------------------------------------------------------------------------

#' \tabular{llll}{
#'
#'  Diagnosis \ truth   \tab \strong{ Diseased } \tab \strong{   Non-diseased}  \cr
#'     -----------------------\tab -----------------------   \tab ------------- \cr
#'  Positive   \tab   se*n     \tab  \eqn{(N-n)(1-sp)}                 \cr
#'  Negative   \tab   (1-se)*n \tab  \eqn{(N-n)sp}             \cr
#'     -----------------------\tab -----------------------   \tab ------------- \cr
#'         \tab   n \tab  \eqn{N-n}      \cr
#'  }
#'-------------------------------------------------------------------------
#'
#'
#' For example,
#'
#'   if prevalence is 0.0001,
#'
#'    population is 10000,
#'
#' specificity = 0.8,
#'
#' sensitivity = 0.9,
#'
#'
#'   then the table is the following.
#'
#'   We can calculates the probability of the event that
#'   a one whose diagnosis is positive is really diseased
#'   is
#'
#'   \deqn{  \frac{9}{1998 + 9} = 9/(1998+9) = 0.00448  }
#'
#'
#'--------------------------------------------------------------------------

#' \tabular{llll}{
#'
#'  Diagnosis \ truth   \tab \strong{ Diseased } \tab \strong{   Non-diseased}  \cr
#'     -----------------------\tab -----------------------   \tab ------------- \cr
#'  Positive   \tab   9     \tab   1998                \cr
#'  Negative   \tab   1 \tab   7992            \cr
#'     -----------------------\tab -----------------------   \tab ------------- \cr
#'         \tab   \eqn{n = 10} \tab  \eqn{N-n=10000-10}      \cr
#'  }
#'-------------------------------------------------------------------------
#'
#'
#'
#'
#'
#'
#' @export
#'
#' @examples
#'
#' CoronaVirus_Disease_2019(10000,10,0.9,0.8)
#'
#' 9/(1998+9)
#'
#'
#'
CoronaVirus_Disease_2019 <- function(N,n,se, sp){
  prob <- se*n/(se*n+(1-sp)*(N-n))
  return(prob)
}





#' @title Who should be inspected?
#' @description
#' Even if we test all people, the result is true with very low probabilties.
#'
#'
#' @inheritParams CoronaVirus_Disease_2019
#' @param pre Prevalence of population
#'
#'
#' \deqn{Prob(Truth = diseased | Diagnosis = Positive) = \frac{Se\times pre}{Se \times pre + (1-pre)\times(1-sp)}   }
#'
#' @details
#'
#'
#'
#'--------------------------------------------------------------------------

#' \tabular{llll}{
#'
#'  Diagnosis \ truth   \tab \strong{ Diseased } \tab \strong{   Non-diseased}  \cr
#'     -----------------------\tab -----------------------   \tab ------------- \cr
#'  Positive   \tab   se*n     \tab  \eqn{(N-n)(1-sp)}                 \cr
#'  Negative   \tab   (1-se)*n \tab  \eqn{(N-n)sp}             \cr
#'     -----------------------\tab -----------------------   \tab ------------- \cr
#'         \tab   n \tab  \eqn{N-n}      \cr
#'  }
#'-------------------------------------------------------------------------
#'
#'
#' For example,
#'
#'   if prevalence is 0.0001,
#'
#'    population is 10000,
#'
#' specificity = 0.8,
#'
#' sensitivity = 0.9,
#'
#'
#'   then the table is the following.
#'
#'   We can calculates the probability of the event that
#'   a one whose diagnosis is positive is really diseased
#'   is
#'
#'   \deqn{  \frac{9}{1998 + 9} = 9/(1998+9) = 0.00448 percent }
#'
#'
#'--------------------------------------------------------------------------

#' \tabular{llll}{
#'
#'  Diagnosis \ truth   \tab \strong{ Diseased } \tab \strong{   Non-diseased}  \cr
#'     -----------------------\tab -----------------------   \tab ------------- \cr
#'  Positive   \tab   9     \tab   1998                \cr
#'  Negative   \tab   1 \tab   7992            \cr
#'     -----------------------\tab -----------------------   \tab ------------- \cr
#'         \tab   \eqn{n = 10} \tab  \eqn{N-n=10000-10}      \cr
#'  }
#'-------------------------------------------------------------------------
#'
#'
#'
#'
#'
#'@return same as \code{ \link{CoronaVirus_Disease_2019}()}
#' @seealso \code{ \link{CoronaVirus_Disease_2019}()}
#' @export
#'
#' @examples
#'
#' CoronaVirus_Disease_2019_prevalence(0.0001, 0.9,0.8)
#' CoronaVirus_Disease_2019_prevalence(0.03,0.9,0.8)
#' CoronaVirus_Disease_2019_prevalence(0.3,0.9,0.8)
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#  If Sensitivity and Specificity is larger, then, the probability is also larger
#'#========================================================================================
#'
#'
#' x <- stats::runif(100,0,1)
#' y <- CoronaVirus_Disease_2019_prevalence(0.1,x,x)
#'
#' dark_theme(4)
#' plot(x,y)
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#  If the prevalence is larger, then, the probability is also larger
#'#========================================================================================
#'
#'
#'
#' x <- stats::runif(100,0,1)
#' y <- CoronaVirus_Disease_2019_prevalence(x,0.9,0.9)
#'
#' dark_theme(4)
#' plot(x,y)
#'
#'
CoronaVirus_Disease_2019_prevalence <- function(pre,se, sp){
  prob <- se*pre/(se*pre+(1-sp)*(1-pre))
  return(prob)

}
