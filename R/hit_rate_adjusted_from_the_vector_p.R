

#' @title hit rate adjusted from a vector p
#'
#' @param p_vector A vector
#'
#' @return A vector
#' @export
#'
#' @examples
#'
#'  p <- c(1,2,3)
#'  a <- hit_rate_adjusted_from_the_vector_p( p )
#'  a
#'
#'  # [1] -0.25 -1.00  3.00
#'
#'  a[3] == 3
#'  a[2] == p[2]/(1-p[3])
#'  a[1] == p[1]/(1-p[3]-p[2])
#'
#'
#'
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'#========================================================================================
#'#                               application in the function ppp_srsc in this package
#'#========================================================================================
#'
#' \donttest{
#'
#'
#'  f <- fit_Bayesian_FROC( dataList = d )
#'  e <-rstan::extract(f)
#'
#' q<-e$p[1,]
#' hit_rate_adjusted_from_the_vector_p(q)
#' t(apply(e$p,hit_rate_adjusted_from_the_vector_p,MARGIN = 1))[1,]
#' q<-e$p[2,]
#' hit_rate_adjusted_from_the_vector_p(q)
#' t(apply(e$p,hit_rate_adjusted_from_the_vector_p,MARGIN = 1))[2,]
#'
#'
#'
#' }
#'
#'
#'
 hit_rate_adjusted_from_the_vector_p <- function(p_vector) {



 p<-p_vector
 C<-length(p)


# hit rate are adjusted ---------- 2019 Oct 8

deno <- array(NA,dim = c(C))
hit_rate<- array(NA,dim = c(C))
c<-C:1

    deno[C-1]=1-p[C];
    for(cd in 3:C){  deno[c[cd]]=deno[c[cd-1]]-p[c[cd-1]];  }


# hit rate are well adjusted in the past, I confirmed again, 2019 Dec 17


    for(cd in 1:C-1){
      hit_rate[cd]=p[cd]/deno[cd];
    }
    hit_rate[C]=p[C];




return(hit_rate)


}

