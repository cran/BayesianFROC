# foo<- function(){
# a<-rstan::extract(f)$a
# a<-runif(10000)/10
# MCMC <- length(a)
# EAP_a <-  array(0, dim=c(  MCMC))
# EAP_a <- 0
# s<-0
# t<-0
# for(mc in 1:MCMC){
#   s<-  EAP_a
#   EAP_a <-  s+ a[mc]
# }
# EAP_a<-EAP_a/MCMC
#
# EAP_aa<-mean(a)
# print(  message(EAP_aa==EAP_a,"\n",
#                 EAP_aa,"\n",EAP_a))
# }

# fooooo  <- function(x,...) {
#   # if(missing(y))y<-1
#
#   x+y
# }

