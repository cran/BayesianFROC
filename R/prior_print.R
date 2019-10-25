


#' @title Print What Prior Are Used
#' @description Prints prior in R console
#' @param prior  An integer, representing type of Prior
#'
#' @return none
#' @export
#'
#' @examples
#'
#'    prior_print_srsc()
#'
prior_print_srsc  <- function(prior=0) {

  message("\n---------------------- Prior --------")


  if (prior==-1) {

    message("\n*  prior:
                          w,m  ~  Gaussian(0,111);
                          v,dz ~  uniform(0,111);

where uniform(a,b) denotes the uniform distribution on {t; a<t<b }
 and Gaussian(a,b) denotes the Gaussian of mean:a and variance: b. ...SD?????

* Non-Informative
* proper


            ")#message





  }
















  if (prior==0) {

message("\n* we use the following prior:

                        w ~ uniform(-inf,inf);
                       dz ~ uniform(   0,inf);
                        m ~ uniform(-inf,inf);
                        v ~ uniform(   0,inf);

where uniform(a,b) denotes the uniform distribution
whose support is the interval {t; a<t<b }.

So, this prior is the non-informative improper prior.


* Non-Informative
* Improper



            ")#message





  }






  if(prior == 1){
    message("\n* we use the following prior:


                          w ~  uniform(-111,111);
    for(cd in 1:C-1) dz[cd] ~  uniform(   0,111);
                          m ~  uniform(-111,111);
                          v ~  uniform(   0,111);

So, this prior is the non-informative proper prior.


* Non-Informative
* proper

              ")#message
  }




  message("\n------------------------------------------")






}#function















#' @title Print What Prior Are Used
#' @description Prints prior in R console
#' @param prior  An integer, representing type of Prior
#'
#' @return none
#' @export
#'
#' @examples
#'
#'    prior_print_MRMC()
#'
prior_print_MRMC  <- function(prior=0) {

  message("\n---------------------- Prior --------")

  if (prior==-1) {

    message("\n*   prior:


                          w, mu ~  uniform(-111,111);

                           dz,v ~  uniform(0,111);



where uniform(a,b) denotes the uniform distribution on {t; a<t<b }

* Non-Informative
* proper

            ")#message
}






















  if (prior==1) {

    message("\n*   prior:


       w ~ Gaussian(0,111);
     mu  ~ Gaussian(0,111);

      v  ~ uniform(0,111);
      dz ~  uniform(0,111);
 hyper_v ~ uniform(0,111);



where uniform(a,b) denotes the uniform distribution on {t; a<t<b }
 and Gaussian(a,b) denotes the Gaussian of mean:a and variance: b. ...SD?????

* Non-Informative
* proper

            ")#message





  }
















  if (prior==0) {

    message("\n* we use the following prior:

                        w ~ uniform(-inf,inf);
  for(cd in 1:C-1) dz[cd] ~ uniform(   0,inf);
                        m ~ uniform(-inf,inf);
                        v ~ uniform(   0,inf);

where uniform(a,b) denotes the uniform distribution
whose support is the interval {t; a<t<b }.

So, this prior is the non-informative improper prior.


* Non-Informative
* Improper



            ")#message





  }






  if(prior == 1){
    message("\n* we use the following prior:


                          w ~  uniform(-111,111);
    for(cd in 1:C-1) dz[cd] ~  uniform(   0,111);
                          m ~  uniform(-111,111);
                          v ~  uniform(   0,111);

So, this prior is the non-informative proper prior.


* Non-Informative
* proper

              ")#message
  }




  message("\n------------------------------------------")






}#function
