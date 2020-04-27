minimal_model_MRMC  <- function() {

  # dataList <- dd
  # m <-dataList$m
  # q<-dataList$q
  # c<-dataList$c
  # h<-dataList$h
  # f<-dataList$f
  # NI<-dataList$NI
  # NL<-dataList$NL
  # C<-dataList$C
  # M<-dataList$M
  # Q<-dataList$Q


  m <-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3
        ,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5
        ,5,5,5,5,5,5,5,5,5,5,5,5)

  q <-c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,1,1,1,1
        ,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,1,1,1,1,1,2,2,2
        ,2,2,3,3,3,3,3,4,4,4,4,4)


  c<-c(5,4,3,2,1,5,4,3,2,1,5,4,3,2,1,5,4,3,2,1,5,4,3,2,1,5,4,3,2,1,5,4,3,2,1,5,4,3,2,1,5,4,3,2
       ,1,5,4,3,2,1,5,4,3,2,1,5,4,3,2,1,5,4,3,2,1,5,4,3,2,1,5,4,3,2,1,5,4,3,2,1,5,4,3,2,1,5,4,3
       ,2,1,5,4,3,2,1,5,4,3,2,1)

  f<-c(
    0,4,20,29,21,0,0,6,15,22,1,15,18,31,19,1,2,4,16,17,1,1,21,24,23,1,1,5,30
    ,40,2,19,31,56,42,2,0,2,30,32,1,7,13,28,19,0,1,7,7,31,7,15,28,41,9,0,2,5
    ,24,31,1,4,18,21,23,1,1,0,11,35,6,14,37,36,18,0,2,4,18,25,0,2,19,23,18,0,2
    ,6,10,30,2,25,40,29,24,1,1,4,24,32
  )


  h<-c(
    50,30,11,5,1,15,29,29,1,0,39,31,8,10,3,10,8,25,45,14,52,25,13,4,1,27,28,29,1
    ,0,53,29,13,2,4,9,16,22,43,14,43,29,11,6,0,18,29,21,0,0,43,29,6,7,1,10,14,19
    ,32,23,61,19,12,9,3,16,29,34,1,0,52,29,10,4,3,10,16,23,43,15,35,29,18,9,0,17,27
    ,24,0,0,34,33,7,13,2,12,16,21,35,15
  )

  C<-5
  M<-5
  Q<-4
  NI<-199
  NL<-142




  N <-C*M*Q

  ff <- numeric(N) #Initialization of Cumulative False alarm
  harray<-array(0,dim=c(C,M,Q));

  for(md in 1:M) {
    for(cd in 1:C) {
      for(qd in 1 : Q){
        for(n  in 1:cd){
          ff[cd+(md-1)*C*Q+(qd-1)*C]<-ff[cd+(md-1)*C*Q+(qd-1)*C]+f[n+(md-1)*C*Q+(qd-1)*C]
        }
        harray[cd,md,qd] <- h[cd+(md-1)*C*Q+(qd-1)*C]
      }}}


  data <- list(N=N,Q=Q, M=M,m=m  ,C=C  , NL=NL,NI=NI
               ,c=c,q=q,
               h=h, f=f,
               ff=ff,
               harray=harray
  )




  Stan.model <- rstan::stan_model( model_code="


data{
  int <lower=0>N;
  int <lower=0>M;
  int <lower=0>C;
  int <lower=0>Q;
  int <lower=0>h[N];
  int <lower=0>f[N];
  int <lower=0>q[N];
  int <lower=0>c[N];
  int <lower=0>m[N];
  int <lower=0>NL;
  int <lower=0>NI;

  int <lower=0>ff[N];
  int <lower=0>harray[C,M,Q];




}
transformed data {
  int <lower=0> NX;
   NX = NI;
}

parameters{
  real    w;
  real <lower =0  >  dz[C-1];
  real               mu[M,Q];
  real <lower=0>      v[M,Q];
  # real <lower=0>      hyper_v[Q];

}

transformed parameters {
  real <lower =0>       dl[C];
  real <lower=0,upper=1> ppp[C,M,Q];
  real <lower =0>      l[C];
  real    z[C];
  real                      aa[M,Q];
  real <lower =0>           bb[M,Q];
  real <lower=0,upper=1>    AA[M,Q];
  real deno[C-1,M,Q];
  real hit_rate[C,M,Q];
  real <lower=0,upper=1>A[M];

  z[1]=w;

  for(md in 1 : M) {
    for(qd in 1 : Q) {
      aa[md,qd]=mu[md,qd]/v[md,qd];
      bb[md,qd]=1/v[md,qd];

      for(cd in 1 : C-1) z[cd+1] = z[cd] + dz[cd];
      ppp[C,md,qd] = 1- Phi((z[C] -mu[md,qd])/v[md,qd]);

      for(cd in 1 : C-1) ppp[cd,md,qd] = Phi((z[cd+1] -mu[md,qd])/v[md,qd])  - Phi((z[cd ] -mu[md,qd])/v[md,qd]);



      for(cd in 1 : C) l[cd] = (-1)*log(Phi(z[cd]));
      dl[C] = fabs(l[C]-0);
      for(cd in 1:C-1) dl[cd]= fabs(l[cd]-l[cd+1]);




    }
  }

  for(md in 1 : M) {
    for(qd in 1 : Q) {
      AA[md,qd]=Phi(  (mu[md,qd]/v[md,qd])/sqrt((1/v[md,qd])^2+1)  );//Measures of modality performance
    }}

  for(md in 1 : M) {
   A[md] = 0;
    for(qd in 1 : Q) {
     A[md] =  A[md] +  AA[md,qd];
    }
   A[md]=   A[md]/M;
    }


  for(md in 1 : M) {
    for(qd in 1 : Q) {
      deno[C-1,md,qd]=1-ppp[C,md,qd];
      for(cd in 3:C){  deno[c[cd],md,qd]=deno[c[cd-1],md,qd]-ppp[c[cd-1],md,qd];  }
    }}


  for(md in 1 : M) {
    for(qd in 1 : Q) {
      for(cd in 1:C-1){
        hit_rate[cd,md,qd]=ppp[cd,md,qd]/deno[cd,md,qd];
      }
      hit_rate[C,md,qd]=ppp[C,md,qd];

    }}



}






model{
    int s=0;


    # for(qd in 1 : Q) {
    #   for(md in 1 : M) {
    #     target += normal_lpdf( AA[md,qd]|A[md],hyper_v[qd]);
    #   }  }
    for(n in 1:N) {
      target +=   poisson_lpmf(ff[n]|l[c[n]]*NX);
    }




    for(qd in 1 : Q) {
      for(md in 1 : M) {
        s=0;
        for(cd in 1 : C){
           target += binomial_lpmf(harray[cd,md,qd]  |  NL-s, hit_rate[c[cd],md,qd]  );
          s = s + harray[cd,md,qd]; }
        }}








      w ~  uniform(-3,3);
      for(cd in 1:C-1) dz[cd] ~  uniform(0.001,7);
      for(md in 1 : M) { for(qd in 1 : Q) {
        mu[md,qd] ~ uniform(-11,11);
        v[md,qd] ~ uniform(0.01,11);

      }}





  }

")


  # fit  <-  rstan::sampling(
  #   object= Stan.model, data=metadata_to_fit_MRMC(data_2modaities_2readers_3confidence),  verbose = FALSE,
  #   seed=123, chains=1, warmup=111, iter=32222
  #   , control = list(adapt_delta = 0.9999999,
  #                    max_treedepth = 15)
  #   # ,init = initial
  # )

  # rstan::traceplot(fit,pars=c("w"))
  # rstan::check_hmc_diagnostics(fit)



  # fit  <-  rstan::sampling(
  #   object= Stan.model, data=metadata_to_fit_MRMC(dd),  verbose = FALSE,
  #   seed=1234567, chains=1, warmup=111, iter=12222
  #   , control = list(adapt_delta = 0.9999999,
  #                    max_treedepth = 15)
  #   # ,init = initial
  # )
  #
  # rstan::traceplot(fit,pars=c("w"))
  # rstan::check_hmc_diagnostics(fit)



  #
  #

  # fit  <-  rstan::sampling(
  #   object= Stan.model, data=data,  verbose = FALSE,
  #   seed=1234567, chains=1, warmup=111, iter=1111
  #   , control = list(adapt_delta = 0.9999999,
  #                    max_treedepth = 15)
  #   # ,init = initial
  # )
  #
  # rstan::traceplot(fit,pars=c("w"))
  # rstan::check_hmc_diagnostics(fit)



  # Good performance
  fit  <-  rstan::sampling(
    object= Stan.model, data=data,  verbose = FALSE,
    seed=123674523, chains=1, warmup=111, iter=1222,
    sample_file =paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"\\samples"),
    control = list(adapt_delta = 0.9999999,
                   max_treedepth = 15)
    # ,init = initial
  )

  rstan::traceplot(fit,pars=c("w"))
  rstan::check_hmc_diagnostics(fit)


  #
  # fit  <-  rstan::sampling(
  #   object= Stan.model, data=data,  verbose = FALSE,
  #   seed=123, chains=1, warmup=111, iter=122
  #   , control = list(adapt_delta = 0.9999999,
  #                    max_treedepth = 15)
  #   # ,init = initial
  # )
  #
  # rstan::traceplot(fit,pars=c("w"))
  # rstan::check_hmc_diagnostics(fit)

  #
  #
  #
  #
  #
  fit  <-  rstan::sampling(
    object= Stan.model, data=data,  verbose = FALSE,
    seed=1234, chains=1, warmup=111, iter=1222,
    sample_file =paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"),"\\samples"),
    control = list(adapt_delta = 0.9999999,
                   max_treedepth = 15)
    # ,init = initial
  )

  rstan::traceplot(fit,pars=c("w"))
  rstan::check_hmc_diagnostics(fit)

  #
  #
  #
  # fit  <-  rstan::sampling(
  #   object= Stan.model, data=data,  verbose = FALSE,
  #   seed=1, chains=1, warmup=111, iter=122
  #   , control = list(adapt_delta = 0.9999999,
  #                    max_treedepth = 15)
  #   # ,init = initial
  # )
  #
  # rstan::traceplot(fit,pars=c("w"))
  # rstan::check_hmc_diagnostics(fit)
  #



}#fun
