
fooo <-function()
{

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
  real <lower=0>      hyper_v[Q];

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


    // for(qd in 1 : Q) {
    //   for(md in 1 : M) {
    //     target += normal_lpdf( AA[md,qd]|A[md],hyper_v[qd]);
    //   }  }
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
}
