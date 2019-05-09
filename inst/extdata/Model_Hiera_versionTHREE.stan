data{
  int <lower=0>mesh; // number of mesh for drawing curves
  real <lower=0>x[2*mesh];//points for x-axis of FROC curves


  int <lower=0>N;
  int <lower=0>M;
  int <lower=0>C;
  int <lower=0>Q;

  int <lower=0>h[N];
  int <lower=0>f[N];//In Stan    we  should not use "i" as variable names.
  int <lower=0>hh[N];
  int <lower=0>ff[N];

  int <lower=0>harray[C,M,Q];
  int <lower=0>farray[C,M,Q];
  int <lower=0>hharray[C,M,Q];
  int <lower=0>ffarray[C,M,Q];

  real <lower=0>hhN[N];
  real<lower=0>ffN[N];
  real <lower=0>hharrayN[C,M,Q];
  real <lower=0>ffarrayN[C,M,Q];

  int <lower=0>S[M,Q];
  int <lower=0>q[N];

  int <lower=0>c[N];
  int <lower=0>m[N];


  int <lower=0>NL;



}

parameters{
  real   w;// 20181108 <upper=100000> cause error
  real <lower =0  >  dz[C];
  real               mu[M,Q];
  real <lower=0>      v[M,Q];
  real <lower=0>      hyper_v[Q];
  // real <lower=0>      hyper_vv[M];

  real <lower=0,upper=1>A[M];
  // real <lower=0,upper=1>AAA;
  real                mmm[M];
  real <lower=0>      vvv[M];
  real <lower=0>      variancemmm[M];
  real <lower=0>      variancevvv[M];
  real <lower=0>      variance_y[Q];
  real <lower=0>      variance_y_hyper;
  real                mean_y_hyper;

}

transformed parameters {

  real <lower =0>     yyy[2*mesh,M,Q];
  real <lower =0>     yy[2*mesh,M];


  real <lower =0>       dl[C];
  real <lower=0,upper=1> ppp[C,M,Q];
  real <lower =0>      l[C+1];
  real <upper=100000>   z[C+1];
  real                      aa[M,Q];
  real <lower =0>           bb[M,Q];
  real <lower=0,upper=1>    AA[M,Q];
  real                      a[M];
  real <lower =0>           b[M];






  for(md in 1 : M) {
    a[md]=mmm[md]/vvv[md];
    b[md]=1/vvv[md];
    for(qd in 1 : Q) {
      aa[md,qd]=mu[md,qd]/v[md,qd];
      bb[md,qd]=1/v[md,qd];
      for(cd in 1 : C) {
        z[1]=w;
        z[cd+1] = z[cd] +      dz[cd];
        ppp[cd,md,qd] = Phi((z[cd+1] -mu[md,qd])/v[md,qd])
        - Phi((z[cd ] -mu[md,qd])/v[md,qd]);
      }
      for(cd in 1 : C+1) {
        l[cd] = (-1)*log(Phi(z[cd]));
      }
      for(cd in 1:C){
        dl[cd]   =fabs(l[cd]-l[cd+1]);
      }
    }//for sentence w.r.t. qd
  }

  for(md in 1 : M) {    for(qd in 1 : Q) {
    AA[md,qd]=Phi(  (mu[md,qd]/v[md,qd])/sqrt((1/v[md,qd])^2+1)  );//Measures of modality performance
  }}

  //y coordinate for FROC curve
     for(ld in 1 : 2*mesh) {for(md in 1 : M) {
    yy[ld,md]=1 - Phi( b[md]*inv_Phi( exp(-x[ld])   )-a[md]   );
       for(qd in 1 : Q) {
yyy[ld,md,qd]=1 - Phi( bb[md,qd]*inv_Phi( exp(-x[ld])   )-aa[md,qd]);
 }}}






}//transformed parameters


model{
  for(qd in 1 : Q) {
    for(md in 1 : M) {
      AA[md,qd] ~ normal(A[md],hyper_v[qd]);
      mu[md,qd] ~ normal(mmm[md],variancemmm[md]);
      v[md,qd] ~ normal(vvv[md],variancevvv[md]);

    for(ld in 1 : 2*mesh) {
      yyy[ld,md,qd]~ normal(yy[ld,md],variance_y[qd]);

      variance_y[qd]~normal(mean_y_hyper,variance_y_hyper );
    }
      // target += normal_lpdf( AA[md,qd]|A[md],hyper_v[qd]);
    }}
  for(n in 1:N) {
    h[n] ~ binomial(NL, ppp[c[n],m[n],q[n]]);
    ff[n] ~ poisson(l[c[n]]*NL);

    // target +=   poisson_lpmf(ff[n]|l[c[n]]*NL);//Chakraborty's model
    //
    //
    //  target += binomial_lpmf(h[n]  |  NL, ppp[c[n],m[n],q[n]]   );
  }

}

  generated quantities{

  real diff_A[M,M];
  real Prob_diff_A[M,M];

  real Prob_diff_A_const[M,M,40];

  for(md in 1 : M) {
  for(mmd in 1 : M) {
  Prob_diff_A[md,mmd]=step(A[md]-A[mmd]);
  }}

  for(md in 1 : M) {
  for(mmd in 1 : M) {
  diff_A[md,mmd]=A[md]-A[mmd];
  }}

  for(d in 1 : 40) {
  for(md in 1 : M) {
  for(mmd in 1 : M) {
  Prob_diff_A_const[md,mmd,d]=step(A[md]-A[mmd] -d*0.5*10^(-3));
  }}}

  }

