data{
  int <lower=0>N;
  int <lower=0>M;
  int <lower=0>C;
  int <lower=0>Q;

  int <lower=0>h[N];
  int <lower=0>f[N];#In Stan    we  should not use "i" as variable names.
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
  real    w;
  real <lower =0  >  dz[C];
  real               mu[M,Q];
  real <lower=0>      v[M,Q];
  real <lower=0>      hyper_v[Q];
  # real <lower=0>      hyper_vv[M];

  real <lower=0,upper=1>A[M];
  # real <lower=0,upper=1>AAA;
  real muANOVA;
  vector                   [M-1] m1A;       //dummy
  vector                   [Q-1] m1B;       //dummy
  matrix               [M-1,Q-1] m1AB;    //dummy
  real<lower=0>   sigmaE;
}



transformed parameters {
  real <lower =0>       dl[C];
  real <lower=0,upper=1> ppp[C,M,Q];
  real <lower =0>      l[C+1];
  real    z[C+1];
  real                      aa[M,Q];
  real <lower =0>           bb[M,Q];
  real <lower=0,upper=1>    AA[M,Q];

  vector                   [M] muA;            //A????
    vector                   [Q] muB;            //B????
    matrix                   [M,Q] muAB;         //???ݍ??p
  vector                   [M-1] m1a;          //dummy
  vector                   [Q-1] m1b;          //dummy
  for(i in 1:(M-1)){ muA[i]  = m1A[i]; m1a[i] = 0.0 ;}
  muA[M]  = -sum(m1A);
  for(j in 1:(Q-1)){ muB[j]  = m1B[j]; m1b[j] = 0.0 ;}
  muB[Q]  = -sum(m1B);
  for(i in 1:(M-1)){ for(j in 1:(Q-1)){
    muAB[i,j] =m1AB[i,j]; m1a[i] =m1a[i]+m1AB[i,j];}  }
  for(j in 1:(Q-1)){ for(i in 1:(M-1)){
    m1b[j] =m1b[j]+m1AB[i,j];}  }
  for(i in 1:(M-1)){muAB[i,Q] = (-1)*m1a[i]; }
  for(j in 1:(Q-1)){muAB[M,j] = (-1)*m1b[j]; }
  muAB[M,Q] = sum(m1a);



  for(md in 1 : M) {
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
    }#for sentence w.r.t. qd
  }

  for(md in 1 : M) {    for(qd in 1 : Q) {
    AA[md,qd]=Phi(  (mu[md,qd]/v[md,qd])/sqrt((1/v[md,qd])^2+1)  );#Measures of modality performance
  }}
}


model{
  for(qd in 1 : Q) {
    for(md in 1 : M) {
      // AA[md,qd] ~ normal(A[md],hyper_v[qd]);
      AA[md,qd] ~ normal(muANOVA+ muA[md]+muB[qd]+muAB[md,qd],sigmaE);

      # A[md] ~ normal(AAA,hyper_vv[md]);
    }  }
  for(n in 1:N) {
    h[n] ~ binomial(NL, ppp[c[n],m[n],q[n]]);
    ff[n] ~ poisson(l[c[n]]*NL);#Chakraborty's model //<-------very very very coution, not n but c[n] 2019 Jun 21
  }
}




generated quantities{

  real diff_A[M,M];



  real Prob_diff_A[M,M];

  real Prob_diff_A_CONST32[100];
  real Prob_diff_A_CONST21[100];
  real Prob_diff_A_CONST31[100];

  for(md in 1 : M) {
    for(mmd in 1 : M) {
      Prob_diff_A[md,mmd]=step(A[md]-A[mmd]);
    }}

  for(md in 1 : M) {
    for(mmd in 1 : M) {
      diff_A[md,mmd]=A[md]-A[mmd];
    }}

  for(d in 1 : 100) {
    Prob_diff_A_CONST32[d]=step(A[3]-A[2]-d*0.01);
    Prob_diff_A_CONST21[d]=step(A[2]-A[1]-d*0.01);
    Prob_diff_A_CONST31[d]=step(A[3]-A[1]-d*0.01);

  }




}

