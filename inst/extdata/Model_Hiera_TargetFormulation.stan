data{
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
   real    w;
  real <lower =0  >  dz[C-1];
  real               mu[M,Q];
  real <lower=0>      v[M,Q];
  real <lower=0>      hyper_v[Q];
 // real <lower=0>      hyper_vv[M];

  real <lower=0,upper=1>A[M];
 // real <lower=0,upper=1>AAA;

}

transformed parameters {
  real <lower =0>       dl[C];
  real <lower=0,upper=1> ppp[C,M,Q];
  real <lower =0>      l[C];
  real <upper=100000>   z[C];
  real                      aa[M,Q];
  real <lower =0>           bb[M,Q];
  real <lower=0,upper=1>    AA[M,Q];

for(md in 1 : M) {
for(qd in 1 : Q) {
  aa[md,qd]=mu[md,qd]/v[md,qd];
  bb[md,qd]=1/v[md,qd];
for(cd in 1 : C-1) {
                z[1]=w;
             z[cd+1] = z[cd] +      dz[cd];
}
            for(cd in 1 : C) {   if (cd==C){
             ppp[cd,md,qd] = 1- Phi((z[cd ] -mu[md,qd])/v[md,qd]);
     }else{

             ppp[cd,md,qd] = Phi((z[cd+1] -mu[md,qd])/v[md,qd])
                           - Phi((z[cd ] -mu[md,qd])/v[md,qd]);
     }
     }











        for(cd in 1 : C) {
       l[cd] = (-1)*log(Phi(z[cd]));
        }






             for(cd in 1:C){
                 if (cd==C) {dl[cd]=fabs(l[cd]-0);
                 }else{

       dl[cd]=fabs(l[cd]-l[cd+1]);
          }
          }



    }//for sentence w.r.t. qd
}

   for(md in 1 : M) {    for(qd in 1 : Q) {
        AA[md,qd]=Phi(  (mu[md,qd]/v[md,qd])/sqrt((1/v[md,qd])^2+1)  );//Measures of modality performance
            }}
}

model{
  for(qd in 1 : Q) {
    for(md in 1 : M) {
      // AA[md,qd] ~ normal(A[md],hyper_v[qd]);
      target += normal_lpdf( AA[md,qd]|A[md],hyper_v[qd]);
    }  }
  for(n in 1:N) {
    // h[n] ~ binomial(NL, ppp[c[n],m[n],q[n]]);
    // ff[n] ~ poisson(l[c[n]]*NL);//Chakraborty's model

    target +=   poisson_lpmf(ff[n]|l[c[n]]*NL);//Chakraborty's model
    target += binomial_lpmf(h[n]  |  NL, ppp[c[n],m[n],q[n]]   );
  }

}


//
//
//  generated quantities{
//
//  real diff_A[M,M];
//  real Prob_diff_A[M,M];
//
//  // real diff_A_second_third[20];
//  // real diff_A_fourth_third[20];
//  //
//  // real diff_A_first_third[40];
//  // real diff_A_first_fifth[40];
//  // real diff_A_second_fifth[40];
//  //  real diff_A_second_fifth_refine[40];
//  //
//  // real diff_A_fourth_fifth[40];
//  // real ninety;
//  // real eighty;
//  //
//  real Prob_diff_A_const[M,M,40];
//  // real outcome;
//
//
//       // for(d in 1 : 20) {
//       // diff_A_second_third[d]=step(A[2]-A[3] -d*0.5*10^(-3));
//       // diff_A_fourth_third[d]=step(A[4]-A[3] -d*0.5*10^(-2));
//       // }
//
//   //    for(d in 1 : 40) {
//   // diff_A_second_fifth_refine[d]=step(A[2]-A[5] -d*0.5*10^(-3));
//   //
//   //  diff_A_first_third[d]=step(A[1]-A[3] -10^(-d));
//   //  diff_A_first_fifth[d]=step(A[1]-A[5] -10^(-d));
//   //  diff_A_second_fifth[d]=step(A[2]-A[5] -10^(-d));
//   //  diff_A_fourth_fifth[d]=step(A[4]-A[5] -10^(-d));
//   //    }
//
//
//      for(md in 1 : M) {
//      for(mmd in 1 : M) {
//        Prob_diff_A[md,mmd]=step(A[md]-A[mmd]);
//      }}
//
//       for(md in 1 : M) {
//      for(mmd in 1 : M) {
//        diff_A[md,mmd]=A[md]-A[mmd];
//      }}
//
//      for(d in 1 : 40) {
//      for(md in 1 : M) {
//      for(mmd in 1 : M) {
//        Prob_diff_A_const[md,mmd,d]=step(A[md]-A[mmd] -d*0.5*10^(-3));
//      }}}
//  // outcome=step(A[4]-A[2])*step(A[2]-A[1])*step(A[1]-A[5])*step(A[5]-A[3]);
//  //
//  // ninety =step(A[2]-A[3])*step(A[4]-A[3])   ;
//  //  eighty =step(A[4]-A[5])*step(A[4]-A[3])*step(A[2]-A[5])*step(A[2]-A[3])*step(A[1]-A[3]) ;
//
//    }

