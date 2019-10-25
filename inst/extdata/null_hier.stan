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
  // int <lower=0>farray[C,M,Q];
  // int <lower=0>hharray[C,M,Q];
  // int <lower=0>ffarray[C,M,Q];

  // real <lower=0>hhN[N];
  // real<lower=0>ffN[N];
  // real <lower=0>hharrayN[C,M,Q];
  // real <lower=0>ffarrayN[C,M,Q];

  int <lower=0>S[M,Q];
  int <lower=0>q[N];

  int <lower=0>c[N];
  int <lower=0>m[N];


  int <lower=0>NL;
  int <lower=0>NI;

  int prior;
int ModifiedPoisson;//Logical



}
  transformed data {
int <lower=0> NX;
if(ModifiedPoisson==0) NX = NI;
if(ModifiedPoisson==1) NX =NL;

print(" 2019 Sept 26-----------------------------")
print(" From th.stan 2019 Oct 1, Adjusted NL")

  }

parameters{
   real    w;
  real <lower =0  >  dz[C-1];
  real               mu[M,Q];
  real <lower=0>      v[M,Q];
  real <lower=0>      hyper_v;
 // real <lower=0>      hyper_vv[M];

  real <lower=0,upper=1>A;
 // real <lower=0,upper=1>AAA;

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

for(md in 1 : M) {    for(qd in 1 : Q) {
     // deno[C]=1;
     deno[C-1,md,qd]=1-ppp[C,md,qd];
      for(cd in 3:C){  deno[c[cd],md,qd]=deno[c[cd-1],md,qd]-ppp[c[cd-1],md,qd];  }
}}







}//transformed parameters {









model{
  int s=0;
  real denoo[C,M,Q];

for(md in 1 : M) {
  for(qd in 1 : Q) {
                    for(cd in 1:C-1){
                      denoo[cd,md,qd]=deno[cd,md,qd];
                    }
                      denoo[C,md,qd]=1;

  }}





  for(qd in 1 : Q) {
    for(md in 1 : M) {
      // AA[md,qd] ~ normal(A[md],hyper_v[qd]);
      target += normal_lpdf( AA[md,qd]|A,hyper_v);
    }  }
  for(n in 1:N) {
    // h[n] ~ binomial(NL, ppp[c[n],m[n],q[n]]);
    // ff[n] ~ poisson(l[c[n]]*NL);// model

    target +=   poisson_lpmf(ff[n]|l[c[n]]*NX); //<-------very very very coution, not n but c[n] 2019 Jun 21// Additivity of Poisson gives us other equivalent modeling description
    // target += binomial_lpmf(h[n]  |  NL, ppp[c[n],m[n],q[n]]   );
  }

  for(qd in 1 : Q) {
    for(md in 1 : M) {
      s=0;
      for(cd in 1 : C){
        target += binomial_lpmf(harray[cd,md,qd]  |  NL-s, ppp[c[cd],md,qd]/denoo[c[cd],md,qd]   );
      s = s + harray[cd,md,qd];
    // target +=   poisson_lpmf(f[n]|dl[c[n]]*NL); //<-------very very very coution, not n but c[n] 2019 Jun 21
// I think the former model of hit decrease the number of the divergent transitions, so  it needs to aboid it. 2019 Sept 26


  }}}






if(prior==0){
   w ~ normal(0,1000);//2019.6.3
      for(md in 1 : M) {    for(qd in 1 : Q) {
   mu[md,qd] ~ normal(0,1000);//2019.6.3
      v[md,qd] ~ chi_square(4);//2019.6.3

      }}

   dz ~ chi_square(4);//2019.6.3

   hyper_v ~ chi_square(4);//2019.6.3

}

if(prior == 1){
                          w ~  uniform(-111,111);
    for(cd in 1:C-1) dz[cd] ~  uniform(0,111);
          for(md in 1 : M) { for(qd in 1 : Q) {
   mu[md,qd] ~ uniform(-111,111);//2019.6.3
      v[md,qd] ~ uniform(0,111);//2019.6.3

      }}


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

