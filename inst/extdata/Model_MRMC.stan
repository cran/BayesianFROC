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
  int PreciseLogLikelihood;//Logical
  int prototype;//Logical

}
  transformed data {
int <lower=0> NX;
if(ModifiedPoisson==0) NX = NI;
if(ModifiedPoisson==1) NX =NL;

print(" 2019 Oct           RRRRRRRR           ");
print(" 2019 Oct           RR     RR           ");
print(" 2019 Oct           RRRRRRRR            ");
print(" 2019 Oct           RR     R                    ");
print(" 2019 Oct           RR      RR.         I.P.       ");


print(" 2019 Oct                                                       ");
print(" 2019 Oct                Thank you           ");
print(" 2019 Oct         f ");
print(" 2019 Oct        fg ");
print(" 2019 Oct       f g ");
print(" 2019 Oct      f  g ");
print(" 2019 Oct      fo g ");
print(" 2019 Oct       f g                         ");
print(" 2019 Oct        ffffo                         foooo          ");
print(" 2019 Oct         g  foo                   fooooooofo        ");
print(" 2019 Oct         g    foo                fo         foo         ");
print(" 2019 Oct      fggoggg  foo              fo           ffoo        ");
print(" 2019 Oct     fo  g   g  fo             fo            ffoo    oo ");
print(" 2019 Oct      fo g  g  goo             f            fffoo       ");
print(" 2019 Oct       oogooooo                 f           foo      oo ");
print(" 2019 Oct    f    g                                 fo ");
print(" 2019 Oct   fo   og                                fo ");
print(" 2019 Oct   fo  og                               ff ");
print(" 2019 Oct    fgg                               f ");
print(" 2019 Oct                                                       ");
print(" 2019 Oct                                                    ");

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
  // real    AA_tilde[M,Q];


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




    }//for sentence w.r.t. qd
}

for(md in 1 : M) {
  for(qd in 1 : Q) {
        AA[md,qd]=Phi(  (mu[md,qd]/v[md,qd])/sqrt((1/v[md,qd])^2+1)  );//Measures of modality performance
            }}




for(md in 1 : M) {
  for(qd in 1 : Q) {
     // deno[C]=1;
     deno[C-1,md,qd]=1-ppp[C,md,qd];
      for(cd in 3:C){  deno[c[cd],md,qd]=deno[c[cd-1],md,qd]-ppp[c[cd-1],md,qd];  }
      // for(cd in (C-2):1){  deno[cd,md,qd]=deno[cd+1,md,qd]-ppp[cd+1,md,qd];  }

}}


for(md in 1 : M) {
  for(qd in 1 : Q) {
                    for(cd in 1:C-1){
                      hit_rate[cd,md,qd]=ppp[cd,md,qd]/deno[cd,md,qd];
                    }
                      hit_rate[C,md,qd]=ppp[C,md,qd];

  }}


}//transformed parameters {









model{
  int s=0;
  // real denoo[C,M,Q];

// for(md in 1 : M) {
//   for(qd in 1 : Q) {
//                     for(cd in 1:C-1){
//                       denoo[cd,md,qd]=deno[cd,md,qd];
//                     }
//                       denoo[C,md,qd]=1;
//
//   }}





  for(qd in 1 : Q) {
    for(md in 1 : M) {
   // if(PreciseLogLikelihood==0) AA[md,qd] ~ normal(A[md],hyper_v[qd]);
   // if(PreciseLogLikelihood==1)   target += normal_lpdf( AA[md,qd]|A[md],hyper_v[qd]);
   target += normal_lpdf( AA[md,qd]|A[md],hyper_v[qd]);


    }  }
  for(n in 1:N) {
    // h[n] ~ binomial(NL, ppp[c[n],m[n],q[n]]);
    // if(PreciseLogLikelihood==0) ff[n] ~ poisson(l[c[n]]*NX);// model
  target +=   poisson_lpmf(ff[n]|l[c[n]]*NX); //very very very coution, not n but c[n] 2019 Jun 21// Additivity of Poisson gives us other equivalent modeling description
   // if(PreciseLogLikelihood==1)  target +=   poisson_lpmf(ff[n]|l[c[n]]*NX); //very very very coution, not n but c[n] 2019 Jun 21// Additivity of Poisson gives us other equivalent modeling description
    // target += binomial_lpmf(h[n]  |  NL, ppp[c[n],m[n],q[n]]   );
  }




  for(qd in 1 : Q) {
    for(md in 1 : M) {
      s=0;
      for(cd in 1 : C){
        // target += binomial_lpmf(harray[cd,md,qd]  |  NL-s, ppp[c[cd],md,qd]/denoo[c[cd],md,qd]   );
      if(prototype==0){ target += binomial_lpmf(harray[cd,md,qd]  |  NL-s, hit_rate[c[cd],md,qd]  );//2019 Oct 11
       s = s + harray[cd,md,qd];
      }else {  target += binomial_lpmf(harray[cd,md,qd]  |  NL-s, ppp[c[cd],md,qd]  );}


    // target +=   poisson_lpmf(f[n]|dl[c[n]]*NL); //very very very coution, not n but c[n] 2019 Jun 21
// I think the former model of hit decrease the number of the divergent transitions, so  it needs to aboid it. 2019 Sept 26


  }}}





if(prior==1){
   w ~ normal(0,111);//2019.6.3
      for(md in 1 : M) {    for(qd in 1 : Q) {
   mu[md,qd] ~ normal(0,111);//2019.6.3
      v[md,qd] ~ uniform(0,111);//2019.6.3

      }}

   dz ~  uniform(0,111);//2019.6.3

   hyper_v ~ uniform(0,111);//2019.6.3

}













if(prior==0){
   w ~ normal(0,1000);//2019.6.3
      for(md in 1 : M) {    for(qd in 1 : Q) {
   mu[md,qd] ~ normal(0,1000);//2019.6.3
      v[md,qd] ~ chi_square(4);//2019.6.3

      }}

   dz ~ chi_square(4);//2019.6.3

   hyper_v ~ chi_square(4);//2019.6.3

}

if(prior == -1){
                          w ~  uniform(-3,3);
    for(cd in 1:C-1) dz[cd] ~  uniform(0.001,7);
          for(md in 1 : M) { for(qd in 1 : Q) {
   mu[md,qd] ~ uniform(-11,11);//2019.6.3
      v[md,qd] ~ uniform(0.01,11);//2019.6.3

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

