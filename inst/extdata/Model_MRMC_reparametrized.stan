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
  real zz;


}
  transformed data {
                  int <lower=0> NX;
                  if(ModifiedPoisson==0) NX = NI;
                  if(ModifiedPoisson==1) NX =NL;

                  print(" 2019 Oct-----------------------------")
                  print(" 2019 Oct-----------------------------")
                  print(" 2019 Oct------This model is very bad. If reparameterized, then model is significantly changed.-----------------------")
                  print(" 2019 Oct------Bayesian model depends on the coordinate of model.-----------------------")
                  print(" 2019 Oct-----------------------------")

  }

parameters{
            //  real    w;
            // real <lower =0  >  dz[C-1];
            real    t1;
            real <lower =0  >  dt[C-1];
            real               mu[M,Q];
            real <lower=0>      v[M,Q];
            real <lower=0>      hyper_v[Q];
            real <lower=0,upper=1>A[M];
}

transformed parameters {
                        real <lower =0>       dl[C,M,Q];
                        real <lower=0,upper=1> ppp[C,M,Q];
                        real <lower =0>      l[C,M,Q];
                        real                      aa[M,Q];
                        real <lower =0>           bb[M,Q];
                        real <lower=0,upper=1>    AA[M,Q];
                        real deno[C-1,M,Q];
                        real hit_rate[C,M,Q];
                        real t[C];
 // real z[C,M,Q];
                 // z[1]=w;
                      // t[C]=tC;
for(md in 1 : M) {
for(qd in 1 : Q) {
  aa[md,qd]=mu[md,qd]/v[md,qd];
  bb[md,qd]=1/v[md,qd];
  // cc=C:1;
// for(cd in 1 : C-1) z[cd+1] = z[cd] + dz[cd];
//                       t[C]=tC;
// for(cd in  2 : C){
//  t[C-cd+1] = t[(C-cd+1)+1] - dt[C-cd+1];
//   // print("t[",cd,"] = ",  t[cd]   );
//   }

                               t[1] = t1;
       for(cd in 1 : C-1) t[cd+1] = t[cd] +dt[cd];
  // for(cd in 1:C)  print("t[",cd,"] = ",  t[cd]   );

      // for(cd in 1 : C)z[cd,md,qd]= mu[md,qd]+t[cd]*v[md,qd];


              ppp[C,md,qd] = 1- Phi(t[C]);

 for(cd in 1 : C-1) ppp[cd,md,qd] = Phi(t[cd+1])- Phi(t[cd] );


// for(cd in 1 : C) print("z[",cd,",", md,",",qd,"]  = ", z[cd,md,qd]   );

        for(cd in 1 : C) l[cd,md,qd] = (-1)*log(Phi(mu[md,qd]+t[cd]*v[md,qd]));

                         dl[C,md,qd] = fabs(l[C,md,qd]-0);

          for(cd in 1:C-1) dl[cd,md,qd]= fabs(l[cd,md,qd]-l[cd+1,md,qd]);

// for(cd in 1 : C) print("dl[",cd,",", md,",",qd,"]  = ", dl[cd,md,qd]   );


    }//for qd
}//for  md

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

                          // tC ~  normal(5,5);

                          // t1 ~  uniform(mean(mu[1,])-mean(v[1,]),111);
                     t1 ~  uniform(-111,111);
for(cd in 1:C-1) dt[cd] ~  uniform(0,zz);
for(md in 1 : M) {
  for(qd in 1 : Q) {
             mu[md,qd] ~ uniform(-111,111);//2019.6.3
              v[md,qd] ~ uniform(0,111);//2019.6.3
      }}



  for(qd in 1 : Q) {
    for(md in 1 : M) {
   // if(PreciseLogLikelihood==0) AA[md,qd] ~ normal(A[md],hyper_v[qd]);
   // if(PreciseLogLikelihood==1)   target += normal_lpdf( AA[md,qd]|A[md],hyper_v[qd]);
   target += normal_lpdf( AA[md,qd]|A[md],hyper_v[qd]);


    }  }
  // for(n in 1:N) {
  //   // h[n] ~ binomial(NL, ppp[c[n],m[n],q[n]]);
  //   // if(PreciseLogLikelihood==0) ff[n] ~ poisson(l[c[n]]*NX);// model
  // // target +=   poisson_lpmf(ff[n]|l[c[n],]*NX); //very very very coution, not n but c[n] 2019 Jun 21// Additivity of Poisson gives us other equivalent modeling description
  //
  //  // if(PreciseLogLikelihood==1)  target +=   poisson_lpmf(ff[n]|l[c[n]]*NX); //very very very coution, not n but c[n] 2019 Jun 21// Additivity of Poisson gives us other equivalent modeling description
  //   // target += binomial_lpmf(h[n]  |  NL, ppp[c[n],m[n],q[n]]   );
  // }

  for(qd in 1 : Q) {
    for(md in 1 : M) {
      s=0;
      for(cd in 1 : C){
        // target += binomial_lpmf(harray[cd,md,qd]  |  NL-s, ppp[c[cd],md,qd]/denoo[c[cd],md,qd]   );
        target += binomial_lpmf(harray[cd,md,qd]  |  NL-s, hit_rate[c[cd],md,qd]  );//2019 Oct 11
        // target += binomial_lpmf(harray[cd,md,qd]  |  NL-s, ppp[c[cd],md,qd]  );
    // target +=   poisson_lpmf(ffarray[cd,md,qd]|l[c[cd],md,qd]*NX); //very very very coution, not n but c[n] 2019 Jun 21// Additivity of Poisson gives us other equivalent modeling description
    target +=   poisson_lpmf(farray[cd,md,qd]|dl[c[cd],md,qd]*NX); //very very very coution, not n but c[n] 2019 Jun 21// Additivity of Poisson gives us other equivalent modeling description

      s = s + harray[cd,md,qd];
    // target +=   poisson_lpmf(f[n]|dl[c[n]]*NL); //very very very coution, not n but c[n] 2019 Jun 21
// I think the former model of hit decrease the number of the divergent transitions, so  it needs to aboid it. 2019 Sept 26


  }}}









}
