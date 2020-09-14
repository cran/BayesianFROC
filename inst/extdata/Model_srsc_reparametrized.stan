data{
                  int prior;  //Prior Selection
                  // int <lower=0>N;      //This is same number as C
                  int <lower=0>NL;     //Number of lesions
                  int <lower=0>NI;     //In my original model, NI is not used.
                  int <lower=0>C;      //Number of confidence levels
                  int <lower=0>h[C];   //Hits
                  int <lower=0>f[C];   //False alarm
                  // real <lower=0>hh[N]; //TPF
                  // real <lower=0>ff[N]; //FPF
                  int <lower=0>fff[C]; //This is used for poisson assumption.
                  int <lower=0>c[C];   //confidence level
                  int PreciseLogLikelihood;//Logical R object are passed to this variable
                  // logic PreciseLogLikelihood;
                  // int is_NL_adjusted;
                  int ModifiedPoisson;//Logical R object are passed to this variable

// param of priors
                  real www;
                  real mmm;
                  real vvv;
                  real zzz;
                  real zz;
                  real ww;
                  real vv;
                  real mm;





}

  transformed data {

                    int  NX;
                    if(ModifiedPoisson==0) NX = NI;
                    if(ModifiedPoisson==1) NX =NL;


 print("--------------------------------------------------------");
 print("--------------------------------------------------------");
 print("--------------------------------------------------------");
 print("--------------------------------------------------------");
 print("--------------------------------------------------------");
 print("--------------------------------------------------------");
 print("--------------------------------------------------------");
 print("--------------------------------------------------------");
 print("--------------------------------------------------------");
 print("--------------------------------------------------------");
 print("--------------------------------------------------------");
 print("--------------------------------------------------------");
 print("zz = ",zz)
 print("NX = ",NX)



  }






parameters{
            // real w;//The Lowest threshold
            // real <lower =0>dz[C-1];//The differences of threshods
            real m; //Not mu !! I regret that this should be mu. I regret !!
            real <lower=0>v;// SD of singal distribution (not variance)
// real t[C];
            real t1;//The Lowest threshold
            real <lower =0>dt[C-1];//The differences of threshods

}


transformed parameters {
                        real <lower=0,upper=1>p[C];//Hit rate, it should be p[1] < p[2] < p[3] < p[4]
                        real <lower=0>l[C];//  l[1] > l[2] > l[3] > l[4]
                        real <lower=0>dl[C];//False rate, it should be dl_[1] > dl_[2] > dl[3] > dl[4]
                        real a=m/v;//abreviation param (not important)
                        real b=1/v;//abreviation param (not important)

                        real <lower=0,upper=1>deno[C-1];
                        real <lower =0> hit_rate[C];
real t[C];

     //                         z[1] = w;
     // for(cd in 1 : C-1) { z[cd+1] = z[cd] +dz[cd];  }

                               t[1] = t1;
       for(cd in 1 : C-1) t[cd+1] = t[cd] +dt[cd];


                            p[C]  = 1 - Phi( t[C]);//To avoid divergent transition
     for(cd in 1 : C-1) {   p[cd] = Phi(t[cd+1])- Phi(t[cd] );  }

     for(cd in 1 : C) {     l[cd] = (-1)*log(Phi(m+t[cd]*v));     }

                            dl[C] = fabs(l[C]-0);//To avoid divergent transition
     for(cd in 1:C-1){     dl[cd] = fabs(l[cd]-l[cd+1]); }

     // deno[C]=1; // Should not make a parameter whose MCMC chain is constant
                        deno[C-1] = 1-p[C];
      for(cd in 3:C){ deno[c[cd]] = deno[c[cd-1]]-p[c[cd-1]];  }

                      hit_rate[C] = p[C];
    for(cd in 1:C-1) hit_rate[cd] = p[cd]/deno[cd];

}//transformed parameters



model{
      int s=0;

        for(n in 1:C) {
                     if(PreciseLogLikelihood==0){//Neglect constant terms in the likelihood
                                         h[n] ~ binomial(NL-s, hit_rate[c[n]]);
                                     s=s+h[n];
                                      // f[n] ~ poisson(dl[c[n]]*NX);
                                       fff[n] ~ poisson( l[c[n]]*NX); // Additivity of Poisson gives us other equivalent modeling description
                              }
                     if(PreciseLogLikelihood==1) { //Include constant terms in the likelihood
                              target += binomial_lpmf(h[n]  |  NL-s, hit_rate[c[n]]  );
                                                  s=s+h[n];
                              // target += poisson_lpmf(f[n]|dl[c[n]]*NX);// <-------very very very coution, not n but c[n] 2019 Jun 21
                              target += poisson_lpmf(fff[n]|l[c[n]]*NX);// <-------very very very coution, not n but c[n] 2019 Jun 21

                                  }
              }


if(prior == -1){
                          // w ~  normal(0,111);
    // for(cd in 1:C-1) dz[cd] ~  uniform(0,111);
                          m ~  normal(0,111);
                          v ~  uniform(0,111);
           // for(cd in 1:C-1) t[cd]~uniform(0,t[cd+1]);
           //                  t[C] ~ uniform(0,100);
    for(cd in 1:C-1) dt[cd] ~  uniform(0,zz);

                          t1 ~  normal(-1,111);

  }
//
//
// if(prior == 0){
//     //                       w ~  uniform(-111,111);
//     // for(cd in 1:C-1) dz[cd] ~  uniform(0,111);
//     //                       m ~ uniform(-111,111);
//     //                       v ~ uniform(0,111);
// }
//
//
// if(prior == 1){
//                           w ~  uniform(-111,111);
//     for(cd in 1:C-1) dz[cd] ~  uniform(0,111);
//                           m ~ uniform(-111,111);
//                           v ~ uniform(0,111);
// }
//
//
//
//
//
//
//
//
//
// //
// // if(prior == 1){
// //                           w ~  normal(ww,www);
// //     for(cd in 1:C-1) dz[cd] ~  normal(zz,zzz);
// //                           m ~ normal(mm,mmm);
// //                           v ~ normal(vv,vvv);
// //   }
//
// if(prior == 2){
//                           w ~  normal(ww,www);
//     for(cd in 1:C-1) dz[cd] ~  chi_square(1);
//                           m ~  normal(mm,mmm);
//                           v ~  normal(vv,vvv);
//   }
//
//
//
// if(prior == 3){
//                           w ~  normal(ww,www);
//     for(cd in 1:C-1) dz[cd] ~  chi_square(1);
//                           m ~  normal(mm,mmm);
//                           v ~  chi_square(1);
//   }
//
//
//
//
//
// if(prior == 4){
//                           w ~  normal(ww,www);
//     for(cd in 1:C-1) dz[cd] ~  exponential(1);
//                           m ~ normal(mm,mmm);
//                           v ~  exponential(1);
//   }
//
//
//
// if(prior == 5){
//                           w ~  normal(ww,www);
//     for(cd in 1:C-1) dz[cd] ~  inv_gamma(1.1,1.1);
//                           m ~  normal(mm,mmm);
//                           v ~  inv_gamma(1.1,1.1);
//   }
//
//
//
// if(prior == 6){
//                           w ~  normal(ww,www);
//     for(cd in 1:C-1) dz[cd] ~  gamma(1.1,1.1);
//                           m ~  normal(mm,mmm);
//                           v ~  gamma(1.1,1.1);
//   }
//
//
//
//
// if(prior == 7){
//                           w ~  normal(ww,www);
//     for(cd in 1:C-1) dz[cd] ~  frechet(1.1,1.1);
//                           m ~  normal(mm,mmm);
//                           v ~  frechet(1.1,1.1);
//   }
//
//
//
// if(prior == 8){
//                           w ~  normal(ww,www);
//     for(cd in 1:C-1) dz[cd] ~  scaled_inv_chi_square(1.1,1.1);
//                           m ~ normal(mm,mmm);
//                           v ~  scaled_inv_chi_square(1.1,1.1);
//   }
//
//
//
// if(prior == 9){
//                           w ~  normal(ww,www);
//     for(cd in 1:C-1) dz[cd] ~  lognormal(1.1,1.1);
//                           m ~  normal(mm,mmm);
//                           v ~  lognormal(1.1,1.1);
//   }
//
//
//
//
// if(prior == 10){
//                           w ~  normal(0,100);
//     for(cd in 1:C-1) dz[cd] ~  exponential(0.5);
//                           m ~ normal(0,100);
//                           v ~  exponential(0.5);
//   }
//
//
//
//
//
//
//


}//model



generated quantities{
                      real <lower=0,upper=1>A;
                      A=Phi(  a/sqrt(b^2+1)  );//AUC, which Measures the modality performance
}


