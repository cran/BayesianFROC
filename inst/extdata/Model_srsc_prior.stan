functions {

    real psi(real x, real mu, real sigma) {
    return Phi((x-mu)/sigma);
  }



  real inv_psi(real x, real mu, real sigma) {
    return mu + sigma * inv_Phi(x);
  }




}












data{
                  int prior;  //Prior Selection
                  // int <lower=0>N;      //This is same number as C
                  int <lower=0>NL;     //Number of lesions = The number of stimulus presentations, and in this case, each stimulus is a beautiful woman,...,no!,,,,lesions or nodules or ...etc.,,,stimulus is lesions,,,what I talking about,,,,,
                  int <lower=0>NI;     //Number of images = the number of trials, subjects or beautifull women,,,no! images, so each image is radiographs, you know,,,,maybe  .
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
  int prototype;//Logical as.integer(T);as.integer(F)

// param of priors
                  real www;
                  // real mmm;
                  // real vvv;
                  real zzz;
                  real zz;
                  real ww;
                  real vv;
                  real mm;


real epsilon;

real BBB;
real AAA;

real mmm;
real mmmm;
real vvv;
real vvvv;



}

  transformed data {

                    int  NX;
                    if(ModifiedPoisson==0) NX = NI;
                    if(ModifiedPoisson==1) NX =NL;


 print("                                          ");
 print("Model_srsc_prior.stan                   ");
 print("                                       ");
  print("   This is a test, not practically success at 2020 July 30 ");
 print(" Now, The author dose not practically success at 2020 July 30 ");

 print("                    o o          ooo           ");
 print("                  o    o        o   o       ");
 print("                  oo    oooooooooo   o      ");
 print("                    o              ooo        ");
 print("                   o     |     |    o       ");
 print("                  o         X        o          ");
 print("                   o                o            ");
 print("                     ooooooooooooooo          ");
 print(" Failure model     ");

 if(prototype==1) print("prototype = ", prototype,", namely the non-generative model is fitted. ");
 if(prototype==0) print("prototype = ", prototype,", namely the generative model is fitted. ");

  }






parameters{
            // real w;//The Lowest threshold
            // real <lower =0>dz[C-1];//The differences of threshods
            real m; //Not mu !! I regret that this should be mu. I regret !!
            real <lower=0>v;// SD of singal distribution (not variance)
              real  z[C];//Thresholds

}


transformed parameters {
              real <lower=0,upper=1>p[C];//Hit rate, it should be p[1] < p[2] < p[3] < p[4]
              real <lower=0>l[C];//  l[1] > l[2] > l[3] > l[4]
              real <lower=0>dl[C];//False rate, it should be dl_[1] > dl_[2] > dl[3] > dl[4]
              // real  z[C];//Thresholds
              real a=m/v;//abreviation param (not important)
              real b=1/v;//abreviation param (not important)

              real <lower=0,upper=1>deno[C-1];
              real <lower =0> hit_rate[C];

               //                        z[1] = w;
               // for(cd in 1 : C-1)  z[cd+1] = z[cd] +dz[cd];

                                  p[C]  = 1 - Phi((z[C] -m)/v);//To avoid divergent transition
               for(cd in 1 : C-1) p[cd] = Phi((z[cd+1] -m)/v)- Phi((z[cd] -m)/v);

               for(cd in 1 : C) l[cd] = (-1)*log(Phi(z[cd]));

                                 dl[C] = fabs(l[C]-0);//To avoid divergent transition
               for(cd in 1:C-1) dl[cd] = fabs(l[cd]-l[cd+1]);

               // deno[C]=1; // Should not make a parameter whose MCMC chain is constant
                                  deno[C-1] = 1-p[C];
                for(cd in 3:C) deno[c[cd]] = deno[c[cd-1]]-p[c[cd-1]];

                                hit_rate[C] = p[C];
              for(cd in 1:C-1) hit_rate[cd] = p[cd]/deno[cd];

}//transformed parameters



model{



//  from here for prior
real lower_p[C];
real upper_p[C];
real lower_q[C];
real upper_q[C];
real lower_z[C];
real upper_z[C];
real hitRate_[C];
real falseRate_[C];
real fe = epsilon;
real g  = 1-2*epsilon;
real he = epsilon^4;
real ssss;
real ffe=1+epsilon^5;
real gg=ffe+NI*2;
//  to here

      int s=0;




m ~ normal (mmm,mmmm);
v ~ chi_square( (vvv)+vvvv);

lower_q[C]  =  inv_Phi ( 1/ffe ) ;
upper_q[C]  =  inv_Phi ( 1/gg ) ;
lower_p[C]  =  inv_psi (   (1-g)  ,m,v  ) ;
upper_p[C]  =  inv_psi (   (1-fe) ,m,v  ) ;
lower_z[C] =  lower_p[C];
upper_z[C] =  upper_p[C];
lower_z[C] =   inv_Phi(exp(-BBB )          );
upper_z[C] =   inv_Phi(exp(-AAA )        );
z[C]     ~  uniform( lower_z[C] ,  upper_z[C]  );

for (cd in 2:C ){
lower_p[c[cd]] =  inv_psi (   (1-g )* psi(  z[c[cd-1]] ,m,v),m,v ) ;
upper_p[c[cd]] =  inv_psi (   (1-fe)* psi(  z[c[cd-1]] ,m,v),m,v ) ;

lower_q[c[cd]] = inv_Phi(Phi( z[c[cd-1]]) *(1/ffe) );
upper_q[c[cd]] = inv_Phi(Phi( z[c[cd-1]]) *(1/gg) );

lower_z[c[cd]] = inv_Phi(exp(-BBB*Phi( z[c[cd-1]]) )        );
upper_z[c[cd]] = inv_Phi(exp(-AAA*Phi( z[c[cd-1]]) )        );

upper_z[c[cd]] = fmin(upper_z[c[cd]] ,  z[c[cd-1]] );
lower_z[c[cd]] = fmin(lower_z[c[cd]] ,  z[c[cd-1]] )-1;

  z[c[cd]]  ~ uniform ( lower_z[c[cd]] ,  upper_z[c[cd]]  );
}









        for(n in 1:C) {
                     if(PreciseLogLikelihood==0){//Neglect constant terms in the likelihood
                                 if(prototype==1) h[n] ~ binomial(NL-0, p[c[n]]);
                                 if(prototype==0) h[n] ~ binomial(NL-s, hit_rate[c[n]]);
                                 if(prototype==0) s=s+h[n];
                                      // f[n] ~ poisson(dl[c[n]]*NI);
                                       fff[n] ~ poisson( l[c[n]]*NX); // Additivity of Poisson gives us other equivalent modeling description
                              }
                     if(PreciseLogLikelihood==1) { //Include constant terms in the likelihood
                              if(prototype==1)target += binomial_lpmf(h[n]  |  NL-0, p[c[n]]  );
                              if(prototype==0)target += binomial_lpmf(h[n]  |  NL-s, hit_rate[c[n]]  );
                              if(prototype==0)s=s+h[n];
                              // target += poisson_lpmf(f[n]|dl[c[n]]*NI);// <-------very very very coution, not n but c[n] 2019 Jun 21
                              target += poisson_lpmf(fff[n]|l[c[n]]*NX);// <-------very very very coution, not n but c[n] 2019 Jun 21

                                  }
              }







}//model



generated quantities{
                      real <lower=0,upper=1>A;
                      A=Phi(  a/sqrt(b^2+1)  );//AUC, which Measures the recognition performance
}


