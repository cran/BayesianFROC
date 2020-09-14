
// 2020 August 13, I made this, and it works fine.
// This model is written to ajust the order of hits and its rate.
// In the Model_srsc.stan, the hit and false alarms are written by the apposite oreder and this is the
//  most biggest that I regret because it makes codes to be complex.
data{
    // int <lower=0>N;      //This is same number as C
    int <lower=0>NL;     //Number of lesions = The number of stimulus presentations, and in this case, each stimulus is a beautiful woman,...,no!,,,,lesions or nodules or ...etc.,,,stimulus is lesions,,,what I talking about,,,,,
    int <lower=0>NI;     //Number of images = the number of trials, subjects that include beautifull women,,,no! lesions as shadows, so each image is radiographs, you know,,,,maybe  .
    int <lower=0>C;      //Number of confidence levels, sometimes it is calles ratings.
    int <lower=0>h[C];   //Hits, The number of True Posotives.
    int <lower=0>f[C];   //False alarm
    // real <lower=0>hh[N]; //TPF
    // real <lower=0>ff[N]; //FPF
    // int <lower=0>fff[C]; //This is used for poisson assumption.
    int <lower=0>c[C];   //confidence level
    int PreciseLogLikelihood;//Logical R object are passed to this variable
    // logic PreciseLogLikelihood;
    // int is_NL_adjusted;
    int prior;  //Prior Selection

    int ModifiedPoisson;//Logical R object are passed to this variable
    int prototype;//Logical as.integer(T);as.integer(F)

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


    int <lower=0>hits[C];   //Hits
    int <lower=0>FalseAlarms[C];   //False alarm

    // int <lower=0>TPF[C];   //Hits
    // int <lower=0>FPF[C];   //False alarm

    for(cd in 1:C) hits[cd] = h[c[cd]];
    for(cd in 1:C) FalseAlarms[cd] = f[c[cd]];




    if(ModifiedPoisson==0) NX = NI;
    if(ModifiedPoisson==1) NX =NL;







print("                                  ");
print("           o o          ooo           ");
print("         o    o        o   o       ");
print("         oo    oooooooooo   o      ");
print("           o              ooo        ");
print("          o     |     |    o       ");
print("         o         *        o          ");
print("          o                o       ");
print("          o ooooooooooooooo      ooo   ");
print("           o             o  o    o   o ");
print("         o    o    o   o     o  o  o ");
print("       o    o  o    ooo        o o o ");
print("        ooo     o              ooo  ");
print("                 o    ooo     o  ");
print("                 o   o   o   o ");
print("                  ooo     ooo  ");
print("                                  ");
print("           o o          ooo           ");
print("         o    o        o   o       ");
print("         oo    oooooooooo   o      ");
print("           o              ooo        ");
print("          o     |     |    o       ");
print("         o         x        o          ");
print("          o                o            ");
print("            ooooooooooooooo          ");
print("                                  ");
print("                                  ");
print("           o o          ooo           ");
print("         o    o        o   o       ");
print("         oo    oooooooooo   o      ");
print("           o              ooo        ");
print("          o     |     |    o       ");
print("         o         *        o          ");
print("          o                o            ");
print("            ooooooooooooooo          ");
print("                                  ");
print("                                  ");
print("                                  ");
print("                                  ");
print("           o o          ooo           ");
print("         o    o        o   o       ");
print("         oo    oooooooooo   o      ");
print("           o              ooo        ");
print("          o     |     |    o       ");
print("         o         *        o          ");
print("          o                o       ");
print("          o ooooooooooooooo      ooo   ");
print("         o              o  o    o   o ");
print("         o  ooooooo    o     o  o  o ");
print("       o    o      ooo        o o o ");
print("        ooo    o              ooo  ");
print("                 o    ooo     o  ");
print("                 o   o   o   o ");
print("                  ooo     ooo  ");
print("                                  ");
print("   Ruicobach Sampo Ikantone (2006 ~ ----) ");
print("                                  ");
print("   Copyright(c) by two doggies, ");
print("                    Ruikosan and Riyaboe");
print("    All rights  reserved ");
print("                                  ");
print("           o o          ooo     ");
print("         o     o       o    o     ");
print("         o       oooo      o     ");
print("           o              o     ");
print("          o     |     |    o     ");
print("         o         *        o     ");
print("           o              o     ");
print("             o          o          ooo     ");
print("           o             o o    o   o     ");
print("         o    o    o   o     o  o o o     ");
print("       o    o  o    ooo        oo     ");
print("        oo      o              o     ");
print("                 o    ooo     o     ");
print("                 o   o   o   o     ");
print("                  ooo     ooo     ");

 if(prototype==1) print("prototype = ", prototype,", namely the non-generative model is fitted. ");
 if(prototype==0) print("prototype = ", prototype,", namely the generative model is fitted. ");

  }






parameters{
            real w;//The Lowest threshold
            real <lower =0>dz[C-1];//The differences of threshods
            real m; //Not mu !! I regret that this should be mu. I regret !!
            real <lower=0>v;// SD of singal distribution (not variance)

}


transformed parameters {
              real <lower=0,upper=1>p[C];//Hit rate, it should be p[1] < p[2] < p[3] < p[4]
              real <lower=0>l[C];//  l[1] > l[2] > l[3] > l[4]
              real <lower=0>dl[C];//False rate, it should be dl_[1] > dl_[2] > dl[3] > dl[4]
              real  z[C];//Thresholds
              real a=m/v;//abreviation param (not important)
              real b=1/v;//abreviation param (not important)

              real <lower=0,upper=1>deno[C-1];
              real <lower =0> hit_rate[C];

                                      z[1] = w;
               for(cd in 1 : C-1)  z[cd+1] = z[cd] +dz[cd];

                                  p[C]  = 1 - Phi((z[C] -m)/v);//To avoid divergent transition
               for(cd in 1 : C-1) p[cd] = Phi((z[cd+1] -m)/v)- Phi((z[cd] -m)/v);

               for(cd in 1 : C) l[cd] = (-1)*log(Phi(z[cd]));

                                 dl[C] = fabs(l[C]-0);//To avoid divergent transition
               for(cd in 1:C-1) dl[cd] = fabs(l[cd]-l[cd+1]);

               // deno[C]=1; // Should not make a parameter whose MCMC chain is constant
                                  deno[1] = 1-p[1];
                for(cd in 2:C-1) deno[cd] = deno[cd-1]-p[cd];

                                hit_rate[1] = p[1];
              for(cd in 2:C) hit_rate[cd] = p[cd]/deno[cd-1];

}//transformed parameters




model{
      int s=0;

        for(n in 1:C) {
                     if(PreciseLogLikelihood==0){//Neglect constant terms in the likelihood
                                 if(prototype==1) hits[n] ~ binomial(NL-0, p[n]);
                                 if(prototype==0) hits[n] ~ binomial(NL-s, hit_rate[n]);
                                 if(prototype==0) s=s+hits[n];
                                      FalseAlarms[n] ~ poisson(dl[n]*NX);
                                       // fff[n] ~ poisson( l[c[n]]*NX); // Additivity of Poisson gives us other equivalent modeling description
                              }
                     if(PreciseLogLikelihood==1) { //Include constant terms in the likelihood
                              if(prototype==1)target += binomial_lpmf(hits[n]  |  NL-0, p[n]  );
                              if(prototype==0)target += binomial_lpmf(hits[n]  |  NL-s, hit_rate[n]  );
                              if(prototype==0)s=s+hits[n];
                              target += poisson_lpmf(FalseAlarms[n]|dl[n]*NX);// <-------very very very coution, not n but c[n] 2019 Jun 21
                              // target += poisson_lpmf(fff[n]|l[c[n]]*NX);// <-------very very very coution, not n but c[n] 2019 Jun 21

                                  }
              }


if(prior == -1){ // Used in  fit_GUI_Shiny()
                          w ~  normal(0,111);
    for(cd in 1:C-1) dz[cd] ~  uniform(0,111);
                          m ~  normal(0,111);
                          v ~  uniform(0,111);
  }


if(prior == 0){// Used in  fit_GUI_Shiny()
    //                       w ~  uniform(-111,111);
    // for(cd in 1:C-1) dz[cd] ~  uniform(0,111);
    //                       m ~ uniform(-111,111);
    //                       v ~ uniform(0,111);
}


if(prior == 1){// Used in  fit_GUI_Shiny()
                          w ~  uniform(-111,111);
    for(cd in 1:C-1) dz[cd] ~  uniform(0,111);
                          m ~ uniform(-111,111);
                          v ~ uniform(0,111);
}









if(prior == 2){
                          w ~  normal(0,11);
    for(cd in 1:C-1) dz[cd] ~  chi_square(1);
                          m ~  normal(0,11);
                          v ~  normal(0,11);
  }



if(prior == 3){
                          w ~  normal(ww,www);
    for(cd in 1:C-1) dz[cd] ~  normal(zz,zzz);
                          m ~  normal(mm,mmm);
                          v ~  normal(vv,vvv);
  }



// if(prior == 3){
//                           w ~  normal(ww,www);
//     for(cd in 1:C-1) dz[cd] ~  chi_square(1);
//                           m ~  normal(mm,mmm);
//                           v ~  chi_square(1);
//   }





if(prior == 4){
                          w ~  normal(ww,www);
    for(cd in 1:C-1) dz[cd] ~  exponential(1);
                          m ~ normal(mm,mmm);
                          v ~  exponential(1);
  }



if(prior == 5){
                          w ~  normal(ww,www);
    for(cd in 1:C-1) dz[cd] ~  inv_gamma(1.1,1.1);
                          m ~  normal(mm,mmm);
                          v ~  inv_gamma(1.1,1.1);
  }



if(prior == 6){
                          w ~  normal(ww,www);
    for(cd in 1:C-1) dz[cd] ~  gamma(1.1,1.1);
                          m ~  normal(mm,mmm);
                          v ~  gamma(1.1,1.1);
  }




if(prior == 7){
                          w ~  normal(ww,www);
    for(cd in 1:C-1) dz[cd] ~  frechet(1.1,1.1);
                          m ~  normal(mm,mmm);
                          v ~  frechet(1.1,1.1);
  }



if(prior == 8){
                          w ~  normal(ww,www);
    for(cd in 1:C-1) dz[cd] ~  scaled_inv_chi_square(1.1,1.1);
                          m ~ normal(mm,mmm);
                          v ~  scaled_inv_chi_square(1.1,1.1);
  }



if(prior == 9){
                          w ~  normal(ww,www);
    for(cd in 1:C-1) dz[cd] ~  lognormal(1.1,1.1);
                          m ~  normal(mm,mmm);
                          v ~  lognormal(1.1,1.1);
  }




if(prior == 10){
                          w ~  normal(0,100);
    for(cd in 1:C-1) dz[cd] ~  exponential(0.5);
                          m ~ normal(0,100);
                          v ~  exponential(0.5);
  }









}//model



generated quantities{
                      real <lower=0,upper=1>A;
                      A=Phi(  a/sqrt(b^2+1)  );//AUC, which Measures the recognition performance
}


