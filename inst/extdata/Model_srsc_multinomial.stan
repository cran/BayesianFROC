

data{
  int <lower=0>N;
  int <lower=0>NL;//Number of lesions = The number of stimulus presentations
  int <lower=0>NI;//Number of images = the number of trials, subjects
  int <lower=0>C;
  int <lower=0>h[N];//Hits
  int <lower=0>f[N];//False alarm
  real <lower=0>hh[N];//TPF
  real <lower=0>ff[N];//FPF
  int <lower=0>fff[N];//This is used for poisson assumption.
  int <lower=0>c[N];
  int ModifiedPoisson;//Logical R object are passed to this variable
  int prior;  //Prior Selection


  int samples_from_likelihood_for_ppp;


                  real www;
                  real mmm;
                  real vvv;
                  real zzz;
                  real zz;
                  real ww;
                  real vv;
                  real mm;





}

transformed data{
   int hitExtended[C+1];
                      int  NX;
                    if(ModifiedPoisson==0) NX = NI;
                    if(ModifiedPoisson==1) NX = NL;


    // int <lower=0>hits[C];   //Hits
    // int <lower=0>FalseAlarms[C];   //False alarm
    //
    //
    // for(cd in 1:C) hits[cd] = h[c[cd]];
    // for(cd in 1:C) FalseAlarms[cd] = f[c[cd]];




                    for (cd in 1:C)   hitExtended[cd] = h[cd];

                    hitExtended[C+1]=NL-sum(h);

print("NL - sum(h) = ",NL-sum(h));

for(cd in 1:(C+1)) print("hitExtended[",cd,"] = ",hitExtended[cd]);





}

parameters{
  real w;
  real <lower =0>dz[C-1];
  real m; //Not mu !! I regret that this should be mu. I regret !!
  real <lower=0>v;
}


transformed parameters {


  // real <upper=100000>z[C];
  real  z[C];//2020Jun27
  real  a;
  real  b;
  real <lower=0,upper=1>p[C];
  real <lower=0>l[C];//2020Jun27
  real <lower=0>dl[C];
  simplex[C+1] p_rev_Extented;


a=m/v;
b=1/v;
z[1]=w;
     for(cd in 1 : C-1)  z[cd+1] =z[cd] +dz[cd];



        // p[1]= Phi((z[1] -m)/v) - 0 ;
        p[C]= 1 - Phi((z[C] -m)/v);

     for(cd in 1 : C-1)  p[cd] =Phi((z[cd+1] -m)/v)- Phi((z[cd] -m)/v);





     for(cd in 1 : C) l[cd] = (-1)*log(Phi(z[cd]));

      for(cd in 1:C-1)   dl[cd] = fabs(l[cd]-l[cd+1]);
                         dl[C]  = fabs(l[C]-0);


        for(cd in 1:C)  p_rev_Extented[cd]  = p[c[cd]];
                        p_rev_Extented[C+1] = 1-sum(p);

}



model{

    // hitExtended ~ multinomial( p_rev_Extented );//target forumulation is required
   target +=  multinomial_lpmf(hitExtended | p_rev_Extented );//target forumulation is required

for(n in 1:N) {
// h[n]   ~ binomial(NL, p[c[n]]);

// target += binomial_lpmf(h[n]  |  NL, p[c[n]]   );

// if(!n==1) fff[n] ~ poisson( l[c[n]]*NL);//Non-Chakraborty's model
// fff[n] ~ poisson( l[c[n]]*NL);//Non-Chakraborty's model

// f[n] ~ poisson(dl[c[n]]*NI); //Chakraborty's model
target +=   poisson_lpmf(f[n]|dl[c[n]]*NX);//Chakraborty's model //<-------very very very coution, not n but c[n] 2019 Jun 21

}









if(prior == -1){ // Used in  fit_GUI_Shiny()
 target += normal_lpdf(w|0,111);
    for(cd in 1:C-1) target += uniform_lpdf(dz[cd]|0,111);
 target += normal_lpdf(m|0,111);
                     target += uniform_lpdf(v|0,111);
  }


if(prior == 0){// Used in  fit_GUI_Shiny()
    //                       w ~  uniform(-111,111);
    // for(cd in 1:C-1) dz[cd] ~  uniform(0,111);
    //                       m ~ uniform(-111,111);
    //                       v ~ uniform(0,111);
}


// if(prior == 1){// Used in  fit_GUI_Shiny()
//                           w ~  uniform(-111,111);
//     for(cd in 1:C-1) dz[cd] ~  uniform(0,111);
//                           m ~ uniform(-111,111);
//                           v ~ uniform(0,111);
// }



if(prior == 1){ // Used in  fit_GUI_Shiny()
                   target += normal_lpdf(  w    |-111,111);
                   target += normal_lpdf(  m    |-111,111);
                   target += uniform_lpdf( v    |0,111);
 for(cd in 1:C-1)  target += uniform_lpdf(dz[cd]|0,111);

  }






if(prior == 2){
                          w ~  normal(0,11);
    for(cd in 1:C-1) dz[cd] ~  chi_square(1);
                          m ~  normal(0,11);
                          v ~  normal(0,11);
  }


//
// if(prior == 3){
//                           w ~  normal(ww,www);
//     for(cd in 1:C-1) dz[cd] ~  normal(zz,zzz);
//                           m ~  normal(mm,mmm);
//                           v ~  normal(vv,vvv);
//   }


if(prior == 3){ // Used in  fit_GUI_Shiny()
                   target += normal_lpdf(  w    |ww,www);
                   target += normal_lpdf(  m    |mm,mmm);
                   target += uniform_lpdf( v    |vv,vvv);
 for(cd in 1:C-1)  target += uniform_lpdf(dz[cd]|zz,zzz);

  }


if(prior == 4){
                          w ~  normal(ww,www);
    for(cd in 1:C-1) dz[cd] ~  exponential(zzz);
                          m ~  normal(mm,mmm);
                          v ~  exponential(vvv);
  }









}//model


generated quantities{

real <lower=0>A;  // Observer Recognition performance ability which is a postive real number between 0 and 1. For further informations, please give me a lemonade.
int hits_post_ext_doubly_indexed[C+1,samples_from_likelihood_for_ppp]; // For fucking calculation of p values
int hits_post_doubly_indexed[C,samples_from_likelihood_for_ppp];    // For fucking calculation of p values

// int hits_post_ext[C+1]; // For fucking calculation of p values
// int hits_post[C];    // For fucking calculation of p values

int false_alarms_post_doubly_indexed[C,samples_from_likelihood_for_ppp];    // For fucking calculation of p values


// int false_alarms_post[C];// For fucking calculation of p values
real ss_doubly_indexed[C,samples_from_likelihood_for_ppp];// For fucking calculation of p values
real tt_doubly_indexed[C,samples_from_likelihood_for_ppp];// For fucking calculation of p values

real ss[C];// For fucking calculation of p values

// real tt[C];// For fucking calculation of p values
// real chi_square_with_posterior_data;// For fucking calculation of p values

real chi_square_with_posterior_data_doubly_indexed[samples_from_likelihood_for_ppp];
real xx[C];// For fucking calculation of p values
real yy[C];// For fucking calculation of p values
real chi_square_with_observed_data;// For fucking calculation of p values
real p_value_logicals_indexed[samples_from_likelihood_for_ppp];// For fucking calculation of p values
real p_value_logicals;// For fucking calculation of p values

// int dummy;
// real hits_post_real;
// real false_alarms_post_real;
//
real TPF_post_pred_doubly_indexed[C,samples_from_likelihood_for_ppp];
real FPF_post_pred_doubly_indexed[C,samples_from_likelihood_for_ppp];

// real TPF_post_pred[C];
// real FPF_post_pred[C];

// test ------
  // int n[100];
  //
  // for(i in 1:100) {
  // // set.seed(i);// REDUNDANT AND NOT WORK
  // n[i] = bernoulli_rng(A);
  // }
  //test ------

// A=Phi(  a/sqrt(b^2+1)  );//Measures of modality performance
A=Phi_approx(  a/sqrt(b^2+1)  );//Measures of modality performance

// Today 2020 Oct 19, the cute author noticed that I should calculate the PPP using here!
// Then calculation time will be much smaller! What a cute! Holy moly!
// Without any tips, lemonade, coffee and money! the author fights to calculate ppp more faster! Hannnhhhh
// Today, the author is not such a couch potato! But a sweet potato!
// I am a great! Great! GGGGRRREEEEAT!!!! Bear! without money! I will do! Even if I am such a couch potato, I will show power of potato!


// hits_post = binomial_rng(1,hit_rate);
for(jjj in 1:samples_from_likelihood_for_ppp){ hits_post_ext_doubly_indexed[,jjj] = multinomial_rng(p_rev_Extented,NL);

// hits_post = hits_post_ext[1:C];
for(cd in 1:C) hits_post_doubly_indexed[cd,jjj] =   hits_post_ext_doubly_indexed[cd,jjj];
   for(cd in 1:C)       TPF_post_pred_doubly_indexed[cd,jjj] = hits_post_doubly_indexed[cd,jjj]/(NL*1.000001) ;
TPF_post_pred_doubly_indexed[,jjj] = cumulative_sum(TPF_post_pred_doubly_indexed[,jjj]);

}

for(jjj in 1:samples_from_likelihood_for_ppp){
 for(n in 1:N) false_alarms_post_doubly_indexed[n,jjj] = poisson_rng(dl[c[n]]*NX);//Caution, it's not n but c[n], instead!!! 2020 Oct 19
}



for(jjj in 1:samples_from_likelihood_for_ppp){
for(cd in 1:C){
    // ss_doubly_indexed[cd,jjj]=(hits_post_ext_doubly_indexed[C+1-cd,jjj]-NL*p_rev_Extented[cd])^2/(NL*p_rev_Extented[cd]);
    ss_doubly_indexed[cd,jjj]=(hits_post_ext_doubly_indexed[cd,jjj]-NL*p_rev_Extented[cd])^2/(NL*p_rev_Extented[cd]);//2022Jan20

   }}

for(cd in 1:C)ss[cd]= mean(ss_doubly_indexed[cd,]);
// dummy = hits_post_ext[1];
// for(cd in 1:C-1) hits_post[cd] =   hits_post[C+1-cd];
// hits_post[C] = dummy;



    // for(cd in 1:C)      hits_post_real = hits_post[cd]/NL;
    // for(cd in 1:C)      false_alarms_post_real = false_alarms_post[cd]/NX;

 for(jjj in 1:samples_from_likelihood_for_ppp){
   for(cd in 1:C) FPF_post_pred_doubly_indexed[cd,jjj] = false_alarms_post_doubly_indexed[cd,jjj]/(NX*1.000001) ;
                  FPF_post_pred_doubly_indexed[,jjj] = cumulative_sum(FPF_post_pred_doubly_indexed[,jjj]);
}

for(cd in 1:C){
    // ss[cd]=(hits_post_ext[C+1-cd]-NL*p_rev_Extented[cd])^2/(NL*p_rev_Extented[cd]);
   for(jjj in 1:samples_from_likelihood_for_ppp) tt_doubly_indexed[cd,jjj]=(false_alarms_post_doubly_indexed[C+1-cd,jjj]- dl[cd]*NX  )^2/(dl[cd]*NX);
  }

 for(jjj in 1:samples_from_likelihood_for_ppp){
chi_square_with_posterior_data_doubly_indexed[jjj] = sum(ss_doubly_indexed[,jjj]) + sum(tt_doubly_indexed[,jjj]);
}



for(cd in 1:C){
    // xx[cd]=(h[C+1-cd]-NL*p_rev_Extented[cd])^2/(NL*p_rev_Extented[cd]);
        xx[cd]=(h[cd]-NL*p_rev_Extented[cd])^2/(NL*p_rev_Extented[cd]);//2022Jan20

    yy[cd]=(f[C+1-cd]- dl[cd]*NX  )^2/(dl[cd]*NX);
  }


chi_square_with_observed_data = sum(xx) + sum(yy);



 for(jjj in 1:samples_from_likelihood_for_ppp)p_value_logicals_indexed[jjj] = (chi_square_with_posterior_data_doubly_indexed[jjj] > chi_square_with_observed_data);

p_value_logicals = mean(p_value_logicals_indexed) ;

}


