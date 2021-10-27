// functions {
//
//     real phi_tilde(real x, real mu, real sigma) {
//     return Phi((x-mu)/sigma);
//   }
//
//
//
//   real inv_phi_tilde(real x, real mu, real sigma) {
//     return mu + sigma * inv_Phi(x);
//   }
//
//  real doggy(real x){
//
//
// print("           o o          ooo           ");
// print("         o    o        o   o       ");
// print("         oo    oooooooooo   o      ");
// print("           o              ooo        ");
// print("          o     |     |    o       ");
// print("         o         x        o          ");
// print("          o                o            ");
// print("            ooooooooooooooo          ");
// print("                                  ");
//    return  x;
//  }
//
//
// }



data{
  int <lower=0>N;
  int <lower=0>M;
  int <lower=0>C;
  int <lower=0>Q;
  int <lower=0>h[N];
  int <lower=0>f[N];
  int <lower=0>q[N];
  int <lower=0>c[N];
  int <lower=0>m[N];
  // int <lower=0>NL;
  int <lower=0>NI;
  int <lower=0> NI_deseased;

  int <lower=0>ff[N];
  int <lower=0>harray[NI,C,M,Q];
  int <lower=0>farray[NI,C,M,Q];

  int <lower=0>caseID[N];
  int <lower=0>NL_casewise[NI];
  // int <lower=0>caseIDs_deseased[ ];


  int modalityID_dummy[N, M];
  int readerID_dummy[N, Q];
  int caseID_dummy[N, NI];


  int ccc[C];

  int ModifiedPoisson;//Logical

  int prior;
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
  // int <lower=0> NX;
  //  NX = NI;

  // int <lower=0> NX;
  int hitExtended[NI_deseased,C+1,M,Q];


  int modalityID_dummy_array[NI,C,M,Q];
  int readerID_dummy_array[NI,C,M,Q];
  int caseID_dummy_array[NI,C,M,Q];



  for(caseid in 1:NI){
    for (mmmm in 1:M) {
      for (qqq in 1:Q) {
        for (cccc in 1:C) {
          modalityID_dummy_array[caseid,cccc,mmmm,qqq] =  modalityID_dummy[ (caseid -1 )*M*Q*C + (mmmm-1)*Q*C+(qqq-1)*C+(C+1-cccc),mmmm];
        }}}}

  for(caseid in 1:NI){
    for (mmmm in 1:M) {
      for (qqq in 1:Q) {
        for (cccc in 1:C) {
          readerID_dummy_array[caseid,cccc,mmmm,qqq] =  readerID_dummy[ (caseid -1 )*M*Q*C + (mmmm-1)*Q*C+(qqq-1)*C+(C+1-cccc),qqq];
        }}}}

  for(caseid in 1:NI){
    for (mmmm in 1:M) {
      for (qqq in 1:Q) {
        for (cccc in 1:C) {
          caseID_dummy_array[caseid,cccc,mmmm,qqq] =  caseID_dummy[ (caseid -1 )*M*Q*C + (mmmm-1)*Q*C+(qqq-1)*C+(C+1-cccc),cccc];
        }}}}



//
// if(ModifiedPoisson==0) NX = NI;
// if(ModifiedPoisson==1) NX =NL;


  for(md in 1 : M) {
    for(qd in 1 : Q) {

for(caseid in 1:NI_deseased){
 for (cd in 1:C)    hitExtended[caseid,cd , md,qd] = harray[caseid,cd,md,qd];
                    hitExtended[caseid,C+1,md,qd] = NL_casewise[caseid]-sum(harray[caseid,,md,qd]);
}
// print("NL - sum(h) = ",NL-sum(harray[,md,qd]));

// for(cd in 1:(C+1)) print("hitExtended[",cd, ",",  md, ",", qd ,"] = ",hitExtended[cd,md,qd]);

}}

 // doggy(11.1);




}

parameters{
  real    w[NI_deseased,M,Q];
  // real               mu[NI_deseased,M,Q];
  // real <lower=0>      v[NI_deseased,M,Q];


  real <lower =0> ground_z;
  real <lower =0>  modalityID_dummy_array_slop_z[NI_deseased,M,Q];
  real <lower =0>  readerID_dummy_array_slop_z[NI_deseased,M,Q];
  real <lower =0> caseID_dummy_array_slop_z[NI_deseased,M,Q];

  real ground_mu;
  real modalityID_dummy_array_slop_mu[NI_deseased,M,Q];
  real readerID_dummy_array_slop_mu[NI_deseased,M,Q];
  real caseID_dummy_array_slop_mu[NI_deseased,M,Q];

   real <lower =0>  ground_v ;

  real <lower =0>  modalityID_dummy_array_slop_v[NI_deseased,M,Q];
  real <lower =0>  readerID_dummy_array_slop_v[NI_deseased,M,Q];
  real <lower =0>  caseID_dummy_array_slop_v[NI_deseased,M,Q];
}

transformed parameters {
  real <lower =0>       dl[C,NI_deseased,M,Q];
  real <lower=0,upper=1> ppp[NI_deseased,C,M,Q];
  simplex[C+1] p_rev_Extented[NI_deseased,M,Q];

  real <lower =0>      l[C,NI_deseased,M,Q];
  real    z[C,NI_deseased,M,Q];
  real                      aa[NI_deseased,M,Q];
  real <lower =0>           bb[NI_deseased,M,Q];
  real <lower=0,upper=1>    AA[NI_deseased,M,Q];
  real deno[NI_deseased,C-1,M,Q];
  real hit_rate[NI_deseased,C,M,Q];
  real <lower=0,upper=1>A[M];


  real               mu[NI_deseased,M,Q];
  real <lower=0>      v[NI_deseased,M,Q];

  real <lower =0  >  dz[C-1,NI_deseased,M,Q];





  for(md in 1 : M) {
    for(qd in 1 : Q) {
    for(caseid in 1:NI_deseased){
      for(cd in 1 : C-1) {
        dz[cd,caseid,md,qd] =ground_z + modalityID_dummy_array_slop_z[caseid,md,qd]*modalityID_dummy_array[caseid,1,md,qd] + readerID_dummy_array_slop_z[caseid,md,qd]*readerID_dummy_array[caseid,1,md,qd] + caseID_dummy_array_slop_z[caseid,md,qd]*caseID_dummy_array[caseid,1,md,qd]  ;
}}}}






for(caseid in 1 : NI_deseased) {
  for(md in 1 : M) {
    for(qd in 1 : Q) {
        z[1,caseid,md,qd]=w[caseid,md,qd];


 mu[caseid,md,qd] =ground_mu + modalityID_dummy_array_slop_mu[caseid,md,qd]*modalityID_dummy_array[caseid,1,md,qd] + readerID_dummy_array_slop_mu[caseid,md,qd]*readerID_dummy_array[caseid,1,md,qd] + caseID_dummy_array_slop_mu[caseid,md,qd]*caseID_dummy_array[caseid,1,md,qd]  ;
 v[caseid,md,qd] =ground_v*exp(modalityID_dummy_array_slop_v[caseid,md,qd]*modalityID_dummy_array[caseid,1,md,qd] + readerID_dummy_array_slop_v[caseid,md,qd]*readerID_dummy_array[caseid,1,md,qd] + caseID_dummy_array_slop_v[caseid,md,qd]*caseID_dummy_array[caseid,1,md,qd] ) ;

}}}






  for(md in 1 : M) {
    for(qd in 1 : Q) {

      for(caseid in 1:NI_deseased){
      aa[caseid,md,qd]=mu[caseid,md,qd]/v[caseid,md,qd];
      bb[caseid,md,qd]=1/v[caseid,md,qd];
      }

    for(caseid in 1:NI_deseased){
      for(cd in 1 : C-1) z[cd+1,caseid,md,qd] = z[cd,caseid,md,qd] + dz[cd,caseid,md,qd];
}


      for(caseid in 1:NI_deseased){
      ppp[caseid,C,md,qd] = 1- Phi((z[C,caseid,md,qd] -mu[caseid,md,qd])/v[caseid,md,qd]);
      for(cd in 1 : C-1) ppp[caseid,cd,md,qd] = Phi((z[cd+1,caseid,md,qd] -mu[caseid,md,qd])/v[caseid,md,qd])  - Phi((z[cd,caseid,md,qd ] -mu[caseid,md,qd])/v[caseid,md,qd]);


  // for(md in 1 : M) {
  //   for(qd in 1 : Q) {
        for(cd in 1:C)  p_rev_Extented[caseid,md,qd][cd]  = ppp[caseid,ccc[cd],md,qd];
                        p_rev_Extented[caseid,md,qd][C+1] = 1-sum(ppp[caseid,,md,qd]);
}
// }}



      for(caseid in 1:NI_deseased){
      for(cd in 1 : C) l[cd,caseid,md,qd] = (-1)*log(Phi(z[cd,caseid,md,qd]));
      dl[C,caseid,md,qd] = fabs(l[C,caseid,md,qd]-0);
      for(cd in 1:C-1) dl[cd,caseid,md,qd]= fabs(l[cd,caseid,md,qd]-l[cd+1,caseid,md,qd]);
}



    }
  }


for(caseid in 1:NI_deseased){
  for(md in 1 : M) {
    for(qd in 1 : Q) {
      AA[caseid,md,qd]=Phi(  (mu[caseid,md,qd]/v[caseid,md,qd])/sqrt((1/v[caseid,md,qd])^2+1)  );//Measures of modality performance
    }}}
for(caseid in 1:NI_deseased){
  for(md in 1 : M) {
   A[md] = 0;
    for(qd in 1 : Q) {
     A[md] =  A[md] +  AA[caseid,md,qd];
    }
   A[md]=   A[md]/Q;//fix 2020 Jan
    }
}

for(caseid in 1:NI_deseased){

  for(md in 1:M) {
    for(qd in 1:Q) {
      deno[caseid,C-1,md,qd]=1-ppp[caseid,C,md,qd];
      for(cd in 3:C){  deno[caseid,c[cd],md,qd]=deno[caseid,c[cd-1],md,qd]-ppp[caseid,c[cd-1],md,qd];  }
    }}
}

for(caseid in 1:NI_deseased){
  for(md in 1 : M) {
    for(qd in 1 : Q) {
      for(cd in 1:C-1){
        hit_rate[caseid,cd,md,qd]=ppp[caseid,cd,md,qd]/deno[caseid,cd,md,qd];
      }
      hit_rate[caseid,C,md,qd]=ppp[caseid,C,md,qd];

    }}
}


}






model{
    int s=0;


    // for(qd in 1 : Q) {
    //   for(md in 1 : M) {
    //     target += normal_lpdf( AA[md,qd]|A[md],hyper_v[qd]);
    //   }  }
    for(caseid in 1:NI_deseased){
    for(qd in 1 : Q) {
      for(md in 1 : M) {
    for(n in 1:N) {
      // target +=   poisson_lpmf(ff[n]|l[c[n]]*NX);
      target +=   poisson_lpmf(ff[n]|l[c[n],caseid,md,qd]);

    }}}}

for(caseid in 1:NI_deseased){
    for(qd in 1 : Q) {
      for(md in 1 : M) {
   target +=  multinomial_lpmf(hitExtended[caseid,,md,qd] | p_rev_Extented[caseid,md,qd] );//target forumulation is required
}}
}

    // for(qd in 1 : Q) {
    //   for(md in 1 : M) {
    //     s=0;
    //     for(cd in 1 : C){
    //        target += binomial_lpmf(harray[cd,md,qd]  |  NL-s, hit_rate[c[cd],md,qd]  );
    //       s = s + harray[cd,md,qd]; }
    //     }}





//
// if(prior ==1){
//
//       w ~  uniform(-3,3);
//       for(cd in 1:C-1) dz[cd] ~  uniform(0.001,2);
//       for(md in 1 : M) { for(qd in 1 : Q) {for(caseid in 1:NI_deseased){
//         mu[caseid,md,qd] ~ uniform(-1,1);
//         v[caseid,md,qd] ~ uniform(0.01,1);
//
//       }}}
//
//       }

//
// if(prior == -1){ // Used in  fit_GUI_Shiny()
//  target += normal_lpdf(w|ww,www);
//     for(cd in 1:C-1) target += uniform_lpdf(dz[cd]|0,zzz);
//
//           for(md in 1 : M) { for(qd in 1 : Q) {
//                         target += normal_lpdf(mu[md,qd]|mm,mmm);
//                         target += uniform_lpdf(v[md,qd]|0,vvv);
//           }}
//
//   }




// if(prior ==1){
 for(md in 1 : M) { for(qd in 1 : Q) {for(caseid in 1:NI_deseased){

      w[caseid,md,qd] ~  normal(ww,www);

      for(cd in 1:C-1) dz[cd,caseid,md,qd] ~  exponential(zzz);
        mu[caseid,md,qd] ~ normal(mm,mmm);
        v[caseid,md,qd] ~ exponential(vvv);

      }}}

      // }


  }












//
//
// generated quantities{
//
// // real <lower=0>A;  // Observer Recognition performance ability which is a postive real number between 0 and 1. For further informations, please give me a lemonade.
// int hits_post_ext[C+1,M,Q]; // For fucking calculation of p values
// int hits_post[C,M,Q];    // For fucking calculation of p values
// int false_alarms_post[C,M,Q];// For fucking calculation of p values
// real ss[C,M,Q];// For fucking calculation of p values
// real tt[C,M,Q];// For fucking calculation of p values
// real chi_square_with_posterior_data[M,Q];// For fucking calculation of p values
// real xx[C,M,Q];// For fucking calculation of p values
// real yy[C,M,Q];// For fucking calculation of p values
// real chi_square_with_observed_data[M,Q];// For fucking calculation of p values
// int p_value_logicals[M,Q];// For fucking calculation of p values
// // int dummy;
// // real hits_post_real;
// // real false_alarms_post_real;
// //
// real TPF_post_pred[C,M,Q];
// real FPF_post_pred[C,M,Q];
//
// int modalityID[C,M,Q];
// int readerID[C,M,Q];
// // A=Phi(  a/sqrt(b^2+1)  );//Measures of modality performance
//
//
// // Today 2020 Oct 19, the cute author noticed that I should calculate the PPP using here!
// // Then calculation time will be much smaller! What a cute! Holy moly!
// // Without any tips, lemonade, coffee and money! the author fights to calculate ppp more faster! Hannnhhhh
// // Today, the author is not such a couch potato! But a sweet potato!
// // I am a great! Great! GGGGRRREEEEAT!!!! Bear! without money! I will do! Even if I am such a couch potato, I will show power of potato!
//
//
// // hits_post = binomial_rng(1,hit_rate);
//    for(qd in 1 : Q) {
//       for(md in 1 : M) {
// hits_post_ext[,md,qd] = multinomial_rng(p_rev_Extented[md,qd],NL);
// }}
// // hits_post = hits_post_ext[1:C];
//    for(qd in 1 : Q) {
//       for(md in 1 : M) {
// for(cd in 1:C) hits_post[cd,md,qd] =   hits_post_ext[cd,md,qd];
// }}
// // dummy = hits_post_ext[1];
// // for(cd in 1:C-1) hits_post[cd] =   hits_post[C+1-cd];
// // hits_post[C] = dummy;
// for(qd in 1 : Q) {
//       for(md in 1 : M) {
//  for(cd in 1:C) false_alarms_post[cd,md,qd] = poisson_rng(dl[ccc[cd]]*NX);//Caution, it's not n but c[n], instead!!! 2020 Oct 19
// }}
//
//     // for(cd in 1:C)      hits_post_real = hits_post[cd]/NL;
//     // for(cd in 1:C)      false_alarms_post_real = false_alarms_post[cd]/NX;
//     for(qd in 1 : Q) {
//       for(md in 1 : M) {
//    for(cd in 1:C)       TPF_post_pred[cd,md,qd] = hits_post[cd,md,qd]/(NL*1.000001) ;
//    for(cd in 1:C)       FPF_post_pred[cd,md,qd] = false_alarms_post[cd,md,qd]/(NX*1.000001) ;
//       }}
//
//         for(qd in 1 : Q) {
//       for(md in 1 : M) {
// TPF_post_pred[,md,qd] = cumulative_sum(TPF_post_pred[,md,qd]);
// FPF_post_pred[,md,qd] = cumulative_sum(FPF_post_pred[,md,qd]);
// }}
//
//
//
//    for(qd in 1 : Q) {
//       for(md in 1 : M) {
//         for(cd in 1:C) {
//           modalityID[cd,md,qd] = md;
//           readerID[cd,md,qd] = qd;
//         }
//
//       }}
//
//
// for(qd in 1 : Q) {
// for(md in 1 : M) {
// for(cd in 1:C){
//     ss[cd,md,qd]=(hits_post_ext[C+1-cd,md,qd]-NL*p_rev_Extented[md,qd][cd])^2/(NL*p_rev_Extented[md,qd][cd]);
//     tt[cd,md,qd]=(false_alarms_post[C+1-cd,md,qd]- dl[cd]*NX  )^2/(dl[cd]*NX);
//   }}}
//
// for(qd in 1 : Q) {
// for(md in 1 : M) {
// chi_square_with_posterior_data[md,qd] = sum(ss[,md,qd]) + sum(tt[,md,qd]);
// }}
//
//
// for(qd in 1 : Q) {
// for(md in 1 : M) {
// for(cd in 1:C){
//     xx[cd,md,qd]=(harray[C+1-cd,md,qd]-NL*p_rev_Extented[md,qd][cd])^2/(NL*p_rev_Extented[md,qd][cd]);
//     yy[cd,md,qd]=(farray[C+1-cd,md,qd]- dl[cd]*NX  )^2/(dl[cd]*NX);
//   }}}
//
//
// for(qd in 1 : Q) {
// for(md in 1 : M) {
// chi_square_with_observed_data[md,qd] = sum(xx[,md,qd]) + sum(yy[,md,qd]);
// }}
//
//
//
//
// for(qd in 1 : Q) {
// for(md in 1 : M) {
// p_value_logicals[md,qd] = (chi_square_with_posterior_data[md,qd] > chi_square_with_observed_data[md,qd]);
// }}
// // The posterior mean of this fucking sucks p_value_logicals can be interpreted as a chis square goodness of fit
// // If this guy is less than, e.g., 0.05 then we reject the null hypothesis that the model is well fitted to a fucking dataset.
// //  Of course, in certain condition, frequentist p value  exactly coincides to the posterior prob of event that the .... so,,, I hate stats. Good luck fucking my life sucks.
//
// }
//
//
//
//
//









// e <-extract_EAP_by_array(f,"p_rev_Extented")
// apply(e, c(1,2), sum)
// Model_MRMC_Multinomial.stan #2020 Nov 23

