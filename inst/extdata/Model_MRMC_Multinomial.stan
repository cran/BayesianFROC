functions {

    real phi_tilde(real x, real mu, real sigma) {
    return Phi((x-mu)/sigma);
  }



  real inv_phi_tilde(real x, real mu, real sigma) {
    return mu + sigma * inv_Phi(x);
  }

 real doggy(real x){


print("           o o          ooo           ");
print("         o    o        o   o       ");
print("         oo    oooooooooo   o      ");
print("           o              ooo        ");
print("          o     |     |    o       ");
print("         o         x        o          ");
print("          o                o            ");
print("            ooooooooooooooo          ");
print("                                  ");
   return  x;
 }


}



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
  int <lower=0>NL;
  int <lower=0>NI;

  int <lower=0>ff[N];
  int <lower=0>harray[C,M,Q];
  int <lower=0>farray[C,M,Q];

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

  int <lower=0> NX;
  int hitExtended[C+1,M,Q];

if(ModifiedPoisson==0) NX = NI;
if(ModifiedPoisson==1) NX =NL;


  for(md in 1 : M) {
    for(qd in 1 : Q) {


 for (cd in 1:C)   hitExtended[cd,md,qd] = harray[cd,md,qd];
                    hitExtended[C+1,md,qd]=NL-sum(harray[,md,qd]);

// print("NL - sum(h) = ",NL-sum(harray[,md,qd]));

// for(cd in 1:(C+1)) print("hitExtended[",cd, ",",  md, ",", qd ,"] = ",hitExtended[cd,md,qd]);

}}

 // doggy(11.1);




}

parameters{
  real    w;
  real <lower =0  >  dz[C-1];
  real               mu[M,Q];
  real <lower=0>      v[M,Q];
  // real <lower=0>      hyper_v[Q];

}

transformed parameters {
  real <lower =0>       dl[C];
  real <lower=0,upper=1> ppp[C,M,Q];
  simplex[C+1] p_rev_Extented[M,Q];

  real <lower =0>      l[C];
  real    z[C];
  real                      aa[M,Q];
  real <lower =0>           bb[M,Q];
  real <lower=0,upper=1>    AA[M,Q];
  real deno[C-1,M,Q];
  real hit_rate[C,M,Q];
  real <lower=0,upper=1>A[M];

  z[1]=w;

  for(md in 1 : M) {
    for(qd in 1 : Q) {
      aa[md,qd]=mu[md,qd]/v[md,qd];
      bb[md,qd]=1/v[md,qd];

      for(cd in 1 : C-1) z[cd+1] = z[cd] + dz[cd];
      ppp[C,md,qd] = 1- Phi((z[C] -mu[md,qd])/v[md,qd]);
      for(cd in 1 : C-1) ppp[cd,md,qd] = Phi((z[cd+1] -mu[md,qd])/v[md,qd])  - Phi((z[cd ] -mu[md,qd])/v[md,qd]);


  // for(md in 1 : M) {
  //   for(qd in 1 : Q) {
        for(cd in 1:C)  p_rev_Extented[md,qd][cd]  = ppp[ccc[cd],md,qd];
                        p_rev_Extented[md,qd][C+1] = 1-sum(ppp[,md,qd]);

// }}



      for(cd in 1 : C) l[cd] = (-1)*log(Phi(z[cd]));
      dl[C] = fabs(l[C]-0);
      for(cd in 1:C-1) dl[cd]= fabs(l[cd]-l[cd+1]);




    }
  }

  for(md in 1 : M) {
    for(qd in 1 : Q) {
      AA[md,qd]=Phi(  (mu[md,qd]/v[md,qd])/sqrt((1/v[md,qd])^2+1)  );//Measures of modality performance
    }}

  for(md in 1 : M) {
   A[md] = 0;
    for(qd in 1 : Q) {
     A[md] =  A[md] +  AA[md,qd];
    }
   A[md]=   A[md]/Q;//fix 2020 Jan
    }


  for(md in 1 : M) {
    for(qd in 1 : Q) {
      deno[C-1,md,qd]=1-ppp[C,md,qd];
      for(cd in 3:C){  deno[c[cd],md,qd]=deno[c[cd-1],md,qd]-ppp[c[cd-1],md,qd];  }
    }}


  for(md in 1 : M) {
    for(qd in 1 : Q) {
      for(cd in 1:C-1){
        hit_rate[cd,md,qd]=ppp[cd,md,qd]/deno[cd,md,qd];
      }
      hit_rate[C,md,qd]=ppp[C,md,qd];

    }}



}






model{
    int s=0;


    // for(qd in 1 : Q) {
    //   for(md in 1 : M) {
    //     target += normal_lpdf( AA[md,qd]|A[md],hyper_v[qd]);
    //   }  }
    for(n in 1:N) {
      target +=   poisson_lpmf(ff[n]|l[c[n]]*NX);
    }

    for(qd in 1 : Q) {
      for(md in 1 : M) {
   target +=  multinomial_lpmf(hitExtended[,md,qd] | p_rev_Extented[md,qd] );//target forumulation is required
}}


    // for(qd in 1 : Q) {
    //   for(md in 1 : M) {
    //     s=0;
    //     for(cd in 1 : C){
    //        target += binomial_lpmf(harray[cd,md,qd]  |  NL-s, hit_rate[c[cd],md,qd]  );
    //       s = s + harray[cd,md,qd]; }
    //     }}






if(prior ==-1){

      w ~  uniform(-3,3);
      for(cd in 1:C-1) dz[cd] ~  uniform(0.001,7);
      for(md in 1 : M) { for(qd in 1 : Q) {
        mu[md,qd] ~ uniform(-11,11);
        v[md,qd] ~ uniform(0.01,11);

      }}

      }

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




if(prior ==0){

      w ~  normal(ww,www);
      for(cd in 1:C-1) dz[cd] ~  exponential(zzz);
      for(md in 1 : M) { for(qd in 1 : Q) {
        mu[md,qd] ~ normal(mm,mmm);
        v[md,qd] ~ exponential(vvv);

      }}

      }


  }














generated quantities{

// real <lower=0>A;  // Observer Recognition performance ability which is a postive real number between 0 and 1. For further informations, please give me a lemonade.
int hits_post_ext[C+1,M,Q]; // For fucking calculation of p values
int hits_post[C,M,Q];    // For fucking calculation of p values
int false_alarms_post[C,M,Q];// For fucking calculation of p values
real ss[C,M,Q];// For fucking calculation of p values
real tt[C,M,Q];// For fucking calculation of p values
real chi_square_with_posterior_data[M,Q];// For fucking calculation of p values
real xx[C,M,Q];// For fucking calculation of p values
real yy[C,M,Q];// For fucking calculation of p values
real chi_square_with_observed_data[M,Q];// For fucking calculation of p values
int p_value_logicals[M,Q];// For fucking calculation of p values
// int dummy;
// real hits_post_real;
// real false_alarms_post_real;
//
real TPF_post_pred[C,M,Q];
real FPF_post_pred[C,M,Q];

int modalityID[C,M,Q];
int readerID[C,M,Q];
// A=Phi(  a/sqrt(b^2+1)  );//Measures of modality performance


// Today 2020 Oct 19, the cute author noticed that I should calculate the PPP using here!
// Then calculation time will be much smaller! What a cute! Holy moly!
// Without any tips, lemonade, coffee and money! the author fights to calculate ppp more faster! Hannnhhhh
// Today, the author is not such a couch potato! But a sweet potato!
// I am a great! Great! GGGGRRREEEEAT!!!! Bear! without money! I will do! Even if I am such a couch potato, I will show power of potato!


// hits_post = binomial_rng(1,hit_rate);
   for(qd in 1 : Q) {
      for(md in 1 : M) {
hits_post_ext[,md,qd] = multinomial_rng(p_rev_Extented[md,qd],NL);
}}
// hits_post = hits_post_ext[1:C];
   for(qd in 1 : Q) {
      for(md in 1 : M) {
for(cd in 1:C) hits_post[cd,md,qd] =   hits_post_ext[cd,md,qd];
}}
// dummy = hits_post_ext[1];
// for(cd in 1:C-1) hits_post[cd] =   hits_post[C+1-cd];
// hits_post[C] = dummy;
for(qd in 1 : Q) {
      for(md in 1 : M) {
 for(cd in 1:C) false_alarms_post[cd,md,qd] = poisson_rng(dl[ccc[cd]]*NX);//Caution, it's not n but c[n], instead!!! 2020 Oct 19
}}

    // for(cd in 1:C)      hits_post_real = hits_post[cd]/NL;
    // for(cd in 1:C)      false_alarms_post_real = false_alarms_post[cd]/NX;
    for(qd in 1 : Q) {
      for(md in 1 : M) {
   for(cd in 1:C)       TPF_post_pred[cd,md,qd] = hits_post[cd,md,qd]/(NL*1.000001) ;
   for(cd in 1:C)       FPF_post_pred[cd,md,qd] = false_alarms_post[cd,md,qd]/(NX*1.000001) ;
      }}

        for(qd in 1 : Q) {
      for(md in 1 : M) {
TPF_post_pred[,md,qd] = cumulative_sum(TPF_post_pred[,md,qd]);
FPF_post_pred[,md,qd] = cumulative_sum(FPF_post_pred[,md,qd]);
}}



   for(qd in 1 : Q) {
      for(md in 1 : M) {
        for(cd in 1:C) {
          modalityID[cd,md,qd] = md;
          readerID[cd,md,qd] = qd;
        }

      }}


for(qd in 1 : Q) {
for(md in 1 : M) {
for(cd in 1:C){
    ss[cd,md,qd]=(hits_post_ext[C+1-cd,md,qd]-NL*p_rev_Extented[md,qd][cd])^2/(NL*p_rev_Extented[md,qd][cd]);
    tt[cd,md,qd]=(false_alarms_post[C+1-cd,md,qd]- dl[cd]*NX  )^2/(dl[cd]*NX);
  }}}

for(qd in 1 : Q) {
for(md in 1 : M) {
chi_square_with_posterior_data[md,qd] = sum(ss[,md,qd]) + sum(tt[,md,qd]);
}}


for(qd in 1 : Q) {
for(md in 1 : M) {
for(cd in 1:C){
    xx[cd,md,qd]=(harray[C+1-cd,md,qd]-NL*p_rev_Extented[md,qd][cd])^2/(NL*p_rev_Extented[md,qd][cd]);
    yy[cd,md,qd]=(farray[C+1-cd,md,qd]- dl[cd]*NX  )^2/(dl[cd]*NX);
  }}}


for(qd in 1 : Q) {
for(md in 1 : M) {
chi_square_with_observed_data[md,qd] = sum(xx[,md,qd]) + sum(yy[,md,qd]);
}}




for(qd in 1 : Q) {
for(md in 1 : M) {
p_value_logicals[md,qd] = (chi_square_with_posterior_data[md,qd] > chi_square_with_observed_data[md,qd]);
}}
// The posterior mean of this fucking sucks p_value_logicals can be interpreted as a chis square goodness of fit
// If this guy is less than, e.g., 0.05 then we reject the null hypothesis that the model is well fitted to a fucking dataset.
//  Of course, in certain condition, frequentist p value  exactly coincides to the posterior prob of event that the .... so,,, I hate stats. Good luck fucking my life sucks.

}














// e <-extract_EAP_by_array(f,"p_rev_Extented")
// apply(e, c(1,2), sum)
// Model_MRMC_Multinomial.stan #2020 Nov 23

