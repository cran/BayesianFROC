data{ // SBC

  int <lower=0>NL; //Number of Binomial trials
  int <lower=0>NI; //No. of Images
  int <lower=0>N;
  int <lower=0>M;
  int <lower=0>C;
  int <lower=0>Q;


//Prior which shold be specified
real www;
real mmm;
real vvv;
real zzz;
real zz;
real ww;
real vv;
real mm;


real A_mean;
real A_variance;
real vv_hyper_v;
real vvv_hyper_v;

}


transformed data {
 int h[C,M,Q];
 int f[C,M,Q];


  real                      w_;
  real <lower =0  >        dz_[C-1];
  real                     mu_[M,Q];//Not m since m is used for modalit ID.
  real <lower=0>            v_[M,Q];//Standard Deviation, not variance
  real <lower=0>      hyper_v_[Q];
  real <lower=0,upper=1>    A_[M];



  real <lower =0>           dl_[C];
  real <lower=0,upper=1>   ppp_[C,M,Q];
  real <lower =0>            l_[C];
  real                       z_[C];
  real                      aa_[M,Q];
  real <lower =0>           bb_[M,Q];
  real <lower=0,upper=1>    AA_[M,Q];








                           w_ = normal_rng (ww,www);
    for(cd in 1:C-1)  dz_[cd] = normal_rng (zz,zzz);
        for(md in 1 : M) {for(qd in 1 : Q) {mu_[md,qd]  = normal_rng (mm+md/2.1,mmm);}}
        for(md in 1 : M) {for(qd in 1 : Q) { v_[md,qd]  = normal_rng (vv,vvv);}}
       for(qd in 1 : Q) { hyper_v_[qd]  = normal_rng (vv_hyper_v, vvv_hyper_v);
        }
     for(md in 1 : M) { A_[md]  = normal_rng (A_mean,A_variance);
        }




for(md in 1 : M) {
for(qd in 1 : Q) {
  aa_[md,qd]=mu_[md,qd]/v_[md,qd];
  bb_[md,qd]=1/v_[md,qd];
for(cd in 1 : C-1) {
                z_[1]=w_;
             z_[cd+1] = z_[cd] +      dz_[cd];
}



  for(cd in 1 : C) {
    if (cd==C){
             ppp_[cd,md,qd] = 1- Phi((z_[cd ] -mu_[md,qd])/v_[md,qd]);
  }else{
             ppp_[cd,md,qd] = Phi((z_[cd+1] -mu_[md,qd])/v_[md,qd])
                           - Phi((z_[cd ] -mu_[md,qd])/v_[md,qd]);
     }
     }











        for(cd in 1 : C) {
       l_[cd] = (-1)*log(Phi(z_[cd]));
        }






             for(cd in 1:C){
                 if (cd==C) {dl_[cd]=fabs(l_[cd]-0);
                 }else{  dl_[cd]=fabs(l_[cd]-l_[cd+1]);
                      }
             }



    }//for sentence w.r.t. qd
}

   for(md in 1 : M) {
     for(qd in 1 : Q) {
        AA_[md,qd]=Phi(  (mu_[md,qd]/v_[md,qd])/sqrt((1/v_[md,qd])^2+1)  );//Measures of modality performance
            }}


for(cd in 1 : C-1){
  for(md in 1 : M) {
    for(qd in 1 : Q) {
            h[cd,md,qd] = binomial_rng(NL, ppp_[cd,md,qd]);
            f[cd,md,qd] = poisson_rng (dl_[cd]*NI);//Chakraborty's model //<-------very very very coution, not n but c[n] 2019 Jun 21
                       }}}
}

parameters{
  real    w;
  real <lower =0  >  dz[C-1];
  real               mu[M,Q];//Not m since m is used for modalit ID.
  real <lower=0>      v[M,Q];//Standard Deviation, not variance
  real <lower=0>      hyper_v[Q];
  real <lower=0,upper=1>A[M];
}


transformed parameters {
  real <lower =0>       dl[C];
  real <lower=0,upper=1> ppp[C,M,Q];
  real <lower =0>      l[C];
  real    z[C];
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



  for(cd in 1 : C) {
              if (cd==C){
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

    for(qd in 1 : Q){ for(md in 1 : M) {
     target += normal_lpdf( AA[md,qd]|A[md],hyper_v[qd]);
    }}


// for(cd in 1 : C-1){ for(md in 1 : M) { for(qd in 1 : Q){
//          target +=  poisson_lpmf(f[cd,md,qd]  | l[cd*NL]);
//          target += binomial_lpmf(h[cd,md,qd]  |  NL, ppp[cd,md,qd ]  );
//             }}}


// for(cd in 1 : C-1){ for(md in 1 : M) { for(qd in 1 : Q){
//             h[cd,md,qd] = binomial_rng(NL, ppp_[cd,md,qd ]);
//             f[cd,md,qd]  = poisson_rng (dl_[cd]*NI);
//                        }}}


                           w ~ normal(ww,www);
      for(cd in 1:C-1)dz[cd] ~ normal(zz,zzz);
      for(md in 1 : M) {for(qd in 1 : Q) {mu[md,qd]  ~ normal(mm+md/2.1,mmm);
         }}
      for(md in 1 : M) {for(qd in 1 : Q) { v[md,qd]  ~ normal (vv,vvv);
        }}


       for(qd in 1 : Q) { hyper_v[qd]  ~ normal (vv_hyper_v, vvv_hyper_v);
        }
     for(md in 1 : M) { A[md]  ~ normal (A_mean,A_variance);
        }


   // w ~ normal(0,1000);#2019.6.3
   // mu ~ normal(0,1000);#2019.6.3
   // dz ~ chi_square(4);#2019.6.3
   //
   // v ~ chi_square(4);#2019.6.3
   // hyper_v ~ chi_square(4);#2019.6.3



   }


generated quantities { // these adhere to the conventions above

int h_[C,M,Q];
int f_[C,M,Q];

// Caution: ranks_ and pars_ should be changed simultaniously, 2019 July 20

vector [1 + C - 1] pars_;
int ranks_[1 + C - 1];



ranks_[1] = w > w_;
// ranks_[2] = m > m_;
// ranks_[3] = v > v_;
for (cd in 1:(C - 1)) ranks_[cd+1] = dz[cd] > dz_[cd];



pars_[1] = w_;
// pars_[2] = m_;
// pars_[3] = v_;
for (cd in 1:(C - 1)) pars_[cd+1] = dz_[cd];



h_ = h;
f_ = f;

}







