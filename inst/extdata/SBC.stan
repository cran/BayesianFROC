data{ // SBC

//This is not prior truth data, but somedata to run
  int <lower=0>N; //This is exactly same as C
  int <lower=0>NL; //Number of Binomial trials
  int <lower=0>NI; //This is redandunt
  int <lower=0>C; // Number of Confidence level
  int <lower=0>c[N]; //Each component means confidence level


//Prior which shold be specified
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
 int h[C];
 int f[C];


  real    w_ ;
  real <lower=0>dz_[C-1] ;
  real m_;
  real <lower =0> v_;



  real <lower=0,upper=1>p_[C];
  real <lower=0>l_[C];
  real  <lower=0>dl_[C];
   real  z_[C];


real a_;
real <lower=0>b_;




                          w_ =  normal_rng (ww,www);
    for(cd in 1:C-1) dz_[cd] = normal_rng (zz,zzz);
                          m_ = normal_rng (mm,mmm);
                          v_ = normal_rng (vv,vvv);






a_=m_/v_;
b_=1/v_;

     for(cd in 1 : C-1) {   z_[1]=w_;
                      z_[cd+1] =z_[cd] +dz_[cd];
                      }


     for(cd in 1 : C) {   if (cd==C) {
                           p_[cd] = 1 - Phi((z_[cd] - m_)/v_);
                                      }else{
                           p_[cd] = Phi((z_[cd+1] - m_)/v_)- Phi( (z_[cd] -m_)/v_);

                                           }
                       }


     for(cd in 1 : C) {l_[cd] = (-1)*log(Phi(z_[cd]));     }
     for(cd in 1:C){
                 if (cd==C) {dl_[cd]=fabs(l_[cd]-0);
                 }else{

       dl_[cd]=fabs(l_[cd]-l_[cd+1]);
          }
          }




           for(n in 1:N) {
    h[n] = binomial_rng(NL, p_[c[n]]);
 // fff[n] ~ poisson( l[c[n]]*NL);//Non-Chakraborty's model
    f[n] = poisson_rng (dl_[c[n]]*NI);//Chakraborty's model //<-------very very very coution, not n but c[n] 2019 Jun 21
 // fff[n] ~ poisson( l[c[n]]*NI);//Non-Chakraborty's model

                       }
}

parameters{
  real w;
  real <lower =0>dz[C-1];
  real m;
  real <lower=0>v;



}


transformed parameters {

  real <lower=0,upper=1>p[C];
  real <lower=0>l[C];
  real <lower=0>dl[C];
  real  z[C];


real a;
real b;

a=m/v;
b=1/v;

     for(cd in 1 : C-1) {   z[1] = w;
                         z[cd+1] = z[cd] +dz[cd];
                        }


     for(cd in 1 : C) {
       if (cd==C) {        p[cd] = 1 - Phi((z[cd] -m)/v);
     }else{
                           p[cd] = Phi((z[cd+1] -m)/v)- Phi((z[cd] -m)/v);

     }
     }


     for(cd in 1 : C) {    l[cd] = (-1)*log(Phi(z[cd]));     }
     for(cd in 1:C){
              if (cd==C) {dl[cd] = fabs(l[cd]-0);
                 }else{

                          dl[cd] = fabs(l[cd]-l[cd+1]);
          }
          }
}


model{
       for(n in 1:N) {
                         h[n]   ~ binomial(NL, p[c[n]]);
 // fff[n] ~ poisson( l[c[n]]*NL);//Non-Chakraborty's model
                         f[n] ~ poisson(dl[c[n]]*NI);//Chakraborty's model //<-------very very very coution, not n but c[n] 2019 Jun 21
 // fff[n] ~ poisson( l[c[n]]*NI);//Non-Chakraborty's model

                       }

   // priors

                          w ~  normal(ww,www);
    for(cd in 1:C-1) dz[cd] ~  normal(zz,zzz);
                          m ~ normal(mm,mmm);
                          v ~ normal(vv,vvv);

}


generated quantities { // these adhere to the conventions above
int h_[C];
int f_[C];
vector [3 + C - 1] pars_;
int ranks_[3 + C - 1];



ranks_[1] = w > w_;
ranks_[2] = m > m_;
ranks_[3] = v > v_;
for (cd in 1:(C - 1)) ranks_[cd+3] = dz[cd] > dz_[cd];



pars_[1] = w_;
pars_[2] = m_;
pars_[3] = v_;
for (cd in 1:(C - 1)) pars_[cd+3] = dz_[cd];



h_ = h;
f_ = f;

}

