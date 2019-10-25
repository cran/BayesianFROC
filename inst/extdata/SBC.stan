data{
 int prior;

//This is not prior parameter, but some data to run
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


real XXX;
real YYY;
real epsilon;
// real sss;




}


transformed data {
 int h[C];
 int f[C];

  real w_ ;
  real <lower=0>dz_[C-1] ;
  real m_;
  real <lower =0> v_;

  real <lower=0,upper=1>p_[C];
  real <lower=0>l_[C];
  real  <lower=0>dl_[C];
  real  z_[C];

real a_;
real <lower=0>b_;

real s;//prior==-2
// real vec[2];//prior==-2
//  vec[1]=0;//prior==-2


real ssss;
int dummy;
 real deno_[C-1];
  real denoo_[C];

print("--------------------- Print a replicated model parameter form a fixed prior and generated data -------")

print("--------------------- Start -------")

for(n in 1:N){
  print("c[",n,"] = ", c[n]);
}
//
//  if(prior==-4){
// m_ = normal_rng (mm,mmm);
// v_ = uniform_rng (vv,vvv);
// w_ =  uniform_rng (v_*m_+1/(C*1.000001),www);
//      for(cd in 1:C -1) {
//  dz_[cd]= normal_rng (v_*m_+v_/C,zzz);
//     }
//  }


//  if(prior==-3){
// m_ = normal_rng (mm,mmm);
// v_ = chi_square_rng (0.5);
//
//      w_ =  normal_rng (      fmax( 0,  v_*m_+1/(C*1.000001)  )  ,www);
//      for(cd in 1:C -1) {
//  dz_[cd]= exponential_rng (        fmax(  1.5,   v_*m_+1/(C*1.000001))    );
//     }
//  }


//  if(prior==-5){
// m_ = normal_rng (mm,mmm);
// v_ = exponential_rng (0.5);
//
//      w_ =  normal_rng (      fmax( 0,  v_*m_+1/(C*1.000001)  )  ,www);
//      for(cd in 1:C -1) {
//  dz_[cd]= exponential_rng (        fmax(  1.5,   v_*m_+1/(C*1.000001))    );
//     }




//
// if(prior==-2){
//      w_ =  normal_rng (ww,www);
//       s =  w_;
//     for(cd in 1:C-1) {
//
//         dz_[cd] = uniform_rng(fmax(inv_Phi(Phi(s)*XXX)-s,0),fmax(inv_Phi(Phi(s)*XXX)-s,0)+2);
//         s=s+dz_[cd];
//     }
//                           m_ = normal_rng (mm,mmm);
//                           v_ = chi_square_rng (0.5);
// }
//
//
// if(prior==-1){
//                           w_ =  normal_rng (ww,www);
//     for(cd in 1:C-1) dz_[cd] = uniform_rng (0.001,zzz);
//                           m_ = normal_rng (mm,mmm);
//                           v_ = uniform_rng (0,vvv);
//
//
// }
//
//
//
// if(prior==0){
//                           w_ =  normal_rng (ww,www);
//     for(cd in 1:C-1) dz_[cd] = normal_rng (zz,zzz);
//                           m_ = normal_rng (mm,mmm);
//                           v_ = normal_rng (vv,vvv);
//
//
// }
//
//
//
//
// prior = 1 ----

if(prior==1){
                                    w_ =  normal_rng (ww,www);
                     print("w_ = ", w_);

              for(cd in 1:C-1){
                // dz_[cd] = chi_square_rng (1);
                 // dz_[cd] =exponential_rng (1);

               // dz_[cd] =beta_rng (2,2);
               // dz_[cd] =chi_square_rng (0.5);
               dz_[cd] =uniform_rng(0.5,2);


        print("dz_[",cd,"] = ", dz_[cd]);

                              }

                                    m_ = normal_rng (mm,mmm);
                     print("m_ = ", m_);

                                    // v_ = chi_square_rng (0.5);
                                       v_ =  uniform_rng(5,10); //v_ should be large to get nonzero hitrate. since rate is over v_, v_ cannot so small leads to large values in Phi and leads to rate=1.
                     print("v_ = ", v_);


}//if(prior==1){

// //
//
//
//
// if(prior==2){
//                           w_ =  normal_rng (ww,www);
//     for(cd in 1:C-1) dz_[cd] = chi_square_rng (3);
//                           m_ = normal_rng (mm,mmm);
//                           v_ = chi_square_rng (3);
//
//
// }
//
//
//
//
// if(prior==3){
//                           w_ =  normal_rng (ww,www);
//     for(cd in 1:C-1) dz_[cd] = exponential_rng (3);
//                           m_ = normal_rng (mm,mmm);
//                           v_ = exponential_rng (3);
//
//
// }
//
// if(prior==4){
//                           w_ =  normal_rng (ww,www);
//     for(cd in 1:C-1) dz_[cd] = exponential_rng (4);
//                           m_ = normal_rng (mm,mmm);
//                           v_ = exponential_rng (4);
//
//
// }
//
//
// if(prior==5){
//                           w_ =  normal_rng (ww,www);
//                           m_ = normal_rng (mm,mmm);
//
//     for(cd in 1:C-1) dz_[cd] = gamma_rng (10,0.1);
//                           v_ = gamma_rng (10,0.1);
//
//
// }
//
// inv_phi
// inv_Phi
 if(prior==-6){
                  print("-----------------------------   a parameter of model  -------")

                  m_ = normal_rng (mm,mmm);
                  print("m_ = ", m_);
                  v_ = uniform_rng (0 ,5);
                  print("v_ = ", v_);

                  w_ =  normal_rng (    v_*m_+1/(C*1.000001)    ,www);
                  print("w_ = ", w_);


                  ssss = w_;
                  // print("ssss = ", ssss);

                  z_[1] =w_;
                  // print("z_[1] = ", z_[1]);
                  // print("epsilon/NI = ", epsilon/NI);
                  // print("exp(epsilon/NI) = ", exp(epsilon/NI));
                  // print("Phi(w_) = ", Phi(w_));
                  // print("exp(normal_lpdf(w_|0,1))) = ", exp(normal_lpdf(w_|0,1)));


                     dz_[1] = uniform_rng(  exp(epsilon/NI)*Phi(w_)/(  Phi(w_) + exp(normal_lpdf(w_|0,1))),
                                            exp(epsilon/NI)*Phi(w_)/(  Phi(w_) + exp(normal_lpdf(w_|0,1)))+0.1
                                            );
                  print("dz_[1] = ", dz_[1]);

                             ssss = ssss+dz_[1];
                          for(cd in 2:C -1) {
                    dz_[cd] = uniform_rng(  ( exp(epsilon/NI)*Phi(ssss)  -   Phi(ssss)  )  /    exp(normal_lpdf(ssss|0,1)),
                                       fmin( ( exp(epsilon/NI)*Phi(ssss)  -   Phi(ssss)  )  /    exp(normal_lpdf(ssss|0,1))+1,
                                              dz_[cd-1]/ exp(normal_lpdf(ssss|0,1))    )  );
                      // dz_[cd] = exponential_rng(  ( exp(epsilon/NI)*Phi(ssss)  -   Phi(ssss)  )  /    exp(normal_lpdf(ssss|0,1))
                      //                          );

                     // dz_[cd] = chi_square_rng(  ( exp(epsilon/NI)*Phi(ssss)  -   Phi(ssss)  )  /    exp(normal_lpdf(ssss|0,1))
                     //                           );

                  print("dz_[",cd,"] = ", dz_[cd]);
                  print("lower of dz[",cd,"] = ", ( exp(epsilon/NI)*Phi(ssss)  -   Phi(ssss)  )  /    exp(normal_lpdf(ssss|0,1)) );
                  print("upper of dz[",cd,"] = ",   dz_[cd-1]/ exp(normal_lpdf(ssss|0,1))     );


                             ssss = ssss+dz_[cd];
                           z_[cd] = ssss;
                  print("z_[",cd,"] = ", z_[cd]);

                      }
                      z_[C] = ssss + dz_[C-1];
                  print("z_[",C,"] = ", z_[C]);

                   }

ssss = w_;
z_[1] =w_;
print("dz_[1] = ", dz_[1]);
         ssss = ssss+dz_[1];
      for(cd in 2:C -1) {
print("dz_[",cd,"] = ", dz_[cd]);
         ssss = ssss+dz_[cd];
       z_[cd] = ssss;
print("z_[",cd,"] = ", z_[cd]);
  }
  z_[C] = ssss + dz_[C-1];
print("z_[",C,"] = ", z_[C]);
a_=m_/v_;
b_=1/v_;

     // for(cd in 1 : C-1) {   z_[1]=w_;
     //                  z_[cd+1] =z_[cd] +dz_[cd];
     //                  }

     for(cd in 1 : C) {
                           print("Phi((z_[",cd,"] -m_)/v_) = ", Phi( (z_[cd] -m_)/v_));
                       }
print("-----------------------------   p_[1] < p_[2] < p_[3] < p_[4]   -------")




     for(cd in 1 : C-1) {
                           p_[cd] = Phi((z_[cd+1] - m_)/v_)- Phi( (z_[cd] -m_)/v_);
                           print("p_[",cd,"] = ", p_[cd]);
                       }
 p_[C] = 1 - Phi((z_[C] - m_)/v_);
                           print("p_[",C,"] = ", p_[C]);

     for(cd in 1 : C) {l_[cd] = (-1)*log(Phi(z_[cd]));
            print("l_[",cd,"] = ", l_[cd]);


     }

     print("-------------------------------   dl_[1] > dl_[2] > dl_[3] > dl_[4]   -------")


     for(cd in 1:C){
                 if (cd==C) {dl_[cd]=fabs(l_[cd]-0);
                 print("dl_[",cd,"] = ", dl_[cd]);

                 }else{

       dl_[cd]=fabs(l_[cd]-l_[cd+1]);
      print("dl_[",cd,"] = ", dl_[cd]);

          }
          }



     // deno[C]=1;
     deno_[C-1]=1-p_[C];
      for(cd in 3:C){  deno_[c[cd]]=deno_[c[cd-1]]-p_[c[cd-1]];  }

  // int s=0;

  for(cd in 1:C-1){
    denoo_[cd]=deno_[cd];
 print("denoo_[",cd,"] = ", denoo_[cd]);

  }
  denoo_[C]=1;
 print("denoo_[",1,"] = ", denoo_[1]);







// }









dummy=0;
           for(n in 1:N) {
    h[n] = binomial_rng(NL-dummy, p_[c[n]]/denoo_[c[n]]);
    dummy = dummy + h[n];
// print("h[",n,"] = ", h[n]);

 // fff[n] ~ poisson( l[c[n]]*NL);//Non-Chakraborty's model
    f[n] = poisson_rng (dl_[c[n]]*NI);//Chakraborty's model //<-------very very very coution, not n but c[n] 2019 Jun 21
 // fff[n] ~ poisson( l[c[n]]*NI);//Non-Chakraborty's model
// print("f[",n,"] = ", f[n]);
                       }

print("----------------------------------   f[1] < f[2] < f[3] < f[4]   -------")

// for(n in 1:N){
//   print("f[",n,"] = ", f[n]);
// }

print("----------------------------------   h[1] > h[2] > h[3] > h[4]   -------")

// for(n in 1:N){
//   print("h[",n,"] = ", h[n]);
// }

print("---------------------------------- Data -------")

for(n in 1:N){
  print("h[",n,"] = ", h[n] , "      f[",n,"] = ", f[n]);
}

print("---------------------------------- Check -------")
print(  "sum(h)  = ",  sum(h)  )
print(  "NL    = ",  NL  )
print(  "sum(h)/NL    = ",  sum(h)/(NL*1.0000001)  )
print(  "inv_Phi(sum(h)/NL)   = ",  inv_Phi(sum(h)/(NL*1.0000001))  )
print(  "inv_Phi(sum(h)/NL)    = ",inv_Phi(sum(h)/(NL*1.0000001))  )
print(  "((z_[C]-m_)/v_)/inv_Phi(sum(h)/NL)   = ",((z_[C]-m_)/v_)/inv_Phi(sum(h)/(NL*1.0000001)))


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

real deno[C-1];

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




               // deno[C]=1;
     deno[C-1]=1-p[C];
      for(cd in 3:C){  deno[c[cd]]=deno[c[cd-1]]-p[c[cd-1]];  }


}


model{

real sss;

int dummydomma=0;

  real denoo[C];

  for(cd in 1:C-1){
    denoo[cd]=deno[cd];
  }
  denoo[C]=1;



//   for (cd in 1:C) {
//  dl[cd]*NI ~ uniform(0.001,10);
// }




//   for (cd in 1:C-1) {
//   Phi(z[cd+1])-Phi(z[cd])*exp(0.001/(NI*1.0001)) ~exponential(3);
// }
//    1-Phi(z[C])*exp(0.001/(NI*1.0001)) ~exponential(3);



       for(n in 1:N) {
                         h[n]   ~ binomial(NL-dummydomma, p[c[n]]/denoo[c[n]]);
                         dummydomma=dummydomma+h[n];
 // fff[n] ~ poisson( l[c[n]]*NL);//Non-Chakraborty's model
                         f[n] ~ poisson(dl[c[n]]*NI);//Chakraborty's model //<-------very very very coution, not n but c[n] 2019 Jun 21
 // fff[n] ~ poisson( l[c[n]]*NI);//Non-Chakraborty's model

                       }

 //       for(n in 1:N) {
 //                         h[n]   ~ binomial(NL, p[n]);
 // // fff[n] ~ poisson( l[c[n]]*NL);//Non-Chakraborty's model
 //                         f[n] ~ poisson(dl[n]*NI);//Chakraborty's model //<-------very very very coution, not n but c[n] 2019 Jun 21
 // // fff[n] ~ poisson( l[c[n]]*NI);//Non-Chakraborty's model
 //
 //                       }
// if(prior==-4){
// m  ~ normal(mm,mmm);
// v  ~ uniform(0,vvv);
//      w  ~  normal (v *m ,www);
//     for(cd in 1:C -1) {
//  dz[cd]~ normal (v *m +v /C,zzz);
//     }
//  }

//  if(prior==-3){
// m  ~ normal(mm,mmm);
// v  ~ chi_square (0.5);
//      w  ~  normal (     fmax( 0,   v *m+1 /(C*1.000001) )            ,www);
//     for(cd in 1:C -1) {
//  dz[cd]~ exponential (   fmax( 1.5, v *m +1 /(C*1.000001)));
//     }
//  }





 if(prior==-6){
                    m ~ normal (mm,mmm);
                    print("m = ", m);
                    v ~ uniform (0.1 ,5);
                    print("v = ", v);
                    w ~  normal (    v*m+1/(C*1.000001)    ,www);
                    print("w = ", w);


                    sss = w;
                    print("sss = ", sss);
                    print("epsilon/NI = ", epsilon/NI);
                    print("exp(epsilon/NI) = ", exp(epsilon/NI));
                    print("Phi(w) = ", Phi(w));
                    print("exp(normal_lpdf(w|0,1))) = ", exp(normal_lpdf(w|0,1)));


                       dz[1] ~ uniform(  exp(epsilon/NI)*Phi(w)/(  Phi(w) + exp(normal_lpdf(w|0,1))),
                                              exp(epsilon/NI)*Phi(w)/(  Phi(w) + exp(normal_lpdf(w|0,1)))+0.1
                                              );
                    print("dz[1] = ", dz[1]);
                    print("lower of dz[1] = ", exp(epsilon/NI)*Phi(w)/(  Phi(w) + exp(normal_lpdf(w|0,1)))   );
                               sss = sss+dz[1];
                    for(cd in 2:C -1) {
                      dz[cd] ~ uniform(    ( exp(epsilon/NI)*Phi(sss)  -   Phi(sss)  )  /    exp(normal_lpdf(sss|0,1)),
                                   fmin(   ( exp(epsilon/NI)*Phi(sss)  -   Phi(sss)  )  /    exp(normal_lpdf(sss|0,1)) +1,   dz_[cd-1]/ exp(normal_lpdf(sss|0,1))    )  );
                    print("dz[",cd,"] = ", dz[cd]);
                    print("lower of dz[",cd,"] = ",   ( exp(epsilon/NI)*Phi(sss)  -   Phi(sss)  )  /    exp(normal_lpdf(sss|0,1))    );
                    print("upper of dz[",cd,"] = ",   dz_[cd-1]/ exp(normal_lpdf(sss|0,1))     );


                               sss = sss+dz[cd];

                        }

}// if(prior==-6){




 // }




//  if(prior==-5){
// m  ~ normal(mm,mmm);
// v  ~ exponential (0.5);
//      w  ~  normal (     fmax( 0,   v *m+1 /(C*1.000001) )            ,www);
//     for(cd in 1:C -1) {
//  dz[cd]~ exponential (   fmax( 1.5, v *m +1 /(C*1.000001)));
//     }
//  }





   // priors
// if(prior==0){
//
//                           w ~  normal(ww,www);
//     for(cd in 1:C-1) dz[cd] ~  normal(zz,zzz);
//                           m ~ normal(mm,mmm);
//                           v ~ normal(vv,vvv);
// }//prior
//
//
//
// prior = 1 ----
if(prior==1){
                            w ~  normal(ww,www);
    // for(cd in 1:C-1) dz[cd] ~ chi_square(0.5);
    // for(cd in 1:C-1) dz[cd] ~ exponential(1);
         for(cd in 1:C-1)  dz_[cd] ~ uniform (0.5,2);
                             m ~ normal(mm,mmm);
                            v ~ uniform(5,10);
}

//
// if(prior==2){
//                           w ~  normal(ww,www);
//     for(cd in 1:C-1) dz[cd] ~ chi_square(3);
//                           m ~ normal(mm,mmm);
//                           v ~ chi_square(3);
// }
//
// if(prior==3){
//                           w ~  normal(ww,www);
//     for(cd in 1:C-1) dz[cd] ~ exponential(3);
//                           m ~ normal(mm,mmm);
//                           v ~ exponential(3);
// }
//
//
//
// if(prior==4){
//                           w ~  normal(ww,www);
//     for(cd in 1:C-1) dz[cd] ~ exponential(4);
//                           m ~ normal(mm,mmm);
//                           v ~ exponential(4);
// }
//
// if(prior==5){
//                           w ~  normal (ww,www);
//                           m~ normal (mm,mmm);
//
//     for(cd in 1:C-1) dz[cd]~ gamma (10,0.1);
//                           v~ gamma (50,0.1);
//
//
// }



// y ~ gamma(alpha, beta)=gamma(k, lambda)
//  variance = k/lambda^2
//  mean = k/lambda










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













