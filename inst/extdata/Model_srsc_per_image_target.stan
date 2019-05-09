data{
  int <lower=0>N;
  int <lower=0>NL;
  int <lower=0>NI;//In my original model, NI is not used.
  int <lower=0>C;
  int <lower=0>h[N];
  int <lower=0>f[N];
  real <lower=0>hh[N];
  real <lower=0>ff[N];
  int <lower=0>fff[N];//This is used for poisson assumption.
  int <lower=0>c[N];

}

parameters{
  real w;
  real <lower =0>dz[C-1];
  real m; //Not mu !! I regret that this should be mu. I regret !!
  real <lower=0>v;



}


transformed parameters {

  real <lower=0,upper=1>p[C];
  real <lower=0>l[C];
  real <lower=0>dl[C];
  // real <upper=100000>z[C];
  real  z[C];


real a;
real b;

a=m/v;
b=1/v;

     for(cd in 1 : C-1) {   z[1]=w;
                      z[cd+1] =z[cd] +dz[cd];
                      }


     for(cd in 1 : C) {   if (cd==C) {p[cd]= 1 - Phi((z[cd] -m)/v);
     }else{
                           p[cd] =Phi((z[cd+1] -m)/v)- Phi((z[cd] -m)/v);

     }
     }


     for(cd in 1 : C) {l[cd] = (-1)*log(Phi(z[cd]));     }
     for(cd in 1:C){
                 if (cd==C) {dl[cd]=fabs(l[cd]-0);
                 }else{

       dl[cd]=fabs(l[cd]-l[cd+1]);
          }
          }
}



model{
for(n in 1:N) {
// h[n]   ~ binomial(NL, p[c[n]]);

target += binomial_lpmf(h[n]  |  NL, p[c[n]]   );

// fff[n] ~ poisson( l[c[n]]*NL);#Non-Chakraborty's model

// f[n] ~ poisson(dl[c[n]]*NI); //Chakraborty's model
target +=   poisson_lpmf(f[n]|dl[c[n]]*NI);//Chakraborty's model

}

}



generated quantities{
real <lower=0>A;
A=Phi(  a/sqrt(b^2+1)  );//Measures of modality performance

}


