
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
  int ModifiedPoisson;//Logical R object are passed to this variable

}

transformed data{
   int hitExtended[C+1];
                      int  NX;
                    if(ModifiedPoisson==0) NX = NI;
                    if(ModifiedPoisson==1) NX =NL;

                    for (cd in 1:C)   hitExtended[cd] = h[cd];

                    hitExtended[C+1]=NL-sum(h);

print("NL - sum(h) = ",NL-sum(h));

for(cd in 1:(C+1)) print("hitExtended[",cd,"] = ",hitExtended[cd]);



print("* This model is described with a multinomial distribution, which is the Chakraborty's FROC model.")

}

parameters{
  real w;
  real <lower =0>dz[C-1];
  real m; //Not mu !! I regret that this should be mu. I regret !!
  real <lower=0>v;
}


transformed parameters {

  real <lower=0,upper=1>p[C];
  real <lower=0>l[C];//2020Jun27
  real <lower=0>dl[C];
  // real <upper=100000>z[C];
  real  z[C];//2020Jun27
   simplex[C+1] p_rev_Extented;

real a;
real b;

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

}



generated quantities{
real <lower=0>A;
A=Phi(  a/sqrt(b^2+1)  );//Measures of modality performance

}

// fit_srsc(d,multinomial = T)

