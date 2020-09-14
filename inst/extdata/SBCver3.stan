// 2020 July 22 this file was made to test the priors
// inv_phi
// inv_Phi
// Phi

// functions {
//   real relative_diff(real x, real y) {
//     real abs_diff;
//     real avg_scale;
//     abs_diff = fabs(x - y);
//     avg_scale = (fabs(x) + fabs(y)) / 2;
//     return abs_diff / avg_scale;
//   }
// }
functions {

    real psi(real x, real mu, real sigma) {
    return Phi((x-mu)/sigma);
  }



  real inv_psi(real x, real mu, real sigma) {
    return mu + sigma * inv_Phi(x);
  }




}

data{

//This is not prior parameter, but some data to run
  int <lower=0>N; //This is exactly same as C
  int <lower=0>NL; //Number of Binomial trials
  int <lower=0>NI; //This is redandunt
  int <lower=0>C; // Number of Confidence level
  int <lower=0>c[N]; //Each component means confidence level



real epsilon;

real BBB;
real AAA;

real mmm;
real mmmm;
real vvv;
real vvvv;
}


transformed data {
 int h[C];
 int f[C];

  // real                   w_ ;
  real                    m_;
  real <lower =0>         v_;

  real <lower=0,upper=1>p_[C];
  real <lower=0>        l_[C];
  real  <lower=0>      dl_[C];
  real                  z_[C];

  real          a_;
  real <lower=0>b_;



  int dummy;
  real deno_[C-1];
  real denoo_[C];


  real lower_p[C];
  real upper_p[C];
  real lower_q[C];
  real upper_q[C];
  real lower_z[C];
  real upper_z[C];
  real hitRate_[C];
  real falseRate_[C];

// fe,g, he -----
 real fe = epsilon;
 real g = 1-2*epsilon;
  // real g = 1-5*epsilon;
  // real g = 1- exp (-epsilon);
 real he = epsilon^4;
 real ssss;

 // real ffe=1+epsilon^5;
 // real gg=ffe+NI*2;











print("==============================================~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~======== Start  ==========")
print("fe = ", fe, ", g = ", g, ", he = ", he, ", e := epsilon = ", epsilon);


//  v  m    -------------------
m_ = normal_rng (mmm,mmmm);
print("m_ = ", m_, " ~ normal_rng(",mmm,mmmm , ")");
// v_ = uniform_rng (vvv ,vvvv);
// v_ = exponential_rng (vvv)+vvvv;
v_ = chi_square_rng (vvv)+vvvv;
print("v_ = ", v_);


//  here ----------------------------


lower_p[C]  =  inv_psi (   (1-g)  ,m_,v_  ) ;
upper_p[C]  =  inv_psi (   (1-fe) ,m_,v_  ) ;

lower_q[C]  =  lower_p[C];
// upper_q[C]  =  inv_Phi ( exp(-he) ) ;
upper_q[C]  =  upper_p[C] ;

lower_z[C]  =  lower_p[C];
upper_z[C]  = fmin( upper_p[C], upper_q[C] );
//   z_[C]     =  uniform_rng ( lower_z[C] ,  upper_z[C]  );----

 z_[C]     =  uniform_rng ( lower_z[C] ,  upper_z[C]  );
 print(  "z_[",C,"]     =  uniform_rng ( lower_z[",C,"] ,  upper_z[",C,"]  );")
 print(  z_[C], "  =  uniform_rng (  ", lower_z[C] ,", ",  upper_z[C], "  );")

for (cd in 2:C ){

print(" lower_p[",c[cd],"] =  inv_psi (psi(  z_[",c[cd-1],"] ,m_,v_)  - g, m_, v_ ) ;")
print(" lower_p[",c[cd],"] =  inv_psi (psi(",  z_[c[cd-1]] ,",",m_,",",v_,")  - ",g,", m_, v_ ) ;")
print(" lower_p[",c[cd],"] =  inv_psi (",psi(  z_[c[cd-1]]  ,m_, v_),"  - ",g,", m_, v_ ) ;")
print(" lower_p[",c[cd],"] =  inv_psi (", psi(  z_[c[cd-1]] ,m_,v_)  - g,", m_, v_ ) ;")

 lower_p[c[cd]] =  inv_psi ( fabs(psi(  z_[c[cd-1]] ,m_,v_)  - g ), m_, v_ ) ;
 upper_p[c[cd]] =  inv_psi ( fabs(psi(  z_[c[cd-1]] ,m_,v_)  - fe ), m_, v_ ) ;

lower_q[c[cd]]  =  lower_p[c[cd]];
upper_q[c[cd]]  =  inv_Phi ( exp(-he) ) ;
//  def of q ------------
 lower_z[c[cd]] = lower_p[c[cd]];
 // upper_z[c[cd]] =  fmin( upper_p[c[cd]], upper_q[c[cd]] );
 upper_z[c[cd]] =  upper_p[c[cd]];

  z_[c[cd]]  = uniform_rng ( lower_z[c[cd]] ,  upper_z[c[cd]]  );
}
for(cd in 1:C){
print("lower_p[",cd,"] = ", lower_p[cd], ", lower_q[",cd,"] = ", lower_q[cd]);
print("upper_q[",cd,"] = ", upper_q[cd], ", upper_p[",cd,"] = ", upper_p[cd],", Check: ", upper_q[cd] >  lower_p[cd], ", ", upper_p[cd] >  lower_q[cd] );
print(" - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ")
}

for(cd in 1:C){
print("upper_p[",cd,"]  - lower_p[",cd,"] = ", upper_p[cd]  - lower_p[cd]);
print("upper_q[",cd,"]  - lower_q[",cd,"] = ", upper_q[cd]  - lower_q[cd]);
print("upper_z[",cd,"]  - lower_z[",cd,"] = ", upper_z[cd]  - lower_z[cd]);
}



ssss =1;
for (cd in 2:C) ssss = (z_[cd] -z_[cd-1]>0)*ssss;

if(ssss)  print("     z_[1] < z_[2] < z_[3]    z_ monotonicity: TRUE" )
if(!ssss) print("     z_[1] < z_[2] < z_[3]    z_ monotonicity: FALSE" )


a_=m_/v_;
b_=1/v_;


// print("-----------------------------   p_[1] < p_[2] < p_[3] < p_[4]   -------")
for(cd in 1 : C-1) {
                           p_[cd] = Phi((z_[cd+1] - m_)/v_)- Phi( (z_[cd] -m_)/v_);
                       }
 p_[C] = 1 - Phi((z_[C] - m_)/v_);


ssss =1;
for (cd in 2:C) ssss = (p_[cd] -p_[cd-1]>0)*ssss;
if(ssss)  print("     p_[1] < p_[2] < p_[3]    p_ monotonicity: TRUE" )
if(!ssss) print("     p_[1] < p_[2] < p_[3]    p_ monotonicity: FALSE" )
     for(cd in 1 : C) {l_[cd] = (-1)*log(Phi(z_[cd]));
     }

     // print("-------------------------------   dl_[1] > dl_[2] > dl_[3] > dl_[4]   -------")


     for(cd in 1:C){
                 if (cd==C) {dl_[cd]=fabs(l_[cd]-0);
                 }else{
       dl_[cd]=fabs(l_[cd]-l_[cd+1]);
          }
          }



     // deno[C]=1;
     deno_[C-1]=1-p_[C];
      for(cd in 3:C)  deno_[c[cd]]=deno_[c[cd-1]]-p_[c[cd-1]];

  for(cd in 1:C-1){
    denoo_[cd]=deno_[cd];
  }
  denoo_[C]=1;



print("------------------------------------")

dummy=0;
           for(n in 1:N) {
             hitRate_[n] = p_[c[n]]/denoo_[c[n]];
             falseRate_[n] = dl_[c[n]]*NI;
              print("hitRate_[",n,"] = ", hitRate_[n]   );
    h[n] = binomial_rng(NL-dummy,  hitRate_[n]);
    dummy = dummy + h[n];
    f[n] = poisson_rng (falseRate_[n] );//Chakraborty's model //<-------very very very coution, not n but c[n] 2019 Jun 21
 // fff[n] ~ poisson( l[c[n]]*NI);//Non-Chakraborty's model
                       }

for(cd in 1:C) {
print("falseRate_[",cd,"] = ",falseRate_[cd]/NI," > ", he ," is ", falseRate_[cd]/NI > he    );
  }

for(cd in 1:C) {
print("hitRate_[",cd,"]   = ",hitRate_[cd]," > ", fe ," is ", hitRate_[cd] > fe    );
  }


for(cd in 1:C) {
print("hitRate_[",cd,"]   = ",hitRate_[cd]," < ", g ," is ", hitRate_[cd] < g    );
  }

print(" ----------------------------------------------------------")
  ssss =1;
for (cd in 2:C) ssss = (hitRate_[cd] < g )*ssss;
if(ssss)  print("                                    hitRate_ Upper bounds: TRUE" )
if(!ssss) print("                                    hitRate_ Upper bounds: FALSE" )


  ssss =1;
for (cd in 2:C) ssss = (hitRate_[cd] > fe )*ssss;
if(ssss)  print("                                    hitRate_ lower bounds: TRUE" )
if(!ssss) print("                                    hitRate_ lower bounds: FALSE" )
print(" ----------------------------------------------------------")









  ssss =1;
for (cd in 2:C) ssss = (hitRate_[cd] -hitRate_[cd-1]>0)*ssss;
if(ssss)  print("                                    hitRate_ monotonicity: TRUE" )
if(!ssss) print("                                    hitRate_ monotonicity: FALSE" )


    ssss =1;
for (cd in 2:C) ssss = (falseRate_[cd] -falseRate_[cd-1]>0)*ssss;
if(ssss)  print("                                    falseRate_ monotonicity: TRUE" )
if(!ssss) print("                                    falseRate_ monotonicity: FALSE" )




  ssss =1;
for (cd in 2:C) ssss = (h[cd-1] -h[cd]>0)*ssss;
if(ssss)  print("                                    h monotonicity: TRUE" )
if(!ssss) print("                                    h monotonicity: FALSE" )


    ssss =1;
for (cd in 2:C) ssss = (f[cd] -f[cd-1]>0)*ssss;
if(ssss)  print("                                    f  monotonicity: TRUE" )
if(!ssss) print("                                    f  monotonicity: FALSE" )







print("------------------------------------")

for(cd in 1:C) print("z_[",cd,"] = ", z_[cd]," ~ uniform (",    lower_z[cd]  ,",    ",    upper_z[cd],"), length of supp. = ",upper_z[cd] -  lower_z[cd]   );

print("------------- Data -------")

for(n in 1:N){
 if(n==1){ print("h[",n,"] = ", h[n] , "      f[",n,"] = ", f[n] ,"  highest confidence");}
 else if(n==N){ print("h[",n,"] = ", h[n] , "       f[",n,"] = ", f[n] ," lowest confidence");}
 else {  print("h[",n,"] = ", h[n] , "       f[",n,"] = ", f[n] );}

}
print("======================================================================================== Finish ==========")

// print(  "sum(h)  = ",  sum(h)  )
// print(  "NL    = ",  NL  )
// print(  "sum(h)/NL    = ",  sum(h)/(NL*1.0000001)  )
// print(  "inv_Phi(sum(h)/NL)   = ",  inv_Phi(sum(h)/(NL*1.0000001))  )
// print(  "inv_Phi(sum(h)/NL)    = ",inv_Phi(sum(h)/(NL*1.0000001))  )
// print(  "((z_[C]-m_)/v_)/inv_Phi(sum(h)/NL)   = ",((z_[C]-m_)/v_)/inv_Phi(sum(h)/(NL*1.0000001)))



 }

parameters{
          real m;
          real <lower=0>v;
          real  z[C];
}


transformed parameters {

  // real <lower=0,upper=1>p[C];
  // real <lower=0>l[C];
  // real <lower=0>dl[C];

  real  p[C];
  real  l[C];
  real  dl[C];

  real a;
  real b;

real deno[C-1];

a=m/v;
b=1/v;



     for(cd in 1 : C) {
       if (cd==C) {        p[cd] = 1 - Phi((z[cd] -m)/v);
     }else{
                           p[cd] = Phi((z[cd+1] -m)/v)- Phi((z[cd] -m)/v);

     }
     }


     for(cd in 1 : C)     l[cd] = (-1)*log(Phi(z[cd]));
     for(cd in 1:C){
              if (cd==C) {dl[cd] = fabs(l[cd]-0);
                 }else{
                          dl[cd] = fabs(l[cd]-l[cd+1]);
          }
          }




               // deno[C]=1;
     deno[C-1]=1-p[C];
      for(cd in 3:C) deno[c[cd]]=deno[c[cd-1]]-p[c[cd-1]];


}


model{

real sss;

int dummydomma=0;

  real denoo[C];

  for(cd in 1:C-1)  denoo[cd]=deno[cd];

  denoo[C]=1;





       for(n in 1:N) {
                         h[n]   ~ binomial(NL-dummydomma, p[c[n]]/denoo[c[n]]);
                         dummydomma=dummydomma+h[n];
                         f[n] ~ poisson(dl[c[n]]*NI);//Chakraborty's model //<-------very very very coution, not n but c[n] 2019 Jun 21
                       }


//These prior causes error
         // for(cd in 1:C-1)  z_[cd] ~ uniform (0.5,2);
         //                     m ~ normal(mm,mmm);
         //                    v ~ uniform(5,10);

                    print("m = ", m);
                    print("v = ", v);




 }//model

generated quantities { // these adhere to the conventions above
int h_[C];
int f_[C];
vector [2 + C ] pars_;
int ranks_[2 + C ];

ranks_[1] = m > m_;
ranks_[2] = v > v_;
for (cd in 1:C) ranks_[cd+2] = z[cd] > z_[cd];

pars_[1] = m_;
pars_[2] = v_;
for (cd in 1:C) pars_[cd+2] = z_[cd];

h_ = h;
f_ = f;

}













