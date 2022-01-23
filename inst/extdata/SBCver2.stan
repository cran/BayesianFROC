//
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

    real phi_tilde(real x, real mu, real sigma) {
    return Phi((x-mu)/sigma);
  }



  real inv_phi_tilde(real x, real mu, real sigma) {
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

 real ffe=1+epsilon^5;
 real gg=ffe+NI*2;











print("==============================================~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~======== Start  ==========");
print("fe = ", fe, ", g = ", g, ", he = ", he, ", e := epsilon = ", epsilon);

if((1-g)/(1-fe) < 1  )      print("Check the condition that (1-g)/(1-fe) < 1 :       TRUE." );
if( ! ((1-g)/(1-fe) < 1  )) print("Check the condition that (1-g)/(1-fe) < 1 :        FALSE. --------------------" );
                  print("1-g  = ", 1-g );
                  print("1-fe = ", 1-fe);

                   print("ffe = ", ffe );
                  print(" gg  = ",gg );
                  // print(" exp( -ffe )= ", exp( -ffe)  );
                  // print(" exp( -gg) = ", exp( -gg )  );
 print(" log( ffe )= ", log( ffe)  );
 print(" log( gg) = ", log( gg )  );
// for(n in 1:N){
//   print("c[",n,"] = ", c[n]);
// }






//  v ......   m    -------------------
                  // m_ = normal_rng (mmm,mmmm);
                  // print("m_ = ", m_);
                  // v_ = uniform_rng (vvv ,vvvv);
                  // v_ = exponential_rng (vvv)+vvvv;
                  v_ = chi_square_rng (vvv)+vvvv;
                  // v_ = chi_square_rng (vvv)+fabs(m_);
                  // v_ = vvv*fabs(m_);

                  print("v_ = ", v_);





    // lower_p[1] = inv_phi_tilde(1-g,m_,v_) ;
    // upper_p[1] = inv_phi_tilde((1-g)/((1-fe)^1),m_,v_);// upper_p ------
    // lower_q[1] = lower_p[1];
    // upper_q[1] = inv_Phi( exp(-C*epsilon) );
        // upper_q[1] = inv_Phi( fmin(exp(-(C+2)*epsilon),0.999) );
 // upper_q[1] = inv_Phi( exp(-epsilon) );
 // lower_q[1] =   upper_q[1]-2;

  // lower_p[1] =   lower_q[1];
  // upper_p[1] =   upper_q[1];
//
// print("lower_p[",1,"] = ", lower_p[1] );
// print("upper_p[",1,"] = ", upper_p[1] );
// print("inv_Phi( exp(-C*epsilon) ) = ",inv_Phi( exp(-C*epsilon) )   );
//
// print("z_[",1,"] ~ uniform (",inv_phi_tilde(1-g,m_,v_)      ,",",
//  fmin(upper_p[1], inv_Phi( exp(-C*epsilon) )), ")"
//       );
// print (                      lower_q[1]  <
//                         fmin(upper_p[1]
//                            , upper_q[1]   )
//                            )

  // lower_z[1] =   lower_q[1];
  // upper_z[1] =   upper_q[1];
    // lower_z[1] = fmax(lower_q[1], lower_p[1] );
    // upper_z[1] = fmin(upper_q[1], upper_p[1] );

// z_[1]  = uniform_rng ( lower_z[1], upper_z[1] );
//
// print("z_[",1,"] = ", z_[1]);

// z_[1]  = uniform_rng (       lower_q[1]  ,
//                         fmin(upper_p[1]
//                            , upper_q[1]
//                              )
//                              );

  // here ----------
//     print("inv_Phi(0.9) = ", inv_Phi(0.9));
// print("inv_Phi(0.99) = ", inv_Phi(0.99));
// print("inv_Phi(0.999) = ", inv_Phi(0.999));
// print("inv_Phi(0.9999) = ", inv_Phi(0.9999));

//
// print("lower_p[",1,"] = ", lower_p[1], ", lower_q[",1,"] = ", lower_q[1]);
//
// print("upper_q[",1,"] = ", upper_q[1], ", upper_p[",1,"] = ", upper_p[1],", Check: ", upper_q[1] >  lower_p[1], ", ", upper_p[1] >  lower_q[1] );

// print(" - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ")


// for(cd in 2:C ) {
//
//     // lower_p[cd] = inv_phi_tilde( phi_tilde(z_[cd-1],m_,v_)/(1-fe),m_,v_);//best
//     // upper_p[cd] = inv_phi_tilde((1-g)/((1-fe)^cd),m_,v_);// //best
//     // // upper_p[cd] = 111;// upper_p ------
//
//     // lower_q[cd] = inv_Phi(fmin(  Phi(z_[cd-1]) *exp(epsilon),0.99) );
//     // upper_q[cd] = inv_Phi(fmin(   exp((-c[cd]-1)*epsilon ),0.99999 ));
//
//
//
//     lower_p[cd] = inv_phi_tilde( fmin(  phi_tilde(z_[cd-1],m_,v_)/(1-fe), 0.99)    ,m_,v_);
//     upper_p[cd] = inv_phi_tilde( fmin(  phi_tilde(z_[cd-1],m_,v_)/(1-g),  0.99999)    ,m_,v_);
//
//     // upper_p[cd] = 111;// upper_p ------
// //
//     // lower_q[cd] = inv_Phi(Phi(z_[cd-1]) *exp(epsilon) ); //best
//     // upper_q[cd] = inv_Phi( exp((-c[cd]-1)*he )); //best
//
//     // lower_q[cd] = inv_Phi( fmin(  exp(he )*Phi(z_[cd-1] ) ,  0.99) );
//         // lower_q[cd] = inv_Phi(   exp(-he )*Phi(z_[cd-1] )   );
//
//     // lower_q[cd] =  lower_p[cd] ;
//
//
//
//        // upper_q[cd] = inv_Phi( exp((-c[cd]-1/2)*epsilon ));
//     // upper_q[cd] = inv_Phi( exp((-c[cd]-2)*epsilon ));
//
//     // upper_q[cd] = inv_Phi( exp((-c[cd]*5)*epsilon ));
//     // upper_q[cd] = inv_Phi( exp((-c[cd]*3)*epsilon ));
//     // upper_q[cd] = inv_Phi( exp((-c[cd]-3)*epsilon ));
//
// // print("z_[",cd,"] ~ uniform (",
// //       fmax(  lower_p[cd], lower_q[cd])
// //       ,",",
// //       fmin(  upper_p[cd], upper_q[cd])
// //      ,")"  );
//
// // print("z_[",cd,"] ~ uniform (",
// //          lower_z[cd]
// //       ,",   ",
// //          upper_z[cd]
// //       ,")"  );
//
// //
// // print("lower_p[",cd,"] = ", lower_p[cd]);
// // print("lower_q[",cd,"] = ", lower_q[cd]);
// //
// // print("upper_p[",cd,"] = ", upper_p[cd]);
// // print("upper_q[",cd,"] = ", upper_q[cd]);
//
//sss
//
//
// print("lower_p[",cd,"] = ", lower_p[cd], ", lower_q[",cd,"] = ", lower_q[cd]);
//
// print("upper_q[",cd,"] = ", upper_q[cd], ", upper_p[",cd,"] = ", upper_p[cd],", Check: ", upper_q[cd] >  lower_p[cd], ", ", upper_p[cd] >  lower_q[cd] );
//
// print(" - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ")
//
//
// //
// //   z_[cd]  = uniform_rng (
// //                           fmax(  lower_p[cd], lower_q[cd]),
// //                           fmin(  upper_p[cd], upper_q[cd])
// //                                );
// //
//
//
//   // z_[cd]  = uniform_rng (
//   //                         fmax(  lower_p[c[cd]], lower_q[c[cd]]),
//   //                         fmin(  upper_p[c[cd]], upper_q[c[cd]])
//   //                              );
//
// //  z Def --------------------
//
//  // lower_z[cd] = lower_q[cd];
//  // upper_z[cd] = upper_q[cd];
//
//  // lower_z[cd] = lower_p[cd];
//  // upper_z[cd] = upper_p[cd];
//     // lower_z[cd] = fmax( lower_q[cd] ,z_[cd-1]  );
//
//     // lower_z[cd] = fmax(fmax(lower_q[cd], lower_p[cd]),lower_z[cd-1]  );
//  // lower_z[cd] = fmax(fmax(lower_q[cd], lower_p[cd]),lower_z[cd-1]  );
//  // upper_z[cd] = fmin(upper_q[cd], upper_p[cd] );
//  //  z_[cd]  = uniform_rng ( lower_z[cd] ,  upper_z[cd]  ); ------
//
//   // z_[cd]  = uniform_rng ( lower_z[cd] ,  upper_z[cd]  );
//
// // print("z_[",cd,"] = ", z_[cd]);
//   }
// print("z_[",C,"] = ", z_[C]);


//  here ----------------------------

// lower_q[C]  =  inv_Phi ( exp(-ffe) ) ;
// upper_q[C]  =  inv_Phi ( exp(-gg) ) ;

lower_q[C]  =  inv_Phi ( 1/ffe ) ;
upper_q[C]  =  inv_Phi ( 1/gg ) ;
lower_p[C]  =  inv_phi_tilde (   (1-g)  ,m_,v_  ) ;
upper_p[C]  =  inv_phi_tilde (   (1-fe) ,m_,v_  ) ;
 lower_z[C] =  lower_p[C];
 upper_z[C] =  upper_p[C];
 //  lower_z[C] = fmax (lower_p[C],  inv_Phi(exp(-BBB )  )       );
 // upper_z[C] =  fmax (upper_p[C],  inv_Phi(exp(-AAA )  )       );
  lower_z[C] =   inv_Phi(exp(-BBB )          );
 upper_z[C] =   inv_Phi(exp(-AAA )        );
 // if(( lower_p[C] < upper_q[C] )*(  lower_q[C] < upper_p[C] )){
 // lower_z[C] =  fmax(lower_p[C],lower_q[C])   ;
 // upper_z[C] =  fmin(upper_p[C],upper_q[C]);
 // }
print("inv_Phi(exp(-",BBB,") = ",     inv_Phi(exp(-BBB ) ) );
print("inv_Phi(exp(-",AAA,") = ",     inv_Phi(exp(-AAA ) ) );
print("inv_Phi(exp(-",BBB,") = ",     inv_Phi(exp(-BBB ) ) ,", inv_Phi(exp(-",AAA,") = ",     inv_Phi(exp(-AAA ) ) );


 z_[C]     =  uniform_rng ( lower_z[C] ,  upper_z[C]  );
m_ =z_[C]*mmmm;
for (cd in 2:C ){
 lower_p[c[cd]] =  inv_phi_tilde (   (1-g )* phi_tilde(  z_[c[cd-1]] ,m_,v_),m_,v_ ) ;
 upper_p[c[cd]] =  inv_phi_tilde (   (1-fe)* phi_tilde(  z_[c[cd-1]] ,m_,v_),m_,v_ ) ;
 // lower_q[c[cd]]    = inv_Phi(Phi( z_[c[cd-1]]) *exp(-ffe) );
 // upper_q[c[cd]]    = inv_Phi(Phi( z_[c[cd-1]]) *exp(-gg) );
  lower_q[c[cd]]    = inv_Phi(Phi( z_[c[cd-1]]) *(1/ffe) );
 upper_q[c[cd]]    = inv_Phi(Phi( z_[c[cd-1]]) *(1/gg) );
//  def of q ------------
//  def of q ------------
 // lower_z[c[cd]] = fmax(fmax(lower_q[c[cd]], lower_p[c[cd]]),lower_z[c[cd-1]]  );
 // upper_z[c[cd]] = fmin(fmax(upper_q[c[cd]], upper_p[c[cd]]),upper_z[c[cd-1]]  );//  lower_z[c[cd]] = lower_p[c[cd]];
 lower_z[c[cd]] = lower_p[c[cd]];
 upper_z[c[cd]] = upper_p[c[cd]];
//
//   lower_z[c[cd]] = fmax (lower_p[c[cd]],  inv_Phi(exp(-BBB*Phi( z_[c[cd-1]]) )  )       );
//    upper_z[c[cd]] = fmax (upper_p[c[cd]],  inv_Phi(exp(-AAA*Phi( z_[c[cd-1]]) )  )       );

  lower_z[c[cd]] =   inv_Phi(exp(-BBB*Phi( z_[c[cd-1]]) )         );
   upper_z[c[cd]] =    inv_Phi(exp(-AAA*Phi( z_[c[cd-1]]) )        );

   upper_z[c[cd]] = fmin(upper_z[c[cd]] ,  z_[c[cd-1]] );
   lower_z[c[cd]] = fmin(lower_z[c[cd]] ,  z_[c[cd-1]] )-1;

print("inv_Phi(exp(-",BBB,")*Phi( z_[c[",cd-1,"]]) ) = ",     inv_Phi(exp(-BBB*Phi( z_[c[cd-1]]) ) ), "inv_Phi(exp(-",AAA,")*Phi( z_[c[",cd-1,"]]) ) = ",     inv_Phi(exp(-AAA*Phi( z_[c[cd-1]]) ) ) );

 //  z_[c[cd]]  = uniform_rng ( lower_z[c[cd]] ,  upper_z[c[cd]]  );
 // lower_z[cd] = fmax(fmax(lower_q[cd], lower_p[cd]),lower_z[cd-1]  );
 // lower_z[cd] = fmax(fmax(lower_q[cd], lower_p[cd]),lower_z[cd-1]  );
 // upper_z[cd] = fmin(upper_q[cd], upper_p[cd] );
 //  z_[cd]  = uniform_rng ( lower_z[cd] ,  upper_z[cd]  ); ------
 // lower_z[c[cd]] = fmax(lower_q[c[cd]], lower_p[c[cd]]);
 // upper_z[c[cd]] = fmin(fmin(upper_q[c[cd]], upper_p[c[cd]] ),lower_z[c[cd-1]]  );
  z_[c[cd]]  = uniform_rng ( lower_z[c[cd]] ,  upper_z[c[cd]]  );
}
// print(" -------------------------------------------------------- here is -----------")
// for(cd in 1:C)print("lower_p[",cd,"] = ", lower_p[cd], ", upper_p[",cd,"] = ", upper_p[cd]);
// for(cd in 1:C) print("z_[",cd,"] = ", z_[cd]," ~ uniform (",    lower_z[cd]  ,",    ",    upper_z[cd],"), length of supp. = ",upper_z[cd] -  lower_z[cd]   );
// here -------
// for (cd in (C-1):1)print("z_[",cd,"] = ", z_[cd]," ~ uniform (",    lower_z[cd]  ,",    ",    upper_z[cd],")"   );

// print("------------------------------------------------")
for(cd in 1:C){
// print("lower_p[",cd,"] = ", lower_p[cd], ", lower_q[",cd,"] = ", lower_q[cd]);
// print("upper_p[",cd,"] = ", upper_p[cd], ", upper_q[",cd,"] = ", upper_q[cd],", Check: ", upper_p[cd] >  lower_p[cd], ", ", upper_q[cd] >  lower_q[cd] );
// print("------------------------------------------------")
print("lower_p[",cd,"] = ", lower_p[cd], ", lower_q[",cd,"] = ", lower_q[cd]);

print("upper_q[",cd,"] = ", upper_q[cd], ", upper_p[",cd,"] = ", upper_p[cd],", Check: ", upper_q[cd] >  lower_p[cd], ", ", upper_p[cd] >  lower_q[cd] );

print(" - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  ");


}

for(cd in 1:C){
print("upper_p[",cd,"]  - lower_p[",cd,"] = ", upper_p[cd]  - lower_p[cd]);
print("upper_q[",cd,"]  - lower_q[",cd,"] = ", upper_q[cd]  - lower_q[cd]);
print("upper_z[",cd,"]  - lower_z[",cd,"] = ", upper_z[cd]  - lower_z[cd]);
}




// print("-----------------------------------")

// for(cd in 1:C) print("z_[",cd,"] = ", z_[cd]," ~ uniform (",    lower_z[cd]  ,",    ",    upper_z[cd],")"   );

ssss =1;
for (cd in 2:C) ssss = (z_[cd] -z_[cd-1]>0)*ssss;

if(ssss)  print("     z_[1] < z_[2] < z_[3]    z_ monotonicity: TRUE" );
if(!ssss) print("     z_[1] < z_[2] < z_[3]    z_ monotonicity: FALSE" );



// print("lower_p[",cd,"] = ", lower_p[cd]);
// for(cd in 1:C)print("lower_q[",cd,"] = ", lower_q[cd]);
// //
// print("upper_p[",cd,"] = ", upper_p[cd]);
// for(cd in 1:C)print("upper_q[",cd,"] = ", upper_q[cd]);



// print("z_[",C,"] = ", z_[C]);
a_=m_/v_;
b_=1/v_;


for(cd in 1 : C) {
   // print("Phi((z_[",cd,"] -m_)/v_) = ", Phi( (z_[cd] -m_)/v_));
                       }
print("-----------------------------   p_[1] < p_[2] < p_[3] < p_[4]   -------");
for(cd in 1 : C-1) {
                           p_[cd] = Phi((z_[cd+1] - m_)/v_)- Phi( (z_[cd] -m_)/v_);
                           // print("p_[",cd,"] = ", p_[cd]);
                       }
 p_[C] = 1 - Phi((z_[C] - m_)/v_);
                           // print("p_[",C,"] = ", p_[C]);


ssss =1;
for (cd in 2:C) ssss = (p_[cd] -p_[cd-1]>0)*ssss;
if(ssss)  print("                                    p_ monotonicity: TRUE" );
if(!ssss) print("                                    p_ monotonicity: FALSE" );



     for(cd in 1 : C) {l_[cd] = (-1)*log(Phi(z_[cd]));
            // print("l_[",cd,"] = ", l_[cd]);


     }

     print("-------------------------------   dl_[1] > dl_[2] > dl_[3] > dl_[4]   -------");


     for(cd in 1:C){
                 if (cd==C) {dl_[cd]=fabs(l_[cd]-0);
                 // print("dl_[",cd,"] = ", dl_[cd]);

                 }else{

       dl_[cd]=fabs(l_[cd]-l_[cd+1]);
      // print("dl_[",cd,"] = ", dl_[cd]);

          }
          }



     // deno[C]=1;
     deno_[C-1]=1-p_[C];
      for(cd in 3:C)  deno_[c[cd]]=deno_[c[cd-1]]-p_[c[cd-1]];

  // int s=0;

  for(cd in 1:C-1){
    denoo_[cd]=deno_[cd];
 // print("denoo_[",cd,"] = ", denoo_[cd]);

  }
  denoo_[C]=1;
 // print("denoo_[",1,"] = ", denoo_[1]);



print("------------------------------------");

dummy=0;
           for(n in 1:N) {
             hitRate_[n] = p_[c[n]]/denoo_[c[n]];
             falseRate_[n] = dl_[c[n]]*NI;
              // print("p_[",c[n],"] = ", p_[c[n]], "denoo_[",c[n],"] = " ,denoo_[c[n]]  );
              // print("p_[",c[n],"] /denoo_[",c[n],"] = ", p_[c[n]]/denoo_[c[n]]  );

              print("hitRate_[",n,"] = ", hitRate_[n]   );
    h[n] = binomial_rng(NL-dummy,  hitRate_[n]);
    dummy = dummy + h[n];
// print("h[",n,"] = ", h[n]);

 // fff[n] ~ poisson( l[c[n]]*NL);//Non-Chakraborty's model
    f[n] = poisson_rng (falseRate_[n] );//Chakraborty's model //<-------very very very coution, not n but c[n] 2019 Jun 21
 // fff[n] ~ poisson( l[c[n]]*NI);//Non-Chakraborty's model
// print("f[",n,"] = ", f[n]);
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
if(ssss)  print("                                    hitRate_ Upper bounds: TRUE" );
if(!ssss) print("                                    hitRate_ Upper bounds: FALSE" );


  ssss =1;
for (cd in 2:C) ssss = (hitRate_[cd] > fe )*ssss;
if(ssss)  print("                                    hitRate_ lower bounds: TRUE" );
if(!ssss) print("                                    hitRate_ lower bounds: FALSE" );
print(" ----------------------------------------------------------");









  ssss =1;
for (cd in 2:C) ssss = (hitRate_[cd] -hitRate_[cd-1]>0)*ssss;
if(ssss)  print("                                    hitRate_ monotonicity: TRUE" );
if(!ssss) print("                                    hitRate_ monotonicity: FALSE" );


    ssss =1;
for (cd in 2:C) ssss = (falseRate_[cd] -falseRate_[cd-1]>0)*ssss;
if(ssss)  print("                                    falseRate_ monotonicity: TRUE" );
if(!ssss) print("                                    falseRate_ monotonicity: FALSE" );




  ssss =1;
for (cd in 2:C) ssss = (h[cd-1] -h[cd]>0)*ssss;
if(ssss)  print("                                    h monotonicity: TRUE" );
if(!ssss) print("                                    h monotonicity: FALSE" );


    ssss =1;
for (cd in 2:C) ssss = (f[cd] -f[cd-1]>0)*ssss;
if(ssss)  print("                                    f  monotonicity: TRUE" );
if(!ssss) print("                                    f  monotonicity: FALSE" );






// for(n in 1:N){
//   print("f[",n,"] = ", f[n]);
// }


// for(n in 1:N){
//   print("h[",n,"] = ", h[n]);
// }

print("------------------------------------");

for(cd in 1:C) print("z_[",cd,"] = ", z_[cd]," ~ uniform (",    lower_z[cd]  ,",    ",    upper_z[cd],"), length of supp. = ",upper_z[cd] -  lower_z[cd]   );

print("---------------------------------- Data -------");

for(n in 1:N){
 if(n==1){ print("h[",n,"] = ", h[n] , "      f[",n,"] = ", f[n] ,"  highest confidence");}
 else if(n==N){ print("h[",n,"] = ", h[n] , "      f[",n,"] = ", f[n] ," lowest confidence");}
 else {  print("h[",n,"] = ", h[n] , "      f[",n,"] = ", f[n] );}

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













