
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
                    if(ModifiedPoisson==1) NX =NL;

                    for (cd in 1:C)   hitExtended[cd] = h[cd];

                    hitExtended[C+1]=NL-sum(h);

print("NL - sum(h) = ",NL-sum(h));

for(cd in 1:(C+1)) print("hitExtended[",cd,"] = ",hitExtended[cd]);



print("* This model is described with a multinomial distribution, which is the Chakraborty's FROC model.");



print("                                                ");
print("                                              ");
print("                   o              o          ");
print("                 o   o          o   o          ");
print("                 o    o        o    o      ");
print("                  oo    oooooooo   o      ");
print("                    o              o        ");
print("                   o     |     |    o       ");
print("                  o         x        o          ");
print("                   o                o            ");
print("                     ooooooooooooooo          ");
print("                                           ");
print("                                           ");
print("                                           ");
print("   Riyakobach Sampocca Ikantone (2004 ~ 2019) ");
print("   Favorite sports:  running, ball, Frisbee    ");
print("   IQ: 230      ");
print("    weight: 20kg?   ");
print("    height: 1m30cm?   ");
print("   Favorite food: Anything ");
print("   Favorite words; Sampo, ball,Frisbee,...etc ");
print("   cause: Cancer           ");
print("   humanity: doggy ");
print("   Favorite womens; Batyan (grandmother), Okasan (mother) ");
print("   Favorite men; I do not know,,,maybe father?");
print("  Favorite time; breakfast, dinner time, Sampo");

print("                                           ");
print("   MCMC runs, please wait ... to get Frisbee ");
print("   I love you                      ");
print("                                           ");
print("  Kind regards,                        ");
print("   doggy                        ");
print("                                               ");
print("   He died in ..(I forget the precise year ");
print("       but maybe ,,, several years ago) around 2020 ");
print("  Please pray for him with me, R.I.P Riyakoboch Sampota Ikuzo. ");

print("                                                ");
print("                                              ");
print("                   o              o          ");
print("                 o   o          o   o          ");
print("                 o    o        o    o      ");
print("                  oo    oooooooo   o      ");
print("                    o              o        ");
print("                   o     |     |    o       ");
print("                  o         *        o          ");
print("                   o                o            ");
print("                     ooooooooooooooo          ");


print("           --------------                ");
print("           |            |                           ");
print("           |            |                          ");
print("           | R.I.P.     |                     ");
print("           |            |       *                ");
print("           | Riyacobach |      *                  ");
print("           |            |       *                    ");
print("           | 2004-2019  |      *                     ");
print("           |            |      |                    ");
print("   --------------------------  |   ----         ");
print("      -----------------------  |  ---         ");
print("               --------------------         ");
print("                                              ");
print("                                              ");
print("                                              ");
print("                                              ");
print("                                              ");
print("           o o          ooo           ");
print("         o    o        o   o       ");
print("         oo    oooooooooo   o      ");
print("           o              ooo        ");
print("          o     |     |    o       ");
print("         o         x        o          ");
print("          o                o            ");
print("            ooooooooooooooo          ");
print("                                  ");
print("                                  ");
print("           o o          ooo           ");
print("         o    o        o   o       ");
print("         oo    oooooooooo   o      ");
print("           o              ooo        ");
print("          o     |     |    o       ");
print("         o         *        o          ");
print("          o                o            ");
print("            ooooooooooooooo          ");
print("                                  ");
print("                                  ");
print("* The author thinks that the most ");
print("  difficult part of this  Stan programm is ");
print("  these doggies! ");
print("                                  ");
print("   Kind regards, ");
print("   The autor of this package ");
print("                                  ");
print("                                  ");
print("                                  ");
print("                                  ");
print("                                  ");
print("                                  ");
print("           o o          ooo           ");
print("         o    o        o   o       ");
print("         oo    oooooooooo   o      ");
print("           o              ooo        ");
print("          o     |     |    o       ");
print("         o         *        o          ");
print("          o                o       ");
print("          o ooooooooooooooo      ooo   ");
print("           o             o  o    o   o ");
print("         o    o    o   o     o  o  o ");
print("       o    o  o    ooo        o o o ");
print("        ooo     o              ooo  ");
print("                 o    ooo     o  ");
print("                 o   o   o   o ");
print("                  ooo     ooo  ");
print("                                  ");
print("   Ruicobach Sampo Ikantone (2006 ~ ----) ");
print("                                  ");
print("                                  ");
print("                                  ");
print("                                  ");
print("           o o          ooo           ");
print("         o    o       o     o       ");
print("         oo     ooooo       o      ");
print("           o              ooo        ");
print("          o     |     |    o       ");
print("         o         *        o          ");
print("          o                o       ");
print("             o          o          ooo   ");
print("           o             o  o    o   o ");
print("         o    o    o   o     o  o  o ");
print("       o    o  o    ooo        o o o ");
print("        ooo     o              ooo  ");
print("                 o    ooo     o  ");
print("                 o   o   o   o ");
print("                  ooo     ooo  ");
print("                                  ");
print("   Ruicobach Sampo Ikantone (2006 ~ ----) ");
print("                                  ");
print("                                  ");
print("                                  ");
print("           o o          ooo           ");
print("         o     o       o    o       ");
print("         o       oooo       o      ");
print("           o              ooo        ");
print("          o     |     |    o       ");
print("         o         *        o          ");
print("          o                o       ");
print("             o          o          ooo   ");
print("           o             o  o    o   o ");
print("         o    o    o   o     o  o  o ");
print("       o    o  o    ooo        o o o ");
print("        ooo     o              ooo  ");
print("                 o    ooo     o  ");
print("                 o   o   o   o ");
print("                  ooo     ooo  ");
print("                                  ");
print("   Ruicobach Sampo Ikantone (2006 ~ ----) ");
print("                                  ");
print("            o              o          ");
print("          o   o          o   o          ");
print("          o    o        o    o      ");
print("           oo      oooo     o      ");
print("             o              o        ");
print("            o     |     |    o       ");
print("           o         *        o          ");
print("            o                o            ");
print("             o             o       ooo   ");
print("           o             o  o    o   o ");
print("         o    o    o   o     o  o  o ");
print("       o    o   o   ooo        o o o ");
print("        ooo       o           ooo  ");
print("                  o    oo     o  ");
print("                 o   o   o   o ");
print("                  ooo     ooo  ");
print("                                  ");
print("   Ruicobach Sampo Ikantone (2006 ~ ----) ");
print("                                  ");
print("                                  ");
print("                                  ");
print("                                  ");
print("                                  ");
print("              o              o          ");
print("            o   o          o   o          ");
print("            o    o        o    o      ");
print("             oo     oooo     o      ");
print("               o           o        ");
print("              o   |     |   o       ");
print("             o       *       o          ");
print("              o            o            ");
print("                o            o     oo   ");
print("              o           o   o   o  o ");
print("            o   o o       o   o  o  o ");
print("            o o   o    X    o o o o     ");
print("                  o          oo          ");
print("                  o    oo    o  ");
print("                 o   o   o   o ");
print("                  ooo     ooo  ");
print("                                  ");
print("   Ruicobach Sampo Ikantone (2006 ~ ----) ");
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
real <lower=0>A;
A=Phi(  a/sqrt(b^2+1)  );//Measures of modality performance

}


