

simple <-function(){



  m <- rstan::stan_model(model_code = '
                            data{int x;
                                 real y;
                            }


                            parameters {real t; }
                            transformed parameters {
                            real s = 1-exp(-t);

                              }
                            model { t ~ uniform(0,y);
                            x ~ binomial(10,t/1-y);
                           }
                             ')
  f <- rstan::sampling(m, data=list(x=5,upper_bound=0.0001), seed = 100,chains=1)



  stan_trace(f)
  stan_dens(f)





  m <- rstan::stan_model(model_code = '
                            data{real xx;}
                              transformed data {
                              real x[10];
                              real y[10];
                              for(i in 1:10){
                              x[i]=uniform_rng(0,20);
                               y[i]=uniform_rng(0,20);}
                              for(i in 1:10)print("x[",i,"] = ", x[i] , " y[",i,"] = ", y[i]);


                              }

                            parameters {real t; }
                            model { xx ~ normal(t,1);
                           }
                             ')
  f <- rstan::sampling(m, data=list(xx=0.5), seed = 100)








  mm <- rstan::stan_model(model_code = '
                            data{real xx;}
                              transformed data {
                              real x[10];
                              real y[10];
                              for(i in 1:10){
                              x[i]=uniform_rng(0,20);
                               y[i]=uniform_rng(0,20);}
                              for(i in 1:10)print("x[",i,"] = ", x[i] , " y[",i,"] = ", y[i]);


                              }

                             ')
  f <- rstan::sampling(mm, data=list(xx=0.5), seed = 100)




























  mm<- rstan::stan_model(model_code = '
                            data{real x;
                                 int n;
                                 int m;
                                 }
                            parameters {real t;}
                            model {
                            if(m==1){
                               if(n==1)  x ~ normal(t,1);

                                  if(n==2)   x ~ normal( t-5,1);
                            }
                         }')


  ff <- rstan::sampling(mm, data=list(x=1,n=2,m=1),  iter = 11111,chains=1)
  fff <- rstan::sampling(mm, data=list(x=1,n=1,m=1), iter = 11111,chains=1)



  m <- rstan::stan_model(model_code = '
                            data{real x;
                                  }
                            parameters {real t;}
                            model {
                              x ~ normal( t-5,1);
                         }')
  f <- rstan::sampling(m, data=list(x=1,n=2), iter = 11111,chains=1)





  stan_trace(f)
  stan_dens(f)













  m <- rstan::stan_model(model_code = '
                            data{real x;}
                            parameters {real <lower=0>t;}
                            model {x ~ normal(t,1);}')
  f <- rstan::sampling(m, data=list(x=1), iter = 1100,chains=1)






  m <- rstan::stan_model(model_code = '
                            data{real x;
                            }
                            parameters {
                                        real  s;
                                        real  t;
                            }
                            model {
                            x ~ normal(s,1);
                            x ~ normal(t,1);
                         t -s ~ uniform(0,1);
                         }')
  f <- rstan::sampling(m, data=list(x=1,y=2), iter = 11100,chains=1)



  extract(f)$t > extract(f)$s



  m <- rstan::stan_model(model_code = '
                            data{real x;
                            }
                            parameters {
                                        real  s;
                                        real  t;
                                        real  u;
                                        real  v;                            }
                            model {
                            x ~ normal(s,1);
                            x ~ normal(t,1);
                            x ~ normal(u,1);
                            x ~ normal(v,1);
                            t -s ~ uniform(0,1);
                            u -t ~ uniform(0,1);
                            v -u ~ uniform(0,1);
                         }')
  f <- rstan::sampling(m, data=list(x=1,y=2), iter = 11100,chains=1)



  extract(f)$t > extract(f)$s




  m <- rstan::stan_model(model_code = '
                            data{real x;}
                            transformed data{real z; z =  Phi( (-1.14194+ 2.66963)/(-0.257783) );
                                             print("Here, we can use print() as a debugger")
                                             print("")

                                             print("z = ", z)

                            }
                            parameters {real y;}
                            model {y ~ normal(z,1);}
                            generated quantities {real zhat = z;}')
  f <- rstan::sampling(m, data=list(x=1), iter = 100,chains=1)
  extract(f)[["zhat"]]















m <- rstan::stan_model(model_code = '
                            data{real x;}
                            parameters {real y[3];}
                            model {
                                   x~uniform(y[1],1);
                                   y[1]~uniform(y[2],1);
                                   y[2]~uniform(y[3],1);
                             }')
f <- rstan::sampling(m, data=list(x=0.5), iter = 100)



m <- rstan::stan_model(model_code = '
                            data{int x[5];}
                            parameters {real y[5];}
                            model {
                                   x[1]~poisson(y[1],11);
                                   x[2]~poisson(y[2],11);
                                   x[3]~poisson(y[3]+y[2]+y[1],11);
                                   x[4]~poisson(y[4]+y[3]+y[2]+y[1],11);
                                   x[5]~poisson(y[5]+y[4]+y[3]+y[2]+y[1],11);

                             }')
f <- rstan::sampling(m, data=list(x=c(1, 1,2,3,4)   ), iter = 100)




m <- rstan::stan_model(model_code = '
                            data{real x;}
                            parameters {real y[5];}
                            model {
                                   x~uniform(y[1],1);
                                   y[1]~uniform(y[2],1);
                                   y[2]~uniform(y[3]+y[2]+y[1],1);
                                   y[3]~uniform(y[4]+y[3]+y[2]+y[1],1);
                                   y[4]~uniform(y[5]+y[4]+y[3]+y[2]+y[1],1);

                             }')
f <- rstan::sampling(m, data=list(x=0.5), iter = 100)







m <- rstan::stan_model(model_code = '
                            data{real x;}
                            parameters {real y[5];}
                            model { real s;
                             x ~ uniform(y[1],11111);
                             s = y[1];
                           for(i in 2:5){
                            y[i] ~ uniform(s,11111);
                            s = s+y[i];
                           }
                             }')
f <- rstan::sampling(m, data=list(x=0.5), seed = 100)





 m <- rstan::stan_model(model_code = '
                            data{real x;}
                            transformed data{real z; z = exp(1/222)* Phi(-1)/(  Phi(-1) + exp(normal_lpdf(-1|0,1))); }
                            parameters {real y;}
                            model {y ~ normal(z,1);}
                            generated quantities {real zhat = z;}')
f <- rstan::sampling(m, data=list(x=1), iter = 100)
extract(f)[["zhat"]]


















m <- rstan::stan_model(model_code = '
                            data{        //1
                            real mm;     //2
                            real mmm;    //3
                            real www;    //4
                            int C;       //5
                            int NI;      //6
                              real epsilon;                  //14

                            }            //7

                            transformed data{ //8
  real m_;                       //9
  real <lower =0> v_;            //10
  real w_;                       //11
  real z_[C];                    //12
  real dz_[C-1];                 //13
  real ssss;                     //15

m_ = normal_rng (mm,mmm);         //16
v_ = uniform_rng (0.1 ,5);        //17
w_ =  normal_rng (      v_*m_+1/(C*1.000001)    ,www);  //18
ssss = w_;  //19
z_[1] =w_;  //20
     print (    Phi(w_)     )

   print (     ssss     );
      print (     ssss     );
   print (     exp(epsilon/NI)     );
      print (     exp(1/NI)     );
   print (     exp(epsilon/3)     );
   print (     exp(1/3)     );
   print (     exp(0.1/3)     );

      print (  "asaaaaaaa",   exp(1)     );

   print (    exp(normal_lpdf(w_|0,1))  );

   print(     exp(epsilon/NI) * Phi(w_)/(  Phi(w_) + exp(normal_lpdf(w_|0,1)))           );
   dz_[1] = uniform_rng(  exp(epsilon/NI) * Phi(w_)/(  Phi(w_) + exp(normal_lpdf(w_|0,1))),
                          exp(epsilon/NI) * Phi(w_)/(  Phi(w_) + exp(normal_lpdf(w_|0,1))) + 3
                          );

           ssss = ssss+dz_[1];
        for(cd in 2:C -1) {
        dz_[cd] = uniform_rng(   exp(epsilon/NI)*Phi(ssss)/(  Phi(ssss) + exp(normal_lpdf(ssss|0,1))),
                                 exp(epsilon/NI)*Phi(ssss)/(  Phi(ssss) + exp(normal_lpdf(ssss|0,1)))+3);
           ssss = ssss+dz_[cd];
         z_[cd] = ssss;
    }
          z_[C] = ssss;



                            }


                            parameters {real y;}
                            model {y ~ normal(1,1);}
                            generated quantities {real zhat = exp(epsilon/NI)* Phi(w_)/(  Phi(w_) + exp(normal_lpdf(w_|0,1)));}')
f <- rstan::sampling(m, data=list(
NI=222,
  epsilon=0.1,
C=4,
    mm=0,
 mmm=1,
 www=1

), iter = 100)
extract(f)[["zhat"]]













m <- rstan::stan_model(model_code = '
                                data{        //1
                                real mm;     //2
                                real mmm;    //3
                                real www;    //4
                                int C;       //5
                                int NI;      //6
                                }            //7

                                transformed data{ //8
      real m_;                       //9
      real <lower =0> v_;            //10
      real w_;                       //11
      real z_[C];                    //12
      real dz_[C-1];                 //13
      real epsilon;                  //14
      real ssss;                     //15

    m_ = normal_rng (mm,mmm);         //16
    v_ = uniform_rng (0.1 ,5);        //17
    w_ =  normal_rng (      v_*m_+1/(C*1.000001)    ,www);  //18
    ssss = w_;  //19
    z_[1] =w_;  //20
         print ( "Phi(w_)  =",   Phi(w_)     )

       print (     ssss     );
          print (     ssss     );
       print ("exp(epsilon/NI) = " ,    exp(epsilon/NI)     );
          print ( "  exp(1/NI)   = ",    exp(1/NI)     );
       print ( "exp(epsilon/3)  = ",    exp(epsilon/3)     );
       print (     exp(1/3)     );
       print (     exp(0.1/3)     );

          print (     exp(1)     );

       print (    exp(normal_lpdf(w_|0,1))  );

       print(     exp(epsilon/NI) * Phi(w_)/(  Phi(w_) + exp(normal_lpdf(w_|0,1)))           );
       dz_[1] = uniform_rng(  exp(epsilon/NI) * Phi(w_)/(  Phi(w_) + exp(normal_lpdf(w_|0,1))),
                              exp(epsilon/NI) * Phi(w_)/(  Phi(w_) + exp(normal_lpdf(w_|0,1))) + 3
                              );

               ssss = ssss+dz_[1];
            for(cd in 2:C -1) {
            dz_[cd] = uniform_rng(   exp(epsilon/NI)*Phi(ssss)/(  Phi(ssss) + exp(normal_lpdf(ssss|0,1))),
                                     exp(epsilon/NI)*Phi(ssss)/(  Phi(ssss) + exp(normal_lpdf(ssss|0,1)))+3);
               ssss = ssss+dz_[cd];
             z_[cd] = ssss;
        }
              z_[C] = ssss;



                                }


                                parameters {real y;}
                                model {y ~ normal(1,1);}
                                generated quantities {real zhat = exp(epsilon/NI)* Phi(w_)/(  Phi(w_) + exp(normal_lpdf(w_|0,1)));}')
f <- rstan::sampling(m, data=list(
  NI=222,
  epsilon=0.1,
  C=4,
  mm=0,
  mmm=1,
  www=1

), iter = 100)
extract(f)[["zhat"]]

#
#
# h<-dd$h
# f<-dd$f
# m<-dd$m
# c<-dd$c
# q<-dd$q
# NL<-dd$NL
# C<-dd$C
# M<-dd$M
# Q<-dd$Q
# NI<-dd$NI

###############################################################################

h<-c(
  61,19,12,9,3,16,29,34,1,0,52,29,10,4 ,3,10,16,23,43,15, # modality 4
  52,25,13,4,1,27,28,29,1,0,53,29,13,2 ,4,9 ,16,22,43,14, # modality 2
50,30,11,5,1,15,29,29,1,0,39,31,8 ,10,3,10,8 ,25,45,14, # modality 1
35,29,18,9,0,17,27,24,0,0,34,33,7 ,13,2,12,16,21,35,15,  # modality 5
43,29,11,6,0,18,29,21,0,0,43,29,6 ,7 ,1,10,14,19,32,23 # modality 3

)

f <-c(
  1, 4,18,21,23,1,1,0,11,35, 6,14,37,36,18,0,2,4,18,25,# modality 4
  1 ,1,21,24,23,1,1,5,30,40,2 ,19,31,56,42,2,0,2,30,32,# modality 2
0 ,4,20,29,21,0,0,6,15,22,1 ,15,18,31,19,1,2,4,16,17,# modality 1
0, 2,19,23,18,0,2,6,10,30, 2,25,40,29,24,1,1,4,24,32,# modality 5
1, 7,13,28,19,0,1,7, 7,31, 7,15,28,41,9 ,0,2,5,24,31# modality 3

)

   a   <- m_q_c_vector_from_M_Q_C(5,4,5)

        m <- a$m
        c <- a$c
        q <- a$q

NI<-199
NL <-142
C<-5
M<-5
Q<-4

dd.orderd <- list(
  h=h,
  f=f,
  m=m,
  c=c,
  q=q,
  NI=NI,
  NL=NL,
  M=M,
  Q=Q,
  C=C
)

###############################################################################












}
