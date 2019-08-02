
<style type="text/css">

h1.title {
  font-size: 35px;
  font-weight: bold;
  font-family: Arial-Black;
   color: #800000           ;
}
h1{
  font-size: 35px;
  font-weight: bold;
  font-family: Arial-Black;
  
  color: #800000            ;

}

h2 {
  font-size: 30px;
  font-weight: bold;
  font-family: Arial-Black;
    
  color: #800000            ;

}

h3 {
  font-size: 26px;
  font-weight: bold;
  font-family: Arial-Black;
    
  color: #800000            ;

}

h4 {
  font-size: 24px;
  font-weight: bold;
  font-family: Arial-Black;
    
  color: #800000            ;

}



h5 {
  font-size: 22px;
  font-weight: bold;
  font-family: Arial-Black;
    
  color: #800000            ;

}



h6 {
  font-size: 19px;
  font-weight: bold;
  font-family: Arial-Black;
    
  color: #800000            ;

}



img {
    border:0;
}


body {
  font-size: 18px;
  <!-- font-weight: normal ; -->
    font-weight:bolder;
  
  font-family: Calibri;
  
    
  color: #800000            ;

  background-color:#EEEEEE; 
  
<!--   margin:0; -->
<!--    padding:0; -->
  
}












p {
    color: #440000      ;
}

</style>

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

<!-- badges: end -->

## Installation

Available from [CRAN](https://CRAN.R-project.org) with the following R
script, which installs the released version of `BayesianFROC` .

``` r

              install.packages("BayesianFROC")
              
              
#     Pleaes execute it from the R console or the R studio console.
```

## What this package?

Execute one of the following R script for a **shiny based GUI**:

``` r
                           library(BayesianFROC)
                           BayesianFROC::fit_GUI()
```

Or

``` r

                           library(BayesianFROC)
                           BayesianFROC::fit_GUI_simple()
```

Or

``` r
                           
                           library(BayesianFROC)
                           fit_GUI_dashboard() 
                                                      
                           
```

Then reader will understand what this package is.

Do not read this boring poor vignette, but execute the above R scripts.

### For details

  - See [vignettes](https://cran.r-project.org/package=BayesianFROC)

  - See our open access pre-print on the arXiv: yet
    
    To upload, I need someone who endorse my paper. If someone can help
    endorse my pre print, please send e-mail to me. I need help to
    upload my paper 2019.Jun.09.

  - **Behavior** **Metrica** in which the author submit the paper.

### Goal of this package `BayesianFROC`

**Comparison** of *modality*. In some context, *modality* is imaging
methods: *MRI*, *CT*, *PET*,…etc, and the another context, if images are
taken for treatment (case) group and untreatment or another treatment
(control) group, then *modality* means *treatment*.

  - ***Fitting***: data is the following two type
      - Single Reader and Single Modality case.
      - Multiple Reader and Multiple modality case (MRMC)
  - ***Comparison*** of the *modalities* by *AUC* (the area under the
    AFROC curve).

## Example

  - Build Data
  - Fit
  - Validation of goodness of fit

This is a basic example which shows how to fit a model to the data
`dataList` representing the following FROC data;

| Confidence Level       | Number of Hits | Number of False alarms |
| :--------------------- | :------------: | :--------------------: |
| 3 = definitely present |       97       |           1            |
| 2 = equivocal          |       32       |           14           |
| 1 = questionable       |       31       |           74           |

where *hit* means True Positive: **TP** and *false* *alarm* means False
Positive: **FP**.

``` r

#0) To avoid the following error in Readme file,
#I have to attach the Rcpp. 
#I do not know why such error occur withou Rcpp. 
#This error occurs only when I run the following R scripts from readme.

#Error
#in do.call(rbind,sampler_params) :second argument must be a list Calles:<Anonymous>...get_divergent_iterations ->sampler_param_vector =. do.call Execution halted

 library(Rcpp)  # This code can remove the above unknown error, if someone know why the error occur, please tell me.







#1) Build  data for singler reader and single modality  case.




  dataList <- list(c=c(3,2,1),     # c is ignored, can omit.
              h=c(97,32,31),
              f=c(1,14,74),
              NL=259,
              NI=57,
              C=3)





#  where,
#        c denotes confidence level, each components indicates that 
#                3 = Definitely lesion,
#                2 = subtle,  
#                1 = very subtle
#        h denotes number of hits 
#          (True Positives: TP) for each confidence level,
#        f denotes number of false alarms
#          (False Positives: FP) for each confidence level,
#        NL denotes number of lesions (signal),
#        NI denotes number of images,


  
  
  
  
  
  
  
  
  
  
  
  
  
#2) Fit the FROC model.


 
   fit <- BayesianFROC::fit_Bayesian_FROC(
     
            # data to which we fit a model                 
                dataList = dataList,
                                        
            # The number of MCMC chains                         
                     cha = 4,
            
            # The number of MCMC samples for each chains                         
                    ite  = 11111,
                    
            # The number of warming up of MCMC simulation for each chains           
                     war = 1111,
            
            # Show verbose summary and MCMC process
                 summary = TRUE
                                                         )
#> Study Design: srsc case
#> False Positive Fraction is calculated
#>  per image.
#> 
#> * Number of Lesions: 259
#> 
#> * Number of Images : 57
#> 
#> 
#> .                     Confidence.Level   False.Positives   True.Positives
#> -------------------  -----------------  ----------------  ---------------
#> Obviouly present                     3                 1               97
#> Relatively obvious                   2                14               32
#> Subtle                               1                74               31
#> 
#> 
#> * Higher number of confidence level indicates reader's higher confidence. In your case, the number 3 is the most high confidence level, i.e., we may say that confidence level 3  means that "definitely lesion is present "
#> 
#> SAMPLING FOR MODEL 'Model_srsc_per_image_target' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 0 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:     1 / 11111 [  0%]  (Warmup)
#> Chain 1: Iteration:  1111 / 11111 [  9%]  (Warmup)
#> Chain 1: Iteration:  1112 / 11111 [ 10%]  (Sampling)
#> Chain 1: Iteration:  2222 / 11111 [ 19%]  (Sampling)
#> Chain 1: Iteration:  3333 / 11111 [ 29%]  (Sampling)
#> Chain 1: Iteration:  4444 / 11111 [ 39%]  (Sampling)
#> Chain 1: Iteration:  5555 / 11111 [ 49%]  (Sampling)
#> Chain 1: Iteration:  6666 / 11111 [ 59%]  (Sampling)
#> Chain 1: Iteration:  7777 / 11111 [ 69%]  (Sampling)
#> Chain 1: Iteration:  8888 / 11111 [ 79%]  (Sampling)
#> Chain 1: Iteration:  9999 / 11111 [ 89%]  (Sampling)
#> Chain 1: Iteration: 11110 / 11111 [ 99%]  (Sampling)
#> Chain 1: Iteration: 11111 / 11111 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 0.183 seconds (Warm-up)
#> Chain 1:                2.039 seconds (Sampling)
#> Chain 1:                2.222 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'Model_srsc_per_image_target' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 0 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
#> Chain 2: Adjust your expectations accordingly!
#> Chain 2: 
#> Chain 2: 
#> Chain 2: Iteration:     1 / 11111 [  0%]  (Warmup)
#> Chain 2: Iteration:  1111 / 11111 [  9%]  (Warmup)
#> Chain 2: Iteration:  1112 / 11111 [ 10%]  (Sampling)
#> Chain 2: Iteration:  2222 / 11111 [ 19%]  (Sampling)
#> Chain 2: Iteration:  3333 / 11111 [ 29%]  (Sampling)
#> Chain 2: Iteration:  4444 / 11111 [ 39%]  (Sampling)
#> Chain 2: Iteration:  5555 / 11111 [ 49%]  (Sampling)
#> Chain 2: Iteration:  6666 / 11111 [ 59%]  (Sampling)
#> Chain 2: Iteration:  7777 / 11111 [ 69%]  (Sampling)
#> Chain 2: Iteration:  8888 / 11111 [ 79%]  (Sampling)
#> Chain 2: Iteration:  9999 / 11111 [ 89%]  (Sampling)
#> Chain 2: Iteration: 11110 / 11111 [ 99%]  (Sampling)
#> Chain 2: Iteration: 11111 / 11111 [100%]  (Sampling)
#> Chain 2: 
#> Chain 2:  Elapsed Time: 0.146 seconds (Warm-up)
#> Chain 2:                2.278 seconds (Sampling)
#> Chain 2:                2.424 seconds (Total)
#> Chain 2: 
#> 
#> SAMPLING FOR MODEL 'Model_srsc_per_image_target' NOW (CHAIN 3).
#> Chain 3: 
#> Chain 3: Gradient evaluation took 0 seconds
#> Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
#> Chain 3: Adjust your expectations accordingly!
#> Chain 3: 
#> Chain 3: 
#> Chain 3: Iteration:     1 / 11111 [  0%]  (Warmup)
#> Chain 3: Iteration:  1111 / 11111 [  9%]  (Warmup)
#> Chain 3: Iteration:  1112 / 11111 [ 10%]  (Sampling)
#> Chain 3: Iteration:  2222 / 11111 [ 19%]  (Sampling)
#> Chain 3: Iteration:  3333 / 11111 [ 29%]  (Sampling)
#> Chain 3: Iteration:  4444 / 11111 [ 39%]  (Sampling)
#> Chain 3: Iteration:  5555 / 11111 [ 49%]  (Sampling)
#> Chain 3: Iteration:  6666 / 11111 [ 59%]  (Sampling)
#> Chain 3: Iteration:  7777 / 11111 [ 69%]  (Sampling)
#> Chain 3: Iteration:  8888 / 11111 [ 79%]  (Sampling)
#> Chain 3: Iteration:  9999 / 11111 [ 89%]  (Sampling)
#> Chain 3: Iteration: 11110 / 11111 [ 99%]  (Sampling)
#> Chain 3: Iteration: 11111 / 11111 [100%]  (Sampling)
#> Chain 3: 
#> Chain 3:  Elapsed Time: 0.137 seconds (Warm-up)
#> Chain 3:                0.922 seconds (Sampling)
#> Chain 3:                1.059 seconds (Total)
#> Chain 3: 
#> 
#> SAMPLING FOR MODEL 'Model_srsc_per_image_target' NOW (CHAIN 4).
#> Chain 4: 
#> Chain 4: Gradient evaluation took 0 seconds
#> Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
#> Chain 4: Adjust your expectations accordingly!
#> Chain 4: 
#> Chain 4: 
#> Chain 4: Iteration:     1 / 11111 [  0%]  (Warmup)
#> Chain 4: Iteration:  1111 / 11111 [  9%]  (Warmup)
#> Chain 4: Iteration:  1112 / 11111 [ 10%]  (Sampling)
#> Chain 4: Iteration:  2222 / 11111 [ 19%]  (Sampling)
#> Chain 4: Iteration:  3333 / 11111 [ 29%]  (Sampling)
#> Chain 4: Iteration:  4444 / 11111 [ 39%]  (Sampling)
#> Chain 4: Iteration:  5555 / 11111 [ 49%]  (Sampling)
#> Chain 4: Iteration:  6666 / 11111 [ 59%]  (Sampling)
#> Chain 4: Iteration:  7777 / 11111 [ 69%]  (Sampling)
#> Chain 4: Iteration:  8888 / 11111 [ 79%]  (Sampling)
#> Chain 4: Iteration:  9999 / 11111 [ 89%]  (Sampling)
#> Chain 4: Iteration: 11110 / 11111 [ 99%]  (Sampling)
#> Chain 4: Iteration: 11111 / 11111 [100%]  (Sampling)
#> Chain 4: 
#> Chain 4:  Elapsed Time: 0.135 seconds (Warm-up)
#> Chain 4:                1.477 seconds (Sampling)
#> Chain 4:                1.612 seconds (Total)
#> Chain 4: 
#> 
#> Divergences:
#> 0 of 40000 iterations ended with a divergence.
#> 
#> Tree depth:
#> 0 of 40000 iterations saturated the maximum tree depth of 15.
#> 
#> Energy:
#> E-BFMI indicated no pathological behavior.
#> 
#> * One of the Stan developer "betanalpha" makes the check_rhat() and it says that
#> Rhat looks reasonable for all parameters.
#> * Very Good Convergence !!  
#> 
#> 
#>      R hat  < 1.01 
#> 
#>  
#> * Your model converged, that is: 
#> * Each R hat is less than or equal to 1.01 for all parameters
#> 
#> * We do not stop, since model converged.
#> Inference for Stan model: Model_srsc_per_image_target.
#> 4 chains, each with iter=11111; warmup=1111; thin=1; 
#> post-warmup draws per chain=10000, total post-warmup draws=40000.
#> 
#>         mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
#> w      -0.82    0.00 0.12  -1.06  -0.90  -0.82  -0.74  -0.58 24286    1
#> dz[1]   1.56    0.00 0.15   1.27   1.45   1.56   1.66   1.87 22096    1
#> dz[2]   1.63    0.00 0.31   1.10   1.41   1.61   1.83   2.31 20651    1
#> m       0.67    0.00 0.50  -0.34   0.34   0.67   1.00   1.65 22911    1
#> v       5.38    0.01 0.94   3.79   4.71   5.29   5.96   7.48 19354    1
#> p[1]    0.12    0.00 0.02   0.08   0.10   0.12   0.13   0.15 26791    1
#> p[2]    0.12    0.00 0.02   0.09   0.11   0.12   0.13   0.16 37310    1
#> p[3]    0.38    0.00 0.03   0.32   0.36   0.38   0.40   0.43 33877    1
#> l[1]    1.58    0.00 0.17   1.27   1.47   1.58   1.69   1.93 24291    1
#> l[2]    0.27    0.00 0.07   0.15   0.22   0.26   0.31   0.41 29749    1
#> l[3]    0.01    0.00 0.01   0.00   0.00   0.01   0.02   0.04 19632    1
#> dl[1]   1.32    0.00 0.15   1.04   1.21   1.31   1.41   1.63 21293    1
#> dl[2]   0.25    0.00 0.06   0.15   0.21   0.25   0.29   0.38 32030    1
#> dl[3]   0.01    0.00 0.01   0.00   0.00   0.01   0.02   0.04 19632    1
#> z[1]   -0.82    0.00 0.12  -1.06  -0.90  -0.82  -0.74  -0.58 24286    1
#> z[2]    0.74    0.00 0.16   0.42   0.63   0.74   0.85   1.07 30210    1
#> z[3]    2.38    0.00 0.37   1.72   2.12   2.36   2.61   3.16 19601    1
#> a       0.13    0.00 0.10  -0.06   0.06   0.13   0.19   0.33 25509    1
#> b       0.19    0.00 0.03   0.13   0.17   0.19   0.21   0.26 19463    1
#> A       0.55    0.00 0.04   0.48   0.52   0.55   0.58   0.63 25438    1
#> lp__  -14.60    0.01 1.61 -18.55 -15.43 -14.27 -13.41 -12.48 16242    1
#> 
#> Samples were drawn using NUTS(diag_e) at Thu Aug 01 20:02:41 2019.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).
#> 
#> * WAIC was caluculated,
#>  since log likelihoodis is a precise value, i.e., the traget += statement are used in the stan file.
#> 
#>  
#>  ----------------------
#>   WAIC =  32.805
#>  ----------------------
#>  * WAIC; Widely Applicable Information Criterion (Watanabe-Akaike Information Criterion)
#> 
#>  
#>  
#>  
#> 
#>   +*+  +*+  +*+                                  -*-  -*-  -*-
#>   ***  ***  ***    Estimates of an FROC model    ***  ***  -*-
#>   +*+  +*+  +*+                                  -*-  -*-  -*-
#> 
#> * In a single reader and a single modality case, the Bayesian model has three kind of parameter, that is, thresholds and mean and standard deviation of the signal distribution of the latent Gaussian random variable. From these parameter, the so-called Area under the curve (AUC) and hit rate for each confidence levels and false alarm rate for each confidence levels are calculated. In the following, the posterior means and 95% credible intervals are shown. I consider the most important parameter is the AUC. So, First, we will see the AUC.
#> 
#>  
#>  
#>  
#> 
#>   +*+  +*+  +*+                                  -*-  -*-  -*-
#>   ***  ***  ***              AUC                 ***  ***  -*-
#>   +*+  +*+  +*+                                  -*-  -*-  -*-
#> * Area under the curve, where "the curve" means the AFROC curve:
#> 
#> 
#> Parameter    posterior.mean   lowerCI   upperCI
#> ----------  ---------------  --------  --------
#> AUC                 0.55024   0.47722   0.62673
#> 
#>  
#>  
#>  
#> 
#>   +*+  +*+  +*+                                  -*-  -*-  -*-
#>   ***  ***  ***        Model Parameters          ***  ***  -*-
#>   +*+  +*+  +*+                                  -*-  -*-  -*-
#> 
#> * Thresholds of Gaussian Assumption(Binormal Assumption):
#> 
#> 
#> Parameter    posterior.mean    lowerCI    upperCI
#> ----------  ---------------  ---------  ---------
#> z[1]               -0.81738   -1.05660   -0.58009
#> z[2]                0.74314    0.42397    1.06790
#> z[3]                2.37720    1.71660    3.15980
#> 
#> 
#> 
#> * Differences of Thresholds of Gaussian Assumption:
#> 
#> 
#> Parameter    posterior.mean   lowerCI   upperCI
#> ----------  ---------------  --------  --------
#> dz[1]                1.5605    1.2701    1.8719
#> dz[2]                1.6341    1.1039    2.3075
#> 
#> 
#> 
#> * Mean and Standard Deviation (S.D.) of the signal distribution in the Gaussian (binormal) Assumption:
#> 
#> 
#> Parameter    posterior.mean   lowerCI   upperCI
#> ----------  ---------------  --------  --------
#> mean                0.66577   -0.3377    1.6543
#> S.D.                5.38140    3.7915    7.4756
#> 
#> * Note that the Mean and Standard Deviation (S.D.) of the noise distribution in the Gaussian (binormal) Assumption is 0 and 1, respectively.
#> 
#>  
#>  
#>  
#> 
#>   +*+  +*+  +*+                                  -*-  -*-  -*-
#>   ***  ***  ***            Hit rate              ***  ***  -*-
#>   +*+  +*+  +*+                                  -*-  -*-  -*-
#> 
#> 
#> 
#> * p[c] means the hit rate of the binomial distribution with confidence level c.
#> 
#> 
#> Parameter    Posterior.Mean    lowerCI   upperCI
#> ----------  ---------------  ---------  --------
#> p[1]                0.11621   0.083856   0.15314
#> p[2]                0.11978   0.086244   0.15824
#> p[3]                0.37567   0.318240   0.43475
#> 
#> * Let h(c) denote the number of hits with confidence level c,
#> then the above p[c] means that
#> 
#>             
#>                  h(c) ~ Binomial(p[c],NL) 
#> 
#> 
#> for each c = 1,2,...,3, where NL denotes the number of lesions and now it is 259.
#> 
#>  
#>  
#>  
#> 
#>   +*+  +*+  +*+                                  -*-  -*-  -*-
#>   ***  ***  ***        false alarm rate          ***  ***  ***
#>   +*+  +*+  +*+                                  -*-  -*-  -*-
#> 
#> 
#> 
#> * l[c] means the false alarm rate of the Poisson distribution with confidence level c.
#> 
#> 
#> Parameter    Posterior.Mean     lowerCI    upperCI
#> ----------  ---------------  ----------  ---------
#> l[1]               1.581400   1.2697000   1.928700
#> l[2]               0.265640   0.1540600   0.409160
#> l[3]               0.012688   0.0007897   0.043979
#> 
#> * Let f(c) denote the number of false alarms with confidence level c,
#> then the above table means that
#> 
#>             f(3) +  f(2) + ...+ f(c) ~ Poisson( l[c]*NI ) 
#> 
#> 
#> or equivalently,
#> 
#>              f(c) ~ Poisson(  ( l[c]-l[c+1] )*NI  ) 
#> 
#> 
#> 
#> for each c = 1,2,...,3, where NI denotes the number of images and now it is 57.
#> 
#> 
#>    AUC      lowerCI    upperCI 
#> ---------  ---------  ---------
#>  0.55024    0.47722    0.62673
#> 
#> * size of the return value:
#> 11.50156Mb

                  
                  
                  
                  
                  
                  

#  validation of fit via alculation of p -value of the chi square goodness of fit, which is 
#  calculated by integrating with  predictive posterior measure.
                  
                  
            BayesianFROC::p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit(
              
              fit,
              
#        In an output, a table is shown only head part.
              head.only = TRUE
              )
#> 
#> 
#> name         chisq.vector.for.observed.data   chisq.vector.for.replicated.data  replication.vs.observed 
#> ----------  -------------------------------  ---------------------------------  ------------------------
#> the 1-th                              2.580                              12.60  TRUE                    
#> the 2-th                              7.490                              11.30  TRUE                    
#> the 3-th                             11.200                              18.70  TRUE                    
#> the 4-th                             17.500                               9.94  FALSE                   
#> the 5-th                              1.300                              12.20  TRUE                    
#> the 6-th                              9.530                              14.80  TRUE                    
#> the 7-th                              0.871                              13.30  TRUE                    
#> the 8-th                              7.730                              16.30  TRUE                    
#> the 9-th                             16.700                              11.90  FALSE                   
#> the 10-th                             7.530                              10.40  TRUE
#> 
#> * We show the head part of data, i.e., first 10 rows  are shown.
#> 
#> * To show all rows of data, use p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit(StanS4class = Your fitted model object, head.only = FALSE)
#> 
#> *  Note that the posterior predictive p value is a rate of TRUE in the right column in the above table.
#> 
#> *  The presence of more TRUE indicates that our goodness of fitting is better.
#> 
#> *  Smaller p value indicates goodness of fit is not better.
#>  The p value of the posterior predictive measure for the chi square discrepancy. 
#>                                                                         0.914625
                   
                   # The auhor thinks it is not correctly programmed, so it needs validaton of programing

                                     
                  
```

# Jafroc data

If you already have Jafroc data, then use the code;

``` r
     dataList <- convertFromJafroc(
                                  No.of.Modalities =5,
                                  No.of.readers    =4,
                                  No.of.confidence.levels = 5
                                    )
```

where you should specify the number of modalities, readers, confidence
levels.

#### The FROC curve

Using the fitted model object `fit` of class `stanfitExtended`, we can
draw the FROC curve (or AFROC curve) as follows;

<!-- !["X-ray of my teeth!!  Let's study together with me !! :-D "](C:/Users/81909/AppData/Local/Temp/RtmpslMWi8/temp_libpath47d4ecf2b66/BayesianFROC/image/FROCcurve.jpeg) -->

``` r
# new.imaging.device = FALSE  is used to include the output image 
# in this README file, so I recommand new.imaging.device = TRUE
BayesianFROC::DrawCurves(fit,
                         new.imaging.device = FALSE)
#> 
#> * R hat statistics criterion is OK, Let's draw curves.
#> 
#>  
#>  ----------------------
#>   WAIC =  32.8
#>  ----------------------
#>  * WAIC; Widely Applicable Information Criterion (Watanabe-Akaike Information Criterion)
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

#### If you want to draw the curve in white background, then use the followings

``` r
# new.imaging.device = FALSE  is used to include the output image 
# in this README file, so I recommand new.imaging.device = TRUE

BayesianFROC::DrawCurves(fit,
                         Colour = FALSE,
                         new.imaging.device = FALSE)
#> 
#> * R hat statistics criterion is OK, Let's draw curves.
#> 
#>  
#>  ----------------------
#>   WAIC =  32.8
#>  ----------------------
#>  * WAIC; Widely Applicable Information Criterion (Watanabe-Akaike Information Criterion)
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

Where the circles means False Positive Fractions (FPFs) and True
Positive Fractions (TPFs). The curve is FROC curve. FROC curve thorough
exactly the expected points of FPFs and TPFs. Thus we can intuitively
see the goodness of fit by comparing the circles and curves. Ha,…
someone reads this boring vignettes? My right arm ache bothering me for
20 months. Ha,… I want to die.

# Latent Distributions

Hit rates are determined the areas of signal Gaussian between
thresholds,

and false alarm rate are defined by the areas of differential
logarithmic cumulative Gaussian between thresholds.

## False rate

``` r
# new.imaging.device = FALSE  is used to include the output image 
# in this README file, so I recommand new.imaging.device = TRUE

BayesianFROC::draw_bi_normal_version_UP(
    fit,new.imaging.device = F,
    dark_theme = T,
    hit.rate = F,
    false.alarm.rate = T,
    both.hit.and.false.rate = F)
#> 
#> .......................................................
#>           Visualization of Latent distributions
#> '''''''''''''''''''''''''''''''''''''''''''''''''''''''
#> 
#> * The mean of the signal distribution:0.666
#> 
#> * The standard deviation of the signal distribution:5.38
#> 
#> *The vertical lines in the plot mean the estimated thresholds. Each estimates is the posterior mean (expected a posterior estimates (EAPs))
#> 
#> * thresholds:-0.817 < 0.743 < 2.38
#> 
#> * Green curve indicates a signal distribution.
#> 
#> * False alarm rate is exactly the area between two thresholds in the differential logarithmic cumulative Gaussian distribution.The area is intuitively indicate the false alarm rate, thus the author color such areas.
#> False alarm rate per image (or lesion) means the differences of parameter lambda
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

    #> 
    #> * False alarm rate per image: 1.31576 0.252952 0.012688

## Hit rate

``` r
# new.imaging.device = FALSE  is used to include the output image 
# in this README file, so I recommand new.imaging.device = TRUE

BayesianFROC::draw_bi_normal_version_UP(
    fit,new.imaging.device = F,
    dark_theme = T,
    hit.rate = T,
    false.alarm.rate = F,
    both.hit.and.false.rate = F)
#> 
#> .......................................................
#>           Visualization of Latent distributions
#> '''''''''''''''''''''''''''''''''''''''''''''''''''''''
#> 
#> * The mean of the signal distribution:0.666
#> 
#> * The standard deviation of the signal distribution:5.38
#> 
#> *The vertical lines in the plot mean the estimated thresholds. Each estimates is the posterior mean (expected a posterior estimates (EAPs))
#> 
#> * thresholds:-0.817 < 0.743 < 2.38
#> 
#> * Green curve indicates a signal distribution.
#> 
#> * False alarm rate is exactly the area between two thresholds in the differential logarithmic cumulative Gaussian distribution.The area is intuitively indicate the false alarm rate, thus the author color such areas.
#> False alarm rate per image (or lesion) means the differences of parameter lambda
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

    #> 
    #> * False alarm rate per image: 1.31576 0.252952 0.012688

One will see that the bi normal assumption is wrong in the FROC context,
and instead of bi normal assumption, we use two latent distributions,
one is Gaussian for signal and another is the differential logarithmic
Gaussian introduced first by the author of this package. For details,
see vignettes of this package.

## Modality Comparison

By fitting hierarchical Bayesian model, we get the characteristics to
compare modality.

Using the data object named `BayesianFROC::dataList.Chakra.Web`
representing multiple modality data, we will fit the model to data by
the following R script. For letting the running time be short, we take
small MCMC iteration, that is, `ite =222` which is too small to obtain
reliable estimates. I think it should be `ite =33333` for actual data
analysis or compatible result with *Jafroc*.

The author try to remove `eval=FALSE`, but it cause stopping of knitr,
so I can not include the following code. The following code sometimes
crash R session, so,… it is heavy for README file??

``` r


#0) To avoid the following error I have to attach the Rcpp. I do not know why such error occur withou Rcpp.
#Error in do.call(rbind,sampler_params) :second argument must be a list Calles:<Anonymous>...get_divergent_iterations ->sampler_param_vector =. do.call Execution halted

library(Rcpp)  # This code can remove the above unknown error, if someone know why the error occur, please tell me.


library(BayesianFROC)



dataList <- dataList.Chakra.Web

fitt <- BayesianFROC::fit_Bayesian_FROC(
  
  # data of multiple reader and multiple case (modalities)
 dataList =   dataList,
  
  # iteration of MCMC
  ite = 1111 # Should be ite = 33333
 )
```

Now, we obtain the fitted model object named `fit` which is an S4 object
of class `stanfitExtended` inherited from `stanfit` of the ***rstan***
package..

# Transform of S4 Class to apply other packages

To apply the functions of other package such as **rstan** or **ggmcmc**,
…, etc in which there are functions for object of class `stanfit`, e.g.,
`rstan::stan_trace()`,
`rstan::stan_dens()`,`rstan::check_hmc_diagnostics()`,…etc, we have to
change the class of the fitted model object by the following manner:

``` r
 fit.stan <- methods::as(fit, "stanfit")
```

Then the above object `fit.stan` is an object of the class `stanfit` and
thus we can apply the function of rstan package as
`rstan::stan_dens(fit.stan)`.

### Prepare pipe operator (redundant)

``` r
# First, get pipe operator
`%>%` <- utils::getFromNamespace("%>%", "magrittr")
```

### Change the class to `stanfit`

``` r
# Change the class from stanfitExtended to stanfit
fit.stan <- methods::as(fit,"stanfit")
```

#### trace plot for object of class `stanfit`

``` r

# Change the class from stanfitExtended to stanfit
#fit.stan <- methods::as(fit,"stanfit")


# Plot about MCMC samples of paremeter name "A", reperesenting AUC
ggmcmc::ggs(fit.stan) %>% ggmcmc::ggs_traceplot(family  = "A")
#> Registered S3 method overwritten by 'GGally':
#>   method from   
#>   +.gg   ggplot2
```

<img src="man/figures/README-unnamed-chunk-16-1.png" width="100%" />

#### posterior density of parameter `A` stored in an object of class `stanfit`

The following plot indicates that *maximal* *posterior* *estimator*
(MAP) is very unstable in each chain in this iteration. By drawing more
samples, it become stable?

``` r

# Change the class from stanfitExtended to stanfit
#fit.stan <- methods::as(fit,"stanfit"
                        
                        
ggmcmc::ggs(fit.stan) %>% ggmcmc::ggs_density(family    = "A")
```

<img src="man/figures/README-unnamed-chunk-17-1.png" width="100%" />

#### Auto correlation for an object of class `stanfit`

``` r


# Change the class from stanfitExtended to stanfit
# fit.stan <- methods::as(fit,"stanfit")


ggmcmc::ggs(fit.stan) %>% ggmcmc::ggs_autocorrelation(family    = "A")
```

<img src="man/figures/README-unnamed-chunk-18-1.png" width="100%" />

How `ggmcmc` painted using these colors was amazing\!

package **shinystan**

For fitted model object `fit.stan` of class `stanfit`, there is a GUI
viewer

``` r

# Change the class from stanfitExtended to stanfit
fit.stan <- methods::as(fit,"stanfit")


shinystan::launch_shinystan(fit.stan)
```

# SBC

### Validation of model via Simulation Based Calibration (SBC)

Talts, S., Betancourt, M., Simpson, D., Vehtari, A., and Gelman, A.
(2018). Validating Bayesian Inference Algorithms with Simulation-Based
Calibration. arXiv preprint arXiv:1804.06788

``` r
BayesianFROC::Simulation_Based_Calibration_single_reader_single_modality_via_rstan_sbc()
#> 
#> 
#> 
#> 
#> * We will obtain rank statistics by the SBC algorithm.
#>   If the histgrams are uniformly distributed,
#>   then it indicates that
#>   the author's Bayesian model is good!
#>   I love you!
#> 
#> 
#> 
#> * Now, The Hamiltonian Monte Carlo Simulation is running ...
#> 
#>   Please wait ...
#> 
```

<img src="man/figures/README-unnamed-chunk-20-1.png" width="100%" />

    #> 20 chains had divergent transitions after warmup
    #> there were a total of 100 divergent transitions across all chains

The author thinks the Reademe file is redundant? or my explanation is
redundant? :’-D or my existence is … redundant? redundant?? I have to
vanish, like vanishing theorem of cohomology. My curvature is not so
ample, I have to vanish.

Nowadays, many people is ignoring the non employed poor and pure person,
that is me\! employ me\!

## Acknowledge

The author is grateful for the many people, Stan developers and stack
overflows known? or unknown people. Their fruitful answer helps my
package development, Thanks\! Thanks a lot\!
