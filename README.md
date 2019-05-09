<!-- README.md is generated from README.Rmd. Please edit that file -->
BayesianFROC
============

The goal of this package (BayesianFROC) is the following:

-   1.  Fit the statistical model for FROC data of Single Reader and Single Modality case.

-   1.  Fit the statistical model for FROC data of Multiple Reader and Multiple Modality (MRMC).

2.1) Compare the modalities by the area under the alternative ROC curve.

FROC = Free-response Receiver Operating Characteristic.

Radiologists need to compare the detection ability of lesions in radiographs taken by different imaging methods, such as MRI, CT, PET ,... . These imaging methods are called modality.

The following example shows how to use this package for data of the case of a single reader and single modality.

``` r

#================The first example======================================
library(Rcpp)
             #((Primitive way)).
 #1) Build the data for singler reader and single modality  case.

  dat <- list(c=c(3,2,1),
              h=c(97,32,31),
              f=c(1,14,74),
              NL=259,
              NI=57,
              C=3)

##where, c denotes confidence level,
#        h denotes number of hits for each confidence level,
#        f denotes number of false alarms for each confidence level,
#        NL denotes number of lesions,
#        NI denotes number of images,


 #2) Fit the FROC model.
   #Since dataset named dat are single reader and single modality,
   #the function build the such model by running the following code.

 fit <- BayesianFROC::fit_Bayesian_FROC(dat)
#> Study Design: srsc case
#> False Positive Fraction is calculated
#>  per image.
#> 
#> * Number of Lesions: 259
#> 
#> * Number of Images : 57
#> 
#> 
#>  Confidence.Level   False.Positives   True.Positives
#> -----------------  ----------------  ---------------
#>                 3                 1               97
#>                 2                14               32
#>                 1                74               31
#> 
#> SAMPLING FOR MODEL 'Model_srsc_per_image_target' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 0 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:    1 / 10000 [  0%]  (Warmup)
#> Chain 1: Iteration: 1000 / 10000 [ 10%]  (Warmup)
#> Chain 1: Iteration: 2000 / 10000 [ 20%]  (Warmup)
#> Chain 1: Iteration: 2001 / 10000 [ 20%]  (Sampling)
#> Chain 1: Iteration: 3000 / 10000 [ 30%]  (Sampling)
#> Chain 1: Iteration: 4000 / 10000 [ 40%]  (Sampling)
#> Chain 1: Iteration: 5000 / 10000 [ 50%]  (Sampling)
#> Chain 1: Iteration: 6000 / 10000 [ 60%]  (Sampling)
#> Chain 1: Iteration: 7000 / 10000 [ 70%]  (Sampling)
#> Chain 1: Iteration: 8000 / 10000 [ 80%]  (Sampling)
#> Chain 1: Iteration: 9000 / 10000 [ 90%]  (Sampling)
#> Chain 1: Iteration: 10000 / 10000 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 0.322 seconds (Warm-up)
#> Chain 1:                1.709 seconds (Sampling)
#> Chain 1:                2.031 seconds (Total)
#> Chain 1: 
#> 
#> Divergences:
#> 0 of 8000 iterations ended with a divergence.
#> 
#> Tree depth:
#> 0 of 8000 iterations saturated the maximum tree depth of 15.
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
#> 1 chains, each with iter=10000; warmup=2000; thin=1; 
#> post-warmup draws per chain=8000, total post-warmup draws=8000.
#> 
#>         mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
#> w      -0.82    0.00 0.12  -1.06  -0.90  -0.82  -0.74  -0.59  4371    1
#> dz[1]   1.56    0.00 0.15   1.28   1.46   1.56   1.66   1.88  3995    1
#> dz[2]   1.63    0.00 0.30   1.12   1.41   1.60   1.81   2.30  4162    1
#> m       0.65    0.01 0.50  -0.35   0.33   0.66   0.98   1.63  5268    1
#> v       5.37    0.02 0.94   3.78   4.71   5.28   5.94   7.42  3192    1
#> p[1]    0.12    0.00 0.02   0.08   0.10   0.12   0.13   0.15  4648    1
#> p[2]    0.12    0.00 0.02   0.09   0.11   0.12   0.13   0.16  7725    1
#> p[3]    0.38    0.00 0.03   0.32   0.36   0.37   0.39   0.43  7553    1
#> l[1]    1.59    0.00 0.16   1.28   1.48   1.58   1.69   1.93  4317    1
#> l[2]    0.27    0.00 0.06   0.15   0.22   0.26   0.31   0.40  5869    1
#> l[3]    0.01    0.00 0.01   0.00   0.00   0.01   0.02   0.04  3893    1
#> dl[1]   1.32    0.00 0.15   1.05   1.22   1.31   1.42   1.64  3703    1
#> dl[2]   0.25    0.00 0.06   0.15   0.21   0.25   0.29   0.37  6071    1
#> dl[3]   0.01    0.00 0.01   0.00   0.00   0.01   0.02   0.04  3893    1
#> z[1]   -0.82    0.00 0.12  -1.06  -0.90  -0.82  -0.74  -0.59  4371    1
#> z[2]    0.74    0.00 0.16   0.44   0.63   0.74   0.85   1.07  5911    1
#> z[3]    2.37    0.01 0.36   1.74   2.12   2.35   2.60   3.15  4058    1
#> a       0.13    0.00 0.10  -0.06   0.06   0.12   0.19   0.33  5473    1
#> b       0.19    0.00 0.03   0.13   0.17   0.19   0.21   0.26  3437    1
#> A       0.55    0.00 0.04   0.48   0.52   0.55   0.57   0.63  5463    1
#> lp__  -14.53    0.03 1.58 -18.50 -15.37 -14.17 -13.36 -12.44  3228    1
#> 
#> Samples were drawn using NUTS(diag_e) at Wed Apr 24 00:00:32 2019.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).
#> 
#> * WAIC was caluculated,
#>  since log likelihoodis is a precise value, i.e., the traget += statement are used in the stan file.
#> 
#>  
#>  ----------------------
#>   WAIC =  32.523
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
#> AUC                  0.5494   0.47706   0.62617
#> 
#>  
#>  
#>  
#> 
#>   +*+  +*+  +*+                                  -*-  -*-  -*-
#>   ***  ***  ***       Binormal Assumption        ***  ***  -*-
#>   +*+  +*+  +*+                                  -*-  -*-  -*-
#> 
#> * Thresholds of Gaussian Assumption(Binormal Assumption):
#> 
#> 
#> Parameter    posterior.mean    lowerCI    upperCI
#> ----------  ---------------  ---------  ---------
#> z[1]               -0.82209   -1.06030   -0.59206
#> z[2]                0.74085    0.43868    1.06640
#> z[3]                2.37080    1.73760    3.15440
#> 
#> 
#> 
#> * Differences of Thresholds of Gaussian Assumption:
#> 
#> 
#> Parameter    posterior.mean   lowerCI   upperCI
#> ----------  ---------------  --------  --------
#> dz[1]                1.5629    1.2780    1.8796
#> dz[2]                1.6299    1.1173    2.3019
#> 
#> 
#> 
#> * Mean and Standard Deviation (S.D.) of the signal distribution in the Gaussian (binormal) Assumption:
#> 
#> 
#> Parameter    posterior.mean    lowerCI   upperCI
#> ----------  ---------------  ---------  --------
#> mean                0.65361   -0.34736    1.6298
#> S.D.                5.37330    3.78490    7.4184
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
#> p[1]                0.11654   0.084957   0.15382
#> p[2]                0.11964   0.086690   0.15794
#> p[3]                0.37506   0.317940   0.43438
#> 
#> * Let h[c] denote the number of hits with confidence level c,
#> then the above p[c] means that
#> 
#>             
#>                  h[c] ~ Binomial(p[c],NL) 
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
#> Parameter    Posterior.Mean      lowerCI    upperCI
#> ----------  ---------------  -----------  ---------
#> l[1]               1.587700   1.28410000   1.934400
#> l[2]               0.266220   0.15446000   0.401140
#> l[3]               0.012725   0.00080445   0.042011
#> 
#> * Let l[c] denote the number of false alarms with confidence level c,
#> then the above table means that
#> 
#>             f[3] + ...+ f[c] ~ Poisson( l[c]*NI ) 
#> 
#> 
#> or equivalently,
#> 
#>              f[c] ~ Poisson(  ( l[c]-l[c+1] )*NI  ) 
#> 
#> 
#> 
#> for each c = 1,2,...,3, where NI denotes the number of images and now it is 57.
#> 
#> * Note that if your samples from the Hamiltonian MonteCarlo Method is small, then the AUCs is not precise values. You should calculate MCMC samples in suffiecietly large numbers and also carefully check the convergence criterion by R hat statistics. Small MCMC sampling gives you the AUCs which are unreliable. I recommand that the MCMC samples is at least 30000 MCMC samples for reliable estimates.
#> 
#> 
#>   AUC      lowerCI    upperCI 
#> --------  ---------  ---------
#>  0.5494    0.47706    0.62617
#> 
#> * size of the return value:
#> 2.924608Mb



# (( REMARK ))
#Note that users not allowed to write the above data as follows:

#  dat <- list(c=c(1,2,3),h=c(31,32,97),f=c(74,14,1),NL=259,NI=57,C=3)

#This package is very rigid format, so please be sure that your format is
#exactly same to the data in this package.
#More precisely, the confidence level vector should be denoted rep(C:1) (Not rep(1:C)).
```

Next, we shall show the comparison of modality.

Using the data object named `BayesianFROC::dataList.Chakra.Web` contained in this package, we will fit the model by the following code. For letting the running time be short, we take `ite =222` which is too small to obtain reliable estimates. I think it should be `ite =33333` for actual data analysis.

``` r
fit <- BayesianFROC::fit_Bayesian_FROC(BayesianFROC::dataList.Chakra.Web,ite = 222)
#> Study Design: MRMC case
#> False Positive Fraction is calculated
#> We calculate false alarm rate by per lesion in MRMC cases.  Per image are not availble in MRMC case.
#> 
#> * Your iteration is very small. You should raise the number of ite
#> 
#> * Warning: The number of iteration is very small. Please raise the variable, (e.g. ite = 30000) to draw more large samples in the Hamiltonian Monte Carlo Simulation.
#> 
#> * Number of Lesions: 142
#> 
#> * Number of Images : . (This is not used for estimates).
#> 
#> 
#>  ModalityID   ReaderID   Confidence.Level   False.Positives   True.Positives
#> -----------  ---------  -----------------  ----------------  ---------------
#>           1          1                  5                 0               50
#>           1          1                  4                 4               30
#>           1          1                  3                20               11
#>           1          1                  2                29                5
#>           1          1                  1                21                1
#>           1          2                  5                 0               15
#>           1          2                  4                 0               29
#>           1          2                  3                 6               29
#>           1          2                  2                15                1
#>           1          2                  1                22                0
#>           1          3                  5                 1               39
#>           1          3                  4                15               31
#>           1          3                  3                18                8
#>           1          3                  2                31               10
#>           1          3                  1                19                3
#>           1          4                  5                 1               10
#>           1          4                  4                 2                8
#>           1          4                  3                 4               25
#>           1          4                  2                16               45
#>           1          4                  1                17               14
#>           2          1                  5                 1               52
#>           2          1                  4                 1               25
#>           2          1                  3                21               13
#>           2          1                  2                24                4
#>           2          1                  1                23                1
#>           2          2                  5                 1               27
#>           2          2                  4                 1               28
#>           2          2                  3                 5               29
#>           2          2                  2                30                1
#>           2          2                  1                40                0
#>           2          3                  5                 2               53
#>           2          3                  4                19               29
#>           2          3                  3                31               13
#>           2          3                  2                56                2
#>           2          3                  1                42                4
#>           2          4                  5                 2                9
#>           2          4                  4                 0               16
#>           2          4                  3                 2               22
#>           2          4                  2                30               43
#>           2          4                  1                32               14
#>           3          1                  5                 1               43
#>           3          1                  4                 7               29
#>           3          1                  3                13               11
#>           3          1                  2                28                6
#>           3          1                  1                19                0
#>           3          2                  5                 0               18
#>           3          2                  4                 1               29
#>           3          2                  3                 7               21
#>           3          2                  2                 7                0
#>           3          2                  1                31                0
#>           3          3                  5                 7               43
#>           3          3                  4                15               29
#>           3          3                  3                28                6
#>           3          3                  2                41                7
#>           3          3                  1                 9                1
#>           3          4                  5                 0               10
#>           3          4                  4                 2               14
#>           3          4                  3                 5               19
#>           3          4                  2                24               32
#>           3          4                  1                31               23
#>           4          1                  5                 1               61
#>           4          1                  4                 4               19
#>           4          1                  3                18               12
#>           4          1                  2                21                9
#>           4          1                  1                23                3
#>           4          2                  5                 1               16
#>           4          2                  4                 1               29
#>           4          2                  3                 0               34
#>           4          2                  2                11                1
#>           4          2                  1                35                0
#>           4          3                  5                 6               52
#>           4          3                  4                14               29
#>           4          3                  3                37               10
#>           4          3                  2                36                4
#>           4          3                  1                18                3
#>           4          4                  5                 0               10
#>           4          4                  4                 2               16
#>           4          4                  3                 4               23
#>           4          4                  2                18               43
#>           4          4                  1                25               15
#>           5          1                  5                 0               35
#>           5          1                  4                 2               29
#>           5          1                  3                19               18
#>           5          1                  2                23                9
#>           5          1                  1                18                0
#>           5          2                  5                 0               17
#>           5          2                  4                 2               27
#>           5          2                  3                 6               24
#>           5          2                  2                10                0
#>           5          2                  1                30                0
#>           5          3                  5                 2               34
#>           5          3                  4                25               33
#>           5          3                  3                40                7
#>           5          3                  2                29               13
#>           5          3                  1                24                2
#>           5          4                  5                 1               12
#>           5          4                  4                 1               16
#>           5          4                  3                 4               21
#>           5          4                  2                24               35
#>           5          4                  1                32               15
#> 
#> SAMPLING FOR MODEL 'Model_Hiera_TargetFormulation' NOW (CHAIN 1).
#> Chain 1: Rejecting initial value:
#> Chain 1:   Log probability evaluates to log(0), i.e. negative infinity.
#> Chain 1:   Stan can't start sampling from this initial value.
#> Chain 1: Rejecting initial value:
#> Chain 1:   Log probability evaluates to log(0), i.e. negative infinity.
#> Chain 1:   Stan can't start sampling from this initial value.
#> Chain 1: Rejecting initial value:
#> Chain 1:   Log probability evaluates to log(0), i.e. negative infinity.
#> Chain 1:   Stan can't start sampling from this initial value.
#> Chain 1: Rejecting initial value:
#> Chain 1:   Log probability evaluates to log(0), i.e. negative infinity.
#> Chain 1:   Stan can't start sampling from this initial value.
#> Chain 1: Rejecting initial value:
#> Chain 1:   Log probability evaluates to log(0), i.e. negative infinity.
#> Chain 1:   Stan can't start sampling from this initial value.
#> Chain 1: Rejecting initial value:
#> Chain 1:   Log probability evaluates to log(0), i.e. negative infinity.
#> Chain 1:   Stan can't start sampling from this initial value.
#> Chain 1: Rejecting initial value:
#> Chain 1:   Log probability evaluates to log(0), i.e. negative infinity.
#> Chain 1:   Stan can't start sampling from this initial value.
#> Chain 1: Rejecting initial value:
#> Chain 1:   Log probability evaluates to log(0), i.e. negative infinity.
#> Chain 1:   Stan can't start sampling from this initial value.
#> Chain 1: Rejecting initial value:
#> Chain 1:   Log probability evaluates to log(0), i.e. negative infinity.
#> Chain 1:   Stan can't start sampling from this initial value.
#> Chain 1: Rejecting initial value:
#> Chain 1:   Log probability evaluates to log(0), i.e. negative infinity.
#> Chain 1:   Stan can't start sampling from this initial value.
#> Chain 1: 
#> Chain 1: Gradient evaluation took 0 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: WARNING: There aren't enough warmup iterations to fit the
#> Chain 1:          three stages of adaptation as currently configured.
#> Chain 1:          Reducing each adaptation stage to 15%/75%/10% of
#> Chain 1:          the given number of warmup iterations:
#> Chain 1:            init_buffer = 6
#> Chain 1:            adapt_window = 34
#> Chain 1:            term_buffer = 4
#> Chain 1: 
#> Chain 1: Iteration:   1 / 222 [  0%]  (Warmup)
#> Chain 1: Iteration:  22 / 222 [  9%]  (Warmup)
#> Chain 1: Iteration:  44 / 222 [ 19%]  (Warmup)
#> Chain 1: Iteration:  45 / 222 [ 20%]  (Sampling)
#> Chain 1: Iteration:  66 / 222 [ 29%]  (Sampling)
#> Chain 1: Iteration:  88 / 222 [ 39%]  (Sampling)
#> Chain 1: Iteration: 110 / 222 [ 49%]  (Sampling)
#> Chain 1: Iteration: 132 / 222 [ 59%]  (Sampling)
#> Chain 1: Iteration: 154 / 222 [ 69%]  (Sampling)
#> Chain 1: Iteration: 176 / 222 [ 79%]  (Sampling)
#> Chain 1: Iteration: 198 / 222 [ 89%]  (Sampling)
#> Chain 1: Iteration: 220 / 222 [ 99%]  (Sampling)
#> Chain 1: Iteration: 222 / 222 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 3.336 seconds (Warm-up)
#> Chain 1:                10.917 seconds (Sampling)
#> Chain 1:                14.253 seconds (Total)
#> Chain 1:
#> * Almost Convergence !! 
#> * You need some effort to reduce R hat !! 
#>  
#>     1.10 <  R hat < 1.50   
#>   
#> * Your R hat is at most 1.50 for all parameters, but greater than 1.10 for some parameter. 
#> * Raising the number of [ite] more than now (ite = 222 ) you might get convergence.
#> 
#> * We do not stop, since model cannot be said not converged.
#> 
#>  * Using this return value which is S4 class generated by rstan::stan and another function in this package, you can draw FROC and AFROC curves.
#> 
#>  * Using this return value, you can apply functions in the package rstan, e.g., rstan::traceplot().
#> 
#> * WAIC was caluculated, since log likelihoodis is a precise value, i.e., the traget += statement are used in the stan file.
#> 
#>  
#>  ----------------------
#>   WAIC =  Inf
#>  ----------------------
#>  * WAIC; Widely Applicable Information Criterion (Watanabe-Akaike Information Criterion)
#> 
#> * We de not draw the FROC and AFROC curves.
#> 
#> * Metadata to draw the curves are callculating ...
#> 
#> 
#> 
#>  ---- Summarizing Tables ----
#> 
#> * Probablity of the event that the AUC of the  first column Modality is greater than that of the second column modality.
#> 
#> * For example, the first row in the first table implies that Probablity of the event that the AUC of the  first  Modality is greater than that of the second modality is equal to  0.320224719101124  with the posterior mean of the difference of the AUC of the first column minus that of second column is -0.0249038738814083 .
#> 
#> 
#> first.modality   second.modality       Prob       Diff
#> ---------------  ----------------  --------  ---------
#> 1                2                  0.32022   -0.02490
#> 1                3                  0.79775    0.03807
#> 1                4                  0.33708   -0.02293
#> 1                5                  0.66854    0.02332
#> 2                3                  0.92135    0.06298
#> 2                4                  0.55618    0.00197
#> 2                5                  0.86517    0.04823
#> 3                4                  0.07865   -0.06101
#> 3                5                  0.35955   -0.01475
#> 4                5                  0.86517    0.04626
#> 
#> 
#> first.modality   second.modality       Prob       Diff
#> ---------------  ----------------  --------  ---------
#> 2                1                  0.67978    0.02490
#> 3                1                  0.20225   -0.03807
#> 4                1                  0.66292    0.02293
#> 5                1                  0.33146   -0.02332
#> 3                2                  0.07865   -0.06298
#> 4                2                  0.44382   -0.00197
#> 5                2                  0.13483   -0.04823
#> 4                3                  0.92135    0.06101
#> 5                3                  0.64045    0.01475
#> 5                4                  0.13483   -0.04626
#> 
#> * Note that if your samples from the Hamiltonian MonteCarlo Method is small, then the AUCs is not precise values. You should calculate MCMC samples in suffiecietly large numbers and also carefully check the convergence criterion by R hat statistics. Small MCMC sampling gives you the AUCs which are unreliable. I recommand that the MCMC samples is at least 30000 MCMC samples for reliable estimates.
#> 
#> ------------------------------
#> 
#> * The following table shows the AUCs for each modality, that is, the area under the AFROC curves.
#> 
#> * The following table shows, from the left, modality ID, expected a posterior estimates and upper and lower credible interbals for AUCs.
#> 
#> 
#>  Modality    AUC.EAP    AUC.CI.lower    AUC.CI.upper 
#> ----------  ---------  --------------  --------------
#>     1        0.68292      0.61496         0.75285    
#>     2        0.70782      0.64527         0.77592    
#>     3        0.64485      0.58416         0.70471    
#>     4        0.70585      0.65019         0.77746    
#>     5        0.65959      0.59259         0.72114
#> 
#> ------------------------------
#> In the Figure of AUCs, the two intervals are shown for each modality, i.e., /n
#> ci_level: 0.8 (80% intervals)
#> outer_level: 0.95 (95% intervals)
#> 
#> * One of the Stan developer "betanalpha" makes the check_rhat() and it says that
#> 
#> * Rhat for parameter hyper_v[2] is 1.17310421074518 .
#> * Rhat for parameter lp__ is 1.15180743551789 .
#> * Inequality [ Rhat > 1.1 ] indicates that the chains very likely have not mixed
#> Divergences:
#> 0 of 178 iterations ended with a divergence.
#> 
#> Tree depth:
#> 0 of 178 iterations saturated the maximum tree depth of 15.
#> 
#> Energy:
#> E-BFMI indicated no pathological behavior.
#> 
#> * size of the return value:
#> 16.978344Mb
```

Now, we obtain the *R* object named `fit`. the object `fit` is an S4 object whose class is named `stanfit.Extended` and defined in this packaga. The S4 class `stanfit.Extended` is an inherited class from the S4 class of the rstan package, that is the S4 class `stanfit.Extended` is an inherited (extended) class of the `stanfit`. So, `stanfit.Extended` is obtained by adding new slots to retain data which is used to draw the FROC curve and AFROC curve, ...etc. e

Thus to apply the functions in the rstan package, we have to change the class by the following manner:

``` r
 fit.stan <- methods::as(fit, "stanfit")
```

Then the above object `fit.stan` is an object of the `stanfit` and thus we can apply the function of rstan package as`rstan::stan_dens(fit.stan)`.

Note that the above dinsity is far from Gaussian since the number of iterations of the Hamiltonian Monte Carlo Method is taken as `ite =222` which is too small.
