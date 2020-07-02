
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

  <!-- background-color:#EEEEEE;     -->
  
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

Available from [CRAN](https://CRAN.R-project.org) .

``` r

              install.packages("BayesianFROC")
              
              
#     Pleaes execute it from the R console (not the R studio console), which installs the released version of `BayesianFROC`
```

## Shiny Based GUIs

A single reader and a single modality (SRSM) case

``` r

                           library(BayesianFROC)
                           BayesianFROC::fit_GUI_Shiny()
```

To fit a model to the SRSM data, `fit_a_model_to()` would be adequate
for the purpose.

Multiple Readers and Multiple Modalities Case

``` r

                           library(BayesianFROC)
                           BayesianFROC::fit_GUI_Shiny_MRMC()
```

### For details

  - See [vignette](https://cran.r-project.org/package=BayesianFROC)
    (Because package size restriction (\< 5Mb), it is omitted.)

  - A pre-print of the author: Generative Models for Free-response
    Receiver Operating Characteristic Analysis

### Goal of this package `BayesianFROC`

**Comparison** of imaging *modality*. In some context, *modality* is
imaging methods: *MRI*, *CT*, *PET*,…etc, and the another context, if
images are taken for treatment (case) group and untreatment (or another
treatment) (control) group, then *modality* means *efficacy of
treatment*.

  - Fit a model to data whose types are the following:
      - Single Reader and Single Modality case.
      - Multiple Reader and Multiple modality case (MRMC)
  - ***Comparison*** of the *modalities* by *AUC* (the area under *the
    AFROC curve*).

## Example

  - Data
  - make data from Jafroc data
  - make data from scratch
  - Fit
      - draw FROC curves using a fitted model object
      - Calculates AUCs
  - Validation
      - goodness of fit
      - posterior predictive p value (PPP)
      - SBC
      - comparison of truth using synthesized datasets from a fixed
        model as a truth

An example dataset to be fitted a model

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
 library(BayesianFROC)


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
                     cha = 1,
            
            # The number of MCMC samples for each chains                         
                    ite  = 555,
                    
            # The number of warming up of MCMC simulation for each chains           
                     war = 111,
            
            # Show verbose summary and MCMC process
                 summary = TRUE
                                                         )

                  
                  
                  
                  
                  
                  

#  validation of fit via alculation of p -value of the chi square goodness of fit, which is 
#  calculated by integrating with  predictive posterior measure.
                  
                  
            BayesianFROC::ppp(
              fit
              )
                   
                   # The auhor thinks it is not correctly programmed, so it needs validaton of programing

                                     
                  
```

# Jafroc (a software)

In order to apply the functions in this package to an xlsx file
representing a dataset formulated for Jafroc, use the following code;

``` r
     dataList <- convertFromJafroc(
                                  No.of.Modalities =5,
                                  No.of.readers    =4,
                                  No.of.confidence.levels = 5
                                    )
```

where it requires to specify the number of modalities, readers,
confidence levels.

Using the above code, an object is created from an xlsx file.

#### The FROC curve

Using the fitted model object `fit` of class `stanfitExtended`, we can
draw the FROC curve (or AFROC curve) as follows;

<!-- !["X-ray of my teeth!!  Let's study together with me !! :-D "]() -->

``` r
# new.imaging.device = FALSE  is used to include the output image 
# in this README file, so I recommand new.imaging.device = TRUE
BayesianFROC::DrawCurves(fit,
                         new.imaging.device = FALSE)
```

#### To draw the curve in white background, use the followings

``` r
# new.imaging.device = FALSE  is used to include the output image 
# in this README file, so I recommand new.imaging.device = TRUE.

BayesianFROC::DrawCurves(fit,
                         Colour = FALSE,
                         new.imaging.device = FALSE)
```

Executing the above code, an imaging device will appears in which there
are circles indicating the so-called False Positive Fractions (FPFs) and
True Positive Fractions (TPFs). In addition, an FROC curve is plotted.
FROC curve thorough exactly the expected points of FPFs and TPFs. Thus
we can intuitively confirm the goodness of fit by comparing the circles
and the curve. Ha,… someone reads this boring vignettes? My right arm
ache bothering me for 20 months. Ha,… I want to die. I developed theory
and package, but this research cannot make me happy, cannot change
anything about my poor life… ha.

# Latent Distributions

Hit rates are determined the areas of signal Gaussian between
thresholds,

and false alarm rate are defined by the areas of differential
logarithmic cumulative Gaussian between thresholds.

## False Rate

``` r
# new.imaging.device = FALSE  is used to include the output image 
# in this README file, so I recommand new.imaging.device = TRUE

BayesianFROC::draw_bi_normal_version_UP(
    fit,
    new.imaging.device      = F,
    dark_theme              = T,
    hit.rate                = F,
    false.alarm.rate        = T,
    both.hit.and.false.rate = F)
```

## Hit Rate

``` r
# new.imaging.device = FALSE  is used to include the output image 
# in this README file, so I recommand new.imaging.device = TRUE

BayesianFROC::draw_bi_normal_version_UP(
    fit,
    new.imaging.device      = F,
    dark_theme              = T,
    hit.rate                = T,
    false.alarm.rate        = F,
    both.hit.and.false.rate = F)
```

One will see that the bi normal assumption is wrong in the FROC context,
and instead of bi normal assumption, we use two latent distributions,
one is Gaussian for signal and another is the differential logarithmic
Gaussian introduced first by the author of this package. For details,
see vignettes of this package.

## Modality Comparison

By fitting hierarchical Bayesian model, we can get the characteristics
such as AUCs for each imaging modality (MRI,PET,CT,etc) to compare
modalities.

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

# Transform of S4 Class for other packages

To apply the functions of other package such as **rstan** or **ggmcmc**,
…, etc in which there are functions for object of class `stanfit`, e.g.,
`rstan::stan_trace()`,
`rstan::stan_dens()`,`rstan::check_hmc_diagnostics()`,…etc, we have to
change the class of the fitted model object by the following manner:

``` r
 fit.stan <- methods::as(fit, "stanfit")
```

Then the above object `fit.stan` is an object of the class `stanfit` and
thus we can apply the function of rstan package, e.g. in the following
manner; `rstan::stan_dens(fit.stan)`.

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
```

#### posterior density of parameter `A` stored in an object of class `stanfit`

The following plot indicates that *maximal* *posterior* *estimator*
(MAP) is very unstable in each chain in this iteration. By drawing more
samples, it become stable?

``` r

# Change the class from stanfitExtended to stanfit
#fit.stan <- methods::as(fit,"stanfit"
                        
                        
ggmcmc::ggs(fit.stan) %>% ggmcmc::ggs_density(family    = "A")
```

#### Auto correlation for an object of class `stanfit`

``` r


# Change the class from stanfitExtended to stanfit
# fit.stan <- methods::as(fit,"stanfit")


ggmcmc::ggs(fit.stan) %>% ggmcmc::ggs_autocorrelation(family    = "A")
```

For fitted model object `fit.stan` of class `stanfit`, there is a GUI
viewer

``` r

# Change the class from stanfitExtended to stanfit
fit.stan <- methods::as(fit,"stanfit")


shinystan::launch_shinystan(fit.stan)
```

# Goodness of fit via posterior predictive p value

Evaluates a p value of chi square goodness of fit. In addition, the
scatter plot are drawn which shows the replicated datasets from the
posterior predictive p value of the data which is used to create a
fitted model object `fit`.

``` r
BayesianFROC::ppp(fit)
```

In previous release, my program for ppp was wrong, so in the current
version I fixed.

# SBC

### Validation of model via Simulation Based Calibration (SBC)

Talts, S., Betancourt, M., Simpson, D., Vehtari, A., and Gelman, A.
(2018). Validating Bayesian Inference Algorithms with Simulation-Based
Calibration. arXiv preprint arXiv:1804.06788

``` r
BayesianFROC::Simulation_Based_Calibration_single_reader_single_modality_via_rstan_sbc()
```

# Errors of Estimator

## Errors of estimates decrease monotonically with respect to sample size.

The author investigate the relation between the sample size and the
error of estimates. Accuracy of estimates are depend on the sample size.
Large sample size leads us to small error. However, in practical
perspective, the number of images or lesions has limitation. The author
thinks it is better to obtain 100 images or lesions. And 100 images or
lesions gives us the error 0.01 in AUC.

``` r
library(BayesianFROC)

a <-BayesianFROC::error_srsc(NLvector = c(
33L,
50L,
111L,
11111L,
1111111L,
111111111L,
999999999L),
# NIvector,
ratio=2,
replicate.datset =3,# This should be more large, e.g. 100 or 200. Larger is better.
ModifiedPoisson = FALSE,
mean.truth=0.6,
sd.truth=5.3,
z.truth =c(-0.8,0.7,2.38),
ite =222
)
```

## X axis is sample size and Y axis is error of estimates.

``` r
BayesianFROC::error_srsc_error_visualization(a)
```

## X axis is sample size and Y axis is variance of estimates.

``` r
BayesianFROC::error_srsc_variance_visualization(a)
```

## Appendix

The author add the program to calculate the event that one is diseased
under the condition that diagnosis is positive.

``` r


#========================================================================================
#  If Sensitivity and Specificity is larger, then, the probability is also larger
#========================================================================================


x <- stats::runif(100,0,1)
y <- CoronaVirus_Disease_2019_prevalence(0.1,x,x)

dark_theme(4)
plot(x,y)


#========================================================================================
#  If the prevalence is larger, then, the probability is also larger
#========================================================================================



x <- stats::runif(100,0,1)
y <- CoronaVirus_Disease_2019_prevalence(x,0.9,0.9)

dark_theme(4)
plot(x,y)
```

## Acknowledge

Dona nobis pacem

R.I.P. Leibriya Riyakoboch

## Now, ….

The author is a homeless, so, please employ me,,, send me a mail whose
address is in [the page :’-D](https://CRAN.R-project.org).

The author also diseased from multiple chemical sensitivity caused the
NO/ONOO- cycle and the initiating toxicant is the *_synthetic detergent
(i.e., syndet)_* which makes very many prurigo nodularises in all of my
body for more than two years and a half.

My nervous system and the immune system have seriously damaged by the
*_synthetic detergent (i.e., syndet)_*. However the company making the
*_synthetic detergent (i.e., syndet)_* never

# Is the author’s model better than the classical model?

if the cell contains more zeros, then, the variance of posterior samples
of samples are large. On the other hands, the author’s model dose
converge with such a data with good posterior samples by taking
sufficiently large MCMC samples.

| Confidence Level       | Number of Hits | Number of False alarms |
| :--------------------- | :------------: | :--------------------: |
| 3 = definitely present |       97       |           0            |
| 2 = equivocal          |       32       |           0            |
| 1 = questionable       |       31       |           74           |

| Confidence Level       | Number of Hits | Number of False alarms |
| :--------------------- | :------------: | :--------------------: |
| 3 = definitely present |       0        |           0            |
| 2 = equivocal          |       32       |           0            |
| 1 = questionable       |       31       |           74           |

| Confidence Level       | Number of Hits | Number of False alarms |
| :--------------------- | :------------: | :--------------------: |
| 3 = definitely present |       97       |           0            |
| 2 = equivocal          |       32       |           14           |
| 1 = questionable       |       0        |           74           |

| Confidence Level       | Number of Hits | Number of False alarms |
| :--------------------- | :------------: | :--------------------: |
| 3 = definitely present |       97       |           0            |
| 2 = equivocal          |       32       |           14           |
| 1 = questionable       |       1        |           74           |

In actual clinical trial, readers do not understand how to use ratings
and someone will use only three in the 5 ratings, then zero cells are
generated in such case, the author’s model will perform than the
classical one.

Such argument is not intend by the author, but many people ask what is
new or dose it improve classical model by the author’s model? I hate
such questions. Because my model exists, and the stories are all made
after making. Haa, when I made such stroies, my model dose never change.

Especially, in the second dataset with three zeros. the fitted curves
are different between the both the author’s model and the classical
model. The author’s fitted curve is more smooth but the classical is not
so. Ha,, I am tired. I hate such a argument.

So,,,, as the Bayesian model, the author finds that the convegence
criteria distincts the author’s model and the classical model. But my
heart will go on a homeless way, now ,,, no longer want to write a
English paper. I consider many statisticians dose not make a Bayesian
model thus they dose not know convergence issues which is the most
biggest concerns when I made a model.
