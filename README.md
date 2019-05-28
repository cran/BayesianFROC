
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

<!-- badges: end -->

## Installation

Available from [CRAN](https://CRAN.R-project.org) with the following R
script, which installs the released version of `BayesianFROC`

``` r
install.packages("BayesianFROC")
```

  - Please execute it from the R console or the R studio console.

  - See our open access pre-print on the arXiv: Nothing
    
    To upload, I need someone who endorse my paper. If someone can help
    endorse my pre print, please send e-mail to me. I need help to
    upload my paper 2019.5.22.

### The goal of this package `BayesianFROC`

1)  Fit the statistical model to FROC data of Single Reader and Single
    Modality case.

2)  To compare imaging methods, MRI, CT, PET,…etc,

<!-- end list -->

  - Fit the hierarchical model to FROC data of Multiple Reader and
    Multiple case

  - Compare the modalities by the area under the alternative
    free-response ROC curve.

FROC = Free-response Receiver Operating Characteristic.

Radiologists need to compare the detection ability of lesions in
radiographs taken by different imaging methods, such as MRI, CT, PET ,…
. *Modality* indicate these imaging methods.

## Example

This is a basic example which shows you how to fit a model for single
reader and single modality.

``` r
#1) Build  data for singler reader and single modality  case.

  dataList <- list(c=c(3,2,1),     # c is ignored, can omit.
              h=c(97,32,31),
              f=c(1,14,74),
              NL=259,
              NI=57,
              C=3)

#  where,
#        c denotes confidence level, 
#                3 = Definitely lesion,
#                2 = subtle,  
#                1 = very subtle
#        h denotes number of hits 
#          (True Positives: TP) for each confidence level,
#        f denotes number of false alarms
#          (False Positives: FP) for each confidence level,
#        NL denotes number of lesions,
#        NI denotes number of images,


#2) Fit the FROC model.


 
                  fit <- BayesianFROC::fit_Bayesian_FROC(dataList)




#  validation of fit via alculation of p -value of the chi square goodness of fit, which is 
#  calculated by integrating with  predictive posterior measure.
                  
                  
                   p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit(fit)
                   
                   

                                     
                  
```

Note that the above list object representing the following FROC data;

| Number of Confidence Level | Number of Hits | Number of False alarms |
| :------------------------- | -------------- | ---------------------- |
| 3 = definitely present     | 97             | 1                      |
| 2 = equivocal              | 32             | 14                     |
| 1 = questionable           | 31             | 74                     |

The FROC curve

``` r
plot(fit,pars = "A")
```

Next, we shall show the comparison of modality.

Using the data object named `BayesianFROC::dataList.Chakra.Web`
contained in this package, we will fit the model by the following code.
For letting the running time be short, we take `ite =222` which is too
small to obtain reliable estimates. I think it should be `ite =33333`
for actual data analysis.

``` r
fit <- BayesianFROC::fit_Bayesian_FROC(
  # data of multiple reader and multiple case (modalities)
  BayesianFROC::dataList.Chakra.Web,
  
  # iteration of MCMC
  ite = 111)
```

Now, we obtain the *R* object named `fit` which is an S4 object whose
class is named `stanfit.Extended` and defined in this package. The S4
class `stanfit.Extended` is an inherited class from the S4 class
`stanfit` of the *rstan* package. So, `stanfit.Extended` is obtained by
adding new slots to retain data which is used to draw the FROC curve and
AFROC curve, …etc.

Thus to apply the functions of the rstan package such as
`rstan::stan_trace()`,
`rstan::stan_dens()`,`rstan::check_hmc_diagnostics()`,…etc, we have to
change the class of the fitted model object by the following manner:

``` r
 fit.stan <- methods::as(fit, "stanfit")
```

Then the above object `fit.stan` is an object of the class `stanfit` and
thus we can apply the function of rstan package as
`rstan::stan_dens(fit.stan)`.

E.g., plot the posterior density of the vector parameter “A” indicating
AUCs for each modality.

``` r
rstan::stan_dens(fit, pars = "A")
```

Note that the above density is far from Gaussian since the number of
iterations of the Hamiltonian Monte Carlo Method is taken as `ite =111`
which is too small.
