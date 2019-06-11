











Bayesian FROC analysis
========================================================
author:Issei Tsunoda
date:"2019-06-09"
autosize: true






Aims of this package
========================================================
# _Modality_ _Comparison_



What is modality?
===========================
#  __Modality__ is imaging methods e.g., MRI, CT, PET,...


Modality comparison
================================
# Which modality is __better__ to detect _signal_?

   lesions, nodules,... in radiographs





Work Flow of this package
========================================================


- Create  data or

  Convert from __Jafroc__ formulation

- Fitting
- Draw Curves
- Comparison of Modalities

For more details of Jafroc please visit the following Chakraborty page:

<http://www.devchakraborty.com/>.


Single Reader and Single Modality
========================================================
## Make a data

```r
 dat <- list(

c = c(3,2,1),    #Confidence level (c.l.)
h = c(97,32,31), #Number of hits for each c.l.
f = c(1,14,74),  #Number of false alarms for each c.l.

NL= 259,       #Number of lesions
NI= 57,        #Number of images
 C= 3
)          #Number of confidence level
```


Data
=========


```r
BayesianFROC::viewdata(dat)
```

```


.                     Confidence.Level   False.Positives   True.Positives
-------------------  -----------------  ----------------  ---------------
Obviouly present                     3                 1               97
Relatively obvious                   2                14               32
Subtle                               1                74               31
```






## Fit the FROC model for the above data.

 
 ```r
 fit <-   BayesianFROC::fit_Bayesian_FROC(dat, summary = F)
 ```

To see summary
========================================================


```r
BayesianFROC:::summary_EAP_CI_srsc(fit)
```
Provieds
- the Expected A Posterior (EAP) estimates
- 95% Credible Intervals




Summary ----  single reader and single modality.
========================================================
 By running the following, we get a estimates as a return value, FROC curves.

 
 ```r
 fit <-   BayesianFROC::fit_Bayesian_FROC(dat,summary=T)
 ```

 `summary =TRUE`, shows estimates.

Multiple readers and Multiple Modalities
========================================================

From here, we show the case of single reader and single modality.

Next, we show the multiple readers and multiple modalities.


Multiple Readers and Multiple Modalities (MRMC)
========================================================
- Make a data of MRMC ------------ `create_dataset()`
- Fitting ( comparison of modalities) --- `fit_Bayesian_FROC()`
- Draw the FROC curve ----------- `DrawCurves_MRMC_pairwise()`
 
 ```r
 # Make a data
 data <- create_dataset()
 
 
 # Fitting
 fit <- fit_Bayesian_FROC(dat)
 
 #Draw curves for the 1st modality and 2nd reader
 DrawCurves(
 
      #Estimates
      fit,
 
      #Specify   modality IDs to draw curves
      modalityID =c(1,2),
 
      #Specify  Reader IDs  to draw curves
      readerID   =c(2,3,4)
 ```
