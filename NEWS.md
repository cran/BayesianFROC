

<style type="text/css">

h1.title {
  font-size: 35px;
  font-weight: bold;
  font-family: Arial-Black;
  color: white			;
}
h1{
  font-size: 35px;
  font-weight: bold;
  font-family: Arial-Black;
  
  color: white			;

}

h2 {
  font-size: 70px;
  font-weight: bold;
 font-family: Calibri
  color: red			;

}

h3 {
  font-size: 26px;
  font-weight: bold;
  font-family: Arial-Black;
    
  color: white			;

}

h4 {
  font-size: 24px;
  font-weight: bold;
  font-family: Arial-Black;
    
  color:  white			;

}



h5 {
  font-size: 22px;
  font-weight: bold;
  font-family: Arial-Black;
    
  color:  white			;

}



h6 {
  font-size: 19px;
  font-weight: bold;
  font-family: Arial-Black;
    
  color: white			;

}



img {
	border:0;
}




body {
  font-size: 18px;
  <!-- font-weight: normal ; -->
    font-weight:bolder;
    font-family: arial black;
    <!--  font-family: Calibri;; -->
    <!-- background-image: url("a.gif"); -->
  color: #800000			;
  background-color:#808080;
    <!-- background-color:#999999; -->

<!--   margin:0; -->
<!-- 	padding:0; -->


  
</body>




}
.center{
  <!-- border: 1px solid #aaa; -->
  width: 400px;
  text-align: center;
}


p {
    color: white					;
    <!-- text-indent:3%; -->
    line-height: 1em;
}

p.initcap{
font-size: 16em;
line-height; 0.6em;
font-weight:bold;
}


button
{
  height:30px;
  width:150px;
  border-radius:8px;
  padding:10px;
  font-size:20px;
  font-family: 'Oswald', sans-serif;
  height:52px;
  cursor:pointer;
  background-color:wheat;
}


</style>


## Future development plan of the author's life

- Please employ me.
- I am M.D. in mathematics
- Now (2020 19 March), I am free.
- Please, emplooooy meeee!
- Pleaaaaaaaaaaaaaaaaaaaaaaaaaaase, eeeeeemplooooooooooooy me.
- I am exp(123.45)/exp(119.9) years old.
- This is not a Joke. Please .... ha ... I want to ... live in peace with clean aire, water.
- To show one of my skil, I made this package and up load to CRAN. 
- I suffer from Chemical Sensivity (CS) and in my initiating toxicant is the Laundary powder named ooooo (I cannot say) made by some large company in Japan. I use it in job. My exposre begin 
irritant stimulas in my whole body. One month later from this exposre, 
my whole body haschronic inflammation causing fire in body and brain. My symptom is very.
- In japan, there are famous three procedure is available to detect CS, i.e., tracking eye movement examination and pupllary function and Standing ability. Unfortunately, the author had positives for all examination at once. By Martin Pall, the mechanism of the reason why the inflammation is chronic is revealed. According to him, the NO!Oh,NO cycle is running by the toxicant and it is loop, so it tooks a long time to recover from CS. As a detoxification in our body, there is the notion of conjugation. To help this, vitammin and minerals are recommended. Anyway, the author of this package is 


## Future development plan of this package

*In GUI, if the second row in the table is vanished, then  ppp()  failed,....  Why does such bug occur?? ha. I am tired. 2020 March 8

* FPF and TPF plot by mean and CI inteval, the scatter plot is hard to see. 

* Bayes Factor 

* SBC

* Mean and Conf. Interval representation for synthesized datasets in the scatter plot of ppp()

* Reduce dependencies
   * My package depends on many unnecessary packages, 
so I have to reduce  or separate these dependencies.


* Applying the central limit theorem, 
  *  we may use normal approximation of the binomial distribution for hits
    * of the two distribution between Gaussian signal and differential logarithmic Gaussian.   Canonical Gaussian and signal Gaussian is element of Poincare upper half plane, and its geodesic distance is easy to calculate. But differential Logarithmic Gaussian is not an element of the Poincare upper half plane, and thus it is difficult to calculate such  Fisher metric. To do so, first, we should define the parametric family of probabilities such that is contains Gaussians and deferential logarithmic Gaussian.

$$d \log \Phi \in \text{Exponential family}?$$
  If not how we can approximate it by exponential family

* SBC for MRMC model
   * I think my hierarchical model is very long time for HMC,
     so,..., if I made it, can R calculate ?

 
* Define methods for generic functions, 
   * such as plot and print and etc, I made some of them, ... I forget.

* Generic function `summary` cannot use, I regret, in my package my initial periods, I use `summary` for extract estimates from `stanfit`, which does not allow me to make generic function `summary`
 for `stanfitExtended`, since the code is overlapped and cause error.


If x1(t),...,xn(t) are curves into plane, then we can define its mean curve

by x(t):= mean(x1,...,xn). Or use mean parameter over all readers to get modaitywise one.

I think, if number of readers are very large,
then I guess such pooling will be required. 2020 Jan



## ver. 0.2.3

 - Tools > project option > build tool

Check Packgae --R CMD check additional options should be specified as following
  
       --as-cran --run-donttest
       
 - the author noticed that he should use 
 
            `if (interactive()) { }`
            
 - BayesianFROC.Rcheck / BayesianFROC-Ex.R shows the example codes designated in `#'@examples`.
 
 - uses `\dontrun` instead `\donttest`
 
 - improves prior to get more uniformly distributed rank statistics of SBC
 
 - the following roxygen is not allowed and causes error on R CMD check.
         
        #' @examples
        # \dontrun{ <- as comment, not code
        #' \donttest{
        #'
        #'
        #'}#donttest
        #'
        #}#dontrun   <- as comment, not code

## ver. 0.2.2

* In case of a single reader and multple modaity case, an array of type, e.g., [3,1]
 is reduced to a vector of dimension 3 and it caused some error. So, the author fixed it.


* The author had forgotten to adjust the number of lesions denoted by NL for the function ppp_srsc
  such that it reflects the number of hits. It caused the true positive fractions of fake datasets to calculates ppp are greater than 1. So, in this update, the author has fixed this.
  
  * A[m] is a mean of (AA[m,q]) over all q, but, the denominator was M in the prev. version. So, I fixed.



## ver. 0.2.1
* 2020 Jan 

* GUI for MRMC

* Non-hierarchical MRMC Model is introduced as a new model to avoid the divergent transition issues


## ver. 0.2.0
* 2019 Oct 21

* In roxygen comments, the following multiple line does not be  allowed.

        \code{
        ssssss
        ssss
        sss
         }

Moreover such multiple line cannot be detected by the R CMD check in my computer but in R CMD check in CRAN detects it. So the debug or find such 
multiple line is very hard to find because the error message never specify 
the information about such a location.

* In .Rd files I should not use \item or \describe or any other. The reason is it cause unknown errors.
  The author  struggled these unknown issues in several days.
  Because, the error is not appear in R CMD check in my computer but in CRAN auto check says the error  that 
  
  Flavor: r-devel-linux-x86_64-debian-gcc, r-devel-windows-ix86+x86_64
Check: PDF version of manual, Result: WARNING
  LaTeX errors when creating PDF version.
  This typically indicates Rd problems.
  LaTeX errors found:
  ! Paragraph ended before \Rd@code was complete.
  <to be read again>
                     \par
  l.16983
  
  
  This error is very heavy to debug.

* The statistical model and theory is significantly changed.
 The previous models are not generate models. To ensure the sum of hits is less than the number of lesions, we have to change the model and the author has changed so that the 
 summation condition satisfies and hence we obtain the generative model so that the model
  generates the dataset of FROC trial.

* The posterior predictive p values (ppp) is wrong in the previous release.
  Thus, in the current release,  I ensure the ppp and fixed. 
    *  So, now, p value is correct! In particular, 
  in case of single reader and single modality data, the `ppp()` very correctly works! 
   * model has changed so, I need to validate the ppp.

* Made a `ppp()` for Predictive Posterior P value and implement on the Shiny GUI in  `fit_GUI_Shiny() `

* For the only one modality case, I made a model to pool AUCs among readers. 


* I attempted to use `rstantools::rstan_create_package("name")` but I failed.
 I am not so young, I do not want to waste a time to fix errors.
 head ache. no. ital.




## ver. 0.1.5
* 2019 August 2

* Revise the GUI of `BayesianFROC::fit_GUI()` so that it is faster, and add more buttons in it.

*  Shiny based  Graphical User Interface for fitting and estimates and drawing curve;

         fit_GUI()            
         fit_GUI_simple()     
         fit_GUI_dashboard()     







* `.css` file is used 
* Develop theory, in particular, the author find some latent variable to determine the false alarm rate,
* __SBC__ for SRSC

 

## ver. 0.1.4

* Provides a GUI  via Shiny for single reader and single modality.

                         BayesianFROC::fit_GUI()  


* Provides posterior mean of chi square goodness of fit for MRMC case.


* Fix the following inconsistent


              model <- stan_model(
              model_code = "parameters { real<lower = 0> y; }
                            transformed parameters { real<upper = -1> z = y; }"
                            )

               fit <- sampling(model)


which cause the error

               [1] "Error in sampler$call_sampler(args_list[[i]]) : Initialization failed."

Some Stan developer taught this  in stack over flows, His answer is very plain, I appreciate him. He helps me many times, thank you.




## ver. 0.1.3

* 2019.Jun

 
* In statistical perspective,

* I use proper priors instead of improper priors which does not be shown yet in vignettes.


* I export the null hypothesis test via Bayes factor. (The result is converse, why???)

* Stan file had been changed from improper priors to proper priors.

* Proper priors decrease WAIC for some dataset.

* Fix English or Grammar in documents of manual and vignettes.

 










### ver. 0.1.2

* Fix errors.
 * 2019.05.22 Revised.
* Core R scripts is  changed but  Stan file is not changed essentially.

* The aim of update is fix English or Grammar in documents of manual and vignettes.

* I found [F7] key is useful for spell check of Rmd file.









### ver. 0.1.1

* 2019.5.9   the first upload  of my package to `CRAN`.
