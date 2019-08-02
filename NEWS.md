

<style type="text/css">

h1.title {
  font-size: 35px;
  font-weight: bold;
  font-family: Arial-Black;
   color: #800000			;
}
h1{
  font-size: 35px;
  font-weight: bold;
  font-family: Arial-Black;
  
  color: #800000			;

}

h2 {
  font-size: 30px;
  font-weight: bold;
  font-family: Arial-Black;
    
  color: #800000			;

}

h3 {
  font-size: 26px;
  font-weight: bold;
  font-family: Arial-Black;
    
  color: #800000			;

}

h4 {
  font-size: 24px;
  font-weight: bold;
  font-family: Arial-Black;
    
  color: #800000			;

}



h5 {
  font-size: 22px;
  font-weight: bold;
  font-family: Arial-Black;
    
  color: #800000			;

}



h6 {
  font-size: 19px;
  font-weight: bold;
  font-family: Arial-Black;
    
  color: #800000			;

}



img {
	border:0;
}




body {
  font-size: 18px;
  <!-- font-weight: normal ; -->
    font-weight:bolder;
  
  font-family: Calibri;
    <!-- background-image: url("a.gif"); -->
  color: #800000			;
  background-color:#EEEEEE;
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
    color: #440000		;
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



### Future development plan

* My package depends on many unnecessary packages, so I have to separte these dependencies.

* Essencially Stan is only requiered but I do not want to assume that user is familiar with R. For such pearson GUI and other unnecessary packages are requiered and these packages will kill my package soon.

* the central limit theorem  change the model to use normal distribution instead of the binomial distribution



* Shiny gives the powerpoint as result.




* Fisher metric of the two distribution between Gaussian signal and differential logarithmic Gaussian. Note that noise is not canonical Gaussian, and it is already implemented. That is canonical Gaussian and signal Gaussian is element of Poincare upper half plane, and its geodesic distance is easy to calculate. But differential Logarithmic Gaussian is not an element of the Poincare upper half plane, and thus it is difficult to calculate such two distributions Fisher metric. To do so, first, we should define the parametric family of probabilities such that is contains Gaussians and deferential logarithmic Gaussian. Hmm, differential logarithmic Gaussian is belong to the exponential family? If so, then the calculation of Fisher metric will be easy.
Now, my right arm has ache which cannot allow me to calculate such quantities.
$$d \log \Phi \in \text{Exponential family}?$$


* SBC for MRMC model
   * I think my hierarchical model is very long time for HMC,
     so,..., if I made it, can R calculate ?
* GUI for MRMC
   * The data of MRMC is so complex to input by GUI, so,...
     it should not be done?
     

* prior selection

* Predictive P value via Stan file
   * I doubt my calculation of p vaule, so,...

* p-value is correct? validation of my program

* Validation of replicated data sets
  * I have to include the non convergent case for evaluation

* Shiny and Graphical user interface for MRMC

* Like Show Gm, I will make a work flow using diagram.

* Validation of my R code.
* validation of my R code, in particular, the p-values which I have no confidence, since it is not rigid.
* In Bayesian p value is not rigid?, oh, robust is better word. So, in my code I do not know why, but 
 my p-value is not robust, so it is not desired one, I have to validate in the future.
 
* Define methods for generic functions, such as plot and print and etc, I made some of them, ... I forget.

* Generic function `summary` cannot use, I regret, in my package my initial periods, I use `summary` for extract estimates from `stanfit`, which does not allow me to make generic function `summary`
 for `stanfitExtended`, since the code is overlapped and cause error.

### ver. 0.1.5
* 2019 August 2

* Revise the GUI of `BayesianFROC::fit_GUI()` so that it is faster, and add more buttons in it.

*  Shiny based  Graphical User Interface for fitting and estimates and drawing curve;

         fit_GUI()            
         fit_GUI_simple()     
         fit_GUI_dashboard()     







* `.css` file is used for the mos
* Develop theory, in particular, the author find some latent variable to determine the false alarm rate,
* I love you.
* __SBC__ for SRSC

 

### ver. 0.1.4

* Provides a Graphical User Interface for fitting via Shiny for single reader and single modality.

                         BayesianFROC::fit_GUI()  

* Ha,..,.my English grammar is very wrong ,.. I fix when I notice it. 


* Provides posterior mean of chi square goodness of fit for MRMC case.

* Vignettes and doc are revised



* Fix the following inconsistent


              model <- stan_model(
              model_code = "parameters { real<lower = 0> y; }
                            transformed parameters { real<upper = -1> z = y; }"
                            )

               fit <- sampling(model)


which cause the error

               [1] "Error in sampler$call_sampler(args_list[[i]]) : Initialization failed."

Some Stan developer taught this  in stack over flows, His answer is very plain, I appreciate him. He helps me many times, thank you.




### ver. 0.1.3

* 2019.Jun

* Revise documents in the PDF manual or vignettes.

* In statistical perspective,

* I use proper priors instead of improper priors which does not be shown yet in vignettes.


* I export the null hypothesis test via Bayes factor. 

* Stan file had been changed from improper priors to proper priors.

* Proper priors decrease WAIC for some dataset.

* Fix English or Grammar in documents of manual and vignettes.

 










### ver. 0.1.2

* Fix errors.
* Revise documents
* In statistical perspective, I did not change anything.
* 2019.05.22 Revised.
* Core R scripts is  changed but  Stan file is not changed essentially.

* The aim of update is fix English or Grammar in documents of manual and vignettes.

* I found [F7] key is useful for spell check of Rmd file.









### ver. 0.1.1

* 2019.5.9   the first upload  of my package to `CRAN`.
