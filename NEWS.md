



### Future development

* the central limit theorem  change the model to use normal distribution instead of the binomial distribution

* SBC

* Validation of replicated datasets

* Shiny and Graphical user interface

* Like ShowGM, I will make a work flow using diagram.





### ver. 0.1.4

* Provides a Graphical User INterface for fitting via Shiny for single reader and single modality.

                         BayesianFROC::fit_GUI()  

* Ha,..,.my English grammer is very wrong ,.. I fix when I notice it. 


* Provides posteriror mean of chi square goodenss of fit for MRMC case.

* Vignettes and doc are revised



* Fix the following inconsistent

                    model <- stan_model(model_code = "parameters { real<lower = 0> y; }
                            transformed parameters { real<upper = -1> z = y; }")

               fit <- sampling(model)


which cause the error

               [1] "Error in sampler$call_sampler(args_list[[i]]) : Initialization failed."





### ver. 0.1.3

* 2019.Jun

* Revise documents in the PDF manual or vignettes.

* In statistical perspective,

* I use proper priors instead of improper priors which does not be shown yet in vignettes.


* I export the null hypothesis test via Bayes factor. 

* Stan file had been changed from improper priors to proper priors.

* Poper priors decrease WAIC for some dataset.

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
