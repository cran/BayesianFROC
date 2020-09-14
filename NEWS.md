

<style type="text/css">

h1.title {
  font-size: 35px;
  font-weight: bold;
  font-family: Arial-Black;
  color: #E05156			;
}
h1{
  font-size: 35px;
  font-weight: bold;
  font-family: Arial-Black;
  
  color: #E05156			;

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
    
  color: #A60000			;

}

h4 {
  font-size: 24px;
  font-weight: bold;
  font-family: Arial-Black;
    
  color:  #A60000			;

}



h5 {
  font-size: 22px;
  font-weight: bold;
  font-family: Arial-Black;
    
  color:  #A60000			;

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

# VORWORT

## Future development plan of the author's life

- Please employ me.
- I am M.D. in mathematics. My math paper will be published, in which I study Gromov-Hausdorff topology and some function space.
- Now (2020 19 March), I am free and also a xxxxxxx. My xxxxxxx life has started cuz to escape from chemical substances, such as insecticide, syndet, softener, exhaust gas, petrochemical products ...etc.

- Please, employ me!
- please, employ me....
- This is not a Joke. Please .... ha ... I want to ... live in peace with clean air, water.
- To show one of my skill, I made this package and up load to CRAN but, .. my life does not change,,,
- I suffer from Chemical Sensitivity (CS) and in my initiating toxicant is a *_synthetic detergent (i.e., syndet)_* named ooooo (I cannot say) made by some large company in Japan. I use it in job. My exposure begin 
irritant stimulus in my whole body. One month later from this exposure, 
my whole body has chronic inflammation causing fire in body and brain. My symptom make my quality of life to be much low.
- In japan, there are famous three procedure is available to detect CS, i.e., tracking eye movement examination and pupllary function and Standing ability. Unfortunately, the author had positives for all examination at once. By Martin Pall, the mechanism of the reason why the inflammation is chronic is revealed. According to him, NO/ONOO- (pronouciation is  NO!Oh,NO. cycle) is running by the toxicant and it is loop, thus another cycle is made, so it took a long time to recover from CS. As a detoxification in our body, there is the notion of conjugation. To help this, vitamin and minerals are recommended. Manufactures never accept the toxicity of their products such as syndet. 

- I hate statistics and FROC analysis, and this package, I really hate. This cannot help, I hate it. Painful life, atopic dermatitis is initiated by exposure of syndet in some company, my quality of life is completely broken. Now, my interest is Dolbeault cohomology of almost complex structure.


## Future development plan of this package

*In GUI, if the second row in the table is vanished, then  ppp()  failed,....  Why does such bug occur?? ha. I am tired. 2020 March 8

* Drop down list for parameter selection of trace plot in GUI

* FPF and TPF plot by mean and CI interval, the scatter plot is hard to see. 

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

by x(t):= mean(x1,...,xn). Or use mean parameter over all readers to get modality-wise one.

I think, if number of readers are very large,
then I guess such pooling will be required. 2020 Jan


 - Prior should be make a suitable dataset, to do so, it needs to create a preimages so that
 the success rate of multinomial or false alarm rate of Poisson should have a monotonicity conditions.
 Because readers has more hits with respect to his ratings, the monotonicity is required.
 Also, each rate should be some suitable interval, e.g., if the rate is close to zero or one, then it cause a generation of more zeros of hits or false positives with which, the fitting will have large R hat and I guess it cause the bias in SBC algorithm. The difficulties is to establish these several conditions simultaneously. To obtain such pre images in a parameter space satisfying the multiple conditions, we have to obtain some analytic inequalities representing the conditions that rate has monotonic relations and not very close to zero or one. If such prior is made, then SBC will tells us that it is suitable or not.


 - extract the param name whose R hat is the largest
 
 - Check tech of CRAN is greater year by year, so uploading is harder and harder and. 
 
 
# INHALT

# When I reinstall R, I should install my pkg at first, then all other pkg also be installed.
Why I did not notice this! 


# I really ...

dissapointed R programming, cuz it dose not give me any loves!

In the current version, I try to include an article but if do, I cannot publish it in journals, so I cannot.


In the future, I should write down priors by target += formulation, but now I did not do that, cuz I am tired. And, using GUIs, the author shuld make GUI to be valid the various priors and show how it affects estimators, but now, it dose not be accomplished cuz now, I wanna sleep.

## ver. 0.3.1
 
 - GUI is more accessible. Cuz the author has aches. My hand has been numb since the exposure to syndet. So, this accessiblity is important for the author.

 - Introduces  informative priors which make MCMC samplings to be more stable. 
 
 - elimination of dependency of the package ggmcmc, cuz it is redundant for this pkg.

 - Until now, the author has forgotten to write down priors by using += formulation. What a cute I am! So, log likelihood was not correct! What a hell! Ha,...the author's brain is fucking, ha, I am tired. Please  please me. 

 - Research paper is included... maybe... if forget.. i am sorry. If J.S.Bach reads it, then he would say "Sie geht aber schon ausserilich duch den enormen umfang der Komposition uber jeden bis dahin gewohnten stil so weit hinaus, dass auch ihre praktische! Gib uns Frieden.", Thank you Bach, Dank! Ich liebe dich! Ich glaube dich!
 
 - In GUI, Hamiltonian Diagnosis for tree depth are included! Heilig! 
 
 - Also, included love and blood from MCS symptoms, aches.
 
 - Until this version, I cannot include Rnw files, but now maybe I can do that! 
 
 - I will publish a paper in this package in which the proof reading is done, so, English and theory never include mistakes, maybe. Roll over Beethoven and tell Tchaikovsky news!
 
 -  In this version, we have made  minor revisions and added new material, including scatter plot and 
 trace plot in  `fit_GUI_Shiny()`  taking account of recent models in the author's model and classical theory.
 We are grateful to the many doggies of the previous edition whose comments have contributed in one way or another to the present version. We wish to thanks also Riyakoboch Sampo Ikuzo and Ruikobo Sampo Ikantone for their assistance with the GUI tags and Riyatoo Lueich for their particular advice.
  July 2020 Issei Tsunoda, Carbus Univ. Rieyahko Korekuka, Mosquitbus Univ.
  
 - Many individuals have assisted me during the development of this package over several years. In particular, the author is grateful for the valuable discussions with Riyakoboch Sampota Ikuzo and his contribution in Statistical models; for the helpful comments and corrections of Luikobo Sampo Ikantone for the contribution of GUIs and for the impeccably typed manuscript which resulted from the dedicated efforts of Oriconna Ruiko. 
 
 - Using type ="l" instead type ="p", R script is much faster. Moreover, objects size is much small! What a cute I am, it is very simple but effective! 
 
 - Plot() is unclear what it is. Thus ggplot is better. In the future, I have to replace plot() to ggplot(), but I am tired. I do not want live such a Hard Dayes Night! Good night! With Aches caused by MCS, 21 August 2020.

 - with aches, ha, syndet is dangerous. I hate hell, all aches and syndet!!
 
 - Before submission, in this package needs many process, such as omission of .rds files. The author makes a function to do this automatically.

       BayesianFROC:::file_remove()
       
 - To escape from chemical substances, now I live in country side to recuperation, and I guess this life is better than fucking metropolis life.       
       


## ver. 0.3.0
 
 - My apologies, in past, I had misunderstood the Chakraborty's model and now, I implement it in this new model in this version.  In my view, if FROC data includes many zeros, then the author's model is better than the classical, traditional model. So, theoretically, my model is complicated and not easy to understand and someone would think the author's model and classical one are equivalent, but it is not true. Because the MCMC sampling differs dramatically between the author's and classical model with respect to FROC data with many zero cells. So,,,,if we write a paper, I emphasize this point. Ha...I hate 


 I diseased from multiple chemical sensitivity which was very heavy, and, now,  my body still a lot of prurigo nodularises, chronic inflammations. The initial toxicant is *_synthetic detergent (i.e., syndet)_*. When I made this pkg, I hope programming and statistics save my life, but it does not.  Above me only sky.
  
 
 - The most biggest failure of this package is the order of hit vector h, it should be replaced by the inverse order. The current order let the codes to be complex. But too late to fix it.
 
 
 
 








## ver. 0.2.3

 - Tools > project option > build tool

Check Package --R CMD check additional options should be specified as following
  
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

* In case of a single reader and multiple modality case, an array of type, e.g., [3,1]
 is reduced to a vector of dimension 3 and it caused some error. So, the author fixed it.


* The author had forgotten to adjust the number of lesions denoted by NL for the function ppp_srsc
  such that it reflects the number of hits. It caused the true positive fractions of fake datasets to calculates ppp are greater than 1. So, in this update, the author has fixed this.
  
  * A[m] is a mean of (AA[m,q]) over all q, but, in the prev. version, the denominator was M but should be R, which are fixed in the current versio.



## ver. 0.2.1
* 2020 Jan 

* GUI for MRMC

* Introduces Non-hierarchical MRMC Model as a new model to avoid the divergent transition issues


## ver. 0.2.0
* 2019 Oct 21

* In oxygen comments, the following multiple line does not be  allowed.

        \code{
        sss
        sss
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


* I attempted to use `rstantools::rstan_create_package("name")` but I failed. Now 2020 August, I have to reconstruct this pkg by it.




## ver. 0.1.5
* 2019 August 2

* Revise the GUI of `BayesianFROC::fit_GUI()` so that it is faster, and add more buttons in it.

*  Shiny based  Graphical User Interface for fitting and estimates and drawing curve;

         fit_GUI()            
         fit_GUI_simple()     
         fit_GUI_dashboard()     

(Now 2020 AUgust, these are replaced by fit_GUI_Shiny() instead )





* `.css` file is used 
* Develop theory, in particular, the author find some latent variable to determine the false alarm rate,
* __SBC__ for SRSC

 

## ver. 0.1.4
* Introduces a GUI  via Shiny for single reader and single modality.

                         BayesianFROC::fit_GUI()  (Now 2020 AUgust, it is fit_GUI_Shiny() instead )


* Introduces posterior mean of chi square goodness of fit for MRMC case.
* Fix the following inconsistent


              model <- stan_model(
              model_code = "parameters { real<lower = 0> y; }
                            transformed parameters { real<upper = -1> z = y; }"
                            )

               fit <- sampling(model)


which cause the error

               [1] "Error in sampler$call_sampler(args_list[[i]]) : Initialization failed."

Some Stan developer taught this  in stack over flows, His answer is very plain, I appreciate him. He helps me many times, I appreciate him.



## ver. 0.1.3
* Inmplements the null hypothesis test via Bayes factor. (The result is converse, why???)
* Stan file had been changed from improper priors to proper priors.

## ver. 0.1.2

* Fix bugs.
* Fix English or Grammar in documents of manual and vignettes.
* I found [F7] key is useful for spell check of Rmd file.

## ver. 0.1.1

* 2019.5.9   the first upload  to `CRAN`.
