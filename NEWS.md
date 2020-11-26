

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
- I am M.D. in mathematics. My math paper will be published, in which I study Gromov-Hausdorff topology and some function space. Reviewer said it can be publishable but today I found a mistake in my proof. So,
- Now (2020 19 March), I am free and also a xxxxxxx. My xxxxxxx life has started cuz to escape from chemical substances, such as insecticide, syndet, softener, exhaust gas, petrochemical products ...etc. Sucks, I found mistake in my theory,.... I have to estimates certain operator norm in some space. If I can do that, I will prove some result.

- Please, employ me!
- please, employ me....
- This is not a Joke. Please .... ha ... I want to ... live in peace with clean air, water.
- To show one of my skill, I made this package and up loaded to CRAN but, .. my life does not change...
- I suffer from Chemical Sensitivity (CS) and in my initiating toxicant is a *_synthetic detergent (i.e., syndet)_* named ooooo (I cannot say) made by some large company in Japan. I use it in job. My exposure began 
irritant stimulus in my whole body. One month later from this exposure, 
my whole body has chronic inflammation causing fire in body and brain. My symptom makes my quality of life to be much low for several years.
- In japan, there are famous three procedures are available to diagnosis CS, i.e., tracking eye movement examination and pupllary function and standing ability. Unfortunately, the author had positives for all examinations at once. By Martin Pall who is the author of *General, Applied and Systems Toxicology*, revealed the mechanism of the reason why the inflammation is chronic. According to him, the nitric oxide (NO) and the peroxynitrite anion(ONOO-) constructs the so-called NO/ONOO- (pronunciation is  NO!Oh,NO!! cycle) is running by the toxicants including syndet, softner, metyl Hg, sarin, ... and it is loops, thus another cycle is made, so it took a long time to recover from CS. 
To detoxify them, melatonin reportedly reacts with NO, ONOO-, etc.

As a detoxification in our body, there is the notion of conjugation. To help this, vitamin and minerals are recommended. Manufactures never accept the toxicity of their products such as syndet.

- I hate statistics and FROC analysis, and this package, I really hate it. This cannot help me, I hate it. Painful life, atopic dermatitis is initiated by exposure of syndet in some company, my quality of life is completely broken. Now, my interest is Dolbeault cohomology of almost complex structure.


## Future development plan of this package

 - should I omit all Jafroc related codes? I did not use it. Cuz completely this pkg is independent from Jaf, so,  I guess

 - should omit .xlsx file for example FROC data,  cuz it is heavy.

 I does not work as Statistician, so I no longer know what is required in practical theme. So, I should quit pkg deve, I am so exhausted, honestly.

###  Explanatory Variables 
 - Biamarkers as explanatory variables for FROC models which can be done via link functions, for simplicity, linear functions are considered. But this is general scheme of Stats, so it is not new. I wanna be dead. Explanatory variable is already appeared as ratings (confidence levels of locarization). The cute author provides arbitrary number of confidence levels, so even if explanatory variables are continuous, we can consider it as a descrete one by deviding into lattice, and then confidence level can be replaced for such explanatory variables. so, when the dimension of explantory variable is one, then there is trivial. The difficulty arises in high dimensional cases.

 - should remove `ppp()` and related functions, cuz they are no longer required and are replaced by codes in generated quatities blocks in Stan files. If parallel calc. of sampling was implemeted, then i should use it.
  
 - Some pretty cute mad peng, i.e., the author, he noticed that ROC analysis by Bayesian will be also required by doggies and if my health was go on, then my heart would go on. Unfortunately, now, I am such a couch potato with MCS symptoms. For three years, i suffer all painful life. Why pretty cute author's life is so painful why... Honesty.  Anyway, my office is sometimes certain station or fucking park at which above me only sky. ROC models are also very similar and easy to implement. If I lived until that time, then I would implement ROC models in this pkg and if I was dead then I would be dead. I would write a programs in station or park with mosquitoes demand of bloods.

* In MRMC case, Chi square calculation should be moved into Generated block in Stan file, then make a slot for p value in case of MRMC

* In GUI, if the second row in the table is vanished, then  ppp()  failed,....  Why does such bug occur?? ha. I am tired. 2020 March 8

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
    * of the two distribution between Gaussian signal and differential logarithmic Gaussian.   Canonical Gaussian and signal Gaussian is element of Poincare upper half plane, and its geodesic distance is easy to calculate. But differential Logarithmic Gaussian is not an element of the Poincare upper half plane, and thus it is difficult to calculate such  Fisher metric. To do so, first, we should define the parametric family of probabilities such that is contains Gaussian and deferential logarithmic Gaussian.

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




I was so disappointed R programming, cuz it dose not give me any loves!

In the current version, I try to include an article but if do, I cannot publish it in journals, so I cannot.
I noticed multi-nomial shotten the descriptions, so, I have to revise.
I use FROC only one time, so I no longer need this. The aim of this pkg development is  to rescue my life, but I figured out it cannot. So, motivation is very low.


In the future, I should write down priors by target += formulation, but now I did not do that, cuz I am tired. And, using GUIs, the author should make GUI to be valid the various priors and show how it affects estimators, but now, it dose not be accomplished cuz now, I wanna sleep.


## ver. 0.4.0

Now, the author is a homeless for 6 months. My smell is fucking sucks! Thanks for K.

### Major revision
   
   Using generated quantities block in Stan files, calculation of posterior predictive p values are dratistically improved. The result is reasonable. Also, in MRMC, the author wrote down the model by using equivalent distributions, namely, multi-nomial parts.

### Minor comments

  - `plot_dataset_of_ppp()` and `plot_dataset_of_ppp_MRMC()` are lanched.

 - To be henest, awww, the PPP is not same as my cute intuition. Why? Homeless is not so ... why?

 - In Shiny, initial values in GUI dose not work, so I put treatments if initial values are correctly picked up.

 - Onedrive disturbs path and also it confuses  Japanese and English.
  To fix it, right click on explore's desktop icon > property > basyo then change Japanese word Desktop into English. Or hyoujyunnimodosu
  
 - Apologies for previous version. First of all,  about p value,  I have to launch Applo 11, cuz calculation of p value is not correct in the previous version. In this version, the calculation is improved using generated quatities block of Stan. To tell the truth, there is an another apology. That is, Applo 13 lifts off because, now, I did not use target += for priors, thus constant terms are not included in calculations. Now, I did not fix it, cuz I am such a couch potato. Furthemore, Applo 14 takes off because in the previous version, `fit_GUI_Shiny()` did not work.

 - I am not sure, but WAIC is influenced by the parameters in Genareted Quantities block in Stan files? I do not know but, I don't wanna bother me, so, I am gonna such a couch potato. It is not good to aim perfect. This is only toy, like a baby's toys. Todays is good.
 
 - comparison of prior and post. But now I am such a couch potato, so ... not yet.
 - PPP calculation is drastically improved, thus, the author gives version number, as a major up date.

 - In this pkg depends on pkgs which is used for the usage of Microsoft Offices, but it is redundant and the author never use it. So, I should omit these unnecessary dependencies, cuz it looks not so stable pkg because it uses another languages. Also, I have removed diagram pkg. 
 
 - Calculation of Post. Pred. P value is, now,  more reliable, but more validation or time is required to verify that it is correctly made. 
  -life is dirty painful, really painful, 2020 nov 26 it is not only symptoms of MCS, I am a homeless, without any support, cold, dirty clothes, sucks, this pkg is made under symptoms of MCS... 
 
 - There is a benefit to include ppp calculations in Stan files, but there is a disadvantage  that it makes MCMC more to be more unstable and require lots of times. Oh my gosh! Dang it!!
 If Prof. R Lang supplies the vectorizations of random sampling functions such as rbinom() rmultinom() then I can remove the calculation part of ppp from Stan and MCMC will be much faster as before. But now, Prof R dose not. Today, my MCS symptoms are calm down. But this typing worsen my aches, so in fingers, aches. Arm also has aches.


 - From this current release, I calculates a PPP(poster. pred. p value) on Stan files but it makes MCMC more unstable. Why? Or this is only incorrect, fucking guess... imagination .... fuck.  

 - `for(cd in 1:C) x[cd] <- x[C+cd-1]` dose not reverse the vector x. 
  In fact, 
  
  `x[1] <- x[3]`
  
  `x[2] <- x[2]`
  
  `x[3] <- x[1] (<- x[3])`
  
  therefore, the resulting vector is `x[3],x[2],x[3]`.
  
  I suffer MCS, this is the very bad world line. I hope that I live in another world sheet. I guess  my dirchlet initial value is not so good. Bad D brane. 

   


 - Vectorization of ppp (posterior predictive p value) is difficult through Prof. R Lang, cuz he dose not support the vectorized functions including `rmultinom()`, `rbinom()`, Haa, come on, Prof. Rlang. But, you know, the author is cute, so I found that, by using fucking generated quantities block in Stan files, we can get desired calculations for ppp with much faster time than via Prof. R Lang! What a hell! Great! Thanks Prof. Stan, you are fire! a mad peng! Now, I guess this notification should be worthwhile to Novel prize! But now, I am not informed yet,  haa, what are Committee doing, i already prepare suits and shoes for the prize. please please me! I will go trip to all worlds using the money of prize. But, now, it is only amount of scholarship, -70000 dollars. I cannot get back such great scholarship. Because, now I am a homeless with MCS patient, prurigo nodularis, atopic dermatitis! 

 - *Vectorization* of the calculation of the _posterior_ _predictive_ p value. Today I noticed that the vectorization can be adopted to calculate ppp. But now, I am being such a couch potato.

 - Even if I remove the bugs in this fucking pkg, anyone dose not gives any lemonade. So, completely  voluntarily I work to remove bugs without lemonade. Even if I can drive away Gozira, in such a difficult jobs, I would not have any lemonade. Today, it is very cold. If someone gives me a cup of hot coffee, then I will make a GUI for prior selections. 

 - I have to apologize that I was a such couch potato and programmed GUIs very rigorously. In the previous version, cuz I was such a couch potato, GUIs included many fucking bugs. But in the current version, the author is, now, such a sweet potato! So, fix bugs. I am done! I love me! What I am scared is that bugs said "I'll be back!".

 - Remove bugs in `fit_GUI_Shiny()`. In the previous version, there are errors, cuz I was such a couch potato. The errors are caused by initial values which are nulls. It's easy to remove this bug, but needs patient and I did not have any patients cuz potato. I had not gonna debug, ha, I'm exhausted. So, I fixed the bug but the new problem  which is that the new GUIs runs rstan in three times in the first running. Ha, shiny programming is suck and my life sucks. Bug is only, that's all it takes. I love errors in this fuking pkg. Oh! Today, I can remove it. I am a cute genious mathematician!
 

I do not think my ppp is correctly programmed. But, now, I cannot reveal where I am wrong. 
It seems correct, but, the calculation is not compatible with my lovely intuition including  visualizations of estimates and dataset synthesized from posterior predictive p value. So, where dose I mis-programmed ,,, Uhnnnnn I cannot. 


In roxygen comment, `\code{}` should not be in mutiple lines but the author always do this and R is nagging. Sorry,  Prof. R Lang. 

 - I'm slacking off. I am hungry. What I want to eat is ... vegetables including  ... vitamin C for conjugations, detoxifications, I need them. 

 - If I could remove the redundant initial fitting caused by shiny's unknown behavaour, then, i would be a mad peng. <- I can do this! Great!
 
 
 - In GUI, to preserve variables in trace plot, autocorrelations, and posteriors, I need some treatment, but, now, I am such a couch potato.
 
 - hide debugging comments in GUI, but now, couch potato. Ha, winter is ...coming, today condition is not good.
 
 
  - In GUI, I found in errors including initial value issues, ha... couch potato.
  
 
  - In GUIs, I fix bugs. Even if I fix bugs, it says "I'll be back". Oh my gosh. Please terminate these fucking bugs!
  
  - Today, 2020 Oct 9, I feel aches as symtoms of MCS. For three years, I have devoted the patient charged to syndet. Three years, I live with aches, it is too  long to ha, Itai, aches, aches, when I recover from MCS, when will I overcome this. I am disapointed radiology cuz it cannot detect my symptoms of MCS comming from syndet.  Dont let me down! Nobody ever let me downn but syndet! aches. I must fight to 
 
 
 
 
 
 
 
 
 
 
 

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
* Implements the null hypothesis test via Bayes factor. (The result is converse, why???)
* Stan file had been changed from improper priors to proper priors.

## ver. 0.1.2

* Fix bugs.
* Fix English or Grammar in documents of manual and vignettes.
* I found [F7] key is useful for spell check of Rmd file.

## ver. 0.1.1

* 2019.5.9   the first upload  to `CRAN`.
