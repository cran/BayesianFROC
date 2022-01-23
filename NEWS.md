

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
`


## ver. 1.0.0 



* In the previous stan file, _calulator of pvalue was wrong_, so I fixed it. Now, P value works fine and it is compatible with my intuition. OK! I have done. The code was slightly wrong as follows in which  I fixed it from `C + cd -1` into `cd`. By this correction, P value calculation looks reasonable. 

 Previous wrong code    `// ss_doubly_indexed[cd,jjj]=(hits_post_ext_doubly_indexed[C+1-cd,jjj]-NL*p_rev_Extented[cd])^2/(NL*p_rev_Extented[cd]);`
Current correct code     `ss_doubly_indexed[cd,jjj]=(hits_post_ext_doubly_indexed[cd,jjj]-NL*p_rev_Extented[cd])^2/(NL*p_rev_Extented[cd]);//2022Jan20`


 Previous wrong code   `// xx[cd]=(h[C+1-cd]-NL*p_rev_Extented[cd])^2/(NL*p_rev_Extented[cd]);`
Current correct code  `xx[cd]=(h[cd]-NL*p_rev_Extented[cd])^2/(NL*p_rev_Extented[cd]);//2022Jan20`

This correction is very small but important, so this is the reason of the "major" update.


# As one of the patients with chemical sensitivity, I strongly criticize companies that make volatile organic compounds such as synthetic detergents, pesticides, and deodorants.


* remove functions and whose file, such as 
`ppp(),` 
`ppp_srsc()`, 
`ppp_MRMC()`, 
`p_value_visualization()`. They did not use Stan for calculation of Post. pred. p value.

But, Stan is not so good to calculate p value. Because even if R does not launch NaN in some 
caculation, stan launched NaN and which causes that fitted model object seems not reliable. Even if MCMC sample of model parameter is drawn well, there is a case that generative quantities block does not get good samples (including NaN). In this case, I calculate by R with samples of model parameter then the calculation is good even if calculation of  generative quantities block is bad.


* In shiny, `icon = shiny::icon("bar-chart-o")`, is omitted cuz it launces unknown warnings. So behaviour of shiny is unstable. 

* In the previous GUI, if rows of table are added, then the GUI was broken, so now, I fixed it. So, new GUI is more confortable. But, in my computer, R-studio session crashed frequently. Why???

* Initial values in Shiny GUI. In the previous version, I did not put a code for initial values of UIs in Shiny and it caused crashing on R session. Now, in the current version, if NAs are passed to UIs, then "if sentence" works to deal it without errors. It should be try-catch sentence?
 
* In plot of post. pred. p value, plot did not be colored correctly unless C is three, so, its solution is that I replaced 3 to C in some line. It was very small correction but I always do such a careless miss.

* FUTURE PLAN: I did not make codes on stan file to calculate transform data. There is a reason, because in Stan, transformed data cannnot be obtained so,..transformed data block is not useful. but ... this make codes to be more complicated.

* Shiny put a input for initial values but it  does not work in some context, so I put a treatment for initial values are NaN or etc. 

* FUTURE: About css  Shiny still does not run without warnning, but I do not understand what the warining says.
 
 

Prof. R. Lang does not "run as administrator" but he try to install packages 
in `C:\Program Files\R\R-4.1.2\library` which allows for administrator.

So the following error occurs 
`is not writable Error in install.packages("devtools") : unable to install packages` 

Moreover, in case of windows, when the above adiministrator issues occurs, Prof. R lang suggests to make a directory to install pkgs on 
Onedrive, which also fails for non English languages. So, in default, R fails to 
install pkgs and Prof. R Lang does not know but he let us to struggle for this issues.
I hope the directory suggtested by R is not on Onedrive.
When I install pkgs on Linux (Ubuntu distribution) on vbox, of course Onedrive issue does not occur.


One solution: Set variable `R_LIBS_USER` and path `C:/Users/tsuno/aaa` in a writable directory (e.g., `C:/Users/tsuno/lib`) for non administrators. Such a directory is not automatically made, so I have to make it. Moreover, if I declare `.libPaths("C:/Users/tsuno/bbb" )` in the .Rprofile in project, then .Rprofile and the above path should be same, i.e., `aaa` = `bbb`. In default, R try to install pkgs into Onedrive but it is problematic. Onedrive is not good for non-English users.

When I use `devtools::release()`, I have to install git (not from R console but from web page)
 If not then
     
    Error in system2("git", c("rev-parse", "--abbrev-ref", "HEAD"), stdout = TRUE) : 
      '"git"' not found
 
 
Before submission, remove files  `inst > extdata > ****.rds ` 



## ver. 0.5.0

 - Now, shiny does not work along my intention. To increase number of confidence levels, we cannnot use the function `BayesianFROC::fit_GUI_Shiny()` but its prototype `BayesianFROC::fit_GUI()` still alive.
 In this package, there is a three pkgs beyond my ability, that is, rstan, shiny, rJava. Now, I remove rJava by removing xlsx package. But, I cannot control shiny in details. so, if I increase rows of TPs and FPs, then shiny is automatically crashed. So,... now, I cannot understand what should I do. If null or NA occurs, then I have to stop some things, maybe.


 - More samples are picked than the previous version. Posterior predictive p value is calculated by the measure $$l(y|\theta)\pi(\theta|y)d\theta dy,$$
    where the former is likelihood and the latter is posterior measure. To calculate it, we need samples of     posterior
 
  $$\theta_1, \theta_2,... \sim \pi(\theta|y)$$ and samples from models, namely,

 $$ y_{1,1},y_{1,2},y_{1,3}, ....,y_{1,N} \sim l(y|\theta_1),$$
 $$ y_{2,1},y_{2,2},y_{2,3}, ....,y_{2,N} \sim l(y|\theta_2),$$
 $$ y_{3,1},y_{3,2},y_{3,3},  ...,y_{3,N} \sim l(y|\theta_3),$$
 $$ y_{4,1},y_{4,2},y_{4,3}, ....,y_{4,N} \sim l(y|\theta_4),$$
 $$:$$
 $$:$$
 
  In the previous version, I implemented in case of N = 1, only. But it was not sufficient theoretically but maybe practically          sufficient? So, in the current version,
   we use arbitrary N in the function ` BayesianFROC::fit_GUI_Shiny()  `. This was done, 1 years ago. but I do not upload because  everything seemed boring about this package.


 - Implements Bad ROC model, MCMC is meaningless but includes it.
 - Some estimate is not printed in GUI, so I fix the bug.

 -  Omit an  .xlsx file which was for example FROC data
 
 - Reduce dependencies of packages such as xlsx, etc, because they are difficult to install or setting PATHs. So, now, I made a new pkg `tpfp` in which separated functions are.


 - To calculate posterior predictive p value for goodness of fit, double integral is calculated. But in the previous version, it is not so. So, I make stan file so that it is correctly double  integral.

 -` usethis::edit_rstudio_snippets()` delete all  default values, why? this is not good.... but return value gives its path, this is nice.

 - rJava and xlsx is difficult to  install or setting of PATHs. so I remove these pkgs.
 
 The following let rJava work fine, but I am not sure this is effective for others. 
 
     if(Sys.getenv("JAVA_HOME")!=""){
        Sys.setenv(JAVA_HOME="")
    }
    library(rJava)
 
 
 
 
 Why  `Sys.getenv()` shows all paths and sometimes contains ?????? which is non-English words, for example,
 
      HOME                             C:/Users/A_user/OneDrive/?????? <- ????? is not English, so causes issues
      R_USER                           C:/Users/A_user/OneDrive/??????

 
 
 
 To correct the OneDrive issues, I have to run the following code on R(Studio) console
 
     # work around mis-encoded environment variables
    USERPROFILE <- Sys.getenv("USERPROFILE")
    HOME <- file.path(USERPROFILE, "Documents", fsep = "\\")
    R_USER <- HOME
    R_LIBS_USER <- file.path(HOME, "R", "win-library", getRversion()[1, 1:2], fsep = "\\")
    Sys.setenv(
      HOME = HOME,
      R_USER = R_USER,
      R_LIBS_USER = R_LIBS_USER
    )
    # update library paths
    if (!isTRUE(file.info(R_LIBS_USER)$isdir))
      dir.create(R_LIBS_USER, recursive = TRUE)
    .libPaths(R_LIBS_USER)
 
 
 message(Sys.getenv("HOME"))
 message(Sys.getenv("R_USER"))
 
 message(Sys.getenv(c("HOME", "R_USER")))
 
 

 - Upper bounds of MCMC samples is flaby in GUI of `fit_GUI_Shiny()` 


I had forgetten the following code before submission, Oh my gosh.

  - `devtools::build_vignettes()`

Before submission, what I have to do are the followings

  - `devtools::build_manual();devtools::build_readme();devtools::build_vignettes()`
   
  - `devtools::check_win_devel()` Check on cloud.
  
  - Do not forget to exclude .rds files in inst < extdata.
  
  - `BayesianFROC:::file_remove()`




## Future development plan of the author's life
- I suffer from Chemical Sensitivity (CS) and in my initiating toxicant is a *_synthetic detergent (i.e., syndet)_* named ooooo (I cannot say) made by some large company in Japan. I use it in job. My exposure began 
irritant stimulus in my whole body. One month later from this exposure, 
my whole body has chronic inflammation causing fire in body and brain. My symptom makes my quality of life to be much low for several years.
- In japan, there are famous three procedures are available to diagnosis CS, i.e., tracking eye movement examination and pupllary function and standing ability. Unfortunately, the author had positives for all examinations at once. By Martin Pall who is the author of *General, Applied and Systems Toxicology*, revealed the mechanism of the reason why the inflammation is chronic. According to him, the nitric oxide (NO) and the peroxynitrite anion(ONOO-) constructs the so-called NO/ONOO- (pronunciation is  NO!Oh,NO!! cycle) is running by the toxicants including syndet, softner, metyl Hg, sarin, ... and it is loops, thus another cycle is made, so it took a long time to recover from CS. 
To detoxify them, melatonin reportedly reacts with NO, ONOO-, etc.

As a detoxification in our body, there is the notion of conjugation. To help this, vitamin and minerals are recommended. Manufactures never accept the toxicity of their products such as syndet.



## Future development plan of this package
 - Implements Good ROC model, MCMC is meaningless, whih will  be done because the following paper.
 
 
 *Reference* Bayesian analysis of a ROC curve for categorical data using a skew-binormal model; Balgobin Nandram and Thelge Buddika Peiris (This paper is nice!); 2018 volume 11 369-384; Statistics and its interface


 


##  Explanatory Variables 

 - should remove `ppp()` and related functions, cuz they are no longer required and are replaced by codes in generated quatities blocks in Stan files. If parallel calc. of sampling was implemeted, then i should use it.
  

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

 


## ver. 0.4.0


### Major revision
   
   Using generated quantities block in Stan files, calculation of posterior predictive p values are dratistically improved. The result is reasonable. Also, in MRMC, the author wrote down the model by using equivalent distributions, namely, multi-nomial parts.

### Minor comments

  - `plot_dataset_of_ppp()` and `plot_dataset_of_ppp_MRMC()` are lanched.

 - In Shiny, initial values in GUI dose not work, so I put treatments if initial values are correctly picked up.

 - Apologies for previous version. First of all,  about p value,  I have to launch Applo 11, cuz calculation of p value is not correct in the previous version. In this version, the calculation is improved using generated quatities block of Stan. To tell the truth, there is an another apology. That is, Applo 13 lifts off because, now, I did not use target += for priors, thus constant terms are not included in calculations. Now, I did not fix it, cuz I am such a couch potato. Furthemore, Applo 14 takes off because in the previous version, `fit_GUI_Shiny()` did not work.

 - I am not sure, but WAIC is influenced by the parameters in Genareted Quantities block in Stan files? I do not know but, I don't wanna bother me, so, I am gonna such a couch potato. It is not good to aim perfect. This is only toy, like a baby's toys. Todays is good.
 
 - comparison of prior and post. But now I am such a couch potato, so ... not yet.
 - PPP calculation is drastically improved, thus, the author gives version number, as a major up date.

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


 - Remove bugs in `fit_GUI_Shiny()`. In the previous version, there are errors, cuz I was such a couch potato. The errors are caused by initial values which are nulls. It's easy to remove this bug, but needs patient and I did not have any patients cuz potato. I had not gonna debug, ha, I'm exhausted. So, I fixed the bug but the new problem  which is that the new GUIs runs rstan in three times in the first running. Ha, shiny programming is suck and my life sucks. Bug is only, that's all it takes. I love errors in this fuking pkg. Oh! Today, I can remove it. I am a cute genious mathematician!
 


In roxygen comment, `\code{}` should not be in mutiple lines but the author always do this and R is nagging. Sorry,  Prof. R Lang. 

 - If I could remove the redundant initial fittings caused by shiny's unknown behavaour, <- I can do this! Great!
 
 
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
