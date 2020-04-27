




#' @title MRMC or srsc: Posterior
#' Predictive P value (PPP) for MRMC or srsc.
#' @description PPP for chi square
#'  goodness of fit statistic.
#'
#' @details I hate the notion
#' of p value and this is the
#'  motivation that I developed
#'   new FROC theory.
#' However, I cannot overcome
#'  the traditional bitch.
#' I hate statistics since p value
#'   is  bitch, monotonically decreases
#'    when the sample size is large.
#'    In some papers, I forget the name, but in some papers,
#'    one pointed out that the frequentist p values precisely coincides
#'    some poseterior probability of some event (I forget this but such as mean1 is greater than mean2).
#'
#'
#'  In some suitable condition, I
#'  conjecture that Bayesian p value
#'    coincides to frequentist p value
#'     in some sense  such as analytically
#'     or its expectation of a posterior
#'     or etc or large MCMC samples
#'  So, p value is bitch and bitch and bitch.
#' I emphasize that notion of p value is
#'  bitch and its background is unknown.
#' In suitable condition, frequentist
#' p value bitch is equal to a probability
#'  of some event measured by posterior.
#' So,... Bayesian method cannot break
#'  the traditional frequentist bitch.
#'   Bayesian and frequentist are all bitch!!
#' Of course, intuitively, it is good.
#'  But, the theoretically,
#'  it does not satisfies naturalist.
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams DrawCurves
#'@inheritParams draw_latent_signal_distribution
#'@param plot Logical, whether
#'replicated datasets are drawn.
#' @return A positive number between zero and one,
#' indicating Posterior
#' Predictive P value (PPP). In addition, it plots replicated
#'  datasets which are used to calculate a ppp.
#' @export
#'
#' @examples
#'
#'
#'
#' \dontrun{
# ####1#### ####2#### ####3#### ####4#### ####5#### ####6#### ####7#### ####8#### ####9####
#'
#'
#'
#'#   The 1-st example: MRMC data
#'#========================================================================================
#'#                        1)  Fit a Model to MRMC Data
#'#========================================================================================
#'
#'
#'
#'
#'                fit <- fit_Bayesian_FROC( ite  = 111,  dataList = ddd )
#'
#'
#'
#'
#'
#'#========================================================================================
#'#  2)  Evaluate Posterior Predictive P value for the Goodness of Fit
#'#========================================================================================
#'
#'
#'
#'
#'
#'
#'                                 ppp(fit)
#'
#'
#'
#'
#'
#'#  If this quantity, namely a p value, is greater,
#'#  then we may say that our goodness of fit is better. (accept the null hypothesis)
#'#  In the traditional procedure, if p-value is less than 0.05 or 0.01 then we reject
#'#  the null hypothesis that our model fit to data well.
#'
#'
#'
#'
#'# Of course, even if p-values is small, we should not ignore our result.
#'# P value bitch is not so clear what it does and in frequentist methods,
#'# we experianced p value is bitch with respect to sample size.
#'# So, in Bayesian context, this bitch might be bitch with respect to ...
#'# Anyway, but ha....many statisticians like this bitch.
#'
#'
#'
#'
#'
#'
#'#   The 2-nd example uses  data named d
#'#========================================================================================
#'#                  1)  Fit a Model to  Data
#'#========================================================================================
#'
#'
#'
#'
#'                        fitt <- fit_Bayesian_FROC( ite  = 111,  dataList = d )
#'
#'
#'
#'
#'#========================================================================================
#'#  2)  Evaluate Posterior Predictive P value for the Goodness of Fit
#'#========================================================================================
#'
#'
#'
#'                                ppp(fitt)
#'
#'
#'
#'#  If this quantity is greater, then we may say that our model is better.
#'
#'#  I made this ppp at 2019 August 25.
#'
#'
#'
#'#========================================================================================
#'#                             PPP is problematic
#'#========================================================================================
#'
#'# Consider the dataset:
#'
#'
#' dat <- list(c=c(4,3,2,1),    #     Confidence level. Note that c is ignored.
#'             h=c(77,97,32,31), #     Number of hits for each confidence level
#'             f=c(77,1,14,74),  #     Number of false alarms for each confidence level
#'
#'             NL=259,        #     Number of lesions
#'             NI=57,         #     Number of images
#'             C=4)           #     Number of confidence level#'
#'
#'
#'# Fit a model to the data
#'
#'
#'              fit <- fit_Bayesian_FROC(dat)
#'
#'
#'# calculate p value
#'
#'
#'
#'              ppp(fit)
#'
#'
#'# Then we can see that FPF and TPF are far  from FROC curve, but p value is not
#'# so small, and thus in this case, ppp is not the desired one for us.
#'
#'
#'# In our model, we need monotonicity condition, namely
#'#
#'#    h[1] > h[2] > h[3] > h[4]
#'#    f[1] < f[2] < f[3] < f[4]
#'#
#'#  However the above dataset is far from this condition, and it would relate the
#'#  above undesired p value.
#'#   Revised 2019 Sept 7
#'# Of course it is no need to satisfy this monotonicity precisely, but good data
#'# should satisfy.
#'# Since doctor should not wrong (false positive) diagnosis with his high confidence.
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'}
#'
#'
#'
#'
ppp <- function(StanS4class,Colour=TRUE,dark_theme=TRUE,plot=TRUE ,summary = TRUE){
  fit <-StanS4class
  studyDesign <- fit@studyDesign

  if (studyDesign == "MRMC")  ppp <- ppp_MRMC(fit,summary = summary)
  if (studyDesign == "srsc.per.image"){  ppp <- ppp_srsc(fit,Colour=Colour,dark_theme=dark_theme,plot=plot,summary = summary);ppp <- ppp$p.value;}
  if (studyDesign == "srsc.per.lesion"){  ppp <- ppp_srsc(fit,Colour=Colour,dark_theme=dark_theme,plot=plot,summary = summary);ppp <- ppp$p.value;}

  names(ppp) <- "Posterior Predictive P value for chi square goodness of fit"


  if (summary==TRUE)return(ppp)
  invisible(ppp)
}

































#' @title Calculates PPP for Models of a single reader and a single modality (Calculation is correct! :'-D)
#' @description Calculates Posterior Predictive P value for chi square (goodness of fit)
#'@inheritParams DrawCurves
#'@inheritParams  draw_latent_signal_distribution
#'@inheritParams fit_Bayesian_FROC

#' @author Issei Tsunoda, Prof. of Curlbus University, Mosquitobus and Gostbus univ. also. My technique of catch mosquitos are execellent, so, I am a prof. ha,, employ me. My health is bad, my life will be over.
#' @return A list, including p value and materials to calculate it.
#'@param plot Logical, whether replicated data are drawn, in the following notation, replicated data are denoted by \eqn{y_1,y_2,...,y_N}.
#'@param replicate.number.from.model.for.each.MCMC.sample A positive integer, representing \eqn{J} in the following notation.

#'Now, I think all I needed is love! ttu ttu tututu Love is all I need.
#'
#'Suppose that  \deqn{\theta_1, \theta_2, \theta_3,...,\theta_n}   is drawn from posterior \eqn{\pi(\theta|D)} of given data \eqn{D}.
#'
#'Let \eqn{y_1,y_2,...,y_n} be samples drawn from
#'
#'\deqn{y_1 \sim likelihood ( . |\theta_1), }
#'\deqn{y_2 \sim likelihood ( . |\theta_2),}
#'\deqn{y_3 \sim likelihood ( .|\theta_3),}
#'\deqn{...,}
#'\deqn{y_n \sim likelihood ( . |\theta_N),}
#'
#'
#'
#'Then the list of return values retains the following:
#' \describe{
#' \item{ \code{chisq_at_observed_data}      }{ \deqn{\chi (D|\theta_1), \chi (D|\theta_2), \chi (D|\theta_3),...,\chi (D|\theta_n),} }
#' \item{ \code{chisq_not_at_observed_data}  }{\deqn{\chi (y_1|\theta_1), \chi (y_2|\theta_2), \chi (y_3|\theta_3),...,\chi (y_n|\theta_n),  }}
#' \item{ \code{Logical}                     }{ The i-th component is a logical vector indicating whether  \deqn{\chi (y_2|\theta_2) > \chi (D|\theta_2)} is satisfied or not. Oppai ga Ippai. If \code{TRUE}, then the inequality holds.}
#' \item{ \code{p.value}                     }{  From the component \code{Logical}, we calculate the so-called \emph{Posterior Predictive P value}. Note that the author hate this notion!! I hate it!! Akkan Beeeee!!! }
#' }
#'
#' @details
#' In addition, this function plots replicated datasets from model at each MCMC sample generated by HMC.
#' Using the Hamiltonian Monte Carlo Sampling: HMC.
#' we can draw the MCMC
#' samples of size  \eqn{n}, say  \deqn{\theta_1, \theta_2, \theta_3,...,\theta_n },
#' namely,
#' \deqn{\theta_1  \sim \pi(.|D), }
#' \deqn{\theta_2 \sim \pi(.|D), }
#' \deqn{\theta_3  \sim \pi(.|D),}
#' \deqn{...,}
#' \deqn{\theta_n  \sim \pi(.|D).}
#'    where \eqn{\pi(\theta|D)} is the posterior for given data \eqn{D}.


#'
#' Then, the function plots the following datasets \eqn{y_1^1,y_2^1,...,y_I^J}.
#'
#'\deqn{ \chi(y_{1,1}|\theta_1), \chi(y_{1,2}|\theta_1), \chi(y_{1,3}|\theta_1),..., \chi(y_{1,j}|\theta_1),...., \chi(y_{1,J}|\theta_1),}
#'\deqn{ \chi(y_{2,1}|\theta_2), \chi(y_{2,2}|\theta_2), \chi(y_{2,3}|\theta_2),..., \chi(y_{2,j}|\theta_2),...., \chi(y_{2,J}|\theta_2),}
#'\deqn{ \chi(y_{3,1}|\theta_3), \chi(y_{3,2}|\theta_3), \chi(y_{3,3}|\theta_3),..., \chi(y_{3,j}|\theta_3),...., \chi(y_{3,J}|\theta_3),}
#'\deqn{...,}
#'\deqn{ \chi(y_{i,1}|\theta_i), \chi(y_{i,2}|\theta_i), \chi(y_{i,3}|\theta_i),..., \chi(y_{i,j}|\theta_i),...., \chi(y_{I,J}|\theta_i),}
#'\deqn{...,}
#'\deqn{ \chi(y_{I,1}|\theta_I), \chi(y_{I,2}|\theta_I), \chi(y_{I,3}|\theta_I),..., \chi(y_{I,j}|\theta_I),...., \chi(y_{I,J}|\theta_I).}
#'
#'
#'where \eqn{L ( . |\theta_i)} is a likelihood at parameter \eqn{\theta_i}.
#'
#'
#'
#' Let \eqn{ \chi(y|\theta)  } be a chi square goodness of fit statistics of our hierarchical Bayesian Model
#'
#'
#' \deqn{\chi(y|\theta) := \sum_{r=1}^R \sum_{m=1}^M \sum_{c=1}^C ( \frac { ( H_{c,m,r}-N_L\times p_{c,m,r})^2}{N_L\times p_{c,m,r}} + \frac{(F_{c,m,r}-(\lambda_{c} -\lambda_{c+1} )\times N_{L})^2}{(\lambda_{c} -\lambda_{c+1} )\times N_{L} }).}
#'
#'
#' and a chi square goodness of fit statistics of our non-hierarchical Bayesian Model
#'
#'\deqn{\chi(y|\theta) :=  \sum_{c=1}^C \biggr( \frac{( H_{c}-N_L\times p_{c})^2}{N_L\times p_{c}} + \frac{(F_{c}-(\lambda_{c} -\lambda_{c+1} ) )\times N_{L}]^2}{(\lambda_{c} -\lambda_{c+1} )\times N_{L} }\biggr).}
#'
#'where  a dataset \eqn{y} denotes \eqn{ (F_{c,m,r}, H_{c,m,r}) } in MRMC case and  \eqn{ (F_{c}, H_{c}) } in a single reader and a single modality case,
#'and model parameter \eqn{\theta}.
#'
#'
#' Then we can calculate the \emph{posterior predictive p value} for a given dataset \eqn{y_0}.
#'
#' \deqn{ \int \int I( \chi(y|\theta) > \chi(y_0|\theta) )   f(y|\theta) \pi(\theta|y_0) d \theta d y  }
#' \deqn{ \approx \int \sum_i I( \chi(y|\theta_i) > \chi(y_0|\theta_i) )   f(y|\theta_i) d y   }
#' \deqn{ \approx \sum_{j=1}^J \sum_{i=1}^I I( \chi(y_{i,j}|\theta_i) > \chi(y_0|\theta_i) )     }

#'
#'
#'
#'   When we plot these synthesized data-sets \eqn{y_{i,j}}, we use the \code{jitter()} which adds a small amount of noise to \strong{avoid overlapping points}.
#' For example, \code{jitter(c(1,1,1,1))} returns  values: \code{1.0161940 1.0175678 0.9862400 0.9986126}, which is changed
#' from \code{1,1,1,1} to be not exactly 1 by adding tiny errors to avoid overlapping. I love you. 2019 August 19
#' Nowadays, I cannot remove my self from some notion, such as honesty, or pain, or,.. maybe these thing is no longer with myself.
#' This programm is made to fix previous release calculation. Now, this programm calculates correct p value.
#'
#' So... I calculate the ppp for MCMC and Graphical User Interface based on Shiny for MRMC, which should be variable such as
#' number of readers, modalities, to generate such ID vectors automatically. Ha,... tired! Boaring, I want to die...t, diet!!
#' Tinko, tinko unko unko. Manko manko. ha.
#'
#' Leberiya, he will be die, ha... he cannot overcome, very old, old guy.
#'  I will get back to meet him. Or I cannot meet him? Liberiya,...very wisdom guy,
#' Ary you already die? I will get back with presents for you. Ball, I have to throgh ball, and he will catch it.
#'
#'
#' The reason why the author made the plot of data drawn from \strong{Posterior Predictive likelihoods with each MCMC parameters} is
#' to understand our programm is correct, that is, each drawing is very mixed. Ha,.... when wright this,... I always think who read it.
#' I love you, Ruikobach. Ruikobach is tiny and tiny, but,... cute. Ruikosan...Ruiko...
#'But he has time only several years. He will die, he lives sufficiently so long, ha.
#'
#'Using this function, user would get \strong{reliable posterior predictive p values}, Cheers! Pretty Crowd!
#'
#'
#'
#' We note that the calculation of posterior perdictive p value (PPP) relies on the law of large number.
#' Thus, in order to obtain the relicable PPP, we need to enough large MCMC samples to approximate
#' the double integral of PPP.
#' For example, the MCMC samples is small, then R hat is far from 1 but, the low MCMC samples leads
#' us to incorrect p value which sometimes said that the model is correct even if the R hat criteria
#' reject the MCMC results.
#'
#'
#'
#'
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#'
#'#========================================================================================
#'#            1) Create a fitted model object with data named  "d"
#'#========================================================================================
#'
#'
#'
#' fit <- fit_Bayesian_FROC( dataList = d,
#'                               ite  = 222 # to restrict running time, but it is too small
#'                            )
#'
#'
#'
#'#========================================================================================
#'#            2) Calculate p value and meta data
#'#========================================================================================
#'
#'
#'
#'                               ppp <- ppp_srsc(fit)
#'
#'
#'
#'#========================================================================================
#'#            3) Extract a p value
#'#========================================================================================
#'
#'
#'
#'
#'                               ppp$p.value
#'
#'
#'# Revised 2019 August 19
#'# Revised 2019 Nov 27
#'

#'}
#'
#'
#'



ppp_srsc <-function( StanS4class,
                     Colour = TRUE,
                     dark_theme=TRUE,
                     plot=TRUE,
                     summary = TRUE,

                     replicate.number.from.model.for.each.MCMC.sample =100#2019 Sept 8
){
  fit <- StanS4class

  if (fit@studyDesign =="srsc.per.image")  ModifiedPoisson <- FALSE
  if (fit@studyDesign =="srsc.per.lesion") ModifiedPoisson <- TRUE

  C <- fit@dataList$C
  N<-C
  c<-C:1
  NL <- fit@dataList$NL
  NI <- fit@dataList$NI


  if (ModifiedPoisson) NX<-NL#2020Feb
  if (!ModifiedPoisson) NX<-NI#NX   2020Feb----

  e <-rstan::extract(fit)




  w <-vector()
  z <- list()
  dz <- list()
  p <- list()
  hitrate <- list()

  l <- list()
  dl <- list()
  m <- vector()
  v  <- vector()
  a <- vector()
  b  <- vector()
  dataList <-list()
  h <- list()
  f <- list()
  h.per <- list()
  f.per <- list()
  estimates <-list()
  # chisq_Not_at_observed_data<- vector()
  extract.TRUE.list<- list()#2019 Sept 8
  Logical.list <- list() #2019 Sept 8
  chisq_at_observed_data <- chi_square_goodness_of_fit(
    fit,dig = 4,h=fit@dataList$h,f=fit@dataList$f

  )
  MCMC <- length(e$w)



  h.list <- list()#2019 Sept 8
  f.list <- list()#2019 Sept 8
  h.per.list <- list()#2019 Sept 8
  f.per.list <- list()#2019 Sept 8
  Logical.list  <- list()#2019 Sept 8
  # extract.TRUE.list  <- list()#2019 Sept 8
  FPF <- list()#2019 Sept 8
  TPF <- list()#2019 Sept 8
  chisq_Not_at_observed_data.list  <- list()#2019 Sept 8


  # process indicator ---------
  message("\n* Process for calculation of the posterior predictive p-value. \n")#Processssssssssssssssssssssssssssssssssssssssssssssssssssss
  cat(0," -> ")#Adjust#Processssssssssssssssssssssssssssssssssssssssssssssssssssss
  sss <-0 #dummy for process indicator#ProcesssssssssProcessssssssssssssssssssssssssssssssssssssssssssssssssssss
  Divisor <-100#ProcesssssssssProcessssssssssssssssssssssssssssssssssssssssssssssssssssss
  if(replicate.number.from.model.for.each.MCMC.sample<100){ Divisor <- 1 }#ProcesssssssssProcessssssssssssssssssssssssssssssssssssssssssssssssssssss
ttt <-1#ProcesssssssssProcessssssssssssssssssssssssssssssssssssssssssssssssssssss
uuu <- c(" ",#ProcesssssssssProcessssssssssssssssssssssssssssssssssssssssssssssssssssss
         " ",#ProcesssssssssProcessssssssssssssssssssssssssssssssssssssssssssssssssssss
         " ",#ProcesssssssssProcessssssssssssssssssssssssssssssssssssssssssssssssssssss
         " ",#ProcesssssssssProcessssssssssssssssssssssssssssssssssssssssssssssssssssss
         " ",#ProcesssssssssProcessssssssssssssssssssssssssssssssssssssssssssssssssssss
         " ",#ProcesssssssssProcessssssssssssssssssssssssssssssssssssssssssssssssssssss
         " ",#ProcesssssssssProcessssssssssssssssssssssssssssssssssssssssssssssssssssss
         " ",#ProcesssssssssProcessssssssssssssssssssssssssssssssssssssssssssssssssssss
         " ",#ProcesssssssssProcessssssssssssssssssssssssssssssssssssssssssssssssssssss
         ""#ProcesssssssssProcessssssssssssssssssssssssssssssssssssssssssssssssssssss
         )#ProcesssssssssProcessssssssssssssssssssssssssssssssssssssssssssssssssssss



   for (seed in 1:replicate.number.from.model.for.each.MCMC.sample) {#2019 Sept 8


     # process indicator ---------
     if(seed %% round(replicate.number.from.model.for.each.MCMC.sample/Divisor)==0){ #ProcesssssssssProcessssssssssssssssssssssssssssssssssssssssssssssssssssss
       sss <- sss +1                                #ProcesssssssssProcessssssssssssssssssssssssssssssssssssssssssssssssssssss
       if(sss%%10==0){  message(paste("  [ ", sss,uuu[ttt]," % ] ",sep = ""))  #ProcesssssssssProcessssssssssssssssssssssssssssssssssssssssssssssssssssss

         ttt<-ttt+1
       }
        if(!sss==100){cat(sss%%10," -> ")} #ProcesssssssssProcessssssssssssssssssssssssssssssssssssssssssssssssssssss
     }#Processssssssssssssssssssssssssssssssssssssssssssssssssssss














    set.seed(seed =  seed)


    chisq_Not_at_observed_data.list[[seed]]  <- vector()#2019 Sept 8
    Logical.list[[seed]]  <- vector()#2019 Sept 8




for (mcmc in 1:MCMC) {
                #Processsssssss
      w[mcmc]<-e$w[mcmc]
      z[[mcmc]]<-e$z[mcmc,]
      p[[mcmc]]<-e$p[mcmc,]
      hitrate[[mcmc]] <- t(apply(e$p,hit_rate_adjusted_from_the_vector_p,MARGIN = 1))[mcmc,]



      dz[[mcmc]]<-e$dz[mcmc,]
      l[[mcmc]]<-e$l[mcmc,]
      dl[[mcmc]]<-e$dl[mcmc,]
      m[mcmc]<-e$m[mcmc]
      v[mcmc]<-e$v[mcmc]
      a[mcmc]<-e$a[mcmc]
      b[mcmc]<-e$b[mcmc]



      h[[mcmc]] <-vector()
      f[[mcmc]] <-vector()
      h.per[[mcmc]] <-vector()
      f.per[[mcmc]] <-vector()
      s <-0# 2020 Feb This is a key idea

      for (n in 1:N) {
        if(!n==1)  s <- s + h[[mcmc]][n-1]# 2020 Feb This is a key idea

        h[[mcmc]][n] <- stats::rbinom(1, NL-s, hitrate[[mcmc]][c[n]])
        # f[[mcmc]][n] <- stats::rpois(1,dl[[mcmc]][c[n]]*NI)#2020Feb
        f[[mcmc]][n] <- stats::rpois(1,dl[[mcmc]][c[n]]*NX)#2020Feb

      }

      chisq_Not_at_observed_data.list[[seed]][mcmc]<- chi_square_goodness_of_fit_from_input_all_param(
        h= h[[mcmc]],
        f= f[[mcmc]],
        # p=p[[mcmc]],
        p=hitrate[[mcmc]],
        lambda=l[[mcmc]],
        NI=NI,
        NL=NL,
        ModifiedPoisson=ModifiedPoisson,
        dig = 4
      )

      # browser()

      for (n in 1:N) {
                           h.per[[mcmc]][c[n]] <- h[[mcmc]][c[n]]/NL
                           # f.per[[mcmc]][c[n]] <- f[[mcmc]][c[n]]/NI#2020Feb
                           f.per[[mcmc]][c[n]] <- f[[mcmc]][c[n]]/NX #2020Feb

      }




    }#for (mcmc in 1:MCMC) {
    Logical.list[[seed]] <- chisq_Not_at_observed_data.list[[seed]]>chisq_at_observed_data
    # extract.TRUE.list[[seed]] <-the_row_number_of_logical_vector( Logical.list[[seed]])




    h.list[[seed]] <-  h #2019 Sept 8
    f.list[[seed]] <- f#2019 Sept 8
    h.per.list[[seed]] <-h.per#2019 Sept 8
    f.per.list[[seed]] <-f.per#2019 Sept 8

    # Logical.list[[seed]] <-Logical#2019 Sept 8
    # extract.TRUE.list[[seed]] <-extract.TRUE#2019 Sept 8

    FPF[[seed]] <-  lapply(f.per, cumsum)
    TPF[[seed]] <-  lapply(h.per, cumsum)

  }#for  j in 1:replicate.number.from.model.for.each.MCMC.sample

  # extract.TRUE  <-  unlist(extract.TRUE.list)
  Logical<-  unlist(Logical.list)



  # p.value <- length(extract.TRUE)/(MCMC*replicate.number.from.model.for.each.MCMC.sample)

  p.value <- mean(Logical) # This counts the rate of TRUE and it corresponds the double integral







  if(plot==TRUE){

    message(" Now, we plot the replicated datasets... wait...")



    if (dark_theme==TRUE)   dark_theme()

    FPF <- jitter(unlist(FPF))
    TPF <- jitter(unlist(TPF))
    BayesianFROC::small_margin()

    plot(FPF,
         TPF,
         cex=0.2,
         ylim = c(0, max(1,TPF) )
    )
    graphics::abline(h=1)


    if (fit@studyDesign=="srsc.per.image")    xlab = 'Replicated false positives per images from the posterior predictive distribution.'
    if (fit@studyDesign=="srsc.per.lesion" )    xlab = 'Replicated false positives per nodule from the posterior predictive distribution.'
    ylab<- "Replicated cumulative hits per lesion"
    main <-"Replicated FROC data from the posterior predictive distribuion."







    if (Colour==TRUE) {

      #Making a dataset to draw a counter plot
      x <-unlist(FPF)
      names(x)<-"CFPs from the posterior predictive distribution"
      y<- unlist(TPF)
      names(y)<-"CTPs from the posterior predictive distribution"
      group <-  rep( 1:C, length(FPF) )

      # plot(x,y, pch=20, cex=0.8, col=grDevices::rainbow(C+6)[group])


      BayesianFROC::small_margin()
      plot(x,y, pch=20, col=grDevices::rainbow(C+6, alpha=0.2)[group], cex=0.8, xlab=xlab, ylab=ylab, main =main,ylim = c(0,1))
      # car::dataEllipse(x,y,factor(group), levels=c(0.70,0.85,0.95),lwd=0.1,
      #                  plot.points=FALSE, col=grDevices::rainbow(C+6), group.labels=NA, center.pch=FALSE)

    }#Colour==TRUE
  }#plot==TRUE

  if (summary==TRUE){

    return( list(
      p.value=p.value,

      Logical=Logical,
      chisq_at_observed_data=chisq_at_observed_data,

      chisq_Not_at_observed_data.list=chisq_Not_at_observed_data.list,
      # q.value=q.value,

      FPF=FPF,
      TPF=TPF,
      h.list=h.list, #2019 Sept 8
      f.list=f.list,#2019 Sept 8
      h.per.list=h.per.list,#2019 Sept 8
      f.per.list=f.per.list,#2019 Sept 8

      Logical.list=Logical.list,#2019 Sept 8
      extract.TRUE.list=extract.TRUE.list#2019 Sept 8

    )
    )}


  invisible( list(
    p.value=p.value,

    Logical=Logical,
    chisq_at_observed_data=chisq_at_observed_data,

    chisq_Not_at_observed_data.list=chisq_Not_at_observed_data.list,
    # q.value=q.value,
    FPF=FPF,
    TPF=TPF,


    h.list=h.list, #2019 Sept 8
    f.list=f.list,#2019 Sept 8
    h.per.list=h.per.list,#2019 Sept 8
    f.per.list=f.per.list,#2019 Sept 8

    Logical.list=Logical.list,#2019 Sept 8
    extract.TRUE.list=extract.TRUE.list#2019 Sept 8


  )
  )













}#function






















#' @title MRMC: Posterior Predictive P value (PPP) for MRMC,
#' @description PPP for chi square goodness of fit statistic
#' @details The author hates the notion of p value and this is the motivation that he developed new theory without p values.
#' However, he cannot overcome the traditional people. he loves mathematics, but he hates statistics.
#' he emphasizes that notion of p value is dangerous (monotonicity w.r.t. sample size) and its background is unknown.
#' Of course, intuitively, it is good. But, the theoritically, it does not ensure some criterion in large sample comtext.
#'
#' So, p value said that my effort is rarely admissible, since its p value said that he is small for various datasets.
#' So, this funcking p value said my effort is wrong, or should change  model.
#' Unfortunately, my hand aches cannot program more models.
#'  Ha,... why many peoply like p value bitch.
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'@inheritParams fit_Bayesian_FROC
#'@inheritParams ppp_srsc

#'@inheritParams DrawCurves
#'
#' @return A positive number indicates Posterior Predictive P value (ppp).
#' @export
#'
#' @examples
#'
#'
#'
#' \dontrun{
#'#========================================================================================
#'#  1)  Fit a Model to MRMC Data
#'#========================================================================================
#'
#'   fit <- fit_Bayesian_FROC( ite  = 111,  dataList = ddd )
#'
#'#========================================================================================
#'#  1)  Evaluate Posterior Predictive P value for the Goodness of Fit
#'#========================================================================================
#'
#'      ppp_MRMC(fit)
#'
#'#  If this quantity is greater, then we may say that our model is better.
#'
#'#  I made this ppp at 2019 August 25.
#'
#'
#'}#'
#'
#'
#'
#'
ppp_MRMC <- function(StanS4class,
                     summary = TRUE,
                     replicate.number.from.model.for.each.MCMC.sample =2#2019 Sept 8

){

  fit <-StanS4class

  b <-Chi_square_goodness_of_fit_in_case_of_MRMC_Posterior_Mean(fit,summary = summary)
  chi.square.observed.data.and.MCMC.samples <-b$chi.square

  a.list <- list()
  c.list <- list()

  chi.square.replicated.data.and.MCMC.samples <- list()

  for (seed in 1:replicate.number.from.model.for.each.MCMC.sample) {#2019 Sept 8
    a.list[[seed]]<-vector()
    c.list[[seed]]<-vector()

    set.seed(seed = seed)
    a.list[[seed]] <- chi_square_at_replicated_data_and_MCMC_samples_MRMC(fit,seed = seed,serial.number =replicate.number.from.model.for.each.MCMC.sample )
    chi.square.replicated.data.and.MCMC.samples[[seed]] <-a.list[[seed]]$chi.square
    c.list[[seed]] <- chi.square.replicated.data.and.MCMC.samples[[seed]] - chi.square.observed.data.and.MCMC.samples

  }#2019 Sept 8

  c<-unlist(c.list)




  d <- c>0

  ppp <- mean(d)
  return(ppp)
}
