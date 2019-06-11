library(BayesianFROC)


#1) Build the data for singler reader and single modality  case.



# To apply functions in the rstan package, we have to chage the class.
# fit_Bayesisan_FROC() return the object of some inherited S4 class from the S4 class stanfit.
# Thus to apply the generic function or any functions for stanfit, we have to change the class
# by the code
#
#   fit.stanfit <- as(fit, "stanfit")
#
# where fit is a return value of the  fit_Bayesisan_FROC().







data.example <- list(
            c=c(3,2,1),    #Confidence level
            h=c(97,32,31), #Number of hits for each confidence level
            f=c(1,14,74),  #Number of false alarms for each confidence level

            NL=259,       #Number of lesions
            NI=57,        #Number of images
            C=3)          #Number of confidence level


data.example <-  give_name_srsc_data(data.example)

viewdata(data.example)

draw.CFP.CTP.from.dataList(data.example)

fit <- fit_Bayesian_FROC(data.example,cha = 3)

fit.stanfit <- as(fit, "stanfit") # Please Change the S4 class from the inherited S4 class to the stanfit S4 class to apply the rstan functions.
summary(fit.stanfit)
print(fit.stanfit)
rstan::traceplot(fit.stanfit,par=c("A"))
rstan::stan_dens(fit.stanfit,par=c("A"))
check_divergences(fit.stanfit)
check_hmc_diagnostics(fit.stanfit)
pairs(fit.stanfit,pars=c("A","lp__","m"))
get_posterior_mean(fit.stanfit)
check_rhat(fit.stanfit)
rstan::stan_hist(fit.stanfit)
rstan::stan_rhat(fit.stanfit)
# plot() cause the error:   Error in as.double(y) : cannot coerce type 'S4' to vector of type 'double'
# plot(fit.stanfit)
# plot(fit.stanfit,pars=c("l","p"))
# plot(fit.stanfit,pars=c("v","m"))
# plot(fit.stanfit,pars=c("w", "z" ,"dz"))




message("

# The R scripts used in the demo.



# example data
", crayon::bgBlack$cyan$bold$italic$underline("data.example <- list(      "),"
", crayon::bgBlack$cyan$bold$italic$underline("  c=c(3,2,1),    #Confidence level"),"
", crayon::bgBlack$cyan$bold$italic$underline("h=c(97,32,31), #Number of hits for each confidence level"),"
", crayon::bgBlack$cyan$bold$italic$underline("f=c(1,14,74),  #Number of false alarms for each confidence level"),"
", crayon::bgBlack$cyan$bold$italic$underline("  NL=259,       #Number of lesions"),"
", crayon::bgBlack$cyan$bold$italic$underline("  NI=57,        #Number of images"),"
", crayon::bgBlack$cyan$bold$italic$underline("  C=3)          #Number of confidence level"),"

# Give a name
", crayon::bgBlack$cyan$bold$italic$underline("data.example <-  give_name_srsc_data(data.example)"),"

# View data by table
", crayon::bgBlack$cyan$bold$italic$underline("viewdata(data.example)"),"

# Draw empirical FROC curve
", crayon::bgBlack$cyan$bold$italic$underline("draw.CFP.CTP.from.dataList(data.example)"),"

# Fit a model to data
", crayon::bgBlack$cyan$bold$italic$underline("fit <- fit_Bayesian_FROC(data.example,cha = 3)"),"

# change S4 class from stanfitExtended to stanfit
", crayon::bgBlack$cyan$bold$italic$underline("fit.stanfit <- as(fit, \"stanfit\")")," # Please Change the S4 class from the inherited S4 class to the stanfit S4 class to apply the rstan functions.

# Following, we use functions from the package rstan
", crayon::bgBlack$cyan$bold$italic$underline("summary(fit.stanfit)"),"
", crayon::bgBlack$cyan$bold$italic$underline("print(fit.stanfit)"),"
", crayon::bgBlack$cyan$bold$italic$underline("rstan::traceplot(fit.stanfit,par=c(\"A\"))"),"
", crayon::bgBlack$cyan$bold$italic$underline("rstan::stan_dens(fit.stanfit,par=c(\"A\"))"),"
", crayon::bgBlack$cyan$bold$italic$underline("check_divergences(fit.stanfit)"),"
", crayon::bgBlack$cyan$bold$italic$underline("check_hmc_diagnostics(fit.stanfit)"),"
", crayon::bgBlack$cyan$bold$italic$underline("pairs(fit.stanfit,pars=c(\"A\",\"lp__\",\"m\"))"),"
", crayon::bgBlack$cyan$bold$italic$underline("get_posterior_mean(fit.stanfit)"),"
", crayon::bgBlack$cyan$bold$italic$underline("check_rhat(fit.stanfit)"),"
", crayon::bgBlack$cyan$bold$italic$underline("rstan::stan_hist(fit.stanfit)"),"
", crayon::bgBlack$cyan$bold$italic$underline("rstan::stan_rhat(fit.stanfit)"),"



        ")



# Demo finished !!




