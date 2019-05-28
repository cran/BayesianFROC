#' @title  Edit Snippet
#' @description Snippet for the package BayesianFROC. Copy and paste to the snippet edition tools in your R studio for the conforable usage of the package BayesianFROC. This is under construction. To edit snippet, you should open the editor located in Tools > Global options > Code > Edit snippets.
#' @return nothing
# @export
#'
# @examples
#'
#'  snippet_for_BayesianFROC()
#'
snippet_for_BayesianFROC  <- function(){

message("





snippet DrawC

	DrawCurves(    modalityID = c(${3:1}), readerID = c(${2:1}), ${1:fit}  )

snippet rel

		BayesianFROC:::release_before()




snippet clear
	BayesianFROC:::clearWorkspace()

snippet d
	devtools::

snippet fff
	BayesianFROC:::fffaaabbb()


snippet extract_EAP
	extract_EAP_CI(fit,\"${1:l}\",${2:fit@dataList\\$C })


snippet fitb
	${4:fit} <- fit_Bayesian_FROC( ite  = ${2:1111}, summary = ${3:FALSE},  cha = 1, dataList = ${1:dataList.Chakra.1} )




snippet demo-drawcurves-srsc
	demo(demo_drawcurves_srsc,package=\"BayesianFROC\")

snippet demo-stan
	demo(demo_stan,package=\"BayesianFROC\")

snippet demo-srsc
	demo(demo_srsc,package=\"BayesianFROC\")

snippet demo-MRMC
	demo(demo_MRMC,package=\"BayesianFROC\")





snippet datalist

	dat <- list(
	c=c(3,2,1),    #Confidence level
	h=c(97,32,31), #Number of hits for each confidence level
	f=c(1,14,74),  #Number of false alarms for each confidence level

	NL=259,       #Number of lesions
	NI=57,        #Number of images
	C=3)          #Number of confidence level






snippet Draw_a_simulated_data_se

	Draw_a_simulated_data_set(
	sd = 5, C = 5,
	seed.for.drawing.a.prior.sample = 1111,
	fun = stats::var,
	NI = 259,
	NL = 259,
	initial.seed.for.drawing.a.data = 1234,
	ModifiedPoisson = F,
	ite = 1111)





snippet simula

	g <- Simulation_Based_Calibration_histogram( NI=1111111, NL=1111111, N=111, ite=11111 )




snippet  Dra
	Draw.a.prior.sample <- Draw_a_prior_sample()
	fit <- Draw_a_simulated_data_set_and_Draw_posterior_samples(Draw.a.prior.sample)




snippet repl
	r <-replicate_model_MRMC()



snippet brow
	browser()




snippet pval
	p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit(
	StanS4class=fit,
	dig=3,
	Colour=TRUE,
	plot.replicated.points=FALSE)

snippet gets
	g <- get_samples_from_Posterior_Predictive_distribution(
		StanS4class = fit,
		Colour = TRUE,
		plot.replicated.points = FALSE
	)


snippet forget
	memoise::forget(fit_Bayesian_FROC)




snippet memo
	fit_Bayesian_FROC <- memoise::memoise(fit_Bayesian_FROC)




snippet validation.dataset_srsc_for_different_NI_NL

	datasets <-validation.dataset_srsc_for_different_NI_NL(
									NLvector = c(100,10000000,1000000000),
												ite = 2222
												)









snippet lib
	library(${1:BayesianFROC})

































#############################################################20190525




snippet fitba
  fit <- fit_Bayesian_FROC( ite  = 1111, summary = FALSE,  cha=1,  dataList = dataList.Chakra.1 )

snippet extract_EAP
	extract_EAP_CI(fit,\"${1:l}\",${2:fit@dataList\\$C })







snippet clear
	BayesianFROC:::clearWorkspace()

        snippet d
        devtools::

        snippet fff
        BayesianFROC:::fffaaabbb()


        snippet extract_EAP
        extract_EAP_CI(fit,\"${1:l}\",${2:fit@dataList\\$C })


        snippet fitb
        fit <- fit_Bayesian_FROC( ite  = ${2:1111}, summary = ${1:FALSE},  cha = 1,  dataList = dataList.Chakra.1 )




        snippet fitba
        ${2:fit} <- fit_Bayesian_FROC( dataList = ${1:dataList.Chakra.1},
        # Substitute your data.


        # To run the code, it is sufficient in default arguments for the following variables.

        ite     = ${3:1111},   # No. of iterations for Monte Carlo (MCMC) Simulation
        summary = ${4:FALSE},  # if TRUE, it shows summary of estimates by the print method for stanfit
        cha     = 1,           # No. of chains for MCMC
        dig     =3             # digit for estimates
        )
        # The variable dataList should be changed in your data.

        snippet demo-drawcurves-srsc
        demo(demo_drawcurves_srsc,package=\"BayesianFROC\")

        snippet demo-stan
        demo(demo_stan,package=\"BayesianFROC\")

        snippet demo-srsc
        demo(demo_srsc,package=\"BayesianFROC\")

        snippet demo-MRMC
        demo(demo_MRMC,package=\"BayesianFROC\")





        snippet datalist

        dat <- list(
        c=c(3,2,1),    #Confidence level
        h=c(97,32,31), #Number of hits for each confidence level
        f=c(1,14,74),  #Number of false alarms for each confidence level

        NL=259,       #Number of lesions
        NI=57,        #Number of images
        C=3)          #Number of confidence level






        snippet Draw_a_simulated_data_se

        Draw_a_simulated_data_set(
        sd = 5, C = 5,
        seed.for.drawing.a.prior.sample = 1111,
        fun = stats::var,
        NI = 259,
        NL = 259,
        initial.seed.for.drawing.a.data = 1234,
        ModifiedPoisson = F,
        ite = 1111)





        snippet simula

        g <- Simulation_Based_Calibration_histogram( NI=1111111, NL=1111111, N=111, ite=11111 )




        snippet  Dra
        Draw.a.prior.sample <- Draw_a_prior_sample()
        fit <- Draw_a_simulated_data_set_and_Draw_posterior_samples(Draw.a.prior.sample)




        snippet repl
        r <-replicate_model_MRMC()



        snippet brow
        browser()




        snippet pval
        p_value_of_the_Bayesian_sense_for_chi_square_goodness_of_fit(
        StanS4class=fit,
        dig=3,
        Colour=TRUE,
        plot.replicated.points=FALSE)

        snippet gets
        g <- get_samples_from_Posterior_Predictive_distribution(
        StanS4class = fit,
        Colour = TRUE,
        plot.replicated.points = FALSE
        )


        snippet forget
        memoise::forget(fit_Bayesian_FROC)




        snippet memo
        fit_Bayesian_FROC <- memoise::memoise(fit_Bayesian_FROC)




        snippet validation.dataset_srsc_for_different_NI_NL

        datasets <-validation.dataset_srsc_for_different_NI_NL(
        NLvector = c(100,10000000,1000000000),
        ite = 2222
        )









        snippet lib
        library(${1:BayesianFROC})







        ")

}
