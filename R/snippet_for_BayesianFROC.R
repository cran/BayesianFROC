#' @title  Edit Snippet
#' @description Snippet for the package BayesianFROC. Copy and paste to the snippet edition tools in your R studio for the conforable usage of the package BayesianFROC. This is under construction. To edit snippet, you can open, by R-stuido, the editor located in Tools > Global options > Code > Edit snippets.
#' @return nothing
#' @export
#' @details
#'
#'  if $ are included such as
#'
#'          foo$b
#'
#'           then in message it should be
#'
#'      message("foo\\$a")
#'
#'2020 JUly2
#'
#'
#' @examples
#'
#'  snippet_for_BayesianFROC()
#'
snippet_for_BayesianFROC  <- function(){

message("

snippet iii
	install.packages(${1:\"BayesianFROC\"})

snippet www
	devtools::load_all(\".\");

snippet sss

	stanModel <- stan_model_of_sbc()

	Simulation_Based_Calibration_single_reader_single_modality_via_rstan_sbc(
	ite = 233,
	M = 11,
	epsilon = ${1:0.04},
	stanModel = stanModel
	)





snippet ddd
  devtools::document();?${1:\"BayesianFROC\"}

snippet error_MRMC
  a <- error_MRMC(ite = 1111, replication.number = 33, NL=333)

snippet demo-review
	demo(demo_for_reviewer_of_my_manuscript,package=\"BayesianFROC\")

snippet methods::as
	fitt <- methods::as(fit,\"stanfit\")


snippet drawcur
	DrawCurves(fit, Colour  = FALSE, new.imaging.device = F)

snippet citation
	citation(${1:\"BayesianFROC\"})



snippet download
  BayesianFROC::dark_theme()
  a<-    cranlogs::cran_downloads(
  packages = \"BayesianFROC\",
  from = \"2019-05-12\",
  #from = \"2019-05-03\",
  to = Sys.Date())
  a
  b <- a\\$count;
  number.of.downloads <-b;
  mean(b)
  date<- 1:as.integer(length(b))

  today <- b[length(b)-2]
  title <- paste(\"today = \",today ,\",\",
  b[length(b)-3],\",\",
  b[length(b)-4],\",\",
  b[length(b)-5],\",\",
  b[length(b)-6],\",\",
  b[length(b)-7],\",\",
  b[length(b)-8],\",\",
  b[length(b)-9]
  )

  plot(date,number.of.downloads  ,type=\"l\",col=\"yellow\", main  = title)
  for (hhh in (0:5)*10) graphics::abline(h=hhh)
  graphics::abline(h=b[length(b)-4], col=\"red\",lty =\"dashed\",lwd =\"2\")
  graphics::abline(h=b[length(b)-3], col=\"red\",lty =\"dashed\",lwd =\"3\")
  graphics::abline(h=b[length(b)-2], col=\"red\",lty =\"solid\",lwd =\"3\")

  plot(date, cumsum(b), type=\"l\",col=\"yellow\", main  = title)

  hist(b,col = \"red\",border=\"yellow\",breaks=77, main  = title)

  for (aaa in 1:50) {
  graphics::abline(h=aaa)
  if (aaa%%5==0) {
    graphics::abline(h=aaa,lwd =\"2\")
    # graphics::abline(v=5,lwd =\"2\")
    # # graphics::abline(v=15,lwd =\"1\",lty =\"dashed\")
  }
  }



  graphics::abline(v =b[length(b)-2], untf = FALSE, col=\"red\",lty =\"solid\",lwd =\"4\");
  graphics::abline(v =b[length(b)-2]-1, untf = FALSE, col=\"red\",lty =\"solid\",lwd =\"4\")

  graphics::abline(v =b[length(b)-3], untf = FALSE, col=\"red\",lty =\"dashed\",lwd =\"2\");
  graphics::abline(v =b[length(b)-3]-1, untf = FALSE, col=\"red\",lty =\"dashed\",lwd =\"2\")

  graphics::abline(v =b[length(b)-4], untf = FALSE, col=\"red\",lty =\"dotdash\",lwd =\"1\");
  graphics::abline(v =b[length(b)-4]-1, untf = FALSE, col=\"red\",lty =\"dotdash\",lwd =\"1\")


  # df<-data.frame(b=b)
  # g <- ggplot2::ggplot(df, ggplot2::aes(x = b))
  # g <- g + ggplot2::geom_histogram(binwidth = 1)
  # plot(g)
  #x<-runif(1000)/10;for(i in 1:1000) y[i]<-  mean(b,trim = x[i]);plot(x,y)


  sum(b)


  jjj<-length(b)
  tails <-list( length =  jjj  , mode = \"vector\")
  tail_area <- vector(length =  jjj, mode = \"numeric\")
  for (iii in 1:jjj) {
  tails[[iii]] <- b[b>b[length(b)-iii]]
  tail_area[iii] <- length(tails[[iii]]) /length(b)
  }
 tail_area_rev <- rev(tail_area)
 tail_area_rev <- round(tail_area_rev,digits = 3)
 tail_area_rev
  # hist(tail_area)
 plot(date,tail_area_rev  ,type=\"l\",col=\"yellow\", main  = title)
 graphics::abline(h=0.5,lwd =\"2\")



snippet fff.pkg.detach
	detach(\"package:BayesianFROC\")

snippet fff.pkg.attached
	names(sessionInfo()\\$otherPkgs)






snippet net
	curl::has_internet()

snippet sim_MRMC
	fit <- Simulation_Based_Calibration_single_reader_single_modality_via_rstan_sbc_MRMCsi()

snippet sim
	fit <- Simulation_Based_Calibration_single_reader_single_modality_via_rstan_sbc()



snippet ttt
	tttttt( ww=-0.81,www =0.001,
	mm=0.65,mmm=0.001,
	vv=5.31,vvv=0.001,
	zz= 1.55,zzz=0.001 )



snippet pipe
	`%>%` <- utils::getFromNamespace(\"%>%\", \"magrittr\")


snippet ins
	install.packages(\"${1:package}\")

snippet doc
	devtools::document();?






snippet vig

	utils::vignette( package = \"${2:survival}\", topic = \"${1:validate}\" )



snippet fitb
	${4:fit} <- fit_Bayesian_FROC( ite  = ${2:1111},  cha = 1, summary = ${3:F},  Null.Hypothesis = ${3:F}, dataList = ${1:dataList.Chakra.1} )








snippet DrawC

	DrawCurves(    modalityID = c(${3:1}), readerID = c(${2:1}), ${1:fit}  )

snippet rel

		BayesianFROC:::release_before()




snippet clear
	BayesianFROC:::clearWorkspace()

snippet d
	devtools::

snippet fffaaabbb
	BayesianFROC:::fffaaabbb()


snippet fff
	${4:fit} <- fit_Bayesian_FROC( ite  = ${2:1111},  cha = 1, summary = ${3:F}, dataList = ${1:dataList.Chakra.1} )


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
	ModifiedPoisson  = FALSE,
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
	ppp(fit)

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




snippet error_srsc

	datasets <-error_srsc(
									NLvector = c(100,10000000,1000000000),
												ite = 2222
												)









snippet lib
	library(${1:BayesianFROC})




snippet llllibrary
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


snippet fff
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
        ModifiedPoisson  = FALSE,
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




snippet error_srsc

  datasets <-error_srsc(
  NLvector = c(100,10000000,1000000000),
  ite = 2222
  )









snippet lib
  library(${1:BayesianFROC})







        ")

}
