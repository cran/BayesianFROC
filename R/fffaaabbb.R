
#' @title Package Development tools
#'@description This is for the author of this package.



# rstantools:::rstan_package_skeleton(
#  stan_files = c("Model_Hiera.stan",
#                  "Model_srsc_per_image.stan")
#                )
#



#' project option build and reload
#' ^
#' ^
#' ^
#' ^
# --no-multiarch --with-keep.source


#  vignette("Appendix",package="BayesianFROC")
fffaaabbb <- function(){


  # md = 1 2 3,...
  #qd =  1 binormal assumptions are printed out
  #qd=   2
  #...




  # Create the removing code for vignettes.
  # date <- paste(format(Sys.time(),"Year%Y.Month.%m.Date.%d.Hours.%H.Minits.%M"))

  message("    tools::texi2pdf(\"myTexFile.tex\") ")

  message(" demo(demo_srsc,package=\"BayesianFROC\")")
  message("\n")
  message("\n")
  message(" demo(demo_MRMC,package=\"BayesianFROC\")")
  message("\n")
  message("\n")
  message(" demo(demo_stan,package=\"BayesianFROC\")")
  message("\n")
  message("\n")

# devtools::spell_check(pkg ="R",vignettes = T)
message( " devtools::spell_check(pkg =\"R\",vignettes = T)")

message("\n")
message("\n")

#  Sys.setenv(LANGUAGE="en")
message(" Sys.setenv(LANGUAGE=\"en\")")
message("\n")
message("\n")

message("Calculation of two binomal distrance is upper half plane.")

message("dataList.high.ability <- list(f=f,h=h,NL=NL,NI=NI,C=C) \n")
message(" devtools::use_data(dataList.high.ability) ")

message("\n")
message("\n")


 #devtools::build_vignettes()
message("devtools::build_vignettes()")
message("\n")
message("\n")

message("browseVignettes(\"BayesianFROC\")")
message("\n")
message("\n")

message("\n vignette()")
message("\n   vignette(\"rstanarm\")  ")

message("\n")
message("\n")


message("\n  file.edit(\"~/.Rprofile\")   ")
message("\n   file.edit(\".Rprofile\")")
message("\n")
message("\n")
message("vignette(\"Appendix\",package=\"BayesianFROC\")")


message("kpsewhich style_file_name")
message("\n")
message("\n")
message("mktexlsr")
message("\n")
message("\n")
# R CMD Rd2pdf  C:\Users\81909\Desktop\111BayesianFROC20180209
message(crayon::bold("R CMD Rd2pdf  C:\\Users\\81909\\Desktop\\111BayesianFROC20180209"))
message("\n")
message("\n")

#
# message("\n")
# message( "chcp 437  " )
# message("\n")
# message("\n")

# R CMD check C:\Users\81909\Desktop\111BayesianFROC20180209
message(crayon::bold("chcp 437 & R CMD check C:\\Users\\81909\\Desktop\\111BayesianFROC20180209"))
message("\n")
message("\n")


}# function




##  C:\Users\81909\TeXworks\configuration













# To extract parameters
# get_posterior_mean(fit,par=c("ppp"))

# If your package name is somepackage and the object saved was nhanes_files with devtools::use_data(nhanes_files, internal = TRUE) then you can access this in your functions by calling somepackage:::nhanes_files.

# I think true parameter for MRMC should be included as a data file
# which can not used by users.
# devtools::use_data(a, b, demo_df, internal = TRUE)



#-----Naming of dataset------------
#----------------------
# names(d$f) <-c("f[3]","f[2]","f[1]")
# names(d$h) <-c("h[3]","h[2]","h[1]")
# names(d$NI) <-c("Number of Images")
# names(d$NL) <-c("Number of Lesions")
# names(d$C) <-c("Number of Confidence levels")
# devtools::use_data( name.of.data)


# if(fit@cha ==1){ get_posterior_mean(fit,par=c("ppp"))   }
# if(!fit@cha ==1)get_posterior_mean(fit,par=c("ppp"))[,"mean-all chains"]


#
# Use the function stan_model to compile, then use the compiled model output in the sampling function. For example:
#
#   m <- stan_model('foo.stan')
#   fit <- sampling(m, data = ...)
#   instead of
#
#   fit <- stan('foo.stan', data = ...)

# Bayeisan chi square

#  R CMD Rd2pdf  C:\Users\81909\Desktop\111BayesianFROC20180209




# print( utils::object.size(datasets),unit="MB")


#  vignette(package= "rstan")
# vignette( "external", package= "rstan")



#   options(mc.cores = parallel::detectCores())


# options(scipen=2)
