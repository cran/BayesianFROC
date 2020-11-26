
#' @title Trace plot
#'
#' @inheritParams   DrawCurves
#' @inheritParams   dark_theme
#' @param param_name  character, indicating param name.
#' @param chains A positive integer, indicating the number of chains in MCMC
#' @param omit_initial_iter A positive integer, except which from the first iteration, trace plot is drawn
# @return
#' @export
#'
#@examples
trace_Plot <- function(StanS4class,
                          param_name= name_of_param_whose_Rhat_is_maximal(StanS4class),                   #"lp__",
                          chains=1:length(StanS4class@stan_args),
                          type=2,
                          new.imaging.device=TRUE,
                          omit_initial_iter = 13
                          ){

  if (max(chains)>length(f@stan_args)) {
return( warning(paste("The number of chains or chain IDs should be less than ", length(f@stan_args)))
)  }

f <-StanS4class
 # if (missing(chains))   chains <- 1:length(f@stan_args)

a<-f@sim
aa <-a[[1]]


 param_names <- names( rstan::get_posterior_mean(f)[,1])
sss<-1

  # for (ccc in 1:length(param_names)) {
  #   if (! (param_names[ccc] == param_name)) {
  #     sss<-sss+1
  #   }
  # }
while (!(param_names[sss] == param_name )&&sss<=length(param_names)){
  sss<-sss+1
# print(  paste("sss =", sss))
}

# browser()
if (sss>length(param_names))  {
  warning(paste("* The parameter named \"",param_name,"\" does not exist." ,sep = "") )
  message("* param_name should be chosen from the followings:")
  print(param_names)
  return(invisible()  )}


# sss<-sss+1
# browser()
cat(paste( "* trace plots of the ",sss, "th parameter (i.e., ", param_name ,")."))
iterations_MCMC <- length(aa[[1]][[1]])
aaa<-list()
for (ccc in chains) {
aaa[[ccc]] <-aa[[ccc]][[sss]][omit_initial_iter:length(aa[[ccc]][[sss]] )]
}
aaa <- unlist(aaa)

#
# p <- list(x = 1:2, y =1:3)
# p$x
# p$"x"
# a<-"x"
# p$a

max <- max(aaa)
min <- min(aaa)
# browser()

Colour1 <-  array(0, dim=c( 100)) #array(0, dim=c( M))
# Colour2 <-  array(0, dim=c( M)) #
Colour1[1]<-"antiquewhite1" # "gray0"  #"orange3"
Colour1[2]<-"brown1"  #"orchid"
Colour1[3]<- "coral1" #"deeppink4"  #"firebrick4" #"dodgerblue1"
Colour1[4]<-"orange2"  #"aquamarine1"  #"darkcyan"
Colour1[5]<-"yellowgreen" #"blue4" #"deeppink4"  #" cyan4 " #"mediumvioletred" # "green4"##"darkgoldenrod4"
Colour1[6]<-"khaki1"#"darkolivegreen"
Colour1[7]<-"antiquewhite1" # "gray0"  #"orange3"
Colour1[8]<-"brown1"  #"orchid"
Colour1[9]<-"dodgerblue1" #"coral1" #"deeppink4"  #"firebrick4"
Colour1[10]<-"orange2"  #"aquamarine1"  #"darkcyan"
Colour1[11]<-"yellowgreen" #"blue4" #"deeppink4"  #" cyan4 " #"mediumvioletred" # "green4"##"darkgoldenrod4"
Colour1[12]<-"khaki1"#"darkolivegreen"
Colour1[13]<-"dodgerblue1" #"coral1" #"deeppink4"  #"firebrick4"
Colour1[14]<-"orange2"  #"aquamarine1"  #"darkcyan"
Colour1[15]<-"yellowgreen" #"blue4" #"deeppink4"  #" cyan4 " #"mediumvioletred" # "green4"##"darkgoldenrod4"
Colour1[16]<-"khaki1"#"darkolivegreen"
Colour1[17]<-"antiquewhite1" # "gray0"  #"orange3"
Colour1[18]<-"brown1"  #"orchid"
Colour1[19]<-"dodgerblue1" #"coral1" #"deeppink4"  #"firebrick4"
Colour1[20]<-"orange2"  #"aquamarine1"  #"darkcyan"
for (ccc in 21:100) {
  Colour1[ccc]<-"orange2"#2020 Jan
}

dark_theme(type = type)
if (new.imaging.device)  grDevices::dev.new()
dark_theme(type = type)

for (ccc in chains) {


  # suppressWarnings(graphics::par(new=TRUE));
  plot(aa[[ccc]][[sss]][omit_initial_iter:iterations_MCMC],
       type ="l",
       col=Colour1[ccc],
       cex=0.01,
       ylim=c(min,max),
       # xlim=c(0,(length(chains)* iterations_MCMC )  ),
       xlim=c(0, (iterations_MCMC - omit_initial_iter)    ),
       xlab="iterations",ylab="posterior samples"
       # ,main= param_names[sss]
       ,main = param_name
       );
# plot(aa[[2]]$w, type ="l",col=Colour1[2]);
# suppressWarnings(graphics::par(new=TRUE));
# plot(aa[[1]]$w, type ="line",col=Colour1[3])

}
graphics::abline(h=mean(aaa), col="brown1",lty ="solid",lwd ="5")

}
