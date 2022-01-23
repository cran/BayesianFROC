.onAttach <- function(libname, pkgname) {


# fit_GUI_Shiny()


  packageStartupMessage(

  crayon::blurred("

#  This package gives the following function, which can run without any variable. So, execute it on R console.

 ", crayon::bgBlack$red$bold$underline$italic("   fit_GUI_Shiny() "),"





",



crayon::bgBlack$yellow$bold$underline$italic("\n Ver."),
  crayon::bgYellow$cyan$bold$underline$italic("1."),
crayon::bgBlack$cyan$bold$underline$italic(" 0."),
 crayon::bgBlack$cyan$bold$underline$italic("0."),
crayon::bgBlack$red$bold$underline$italic("  \" p value  \"  "),

crayon::bgRed$red$bold$underline$italic(" "),
crayon::bgRed$yellow$bold$underline$italic(" "),
crayon::bgRed$white$bold$underline$italic(" ")

  )
 # ,
 # "                 ",
 # crayon::cyan(  date()  ),
 # "\n",
 # "                                  ",
 # crayon::cyan("Month: "), crayon::cyan$bold$underline$italic(format(Sys.time(), "%b") )," ",
 # " ",
 # crayon::cyan("Date: "), crayon::red$bold$underline$italic(format(Sys.time(), "%d") ),"  ",
 # "",
 # crayon::cyan("Year: "), crayon::cyan$bold$underline$italic(format(Sys.time(), "%Y"   ) ),
 #
 # " ",
 # crayon::cyan("Time: "), crayon::red$bold$underline$italic(format(Sys.time(), "%H:%M") ),"\n"





)
  }



.onLoad <- function(libname, pkgname) {

  # packageStartupMessage("\n# The package \"BayesianFROC\" is loaded. \n")

  }
