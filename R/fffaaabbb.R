
#' @title Package Development tools and memo.
#'@description This is for the author of this package.
#' @param a,b,c indicating version




#' project option build and reload
#' ^
#' ^
#' ^
#' ^
# --no-multiarch --with-keep.source


#  vignette("Appendix",package="BayesianFROC")

fffaaabbb <- function(a=1,b=0,c=0){


  message("If develop version is installed in my PC, then running stan via shiny fails. So, I always uninstall the develop version to run stan with shiny. why")




  message(" R CMD check cannot relicate that of check of the CRAN auto check.
          Then Rd. files error gives a line which is a single number.

          However, acutual .Rd files are multiple and thus, we need to combine these .Rd files
          to a single .Rd file.

          To do so, first, open command prompt on the file [man] in which multiple .Rd files exists.
          Then from comand prompt, execute the code

    ", crayon::bgWhite$red("      type *.Rd > combine_all_Rd.txt "),"

          then it will creates a single file named allRd.txt. Using this file we can fine the error
          described by the single number.

          2019 Oct


usethis::use_package(\"xlsx\")

          ")











  message( "

#`@include is used for .R file defining class is loaded first and second the .R file of method are load.
SO, the file name defining class should be aaaaaclass, to begin load first.

Roxygen2 is technical or ... non intuitive for S4 class descritption or method description


file.edit(\"./.Rprofile\")
file.edit(\"~/.Rprofile\")



           ")






  message( "

* In top directory, there is a file vignettes, in which there are many Rmd files which the author should edit.
* If edit, then execute the R script devtools::build_vignettes() to create the doc file in the top directory and
  the code move the html, R and copy the  Rmd file to the doc file.

*Note that the Rmd file in doc directory should be not editted, but the Rmd files in the vignettes directory, we should edit.

* Work flow is

  ** Edit Rmd in vignettes directory
  ** Move the resulting html and R and copy the Rmd to the doc file which should not edit.




  A <- c( 11,22,44,55,33)
  B<-  c( 111,222,444,555,333)
  a<-sort(A, method = \"shell\", index.return = TRUE) # is stable
  BB<-B[a$ix]

  a<-sort(A, method = \"shell\", index.return = F) # is stable




2019 August
In R file contains several function object, then [Ctrl] + [yajirushi] jump to different object.

##########################################
# Extract sub data frame.
#                        2020 March
df <- data.frame(
 factor = c(1,1,1,2,2,2,3,3,3),
  x     = c(1,2,3,1,2,3,1,2,3),
  y =1:9
)

  df[df$factor %in% c(1,3), ]
############################################





In R-Studio  =====================================

Tools > project option > build tool

Check Packgae --R CMD check additional options should be specified as following
  ", crayon::bgWhite$red("
--as-cran --run-dontrun
 "),"

================================== 2020 March 20========

           ")


  # Create the removing code for vignettes.
  # date <- paste(format(Sys.time(),"Year%Y.Month.%m.Date.%d.Hours.%H.Minits.%M"))

  message("    tools::texi2pdf(\"myTexFile.tex\") ")




#  Sys.setenv(LANGUAGE="en")
message(" Sys.setenv(LANGUAGE=\"en\")")
message("\n")
message("\n")



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



message("kpsewhich style_file_name")
message("\n")
message("\n")
message("mktexlsr")
message("\n")
message("\n")




message( "



*   tool > global option > R markdown > show output preview in [Window]

* Sakura editor Greb search or replace is useful


           ")




message( "
*  Ctrl + shiht + C Comments, the most important
*  Ctrl + Shift + B = Install and Restart <- Today I know it 2019.6.10
*  Ctrl + shift + L
*  Ctrl + shift + M  pipe operator

*  Ctrl + shiht + E
*  Ctrl + shiht + D
*  Ctrl + shiht + tab
*  Ctrl + tab

* Shift + alt + r   terminal is open in R  studio

*  Ctrl + shiht + K
*  Ctrl + shiht + w
*  Ctrl + w   <- close a single source file


*  Ctrl + 1
*  Ctrl + 2
*  Ctrl + tab (Ctrl + F11) <-----good 2020 April 9
*  Ctrl + tab +shift (Ctrl + F12)<-----good 2020 April 9
*  Ctrl + .    <-This is very useful 2019 Nov 20. Do not confuse Ctrl + shift + o
*  Ctrl + Shift + o   <-Open the side menu of program code rewritten by # -----
*  Ctrl + w
*  Ctrl + t = tex complile <- I always forget
* [F2] key to change file name
* Alt + shift  + yajirusi


* my favorite 2019 Nov 20
*  Ctrl  + .         <- Open file
*  Ctrl  + D         <- delete a single command line
*  Ctrl  + f         <- find a word
*  Ctrl + shiht + w  close all files
*  Ctrl + w          close a single file
*  Ctrl + f          with active words specified by dragging manner
*  Ctrl + Shift + o
*  Ctrl + l          clean the R studio console 2020
*  Ctrl + a
*  Ctrl + shiht + alt + r   make a roxygen document for function <---------------

* Shift + F10     right click






* In R studio, put cursor in the Rmd file, then the following is  availiable:

           [Edit] > [Check spelling]

  which is has short cut key, that is [F7].



   devtools::spell_check();devtools::build_readme();devtools::build_vignettes();

========================================================

Path

Control panel > system and secrity > system > system syousai settei > kankyouhensu

C:\\Program Files\\R\\R-4.0.0\\bin


options(defaultPackages =c(getOption(\"defaultPackages\"),\"BayesianFROC\") )



------------------------------------
-----------------------------------
Update R and Stan

* Delete all R and Rtools from c:
* install R and Rtools
* Make a file named \" Makevars.win \"  the .R file which is in document.
* Write the following in the  Makevars.win

CXX14 = C:/Rtools/mingw64/bin/g++

CXX14 = C:/rtools40/mingw64/bin/g++ -m$(WIN) -v
CXX14FLAGS += -mtune=native -march=native -Wno-ignored-attributes -Wno-deprecated-declarations







           ")




message("devtools::check_win_devel()  is check pkg via web")


#
# message("\n")
# message( "chcp 437  " )
# message("\n")
# message("\n")

# R CMD check C:\Users\81909\Desktop\111BayesianFROC20180209
message(" R CMD check C:\\Users\\tsuno\\Desktop\\111BayesianFROC20191001 --as-cran")

message(crayon::bold("chcp 437 & R CMD check C:\\Users\\tsuno\\Desktop\\111BayesianFROC20191001"))
message("\n")
message("\n")
message(" R CMD check C:\\Users\\81909\\Desktop\\111BayesianFROC20191001 --as-cran ")
message("\n")
message("\n")
message(" R CMD build C:\\Users\\81909\\Desktop\\111BayesianFROC20191001  ")
message("\n")
message("\n")
message(" R CMD check C:\\Users\\81909\\Desktop\\BayesianFROC_",a,".",b,".",c,".tar.gz --as-cran --run-dontrun")

message(" R CMD check C:\\Users\\81909\\BayesianFROC_",a,".",b,".",c,".tar.gz --as-cran ")



message("
Path
Control panel > system and secrity > system > system syousai settei > kankyouhensu
C:\\Program Files\\R\\R-4.0.0\\bin
 ")

message("R CMD check C:\\Users\\81909\\Desktop\\BayesianFROC_",a,".",b,".",c,".tar.gz --as-cran --run-donttest --run-dontrun")
message("* Version such as BayesianFROC_",a,".",b,".",c,".tar.gz should be changed")
message(" R CMD build C:\\Users\\81909\\Desktop\\111BayesianFROC20191001  ")
message(" R CMD check C:\\Users\\81909\\Desktop\\BayesianFROC_",a,".",b,".",c,".tar.gz --as-cran --run-donttest")
message("\n")
message("\n")

# R CMD Rd2pdf  C:\Users\81909\Desktop\111BayesianFROC20180209
message("START ------------------PDF-----------")
message("# After the execution of R CMD check, BayesianFROC_",a,".",b,".",c,".tar.gz wll be created in Desktop, then using BayesianFROC_",a,".",b,".",c,".tar.gz, we can create pdf manual as follows.")

message("#file.remove( .....) is an R script, so exeute it from R console ")
message(" file.remove(\"C:\\\\Users\\\\81909\\\\111BayesianFROC20191001.pdf\")")
message(crayon::bold("R CMD Rd2pdf  C:\\Users\\81909\\Desktop\\111BayesianFROC20191001"))
message("\n PDf is created in the PATH C:\\Users\\81909  and we should omit it by hand to create another pdf manual.")
message("Fin ------------------PDF-----------")

message("\n")
message("\n")
message("\n
        methods::getMethod(signature = \"stanfit\",f=\"summary\")

        ")

message("

a<-parse(text =  \"a <-1\")
 eval(a)
 a

        ")

message("
Now ( 2020 Sept), the following lines work fine.... 2020 Sept
cd ..


R CMD build C:\\Users\\81909\\Desktop\\111BayesianFROC20191001
R CMD check C:\\Users\\81909\\Desktop\\BayesianFROC_",a,".",b,".",c,".tar.gz




R CMD build C:\\Users\\tsuno\\OneDrive\\Desktop\\111BayesianFROC20220122
R CMD check C:\\Users\\tsuno\\BayesianFROC_",a,".",b,".",c,".tar.gz --as-cran --run-donttest --run-dontrun




R CMD build C:\\Users\\81909\\Desktop\\111BayesianFROC20191001
R CMD Rd2pdf  C:\\Users\\81909\\Desktop\\111BayesianFROC20191001
        ")

message("\n")
message("\n")
message("R CMD check C:\\Users\\81909\\Desktop\\BayesianFROC_",a,".",b,".",c,".tar.gz --as-cran --run-donttest --run-dontrun")


message("

  cd C:\\Users\\tsuno\\Desktop\\111BayesianFROC20191001
git init
 git add C:\\Users\\tsuno\\Desktop\\111BayesianFROC20191001
git commit -m \"Initial Commit\"
git remote add origin https://github.com/tsunodaissei/bayesianfroc
git push -u origin master
git clone https://github.com/tsunodaissei/bayesianfroc
"
)


message(" R CMD check C:\\Users\\tsuno\\Desktop\\111BayesianFROC20191001 --as-cran")

}# function

