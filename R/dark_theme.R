

#' @title Dark Theme
#' @description Putting it before plotting, plot area become the dark theme.
#' @return Nothing
#' @param type 1,2,3,4,5,6,7,...
#' @export
#'
#' @examples
#'
#'  dark_theme(1)
#'
#'  graphics::plot(c(1,2,3),c(1,2,3))
#'
#'
#'  dark_theme(2)
#'
#'  graphics::plot(c(1,2,3),c(1,2,3))
#'
dark_theme <- function(type=1){

  Colour1 <-  array(0, dim=c( 20)) #array(0, dim=c( M))
  Colour1[1]<-"antiquewhite1" # "gray0"  #"orange3"
  Colour1[2]<-"brown1"  #"orchid"
  Colour1[3]<-"dodgerblue1" #"coral1" #"deeppink4"  #"firebrick4"
  Colour1[4]<-"orange2"  #"aquamarine1"  #"darkcyan"
  Colour1[5]<-"yellowgreen" #"blue4" #"deeppink4"  #" cyan4 " #"mediumvioletred" # "green4"##"darkgoldenrod4"
  Colour1[6]<-"khaki1"#"darkolivegreen"
  Colour1[7]<-"darkorange4"
  Colour1[8]<-"slateblue4"
  for (cc in 9:20) {
    Colour1[cc] <- as.character(cc);
  }


  if (type==1) {


  graphics::par(bg= "gray12", #"gray27",#"gray40",#"black",# ,
                fg="gray",
                col.lab="bisque2" ,#"bisque" ,#  "antiquewhite1",
                col.axis="bisque2" ,##"bisque" "antiquewhite1",
                col.main="bisque2" ,
                cex.lab=1.5,
                cex.axis=1.3
  )
  }#if type ==



  if (type==2) {


    graphics::par(bg= "gray12", #"gray27",#"gray40",#"black",# ,
                  fg=Colour1[2] ,
                  col.lab=Colour1[2] ,#"bisque" ,#  "antiquewhite1",
                  col.axis=Colour1[2] ,##"bisque" "antiquewhite1",
                  col.main=Colour1[2] ,
                  cex.lab=1.5,
                  cex.axis=1.3
    )
  }#if type ==




  if (type==3) {


    graphics::par(bg= "gray12", #"gray27",#"gray40",#"black",# ,
                  fg=Colour1[3] ,
                  col.lab=Colour1[3] ,#"bisque" ,#  "antiquewhite1",
                  col.axis=Colour1[3] ,##"bisque" "antiquewhite1",
                  col.main=Colour1[3] ,
                  cex.lab=1.5,
                  cex.axis=1.3
    )
  }#if type ==


  if (type==4) {


    graphics::par(bg= "gray12", #"gray27",#"gray40",#"black",# ,
                  fg=Colour1[4] ,
                  col.lab=Colour1[4] ,#"bisque" ,#  "antiquewhite1",
                  col.axis=Colour1[4] ,##"bisque" "antiquewhite1",
                  col.main=Colour1[4] ,
                  cex.lab=1.5,
                  cex.axis=1.3
    )
  }#if type ==



  if (type==5) {


    graphics::par(bg= "red4", #"gray27",#"gray40",#"black",# ,
                  fg=Colour1[5] ,
                  col.lab=Colour1[5] ,#"bisque" ,#  "antiquewhite1",
                  col.axis=Colour1[5] ,##"bisque" "antiquewhite1",
                  col.main=Colour1[5] ,
                  cex.lab=1.5,
                  cex.axis=1.3
    )
  }#if type ==


  if (type==6) {


    graphics::par(bg= "blue4", #"gray27",#"gray40",#"black",# ,
                  fg=Colour1[6] ,
                  col.lab=Colour1[6] ,#"bisque" ,#  "antiquewhite1",
                  col.axis=Colour1[6] ,##"bisque" "antiquewhite1",
                  col.main=Colour1[6] ,
                  cex.lab=1.5,
                  cex.axis=1.3
    )
  }#if type ==



  if (type==7) {


    graphics::par(bg= "gray0", #"gray27",#"gray40",#"black",# ,
                  fg=Colour1[6] ,
                  col.lab=Colour1[7] ,#"bisque" ,#  "antiquewhite1",
                  col.axis=Colour1[7] ,##"bisque" "antiquewhite1",
                  col.main=Colour1[7] ,
                  cex.lab=1.5,
                  cex.axis=1.3
    )
  }#if type ==




}