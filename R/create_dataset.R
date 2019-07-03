#' @title   Create a dataset
#'@description
#' Create the Passing data to  the  function \code{ \link{fit_Bayesian_FROC}}.
#'
#'
#'    This is an interactive creator of dataset for FROC data. Using this return value, you can build the FROC model for your data by applying the  function \code{ \link{fit_Bayesian_FROC}} in this package.
#'@return A return value, i.e. a list, which are used to build FROC models.
#' @export create_dataset
 create_dataset  <- function(){



  message("From now on, Let's create the FROC dataset together !!\n \n \n \n")


   message("Are you sure you write the  code as follows:\n\n  data <- create_dataset(),\n[ data <- ] -------------- Do not forget !!  \n
       1: Yes.\n
       2:  No.\n

       0: Exit\n")
   a<- readline("Please enter (1 or 2):")
   if(!(a ==1) ){ return( warning(" Exit.\n\n* When you try again, you should write in the R console as follows:\n\n*  Your.data.name <- create_dataset()\n"))}







   message("Please enter 1 or 2, according to the dataset which you want to create:\n
                1:  Single Reader and Single Modality (SRSC)\n
                2:  Multiple Reader and Multiple Modality (MRMC)\n")
   a<- readline("Please enter study design (1 or 2):")
if(!(a ==2) & ! (a==1)){ return( warning("Warning: Please input 1 or 2."))}
     # switch(a,
     #       "1" =  "type"  <- "SRSC" ,
     #        "2" =  "type"   <- "MRMC"
     #        )
#
# if(type=="SRSC"){
  if(a==1){

  NL<- readline("Please enter Number of lesions:")
  if(is.numeric(NL)){ return( warning("Warning: Please input numeric."))}

  NI<- readline("Please enter Number of images:")
  if(is.numeric(NI)){ return( warning("Warning: Please input numeric."))}
  NL<-as.numeric(NL)
  NI<-as.numeric(NI)
  message("Please enter Number of confidence level:\n ")
            message("   For example, if your confidence level is as follows,then your input is 5:\n
                5: Definitely Positive,\n
                4: Probably Positive,\n
                3: Equivocal,\n
                2: Pobably Negative,\n
                1: Definitely Negative. ")
            C <- readline("Please enter confidence number:")
            if(as.integer(C)==0){return(warning("No of confidence level never 0."))}
            if(is.numeric(C)){ return( warning("Warning: Please enter numeric:"))}
            C <- as.integer(C)
            h <-  array(0, dim=c(C))
            # C <- as.numeric(C)

      for (cd in 1:C) {
         s <- C-cd+1
        message("Enter number of hits with confidence level", s,". \n")
        h[cd] <-as.numeric( readline("Your enter:"))
         if(sum(h)  > NL){
          return(warning("Your total number of hits until now ",sum(as.numeric(h))," is greater than Your number of lesions ",NL,". "))
        }
}#for
            C <-as.integer(C)
            f <-  array(0, dim=c(C))

      for (cd in 1:C) {
        s <-C-cd+1
        message("Enter number of false alarms with confidence level", s,". \n")
        f[cd] <- readline("Your enter:")
      }

      f<- as.numeric(f)
      h<- as.numeric(h)
      NL<- as.numeric(NL)
      NI<- as.numeric(NI)
      dataList <- list(f=f,h=h,NL=NL,NI=NI,C=C)
      viewdata_srsc(dataList)
      message("* Using this return value's list, you can apply another function of this package to build FROC models. \n \n \n")

      dataList <- give_name_srsc_data(dataList)
      message("\n* If you forget to make an object for return value of the function, then use an object [  .Last.value  ] which retains the last value. \n")

      return(dataList)
      # return( assign("s",list(f=f,h=h,NL=NL,NI=NI,C=C)  ))
  }


   if(a==2){

     message("* let's make a FROC data for MRMC case together.\n ")



     Q<- readline("Please enter Number of readers:")
     if(is.numeric(Q)){ return( warning("Warning: Please input numeric."))}

     M<- readline("Please enter Number of modalities:")
     if(is.numeric(M)){ return( warning("Warning: Please input numeric."))}


     NI<- readline("Please enter Number of images:")
     if(is.numeric(NI)){ return( warning("Warning: Please input numeric."))}
     NL<- readline("Please enter Number of lesions:")
     if(is.numeric(NL)){ return( warning("Warning: Please input numeric."))}


     message("Please enter Number of confidence level:\n ")
     message("   For example, if your confidence level is as follows,then your input is 5:\n
         5: Definitely Positive,\n
         4: Probably Positive,\n
         3: Equivocal,\n
         2: Pobably Negative,\n
         1: Definitely Negative. ")
     C <- readline("Your enter confidence number:")
     if(is.numeric(C)){ return( message("Warning: Please enter numeric:"))}
     C <- as.integer(C)
     M <- as.integer(M)
     Q <- as.integer(Q)
     NL<- as.numeric(NL)
     NI<- as.numeric(NI)

#----- This Code is some most important code ----#
     m<- rep(1:M, each=C*Q)
     q <-  rep(seq(1,Q,1), M, each=C)
     c <-rep(rep(C:1), M*Q)
# I refer this at 2019. 1 .31.

message(" -------  Notations and Symbols ----------\n ")

message("* Q means the noumber of readers.\n ")
message("* q means the ID vector of readers. \n ")
message("* m means the ID vector of modalities !!\n ")
message("* NL means the Number of Lesions !!\n ")
message("* NI means the Number of Images !!\n ")
message("* M means the number of Modalities !!\n ")
message("* C means the number of Confidenve levels !!\n ")
message("* c means the ID vector of Confidenve levels !!\n ")
message("\n*  unfinished \n ")

message("* f means the No. of false alarms.\n ")
message("* h means the No. of hits.\n \n\n\n\n ")






   message("Please enter 1 or 2:\n
                1:  Continue for hits and false alarms, it deem very tight. \n
                2:  Quit.\n")
   b<- readline("Please enter (1 or 2):")

        if(!(b ==2) & ! (b==1)){
                    message(" Choice is only 1 or 2:\n
                    1:  Continue for hits and false alarms, it deem very tight. \n
                    2:  Quit.\n")
                    b<- readline("Please enter only (1 or 2):")
          }


if(b==2){

message("* Please enter number of hits and false alarms by yourself.\n
* it should be compatible with confidence level vector c, do not change the vector c.\n
* So, you have to match your entered hits and false alarms with the confidence level vector c.\n
* Anyway, Bye-Bye !! Good luck !!\n")

message("An unfinished dataset are created with list format!! \n \n \n ")







N <-C*M*Q
#For Draw the Sample points on FROC curve. Assessment of Fit for FROC.
hh <- rep(NA, length=N)  #Initialization of Cumulative Hits
ff <- rep(NA, length=N)  #Initialization of Cumulative False alarm

for(md in 1:M) {
  for(cd in 1:C) {
    for(qd in 1 : Q){
      for(n  in 1:cd){
        ff[cd+(md-1)*C*Q+(qd-1)*C]<-paste("FP:  moda= ",md," reader = ",qd," conf =",C-cd+1, "." )
        hh[cd+(md-1)*C*Q+(qd-1)*C]<-paste("TP:  moda= ",md," reader = ",qd," conf =",C-cd+1, "." )
      }
    }}}




message("* An incomplet deataset is created.  \n\n\n ")
h <-as.numeric(hh) ;  f <-as.numeric(ff)
dataList <- list( M=M,Q=Q,NL=NL,NI=NI,C=C,c=c,m=m,q=q,f=f,h=h)
# assign("dataList", dataList, envir=globalenv())


message("*  Return values list, please enter hits and false alarms by yourself.  \n\n\n ")
message("\n* If you forget to make an object for return value of the function, then use an object [  .Last.value  ] which retains the last value. \n")
     return(dataList)
   }
}#if









   if(b==1){




N <-C*M*Q
#For Draw the Sample points on FROC curve. Assessment of Fit for FROC.
hh <- rep(NA, length=N)  #Initialization of Cumulative Hits
ff <- rep(NA, length=N)  #Initialization of Cumulative False alarm

for(md in 1:M) {
  for(cd in 1:C) {
    for(qd in 1 : Q){
              message("* Enter number of false alarms (False Positives)\n")
              message("    for modaity= ",md,", \n")
              message("        reader = ",qd,", \n")
              message("          conf = ",C-cd+1,".\n")
          message("To exit and get the data until now, enter \" TRUE \".\n")
          bb <- readline("Your enter:")
              if(bb==TRUE){
                    message("* An incomplet deataset is created.  \n\n\n ")
                    h <-as.numeric(hh) ;f <-as.numeric(ff)
                    dataList <<- list( M=M,Q=Q,NL=NL,NI=NI,C=C,c=c,m=m,q=q,f=f,h=h)
                    return(list( M=M,Q=Q,NL=NL,NI=NI,C=C,c=c,m=m,q=q,f=f,h=h))
              }
         ff[cd+(md-1)*C*Q+(qd-1)*C]<-bb


 }}}


for(md in 1:M) {
  for(cd in 1:C) {
    for(qd in 1 : Q){
      message("* Enter number of hits (True Positives)\n")
      message("    for modaity= ",md,", \n")
      message("        reader = ",qd,", \n")
      message("          conf = ",C-cd+1,".\n")
      message("To exit and get the data until now, enter \" TRUE \".\n")
      bb <- readline("Your enter:")
      hh[cd+(md-1)*C*Q+(qd-1)*C]<-bb
 if(bb== TRUE){
   message("* An incomplet dataset is created.  \n\n\n ")
   h <-as.numeric(hh) ;f <-as.numeric(ff)
      dataList <- list( M=M,Q=Q,NL=NL,NI=NI,C=C,c=c,m=m,q=q,f=f,h=h)
      return(list( M=M,Q=Q,NL=NL,NI=NI,C=C,c=c,m=m,q=q,f=f,h=h))
 }

    }}}





message("* A complet deataset is created.  \n\n\n ")
h <-as.numeric(hh) ;f <-as.numeric(ff)

dataList <- list( M=M,Q=Q,NL=NL,NI=NI,C=C,c=c,m=m,q=q,f=f,h=h)
return(list( M=M,Q=Q,NL=NL,NI=NI,C=C,c=c,m=m,q=q,f=f,h=h))



   }#if




}
