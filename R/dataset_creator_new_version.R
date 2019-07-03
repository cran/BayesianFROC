
#' @title   Create a Dataset (version 2)  Interactively
#'@description
#' Create the Passing data to  the  function \code{ \link{fit_Bayesian_FROC}}.

#'
#'      This is an interactive creator of dataset for FROC data.
#'
#'@details
#'
#' This provide the intaractive making of FROC dataset by using table to summarize hits and false alarm data.
#'
#'
#'Using this return value, you can build the FROC model for your data by applying the function  \code{ \link{fit_Bayesian_FROC}()  }  in this package.
#'
#'
#'Should carefully for the order of confidence levels.
#'
#'
#'@return A  list  representing FROC data, to build FROC fitted model object by \code{\link{fit_Bayesian_FROC}()}.
#' @export dataset_creator_new_version


dataset_creator_new_version <- function(){


   message("From now on, Let's create the FROC dataset together !!\n \n \n \n")



  message("To exit, push esc key. \n \n \n \n")



  message("Please enter 1 or 2, according to the dataset which you want to create:\n
          1:  Single Reader and Single Modality (SRSC)\n
          2:  Multiple Reader and Multiple Modality (MRMC)\n")
  a<- readline("Please enter study design (1 or 2):")
  if(!(a ==2) & ! (a==1)){ return( warning("Warning: Please input 1 or 2."))}


  if(a==1){

    NL<- readline("Please enter Number of lesions:")
    if(is.numeric(NL)){ return( warning("Warning: Please input numeric."))}

    NI<- readline("Please enter Number of images:")
    if(is.numeric(NI)){ return( warning("Warning: Please input numeric."))}
    NL<-as.numeric(NL)
    NI<-as.numeric(NI)
    message("Please enter Number of confidence level:\n ")
    message("
* For example, if your confidence level is as follows,then your input is 5:\n

            5: Definitely Positive\n
            4: Probably Positive\n
            3: Equivocal\n
            2: Probably,... Positive ?\n
            1: Probably,...,ohuu Positive ??

* So, a high number corresponds to a high cofidence level.
            ")
    C <- readline("Please enter confidence number:")
    if(as.integer(C)==0){return(warning("No of confidence level never 0."))}
    if(is.numeric(C)){ return( warning("Warning: Please enter numeric:"))}
    C <- as.integer(C)
    h <- vector()

    for (cd in 1:C) {
      s <- C-cd+1
       h[cd] <- paste("Enter TPs in confidence ", C-cd+1,sep = "")

    }#for
    C <-as.integer(C)
    f <-  vector()

      for (cd in 1:C) {
        s <-C-cd+1
       f[cd] <- paste("Enter FPs in confidence ", C-cd+1,sep = "")

      }

    # f<- as.numeric(f)
    # h<- as.numeric(h)
    NL<- as.numeric(NL)
    NI<- as.numeric(NI)
    c <- C:1
    dataFrame <- data.frame(confidence.level =c,Number.of.False.Alarms =f,Number.of.Hits =h)
    suppressWarnings( dataFrame <-utils::edit(dataFrame) )
    f <-  as.numeric(as.character(dataFrame$Number.of.False.Alarms))
    h <- as.numeric(as.character(dataFrame$Number.of.Hits))

    dataList <- list(f=f  ,h= h ,NL=NL,NI=NI,C=C)

    if(sum(dataList$h)  > NL){
       warning("Your total number of hits until now ",sum(as.numeric(h))," is greater than Your number of lesions ",NL,". ", "That is, ", crayon::red(   sum(as.numeric(h))," > ",NL ), ", such case  never occur in the FROC data.")

      message(crayon::red("\n* The followign contradiction occurred. \n  \n Total numuber of Hits > No of lesions."))
      }
    viewdata_srsc(dataList)

    dataList <- give_name_srsc_data(dataList)

   }


  if(a==2){

    message("* let's make a FROC data for MRMC case together.\n ")


    NI<- readline("Enter the Number of images:")
    if(is.numeric(NI)){ return( warning("Warning: Please input numeric."))}
    NL<- readline("Enter the Number of lesions:")
    if(is.numeric(NL)){ return( warning("Warning: Please input numeric."))}



    Q<- readline("Enter the Number of readers:")
    if(is.numeric(Q)){ return( warning("Warning: Please input numeric."))}

    M<- readline("Enter the Number of modalities:")
    if(is.numeric(M)){ return( warning("Warning: Please input numeric."))}


    message("Enter the Number of confidence level:\n ")
    message("   For example, if your confidence level is as follows,then your input is 5:\n
            5: Definitely Positive,\n
            4: Probably Positive,\n
            3: Equivocal,\n
            2: Pobably Negative,\n
            1: Definitely Negative. ")
    C <- readline("Enter the confidence number:")
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
    message("* f means the No. of false alarms.\n ")
    message("* h means the No. of hits.\n \n\n\n\n ")








      N <-C*M*Q
      #For Draw the Sample points on FROC curve. Assessment of Fit for FROC.
      hh <- rep(NA, length=N)  #Initialization of Cumulative Hits
      ff <- rep(NA, length=N)  #Initialization of Cumulative False alarm

      for(md in 1:M) {
        for(cd in 1:C) {
          for(qd in 1 : Q){
            for(n  in 1:cd){
              ff[cd+(md-1)*C*Q+(qd-1)*C]<-paste("Enter FPs:  modalityID= ",md," reader = ",qd," conf =",C-cd+1, "." )
              hh[cd+(md-1)*C*Q+(qd-1)*C]<-paste("Enter TPs:  modalityID= ",md," reader = ",qd," conf =",C-cd+1, "." )
            }
          }}}


      dataFrame <- data.frame(modalityID=m,readerID=q,confidence.level =c,False.Alarms =ff,Hits =hh)
      suppressWarnings( dataFrame <-utils::edit(dataFrame) )
      f <-  as.numeric(as.character(dataFrame$False.Alarms))
      h <- as.numeric(as.character(dataFrame$Hits))


      dataList <- list( M=M,Q=Q,NL=NL,NI=NI,C=C,c=c,m=m,q=q,f=f,h=h)


       message("\n* If you forget to make an object for return value of the function, then using an object [  .Last.value  ] which retains the last value, you can obtain the dataset which created now. \n")

       viewdata(dataList)


     if(sum(dataList$h)  > NL){
       warning("Your total number of hits until now ",sum(as.numeric(h))," is greater than Your number of lesions ",NL,". ")

       message(crayon::red("\n* The followign contradiction occurred. \n  \n Total numuber of Hits > No of lesions. Thus it will not work when it is used to fit a model."))
     }


    }#if a ==2

  message("\n* If you forget to make an object for return value of the function, then use an object [", crayon::red( ".Last.value"  ) ,"] which retains the last value. \n")
  message("\n*  An object [", crayon::red( ".Last.value"  ) ,"] is your input dataset. !!\n")


  date <-paste(format(Sys.time(),"Year %Y;  Month %m; Date %d; Hour %H; Minit %M"))
  timeaaa <-paste(format(Sys.time(),"%Y%m%dwithTime%Hh%Mm"))

   message("\n* Execute the following code form the R console (or R studio console),  if you forget to retain the dataset created by you [ ",date," ]. \n\n   Created.dataset",timeaaa," <-   .Last.value    \n")

   # path_for_save <- getwd()
   # dir.create( paste(path_for_save,"/", timeaaa, sep = "")  )
   #

  return(dataList)














}#function
