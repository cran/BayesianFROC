## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
 dat <- list(
#Confidence level.
c = c(5,4,3,2,1), 

#Number of hits for each confidence level.
h = c(1,0,0,0,0),

#Number of false alarms for each confidence level.
f = c(0,0,1,0,2),  

#Number of lesions
NL= 2,   

#Number of images
NI= 1,  

#Number of confidence level
 C= 3
)        

