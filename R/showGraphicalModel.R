
#' @title Show the Graphical Model
#' for the case of a single reader and a single modality
#' @description This function shows
#' the graphical model for a single
#'  reader and a single modality
#'  FROC statistical model.
#' @export
#'
#' @examples
#'\donttest{
#'   showGM()
#'}# donttest
showGM <- function(){

  message("\n* The figure shows the graphical model for our FROC model with respect to a single reader and a single modality case.")
message("\n")
  message("p: Hit rate, i.e., Hits ~ Binomial(p,NL), where NL is the number of Lesions")
  message("l: False rate, i.e., cumulative False alarms ~ Poisson(lambda*NX), where NX=NL or NI (number of images)")
message("z: Thresholds of binormal assumption")
message("m: mean of the signal distribution")
message("v: standard deviation of the noise distribution")
message("\n a = m/v")
message(" b = 1/v \n ")
message("AUC=Phi(  a/sqrt(b^2+1)  ), Phi is the cumulative distribution function of the Gaussian distribution with mean 0 and variance 1.")








DiagrammeR::grViz( "

                   digraph sake_flow{

Hits[color='gray' style='filled'  fillcolor = 'gray' shape = 'box' ]
False_Alarms[color='gray' style='filled'  fillcolor = 'gray' shape = 'box' ]
No_of_Images[color='gray' style='filled'  fillcolor = 'gray' shape = 'box' ]
No_of_Lesions[color='gray' style='filled'  fillcolor = 'gray' shape = 'box' ]

z[color='red' style='filled'  fillcolor = 'salmon'   ]
m[color='red' style='filled'  fillcolor = 'salmon'  ]
v[color='red' style='filled'  fillcolor = 'salmon'   ]

p[  style='filled'  fillcolor = 'LimeGreen' shape = 'doublecircle'   ]
l[  style='filled'  fillcolor = 'LimeGreen' shape = 'doublecircle'   ]
a[  style='filled'  fillcolor = 'LimeGreen' shape = 'doublecircle'   ]
b[  style='filled'  fillcolor = 'LimeGreen' shape = 'doublecircle'   ]
AUC[  style='filled'  fillcolor = 'LimeGreen' shape = 'doublecircle'   ]


m->a
v->a->AUC
v->b->AUC


z->l->False_Alarms
No_of_Lesions ->Hits
No_of_Images ->False_Alarms
z->p->Hits
m->p
v->p
m->l
v->l


}


                   ")
}#function

