

layout <- function(fit){
if(fit@studyDesign=="MRMC"){return(message("This is only a single reader and a single modality case. "))}
  suppressWarnings(graphics::par(new=TRUE));

 ff <- fit@metadata$ff
 hh <- fit@metadata$hh
 NL <- fit@dataList$NL
 NI <- fit@dataList$NI
 C <- fit@dataList$C

CFP <- vector()
CHP <- vector()
for (cd in 1:C) {
  ssss <-""
  tttt <-""

  for (ccd in C:cd) {

    if(ssss==""){
      ssss <- paste(ssss,"H[",ccd,"]",sep = "")
      tttt <- paste(tttt,"F[",ccd,"]",sep = "")
    }else{
      ssss <- paste(ssss," + H[",ccd,"]",sep = "")
      tttt <- paste(tttt," + F[",ccd,"]",sep = "")
    }

    CHP[cd] <- paste( "( ",ssss," ) /",NL,sep = "")
    CFP[cd] <- paste( "( ",tttt," ) /",NI,sep = "")

    # CC <- as.character(C)
    # name.index[cd] <- paste("c=",cd,sep = "")
    # graphics::text(ff[cd]- max(ff)/10,hh[cd] - max(hh)/10,expression( paste("=",sum(H[c],c=name.index[cd],CC))))
    if (C-cd+1==1) {
      graphics::text(ff[C-cd+1]+ max(ff)/13,
                     hh[C-cd+1] + max(hh)/7 +0.1,
                     CFP[cd])


      graphics::text(ff[C-cd+1]+ max(ff)/13,
                     hh[C-cd+1] + max(hh)/10,
                     CHP[cd])

    }
    if ( (!C-cd+1==1)&&C-cd+1==C) {


      graphics::text(ff[C-cd+1]+ max(ff)/13,
                     hh[C-cd+1] + max(hh)/7 +0.1,
                     CFP[cd])

      graphics::text(ff[C-cd+1]- max(ff)/6,
                     hh[C-cd+1] + max(hh)/10,
                     CHP[cd])

    }
    graphics::text(ff[C-cd+1]+ max(ff)/13,
                   hh[C-cd+1] + max(hh)/7 +0.1,
                   CFP[cd])

    graphics::text(ff[C-cd+1] + max(ff)/13,
                   hh[C-cd+1] + max(hh)/10,
                   CHP[cd])

  }#        for (ccd in 1:C) {
}#        for ( cd in 1:C) {


}
