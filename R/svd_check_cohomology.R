name <- function(variables) {


a <-matrix(
   c(
      1,-1,0 ,1,0,0,
      1, 0,-1 ,0,1,0,
      0, 1,-1 ,0,0,1,
      0,0,0 ,1,-1,1
   )

   , nrow =4, ncol = 6,byrow=TRUE)
a



svd(a)



b <-matrix(
   c(
      1,0,0,0,0,0,
      0,1,0,0,0,0,
      0,0,1,0,0,0
   )

   , nrow =3, ncol = 6,byrow=TRUE)
b

c<-a%*%t(b)

svd(c)






bb <-matrix(
   c(
      1,0,0,0,0,0,
      0,0,0,0,1,0,
      0,0,1,0,0,0
   )

   , nrow =3, ncol = 6,byrow=TRUE)
bb

cc<-a%*%t(bb)

svd(cc)







bbb <-matrix(
   c(
      1,0,0,0,0,0,
      0,0,0,0,1,0,
      0,0,0,1,0,0
   )

   , nrow =3, ncol = 6,byrow=TRUE)
bbb

ccc<-a%*%t(bbb)

svd(ccc)












bbbb <-matrix(
   c(
      1,0,0,0,0,0,
      0,0,0,0,1,0,
      0,1,0,0,0,0
   )

   , nrow =3, ncol = 6,byrow=TRUE)
bbbb

cccc<-a%*%t(bbbb)

svd(cccc)





bbbbb <-matrix(
   c(
      1,0,0,0,0,0,
      0,0,0,0,1,0,
      0,0,0,0,0,1
   )

   , nrow =3, ncol = 6,byrow=TRUE)
bbbbb

ccccc<-a%*%t(bbbbb)

svd(ccccc)










 d <-matrix(
   c(
     1:4,
     1:4,
     1:4



   )

   , nrow =3, ncol = 4,byrow=TRUE)
d

d%*%t(d)










}
