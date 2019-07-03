



# f1 <- function(){
#   A <- array(1:100, c(3, 4, 5))
#   B <- array(1:80, c(4,5))
#
#   C <- array(aperm(sapply(1:dim(A)[1], function(i) A[i,,] - B)), dim(A))
#
# }
#
#
# f2<-function(){
#
#   A <- array(1:100, c(3, 4, 5))
#   B <- array(1:80, c(4,5))
#   sweep(A, c(2,3), B)
# }
#
# f3 <- function(){
#
#   A <- array(1:100, c(3, 4, 5))
#   B <- array(1:80, c(4,5))
#
#   # Perform calculation
#   res <- array(t(apply(A, MARGIN = 1, function(x)x-B)), c(3, 4, 5))
# }
#
#
# library(microbenchmark)
# library(ggplot2)
#
# mb <- microbenchmark(
#   f1 = f1(),
#   f2 = f2(),
#   f3 = f3()
# )
#
# mb
# autoplot(mb)












study <- function(){





A <- array(NA,c(2,3,4))
for (i in 1:2) {for(j in 1:3){for(k in 1:4){
  A[i,j,k] <- i*1000000+j*100000+k*10000
}}}

B <- array(NA,c(100,2))
for (h in 1:100) {for(i in 1:2){B[h,i] <- h*10+i }}





A <- array(NA,c(2,3,4))
for (i in 1:2) {for(j in 1:3){for(k in 1:4){
  A[i,j,k] <- i*1000000+j*100000+k*10000
}}}

B <- array(NA,c(2,3))
for (h in 1:2) {for(i in 1:3){B[h,i] <- h*10+i }}




C <- array(NA, c(dim(B)[1], dim(A)))
# Approach 1
for (h in 1 : dim(B)[1])
  for(i in 1 : dim(A)[1])
    C[h, i,, ] <-  A[i,, ] - B[h, i]

# Approach 2
for (h in 1 : dim(B)[1])
  C[h,,,] <-  sweep(A, 1, B[h, ], "-")

}
