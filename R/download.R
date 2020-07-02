# BayesianFROC::dark_theme()
# a<-    cranlogs::cran_downloads(
#   packages = "BayesianFROC",
#   from = "2019-05-12",
#   #from = "2019-05-03",
#   to = Sys.Date())
# a
# b <- a$count;
# number.of.downloads <-b;
# mean(b)
# date<- 1:as.integer(length(b))
#
# today <- b[length(b)-2]
# title <- paste("today = ",today ,",",
#                b[length(b)-3],",",
#                b[length(b)-4],",",
#                b[length(b)-5],",",
#                b[length(b)-6],",",
#                b[length(b)-7],",",
#                b[length(b)-8],",",
#                b[length(b)-9]
# )
#
# plot(date,number.of.downloads  ,type="l",col="yellow", main  = title)
# for (hhh in (0:5)*10) graphics::abline(h=hhh)
# graphics::abline(h=b[length(b)-4], col="red",lty ="dashed",lwd ="2")
# graphics::abline(h=b[length(b)-3], col="red",lty ="dashed",lwd ="3")
# graphics::abline(h=b[length(b)-2], col="red",lty ="solid",lwd ="3")
#
# plot(date, cumsum(b), type="l",col="yellow", main  = title)
#
# hist(b,col = "red",border="yellow",breaks=77, main  = title)
#
# for (aaa in 1:50) {
#   graphics::abline(h=aaa)
#   if (aaa%%5==0) {
#     graphics::abline(h=aaa,lwd ="2")
#     # graphics::abline(v=5,lwd ="2")
#     # # graphics::abline(v=15,lwd ="1",lty ="dashed")
#   }
# }
#
#
#
# graphics::abline(v =b[length(b)-2], untf = FALSE, col="red",lty ="solid",lwd ="4");
# graphics::abline(v =b[length(b)-2]-1, untf = FALSE, col="red",lty ="solid",lwd ="4")
#
# graphics::abline(v =b[length(b)-3], untf = FALSE, col="red",lty ="dashed",lwd ="2");
# graphics::abline(v =b[length(b)-3]-1, untf = FALSE, col="red",lty ="dashed",lwd ="2")
#
# graphics::abline(v =b[length(b)-4], untf = FALSE, col="red",lty ="dotdash",lwd ="1");
# graphics::abline(v =b[length(b)-4]-1, untf = FALSE, col="red",lty ="dotdash",lwd ="1")
#
#
# # df<-data.frame(b=b)
# # g <- ggplot2::ggplot(df, ggplot2::aes(x = b))
# # g <- g + ggplot2::geom_histogram(binwidth = 1)
# # plot(g)
# #x<-runif(1000)/10;for(i in 1:1000) y[i]<-  mean(b,trim = x[i]);plot(x,y)
#
#
# sum(b)
#
#
# jjj<-length(b)
# tails <-list( length =  jjj  , mode = "vector")
# tail_are <- vector(length =  jjj, mode = "numeric")
# for (iii in 1:jjj) {
#   tails[[iii]] <- b[b>b[length(b)-iii]]
#   tail_area[iii] <- length(tails[[iii]]) /length(b)
# }
#  tail_area_rev <- rev(tail_area)
#  tail_area_rev <- round(tail_area_rev,digits = 3)
#  tail_area_rev
# # hist(tail_area)
#  plot(date,tail_area_rev  ,type="l",col="yellow", main  = title)
#  graphics::abline(h=0.5,lwd ="2")
#
