#
#
#
# multinom <- "data {
#   int y[4];
# }
#
# transformed data {
#   int N = sum(y);
# }
#
# parameters {
#   simplex[4] p1;
#   simplex[4] p2;
#   simplex[4] p3;
#
# }
#
# model {
#   y ~ multinomial(p1);
#
#   y[1] ~ binomial(N, p2[1]);
#   y[2] ~ binomial(N - y[1], p2[2] / (1 - p2[1]));
#   y[3] ~ binomial(N - y[1] - y[2], p2[3] / (1 - p2[1] - p2[2]));
#
#   y[3] ~ binomial(N, p3[3]);
#   y[2] ~ binomial(N - y[3], p3[2] / (1 - p3[3]));
#   y[1] ~ binomial(N - y[3] - y[2], p3[1] / (1 - p3[3] - p3[2]));
# }
#
# generated quantities {
#   real lp1 = multinomial_lpmf(y | p1);
#   real lp2 = binomial_lpmf(y[1] | N, p1[1]) +
#     binomial_lpmf(y[2] | N - y[1], p1[2] / (1 - p1[1])) +
#     binomial_lpmf(y[3] | N - y[1] - y[2], p1[3] / (1 - p1[1] - p1[2]));
#
#    real lp3 = binomial_lpmf(y[3] | N, p3[3]) +
#     binomial_lpmf(y[2] | N - y[3], p3[2] / (1 - p3[3])) +
#     binomial_lpmf(y[1] | N - y[1] - y[2], p3[1] / (1 - p3[3] - p3[2]));
#
#
#   real diff = lp1 - lp2;
# }
#
# "
#
#
#
#
# # library(cmdstanr)
#
# # prob = c(0.1, 0.2, 0.3, 0.4)
# # N = 20
# N = 200
#
# y = as.vector(rmultinom(1, N, prob))
# model =  rstan::stan_model( model_code=multinom)
#
#
# # model = cmdstan_model("multinom.stan")
# fit1 <- sampling(model, data =list(y=c(0,0,1,100)),iter=11111,seed = 1)
# fit2 <- sampling(model, data =list(y=c(0,0,1,100)),iter=11111,seed = 2)
#
# # fit = model$sample(data = list(y = y))
# # fit$print(max_rows = 12)
# e<-extract(fit1)
# mean( abs( e$p1-e$p2));mean( abs( e$p2-e$p3));mean( abs( e$p1-e$p3))
# mean( abs( e$lp1-e$lp2))
#
# ee<-extract(fit2)
# mean( abs( ee$p1-ee$p2));mean( abs( ee$p2-ee$p3));mean( abs( ee$p1-ee$p3))
# mean( abs( ee$lp1-ee$lp2))
#
#
# mean( abs( e$p1-ee$p1));mean( abs( e$p2-ee$p2));mean( abs( e$p3-ee$p3))
# mean( abs( e$lp1-ee$lp1))
#
#
#
# diff <- ( e$p1[,1] - mean(e$p1[,1]))/sd(e$p1[,1]) - ( e$p2[,1] - mean(e$p2[,1]))/sd(e$p2[,1])
# mean(abs(diff))
#
