library(DescTools)
#rm(list=ls())
sig2=1;sig=sqrt(sig2)
sig2.a=1;sig.a=sqrt(sig2.a)
d = read.table("../data/lemmings.txt",header=TRUE)
names(d) = c("y","year")
y = d$y
y = y[!is.na(y)]
nT = length(y)

n = length(y)
index=c(1:n)[!is.na(y)]
#init_fun = function()list(x=x.hat[,1],a=0.5)
d_bin = list(N=n,y=y,sig=sig)
library(rstan)
fit = stan(file="lemmings.stan",data=d_bin,iter=100000)

saveRDS(fit,"Lemmings_stan.RDS")