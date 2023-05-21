#Particle MCMC on lemmings data
#Model:
#x_t=ax_{t-1}+eps
#y_t~Binomial(exp(x_t)/[1+exp(x_t)])
#Inference based on fixed parameters
#library(DescTools)
#library(LaplacesDemon)
rm(list=ls())
source("SMClemmings.R")
sig2=1;sig=sqrt(sig2)
sig2.a=1;sig.a=sqrt(sig2.a)
a=0.9
d = read.table("../data/lemmings.txt",header=TRUE)
names(d) = c("y","year")
y = d$y
nT = length(y)

M = 1000
N = 100
sig.prop = 0.05
#initial value on a
a.cur = runif(1,-1,1)
logL.cur = SMClemmings(y,a=a.cur)$logLT
acc = 0
a.M = rep(NA,M)
a.M[1] = a.cur
for(i in 2:M)
{
  x.cur = log(1+a.cur)-log(1-a.cur)
  x.prop = x.cur + rnorm(1,0,sig.prop)
  a.prop = (exp(x.prop)-1)/(exp(x.prop)+1)
  logL.prop = SMClemmings(y,a=a.prop)$logLT
  r = exp(logL.prop-logL.cur)
  #show(c(a.prop,logL.prop))
  #Correct for proposal distribution on a-scale
  r = r*(1-a.prop^2)/(1-a.cur^2)
  if(runif(1)<r)
  {
    a.cur = a.prop
    logL.cur = logL.prop
    acc = acc + 1
  }
  a.M[i] = a.cur
  show(c(i,a.cur,a.prop,logL.prop))
}