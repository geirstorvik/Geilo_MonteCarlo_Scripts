#Particle MCMC on lemmings data
#Model:
#x_t=ax_{t-1}+eps
#y_t~Binomial(exp(x_t)/[1+exp(x_t)])
#Inference based on fixed parameters
library(DescTools)
library(LaplacesDemon)
rm(list=ls())
source("SMClemmings2.R")
sig2=1;sig=sqrt(sig2)
sig2.a=1;sig.a=sqrt(sig2.a)
a=c(0.9,0.1)
d = read.table("../data/lemmings.txt",header=TRUE)
names(d) = c("y","year")
y = d$y
nT = length(y)

M = 1000
N = 100
sig.prop = 0.1
#initial value on a
a.cur = runif(2,-1,1)
a.cur[2] = runif(1,-1,min(1-a[1],1+a[1]))
logL.cur = SMClemmings2(y,a=a.cur,N=N)$logLT
acc1 = acc2 = 0
a.M = matrix(nrow=M,ncol=2)
a.M[1,] = a.cur
for(i in 2:M)
{
  print(i)
  # Changing a[1],  a[2]-1  < a[1] < 1-a[2]
  r = c(a.cur[2]-1,1-a.cur[2])
  x.cur = log(a.cur[1]-r[1])-log(r[2]-a.cur[1])
  x.prop = x.cur + rnorm(1,0,sig.prop)
  a.prop = a.cur
  a.prop[1] = (r[1]+r[2]*exp(x.prop))/(1+exp(x.prop))
  logL.prop = SMClemmings2(y,a=a.prop,N=N)$logLT
  alpha = exp(logL.prop-logL.cur)
  #Correct for proposal distribution on a-scale
  alpha = alpha*(a.prop[1]-r[1])*(r[2]-a.prop[1])/((a.cur[1]-r[1])*(r[2]-a.cur[1]))
  #show(c(alpha,logL.cur-logL.prop))
  if(runif(1)<alpha)
  {
    a.cur = a.prop
    logL.cur = logL.prop
    acc1 = acc1 + 1
  }
  # Changing a[2],  1-|a[1]|  < a[2] < 1+|a[1]|
  r = c(-1,min(1-a.cur[1],1+a.cur[1]))
  x.cur = log(a.cur[2]-r[1])-log(r[2]-a.cur[2])
  x.prop = x.cur + rnorm(1,0,sig.prop)
  a.prop = a.cur
  a.prop[2] = (r[1]+r[2]*exp(x.prop))/(1+exp(x.prop))
  logL.prop = SMClemmings2(y,a=a.prop,N=N)$logLT
  alpha = exp(logL.prop-logL.cur)
  #Correct for proposal distribution on a-scale
  alpha = alpha*(a.prop[2]-r[1])*(r[2]-a.prop[2])/((a.cur[2]-r[1])*(r[2]-a.cur[2]))
  #show(c(alpha,logL.cur-logL.prop))
  if(runif(1)<alpha)
  {
    a.cur = a.prop
    logL.cur = logL.prop
    acc2 = acc2 + 1
  }
  a.M[i,] = a.cur
  if(i%%10==0)
    logL.cur = SMClemmings2(y,a=a.cur,N=N)$logLT
}
show(c(acc1,acc2)/M)
#pdf("PMCMC_lem2_2.pdf",height=5,width=10)
plot.ts(a.M)
#dev.off()