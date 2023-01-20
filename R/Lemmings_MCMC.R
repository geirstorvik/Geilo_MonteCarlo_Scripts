#Model:
#x_t=ax_{t-1}+eps
#y_t~Binomial(exp(x_t)/[1+exp(x_t)])
#a~Unif[0,1]
#Inference based on fixed parameters
library(DescTools)
#rm(list=ls())
sig2=1;sig=sqrt(sig2)
d = read.table("../data/lemmings.txt",header=TRUE)
names(d) = c("y","year")
y = d$y
nT = length(y)

#Initialization, ignoring time dependence
a = 0.5
sig.m = sig/sqrt(1-a^2)
x = rep(NA,nT)
for(i in 1:nT)
{
 #Rejection sampling
 acc = FALSE
  while(!acc)
 {
  x[i] = rnorm(1,0,sig.m)
  p = 1
  if(!is.na(y[i]))
   p = exp(x[i]*y[i])/(1+exp(x[i]))
  if(runif(1)<p)
   acc = TRUE
 }
}

#MCMC steps
M=5000
a.sim = rep(a,M)
x.sim = matrix(nrow=M,ncol=nT)
for(n in 1:M)
{
  #Gibbs sampling for a
  sig.a = sig/sqrt(sum(x[-1]^2))
  mu.a = sum(x[-1]*x[-nT])/sum(x[-1]^2)
  a = rnorm(1,mu.a,sig.a)
  while((a<0) | (a>1))
    a = rnorm(1,mu.a,sig.a)
  #M-H for x-process
  #i=1
  p.cur = exp(x[1])/(1+exp(x[1]))
  sig.cond = sig
  x.prop = rnorm(1,a*x[2],sig.cond)
  p.prop = exp(x.prop)/(1+exp(x.prop))
  if(runif(1)<(dbinom(y[1],1,p.prop)/dbinom(y[1],1,p.cur)))
     x[1] = x.prop
  sig.cond = sig*sqrt((1-a^2)/(1-a^4))
  for(i in 2:(nT-1))
  {
    p.cur = exp(x[i])/(1+exp(x[i]))
    x.prop = rnorm(1,a*(x[i-1]+x[i+1])/(1+a^2),sig.cond)
    p.prop = exp(x.prop)/(1+exp(x.prop))
    r = 1
    if(!is.na(y[i]))
      r = dbinom(y[i],1,p.prop)/dbinom(y[i],1,p.cur)
    if(runif(1)<r)
      x[i] = x.prop
  }
  p.cur = exp(x[nT])/(1+exp(x[nT]))
  sig.cond = sig
  x.prop = rnorm(1,a*x[nT-1],sig.cond)
  p.prop = exp(x.prop)/(1+exp(x.prop))
  if(runif(1)<(dbinom(y[nT],1,p.prop)/dbinom(y[nT],1,p.cur)))
    x[nT] = x.prop
  #Store
  a.sim[n] = a
  x.sim[n,] = x
}

library(ggplot2)
x.hat = matrix(nrow=nT,ncol=3)
for(i in 1:nT)
  x.hat[i,] = quantile(x.sim[-c(1:2500),i],prob=c(0.5,0.025,0.975))
pdf("Lemmings_res_MCMC_x.pdf",height=5,width=10)
plotd = data.frame(id=rep(1:nT,3),x=as.vector(x.hat),
                   group=c(rep(1,nT),rep(2,nT),rep(3,nT)),
                   xhat=c(rep("Mode",nT),rep("Quant",nT),rep("Quant",nT)))
plotd$group = as.factor(plotd$group)
ggplot(plotd, aes(x=id,y=x,group=group,colour=xhat)) +
  geom_point()+geom_line(aes(lty=xhat)) 
dev.off()

pdf("Lemmings_res_MCMC_a.pdf",height=5,width=5)
ggplot(data.frame(a=a.sim[-c(1:2500)]), aes(x=a)) + geom_histogram(col="red")
dev.off()
pdf("Lemmings_res_MCMC_a_acf.pdf",height=5,width=5)
acf(a.sim[-c(1:2500)])
dev.off()