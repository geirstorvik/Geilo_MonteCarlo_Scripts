#Model:
#x_t=ax_{t-1}+eps
#y_t~Binomial(exp(x_t)/[1+exp(x_t)])
#a~Unif[0,1]
#Inference based on fixed parameters
library(DescTools)
rm(list=ls())
sig2=1;sig=sqrt(sig2)
sig2.a=1;sig.a=sqrt(sig2.a)
d = read.table("data/lemmings.txt",header=TRUE)
names(d) = c("y","year")
y = d$y
nT = length(y)
logit = function(x){log(x/(1-x))}
logitinv = function(x){1/(1+exp(-x))}

N = 10000
x.sim = matrix(nrow=nT,ncol=N)
a.sim = matrix(nrow=nT,ncol=N)
w.sim = matrix(nrow=nT,ncol=N)
x.hat = matrix(nrow=nT,ncol=3)
a.hat = matrix(nrow=nT,ncol=3)
a.unique = rep(NA,nT)
set.seed(345)

#Initialization
x.sim[1,]=rnorm(N,0,sig)
a.sim[1,] = runif(N)
w.sim[1,] = dbinom(y[1],1,exp(x.sim[1,])/(1+exp(x.sim[1,])))
x.hat[1,1:3] = Quantile(x.sim[1,],weights=w.sim[1,],c(0.5,0.025,0.975))
a.hat[1,1:3] = Quantile(a.sim,weights=w.sim[1,],c(0.5,0.025,0.975))
a.unique[1] = length(unique(a.sim[1,]))
#Resample
ind = sample(1:N,N,replace=T,prob=w.sim[1,])
w = rep(1/N,N)
q.a = 0.1
for(i in 2:nT)
{
  x.sim[i,]=rnorm(N,a.sim[i-1,ind]*x.sim[i-1,ind],sig)
  z = logit(a.sim[i-1,ind])+rnorm(N,0,q.a)
  a.sim[i,]=logitinv(z)
  if(!is.na(y[i]))
    w = w*dbinom(y[i],1,exp(x.sim[i,])/(1+exp(x.sim[i,])))
  x.hat[i,1:3] = Quantile(x.sim[i,],weights=w,c(0.5,0.025,0.975))
  a.hat[i,1:3] = Quantile(a.sim[i,],weights=w,c(0.5,0.025,0.975))
  a.unique[i] = length(unique(a.sim[i,]))
  w.sim[i,] = w
  #Resample
  ind = sample(1:N,N,replace=T,prob=w)
  w = rep(1/N,N)
}

library(ggplot2)
pdf("Lemmings_res_filt_parest2.pdf",height=5,width=10)
plotd = data.frame(id=rep(1:nT,3),x=as.vector(a.hat),
                   group=c(rep(1,nT),rep(2,nT),rep(3,nT)),
                   ahat=c(rep("Mode",nT),rep("Quant",nT),rep("Quant",nT)))
plotd$group = as.factor(plotd$group)
ggplot(plotd, aes(x=id,y=x,group=group,colour=ahat)) +
  geom_point()+geom_line(aes(lty=ahat)) 
dev.off()


