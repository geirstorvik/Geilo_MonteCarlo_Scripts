#Model:
#x_t=ax_{t-1}+eps
#y_t~Binomial(exp(x_t)/[1+exp(x_t)])
#Inference based on fixed parameters
library(DescTools)
rm(list=ls())
sig2=1;sig=sqrt(sig2)
sig2.a=1;sig.a=sqrt(sig2.a)
a=0.5
d = read.table("data/lemmings.txt",header=TRUE)
names(d) = c("y","year")
y = d$y
nT = length(y)

N = 10000
x.sim = matrix(nrow=nT,ncol=N)
x.hat = matrix(nrow=nT,ncol=3)
w.sim = matrix(nrow=nT,ncol=N)
set.seed(345)

#Initialization
x.sim[1,]=rnorm(N,0,sig)
w.sim[1,] = dbinom(y[1],1,exp(x.sim[1,])/(1+exp(x.sim[1,])))
x.hat[1,1:3] = Quantile(x.sim[1,],weights=w.sim[1,],c(0.5,0.025,0.975))
#Resample
ind = sample(1:N,N,replace=T,prob=w.sim[1,])
x.sim[1,] = x.sim[1,ind]
w = rep(1/N,N)
x.cur = x.sim[1,]
for(i in 2:nT)
{
  x.sim[i,]=rnorm(N,a*x.cur,sig)
  if(!is.na(y[i]))
    w = w*dbinom(y[i],1,exp(x.sim[i,])/(1+exp(x.sim[i,])))
  #x.hat[i,1] = weighted.mean(x.sim[i,],w)
  x.hat[i,1:3] = Quantile(x.sim[i,],weights=w,c(0.5,0.025,0.975))
  w.sim[i,] = w
  #Resample
  ind = sample(1:N,N,replace=T,prob=w)
  x.sim[1:i,] = x.sim[1:i,ind]           #Note: Resampling only current
  w = rep(1/N,N)
}

library(ggplot2)
pdf("Lemmings_res_filt.pdf",height=5,width=10)
plotd = data.frame(id=rep(1:nT,3),x=as.vector(x.hat),
                   group=c(rep(1,nT),rep(2,nT),rep(3,nT)),
                   xhat=c(rep("Mode",nT),rep("Quant",nT),rep("Quant",nT)))
plotd$group = as.factor(plotd$group)
ggplot(plotd, aes(x=id,y=x,group=group,colour=xhat)) +
  geom_point()+geom_line(aes(lty=xhat)) 
dev.off()


#Smoothing (FFBS algorithm)
w.hat = w.sim
x.smo = matrix(nrow=nT,ncol=3)
x.smo[nT,] = x.hat[nT,]
B.t = sample(1:N,prob=w.hat[nT,],replace=TRUE)
for(i in (nT-1):1)
{
  w.hat[i,] = w.sim[i,]*dnorm(x.sim[i+1,B.t],a*x.sim[i,],sig.a)
  x.smo[i,1:3] = Quantile(x.sim[i,],weights=w.hat[i,],c(0.5,0.025,0.975))
  B.t = sample(1:N,prob=w.hat[i,],replace=TRUE)
}

pdf("Lemmings_res_smo.pdf",height=5,width=10)
plotd = data.frame(id=rep(1:nT,3),x=as.vector(x.smo),
                   group=c(rep(1,nT),rep(2,nT),rep(3,nT)),
                   xsmo=c(rep("Mode",nT),rep("Quant",nT),rep("Quant",nT)))
plotd$group = as.factor(plotd$group)
ggplot(plotd, aes(x=id,y=x,group=group,colour=xsmo)) +
  geom_point()+geom_line(aes(lty=xsmo)) 
dev.off()

pdf("Lemmings_res_smo2.pdf",height=5,width=10)
plotd = data.frame(id=rep(1:nT,6),x=c(as.vector(x.hat),as.vector(x.smo)),
                   group=c(rep(1,nT),rep(2,nT),rep(3,nT),
                           rep(4,nT),rep(5,nT),rep(6,nT)),
                   xsmo=c(rep("Filt",3*nT),rep("Smot",3*nT)),
                   lty=c(rep("Mode",nT),rep("Quant",2*nT),
                         rep("Mode",nT),rep("Quant",2*nT)))
plotd$group = as.factor(plotd$group)
ggplot(plotd, aes(x=id,y=x,group=group,colour=xsmo)) +
  geom_line(aes(lty=lty))#+geom_point()
dev.off()



L1=x.hat[,3]-x.hat[,2]
L2=x.smo[,3]-x.smo[,2]
plot.ts(cbind(L1,L2),plot.type="single")
#N.unique = rep(NA,nT)
#for(i in 1:nT)
#  N.unique[i] = length(unique(x.sim[i,]))
#plot.ts(N.unique)

