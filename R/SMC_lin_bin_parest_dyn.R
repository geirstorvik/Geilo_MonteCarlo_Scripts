#SMC with parameter estimation
#Estimating parameters simultaneously using dynamics into theta (a)
q = 0.5
#Initialization
x.sim[1,]=rnorm(N,0,sig)
w = dbinom(y[1],1,exp(x.sim[1,])/(1+exp(x.sim[1,])))
#Resample
ind = sample(1:N,N,replace=T,prob=w)
x.sim[1,] = x.sim[1,ind]
w = rep(1/N,N)
x.hat[1,1] = mean(x.sim[1,])
x.hat[1,2:3] = quantile(x.sim[1,],c(0.025,0.975))
a.sim = rnorm(N,0,sqrt(sig2.a))
a.hat = matrix(nrow=nT,ncol=3)
a.hat[1,1] = mean(a.sim)
a.hat[1,2:3] = quantile(a.sim,c(0.025,0.975))
for(i in 2:nT)
{
  a.sim = 0.75*a.sim + rnorm(N,0,q)
  x.sim[i,]=rnorm(N,a.sim*x.sim[i-1,],sig)
  if(!is.na(y[i]))
    w = w*dbinom(y[i],1,exp(x.sim[i,])/(1+exp(x.sim[i,])))
  #Resample
  ind = sample(1:N,N,replace=T,prob=w)
  x.sim[1:i,] = x.sim[1:i,ind]           #Note: Resampling the whole path! 
  a.sim = a.sim[ind]
  w = rep(1/N,N)
  x.hat[i,1] = mean(x.sim[i,])
  x.hat[i,2:3] = quantile(x.sim[i,],c(0.025,0.975)) 
  a.hat[i,1] = mean(a.sim)
  a.hat[i,2:3] = quantile(a.sim,c(0.025,0.975)) 
}
N.unique = rep(NA,nT)
for(i in 1:nT)
  N.unique[i] = length(unique(x.sim[i,]))
plot.ts(N.unique)
matplot(cbind(1:nT,1:nT,1:nT),a.hat,type="l",lty=c(1,2,2),col=1,xlab="time")

a.sim.dyn = a.sim