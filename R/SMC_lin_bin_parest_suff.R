#SMC with parameter estimation
#Using sufficient statistics
sig2=sig^2
sig2.a = 2
#Initialization
x.sim[1,]=rnorm(N,0,sig)
w = dbinom(y[1],1,exp(x.sim[1,])/(1+exp(x.sim[1,])))
#Resample
ind = sample(1:N,N,replace=T,prob=w)
x.sim[1,] = x.sim[1,ind]
w = rep(1/N,N)
x.hat[1,1] = mean(x.sim[1,])
x.hat[1,2:3] = quantile(x.sim[1,],c(0.025,0.975))
a.hat = matrix(nrow=nT,ncol=3)
S = matrix(0,nrow=2,ncol=N)
for(i in 2:nT)
{
  a.sim = rnorm(N,sig2.a*S[1,]/(sig2.a*S[2,]+sig2),sqrt(sig2*sig2.a)/(sig2.a*S[2,]+sig2))
  a.hat[i-1,1] = mean(a.sim)
  a.hat[i-1,2:3] = quantile(a.sim,c(0.025,0.975)) 
  x.sim[i,]=rnorm(N,a.sim*x.sim[i-1,],sig)
  if(!is.na(y[i]))
    w = w*dbinom(y[i],1,exp(x.sim[i,])/(1+exp(x.sim[i,])))
  #Update sufficient statistics
  S[1,] = S[1,]+x.sim[i,]*x.sim[i-1,]
  S[2,] = S[2,]+x.sim[i-1,]^2
  #Resample
  ind = sample(1:N,N,replace=T,prob=w)
  x.sim[1:i,] = x.sim[1:i,ind]           #Note: Resampling the whole path! 
  S = S[,ind]
  w = rep(1/N,N)
  x.hat[i,1] = mean(x.sim[i,])
  x.hat[i,2:3] = quantile(x.sim[i,],c(0.025,0.975))  
}
N.unique = rep(NA,nT)
for(i in 1:nT)
  N.unique[i] = length(unique(x.sim[i,]))
#plot.ts(N.unique)

N.unique = rep(NA,nT)
for(i in 1:nT)
  N.unique[i] = length(unique(x.sim[i,]))
plot.ts(N.unique)
matplot(cbind(1:nT,1:nT,1:nT),a.hat,type="l",lty=c(1,2,2),col=1,xlab="time",ylim=c(-1,1))

matlines(cbind(1:nT,1:nT),cbind(rep(qnorm(0.025,0,sig.a),nT),rep(qnorm(0.025,0,sig.a),nT)),col=2)

a.sim.suff = a.sim


