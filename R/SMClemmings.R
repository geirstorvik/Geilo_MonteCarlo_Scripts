#Model:
#x_t=ax_{t-1}+eps
#y_t~Binomial(exp(x_t)/[1+exp(x_t)])
#Inference based on fixed parameters
SMClemmings = function(y=y,N=10000,sig=1,a=0.5)
{
  nt = length(y)
  sig.a = sig/sqrt(1-a^2)
  x.sim = matrix(nrow=nT,ncol=N)
  x.hat = matrix(nrow=nT,ncol=3)
  w.sim = matrix(nrow=nT,ncol=N)
  logL = rep(NA,nT)

  #Initialization
  x.sim[1,]=rnorm(N,0,sig.a)
  w.sim[1,] = dbinom(y[1],1,exp(x.sim[1,])/(1+exp(x.sim[1,])))
  logL[1] = log(max(w.sim[1,]))+log(mean(w.sim[1,]/max(w.sim[1,])))
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
    logL[i] = logL[i-1] + log(max(w.sim[i,]))+log(mean(w.sim[i,]/max(w.sim[i,])))
    #Resample
    ind = sample(1:N,N,replace=T,prob=w)
    x.cur = x.sim[i,ind]           #Note: Resampling only current
    w = rep(1/N,N)
  }
  list(x.sim=x.sim,w.sim=w.sim,x.hat=x.hat,logL=logL,logLT=sum(logL))
}
