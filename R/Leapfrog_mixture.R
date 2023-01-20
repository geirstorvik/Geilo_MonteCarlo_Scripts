#Leapfrog with mixture of Gaussian
pr = 0.7
mu = c(0,4)
sig = c(0.5,0.1)
distr = function(x)
{
  pr*dnorm(x,mu[1],sig[1]) + (1-pr)*dnorm(x,mu[2],sig[2])
}
x = seq(-2,5,length=100)
plot(x,distr(x),type="l")
N = 100000
D = 50000
#Ordinary M-H

X = rep(NA,N)
X[1] = rnorm(1)
sig.prop = 1.5
acc = 0
ptm <- proc.time()
for(i in 2:N)
{
  x.prop = rnorm(1,X[i-1],sig.prop)
  R = distr(x.prop)/distr(X[i-1])
  if(runif(1)<R)
  {
    X[i] = x.prop
    acc = acc + 1
  }
  else
    X[i] = X[i-1]
}
show(proc.time() - ptm)
X.MH = X[-c(1:D)]
cat("Acceptance rate=",acc/(N-1),"\n")

#Leapfrog
m = 5
U = function(x)
{
  -log(distr(x))
}
Hfunc = function(x,p)
{
  U(x)+0.5*p^2/m
}
dU = function(x)
{
  d1 = dnorm(x,mu[1],sig[1],log=TRUE)
  d2 = dnorm(x,mu[2],sig[2],log=TRUE)
  dr = exp(d2-d1)
  r = -(pr*(x-mu[1])/sig[1]^2 + (1-pr)*dr*(x-mu[2])/sig[2]^2)/(pr+dr)
}
eps = 0.025
L = 10
X = rep(NA,N)
X[1] = rnorm(1)
sig.prop = 1.5
acc = 0
ptm <- proc.time()
for(i in 2:N)
{
  eps = runif(1,0.8,1.2)
  p = rnorm(1,0,sqrt(m))
  H = Hfunc(X[i-1],p) 
  x.prop = X[i-1]
  for(j in 1:L)
  {
    p.half = p - 0.5*eps*dU(x.prop)
    x.prop = x.prop + eps*p.half/m
    p.prop = p.half - 0.5*eps*dU(x.prop)
  }
  H.prop = Hfunc(x.prop,p.prop)
  
  R = exp(H-H.prop)
#  show(c(X[i-1],x.prop,H,H.prop,R))
  if(runif(1)<R)
  {
    X[i] = x.prop
    acc = acc + 1
  }
  else
    X[i] = X[i-1]
}
X.HMC = X[-c(1:D)]
show(proc.time() - ptm)
cat("Acceptance rate=",acc/(N-1),"\n")
acf(X.MH,ylim=c(-0.1,1),lag.max=100,col=3,main="X1")
par(new=T)
acf(X.HMC,ylim=c(-0.1,1),lag.max=100,col=2,main="",lwd=2)
legend("topright",c("MH","HMC"),lty=1,col=3:2)
par(mfrow=c(1,2))
hist(X.MH,30,prob=TRUE,ylim=c(0,1));lines(x,distr(x),col=2)
hist(X.HMC,30,prob=TRUE,ylim=c(0,1));lines(x,distr(x),col=2)
