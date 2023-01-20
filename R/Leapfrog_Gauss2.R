#Leapfrog with bivariate Gaussian
mu = c(0,0)
a = 0.95
Sigma = matrix(c(1,a,a,1),ncol=2)
library(mvtnorm)

#Ordinary M-H
cat("Random walk M-H\n")
N = 10000
sig.prop=1.2
X = matrix(nrow=N,ncol=2)
X[1,] = rnorm(2)
acc = 0
ptm <- proc.time()
for(i in 2:N)
{
  j = sample(1:2,1)
  y = X[i-1,]
  y[j] = rnorm(1,X[i-1,j],sig.prop)
  R = dmvnorm(y,mu,Sigma)/dmvnorm(X[i-1,],mu,Sigma)
  if(runif(1)<R)
  {
    X[i,] = y
    acc = acc +1
  }
  else
    X[i,] = X[i-1,]
}
show(proc.time() - ptm)
X.MH = X

cat("Acceptance rate=",acc/(N-1),"\n")

#Leapfrog
cat("Hamiltonean MCMC\n")
Hfunc = function(x,p)
{
#  -dmvnorm(x,mu,Sigma,log=TRUE)+0.5*sum(p^2)
  0.5*sum(x*solve(Sigma,x))+0.5*sum(p^2)
}
dU = function(x)
{
  solve(Sigma,x-mu)
}
eps = 0.1
L = 5
N = 10000
X = matrix(nrow=N,ncol=2)
X[1,] = rnorm(2)
acc = 0
ptm <- proc.time()
for(i in 2:N)
{
  p = rnorm(2)
  H = Hfunc(X[i-1,],p)
  x.prop = X[i-1,1]
  for(j in 1:L)
  {
    p.half = p - 0.5*eps*dU(x.prop)
    x.prop = x.prop + eps*p.half
    p.prop = p.half - 0.5*eps*dU(x.prop)
  }
  H.prop = Hfunc(x.prop,p.prop)
  R = exp(H-H.prop)
#  show(c(X[i-1,],x.prop,H,H.prop,R))
  if(runif(1)<R)
  {
    X[i,] = x.prop
    acc = acc + 1
  }
  else
    X[i,] = X[i-1,]
}
show(proc.time() - ptm)
pdf("Res_Gauss2_Ham.pdf",height=5,width=12)
cat("Acceptace rate=",acc/(N-1),"\n")
acf(X.MH[,1],ylim=c(-0.1,1),lag.max=200,col=3,main="X1")
par(new=T)
acf(X[,1],ylim=c(-0.1,1),lag.max=200,col=2,main="",lwd=2)
dev.off()

