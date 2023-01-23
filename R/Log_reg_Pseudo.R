#Siumulate data - logistic regression
set.seed(34534)
n = 100
p = 2
x= matrix(rnorm(n*p),ncol=p)
X = cbind(1,x)
p = ncol(X)
beta = matrix(c(1,0.5,0.3),ncol=1)
eta = X%*%beta
pr = 1/(1+exp(-eta))
y = rbinom(n,1,prob=pr)
d = data.frame(x=X,y=y)

#fitting glm
fit = glm(y~X-1)
s = summary(fit)$coef

sd.beta = 10

#function calculating log-likelihood
logL = function(beta,gam,y=y,X=X)
{
  eta = X[,gam==1]%*%matrix(beta,ncol=1)
  pr = 1/(1+exp(-eta))
  sum(dbinom(y,1,prob=pr,log=TRUE))
}
#Pseudo-marginal for logistic regression

accA=accD=accM=0
M = 1000
M2=100
gam.M = matrix(TRUE,nrow=M,ncol=p)
logLhat.M = rep(NA,M)
gam.sim = rep(1,p)
L = rep(NA,M2)
for(j in 1:M2)
{
  beta.sim = rnorm(p,s[,1],s[,2])
  L[j] = logL(beta.sim,gam.sim,y,X)+sum(dnorm(beta.sim,0,sd.beta,log=TRUE))-
    sum(dnorm(beta.sim,s[gam.prop==1,1],s[gam.prop==1,2],log=TRUE))
}
pi.hat.cur = mean(exp(L))
acc = 0
for(i in 2:M)
{
  #Select variable to change, not includinng intercept
  gam.prop = gam.sim
  j = sample(2:p,1)
  gam.prop[j] = 1-gam.prop[j]
  k = sum(gam.prop)
  for(j in 1:M2)
  {
    beta.sim = rnorm(k,s[gam.prop==1,1],s[gam.prop==1,2])
    L[j] = logL(beta.sim,gam.prop,y,X)+sum(dnorm(beta.sim,0,sd.beta,log=TRUE))-
      sum(dnorm(beta.sim,s[gam.prop==1,1],s[gam.prop==1,2],log=TRUE))
  }
  pi.hat.prop = mean(exp(L))
  r = pi.hat.prop/pi.hat.cur
  if(runif(1)<r)
  {
    pi.hat.cur = pi.hat.prop
    gam.sim = gam.prop
    acc = acc + 1
    show(gam.prop)
    show(r)
    show(L)
  }
  gam.M[i,] = gam.sim
  logLhat.M[i] = pi.hat.cur
} 
 