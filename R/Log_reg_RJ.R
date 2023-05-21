#Siumulate data - logistic regression
set.seed(34534)
n = 100
p = 2
x= matrix(rnorm(n*p),ncol=p)
X = cbind(1,x)
p = ncol(X)
beta = matrix(c(1,0.7,0.2),ncol=1)
beta = matrix(c(1,0.5,0.3),ncol=1)
eta = X%*%beta
pr = 1/(1+exp(-eta))
y = rbinom(n,1,prob=pr)
d = data.frame(x=X,y=y)

#fitting glm
fit = glm(y~X-1,family=binomial)
s = summary(fit)$coef

sd.beta = 10
prob.gam = c(0.5,0.5)
prob.gam = prob.gam/(1-prob.gam)
fit = glm(y~X-1)
s = summary(fit)$coef

sd.beta = 10

#function calculating log-likelihood
logL = function(beta,gam,y=y,X=X)
{
  eta = X%*%matrix(beta,ncol=1)
  pr = 1/(1+exp(-eta))
  sum(dbinom(y,1,prob=pr,log=TRUE))#+sum(dnorm(beta[gam==1],0,sd.beta,log=TRUE))
}
#RJ for logistic regression

accA=accD=accM=0
M = 10000
M2=10
beta.M = matrix(nrow=M,ncol=p)
gam.M = matrix(TRUE,nrow=M,ncol=p)
logpi.M = rep(NA,M)
beta.sim = rnorm(p,s[,1],s[,2])
gam.sim = rep(TRUE,p)
logpi.cur = logL(beta.sim,gam.sim,y,X)
beta.M[1,] = beta.sim

gam.sim = rep(1,p)
for(i in 2:M)
{
  #Select variable to change, not includinng intercept
  j = sample(2:p,1)
  if(!gam.sim[j])  #Add component
  {
    beta.prop = beta.sim
    beta.prop[j] = rnorm(1,s[j,1],s[j,2])
    gam.prop = gam.sim
    gam.prop[j] = TRUE
    logpi.prop = logL(beta.prop,gam.prop,y,X)
    r = exp(logpi.prop-logpi.cur-dnorm(beta.prop[j],s[j,1],s[j,2],log=TRUE))*prod(prob.gam^(gam.prop[-1]-gam.sim[-1]))
    #r=-1
    r = exp(logpi.prop-logpi.cur-dnorm(beta.prop[j],s[j,1],s[j,2]))
    #r=-1
    if(runif(1)<r)
    {
      beta.sim = beta.prop
      gam.sim = gam.prop
      logpi.cur = logpi.prop
      accA = accA+1
    }
  }
  else if(gam.sim[j])  #Delete component
  {
    beta.prop = beta.sim
    beta.prop[j] = 0
    gam.prop = gam.sim
    gam.prop[j] = FALSE
    logpi.prop = logL(beta.prop,gam.prop,y,X)
    r = exp(logpi.prop-logpi.cur+dnorm(beta.sim[j],s[j,1],s[j,2],log=TRUE))*prod(prob.gam^(gam.prop[-1]-gam.sim[-1]))
    #r=-1
    r = exp(logpi.prop-logpi.cur+dnorm(beta.sim[j],s[j,1],s[j,2]))
    #r=-1
    if(runif(1)<r)
    {
      beta.sim = beta.prop
      gam.sim = gam.prop
      logpi.cur = logpi.prop
      accD = accD+1
    }
  }
  for(k in 1:M2)  #Move component  
  {
    j = sample(1:p,1)
    beta.prop = beta.sim
    beta.prop[j] = rnorm(1,beta.sim[j],s[j,2])
    logpi.prop = logL(beta.prop,gam.sim,y,X)
    r = exp(100*(logpi.prop-logpi.cur))
    if(runif(1)<r)
    {
      beta.sim = beta.prop
      logpi.cur = logpi.prop
      accM = accM+1
    }
  }
  beta.M[i,] = beta.sim
  gam.M[i,] = gam.sim
  logpi.M[i] = logpi.cur
}
show(colMeans(gam.M))
show(table(gam.M[,2],gam.M[,3])/M)

