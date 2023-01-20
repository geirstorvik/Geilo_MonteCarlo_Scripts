#Reversible jump for mixture model
#M1: z~N(0,1)
#M2: z~N((0,0),(1,-0.9,-0.9,1))
library(mvtnorm)
theta = 0
N = 100
z = rnorm(N)
pi.hat.cur = 
Sigma = matrix(c(1,-0.9,-0.9,1),ncol=2)
M = 10000
theta.M = rep(NA,M)

for(i in 1:M)
{
 
   acc = 0
   if(theta==0)
   {
    u1 = rnorm(1,-0.9*z[1],sqrt(1-0.9^2))
    z[2] = u1
    r = dmvnorm(z,sigma=Sigma)*3/(dnorm(z[1],0,1)*dnorm(u1,0.9*z[1],sqrt(1-0.9^2)))
    if(runif(1)<r)
    {
     acc = 1
    }
   }
   if(theta==1)
   {
    u1 = z[2]
    r = dnorm(z[1],0,1)*dnorm(u1,0.9*z[1],sqrt(1-0.9^2))/(3*dmvnorm(z,sigma=Sigma))
    if(runif(1)<r)
    {
      acc = 1
    }
   }
   if(acc)
    theta = 1-theta
   if(theta==1)
    z[1] = rnorm(1)
   if(theta==2)
    z = rmvnorm(1,sigma=Sigma)
   theta.M[i] = theta
}