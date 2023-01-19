#Example Metropolis-Hastings
#Target distribution 
f= function(x)
  {
    exp(-abs(x)^3/3)
}
#Proposal distribution: Gaussian distribution centered at previous value


N = 10000    # Number of iterations
x = rep(NA,N)

#Initial value
x = rnorm(1)
acc = 0
for(i in 2:N)
{
  y = rnorm(1,x[i-1],4)
  #R = f(y)*dnorm(x[i-1],y,1)/(f(x[i-1])*dnorm(y,x[i-1],1))
  R = f(y)/f(x[i-1])
  if(runif(1)<R)
  {
    x[i] = y
    acc = acc+1
  }
  else
   x[i] = x[i-1]
}
par(mfrow=c(3,1),mar=rep(2,4))
plot.ts(x,type="s")
hist(x,100,prob=T)
y = seq(min(x),max(x),length=100)
#Approximate normalizing constant numerically
k = sum(f(y)*(y[2]-y[1]))
lines(y,f(y)/k,col=2)
cat("Acceptance rate=",acc/(N-1),"\n")
acf(x)
