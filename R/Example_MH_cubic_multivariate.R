#Example Metropolis-Hastings
#Target distribution 
f= function(x)
  {
    exp(-sum(x^2)^(3/2)/3)
}
#Proposal distribution: Gaussian distribution centered at previous value
p = 50

N = 10000    # Number of iterations
x = matrix(nrow=N,ncol=p)

#Initial value
x[1,] = rnorm(p)
acc = 0
for(i in 2:N)
{
  j = sample(1:p,1)
  y = x[i-1,]
  y[j] = rnorm(1,x[i-1,j],2)
  R = f(y)*dnorm(x[i-1,j],y[j],1)/(f(x[i-1,])*dnorm(y[j],x[i-1,j],1))
  if(runif(1)<R)
  {
    x[i,] = y
    acc = acc+1
  }
  else
   x[i,] = x[i-1,]
}
pdf("Res_cubic_mult_MH.pdf",height=5,width=12)
par(mfrow=c(1,2))
plot.ts(x[,1])
plot.ts(rowSums(x^2),ylab="Sum(x)")
dev.off()
cat("Acceptance rate=",acc/(N-1),"\n")
