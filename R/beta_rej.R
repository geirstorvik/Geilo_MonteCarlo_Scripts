#Simulate from beta distribution with rejection sampling
alpha = 2;beta=3
x.opt = (alpha-1)/(alpha+beta-2)
f.opt = dbeta(x.opt,alpha,beta)
x = seq(0,1,length=100)
plot(x,dbeta(x,alpha,beta),type="l")
abline(h=f.opt,col=2)

n = 10000
y = runif(n)
u = runif(n)
dev.copy2pdf(file="../doc/beta_rej.pdf")
acc = u < dbeta(y,alpha,beta)/f.opt
x.sim = y[acc]
hist(x.sim,freq=F)
lines(x,dbeta(x,alpha,beta),col=2)
cat("Acceptance, true and estimated:",1/f.opt,mean(acc))
