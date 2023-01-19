#Model:
#x_t=ax_{t-1}+eps
#y_t~Binomial(exp(x_t)/[1+exp(x_t)])
#Prior: a~N(0,1)
sig2=1;sig=sqrt(sig2)
sig2.a=1;sig.a=sqrt(sig2.a)

d = read.table("../data/lemmings.txt",header=TRUE)
y = d$LemmingYear
nT = length(y)
par(mfrow=c(2,1),mar=c(2,2,2,2))

N = 100000

x.sim = matrix(nrow=nT,ncol=N)
x.hat = matrix(nrow=nT,ncol=3)


set.seed(345)
a=0.9
source("SMC_lin_bin_fixpar.R")

set.seed(345)
source("SMC_lin_bin_parest_direct.R")

set.seed(345)
source("SMC_lin_bin_parest_dyn.R")

set.seed(345)
source("SMC_lin_bin_parest_suff.R")

d1 = density(a.sim.dir)
d2 = density(a.sim.dyn)
d3 = density(a.sim.suff)
par(mfrow=c(1,1))
matplot(cbind(d1$x,d2$x,d3$x),cbind(d1$y,d2$y,d3$y),type="l",lty=1,col=1:3)
legend("topleft",c("Direct","Dynamic","Suff-stat"),lty=1,col=1:3)