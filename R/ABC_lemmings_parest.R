#Model:
#x_t=ax_{t-1}+eps
#y_t~Binomial(exp(x_t)/[1+exp(x_t)])
#a~Unif[0,1]
#Inference based on fixed parameters
library(DescTools)
#rm(list=ls())
sig2=1;sig=sqrt(sig2)
sig2.a=1;sig.a=sqrt(sig2.a)
d = read.table("../data/lemmings.txt",header=TRUE)
names(d) = c("y","year")
y = d$y
nT = length(y)
S = function(y)
{
  dy = diff(y)
  dy[is.na(dy)] = 0
  c(mean(dy==-1),mean(dy==1))
}
logit = function(x){log(x/(1-x))}
logitinv = function(x){1/(1+exp(-x))}

sim.y = function(a,y)
{
  n = length(y)
  x.sim = arima.sim(list(order=c(1,0,0),ar=a),n=n)
  p.sim = 1/(1+exp(-x.sim))
  y.sim = rbinom(n,1,p.sim)
  y.sim[is.na(y)] = NA
  S(y.sim)
}

N = 100000;eps=0.001
S.obs = S(y)
a.sim = NULL
for(i in 1:N)
{
  a =  runif(1)
  S.sim = sim.y(a,y)
  if(sum((S.obs-S.sim)^2)<eps)
    a.sim = c(a.sim,a)
}

fit.stan = readRDS("Lemmings_stan.RDS")
library(rstan)
a.sim.stan = extract(fit.stan,pars="a")$a
a.sim.abc = a.sim

df = data.frame(a=c(a.sim.stan,a.sim.abc),
                val=c(rep("stan",length(a.sim.stan)),rep("abc",length(a.sim.abc))))

pdf("lemmings_stan_abc.pdf",height=5,width=10)
ggplot(df, aes(x=a, fill=val)) + geom_density(alpha=.25)
dev.off()

