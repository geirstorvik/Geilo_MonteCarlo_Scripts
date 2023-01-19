y = c(115,  70,  14,  14, 153,  23,  25,  15,  28,  58,  21)
#y2 = c(845,  331,   76,   47, 1105,  126,  186,   58,  156,  258,   82)
pop = c(693494,479892,265238,241235,1241165,371385,419396,307231,636531,468702,243311)
a=1
alpha=10;beta=(alpha-1)*0.0002/a
#Gibbs sampling
b=1000
M=1000
theta.M = matrix(nrow=M,ncol=11)
b.M = rep(NA,M)
for(i in 1:M)
{
 theta = rgamma(11,a+y,b+pop)
 b = rgamma(1,alpha+11,beta+sum(theta))
 theta.M[i,] = theta
 b.M[i] = b
}

pdf("Res_Covid_theta.pdf",height=8,width=12)
plot.ts(theta.M,plot.type="single",col=1:11)
dev.off()
pdf("Res_Covid_b.pdf",height=5,width=12)
par(mfrow=c(1,2))
plot.ts(b.M)
acf(b.M)
dev.off()
