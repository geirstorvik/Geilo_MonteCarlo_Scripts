#Simulate from Ising model
set.seed=1234
n = 30
#Include borders around grid, put borders to zero 
x = matrix(0,nrow=n+2,ncol=n+2)
#Initialize
x[1+1:n,1+1:n] = 2*rbinom(n*n,1,0.5)-1
M = 100000
beta = 0.7
ss = sum(x[-1,]==x[-(n+2),]) + sum(x[,-1]==x[,-(n+2)])
for(it in 1:M)
{
 i = sample(1:n,1)
 j = sample(1:n,1)
 n1 = (x[i+1,j+2]==1)+(x[i+1,j]==1)+(x[i+2,j]==1)+(x[i,j]==1)
 n2 = (x[i+1,j+2]== -1)+(x[i+1,j]== -1)+(x[i+2,j+1]== -1)+(x[i,j+1]== -1)
 p = exp(beta*n1)/(exp(beta*n1)+exp(beta*n2))
 x[i+1,j+1] = 2*rbinom(1,1,p)-1
 ss = c(ss,sum(x[-1,]==x[-(n+2),]) + sum(x[,-1]==x[,-(n+2)]))
}
pdf("Ising_res.pdf",height=5,width=10)
par(mfrow=c(1,2))
image(x)
plot.ts(ss)
dev.off()