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

x = seq(0,1,length=100)
y1 = dbeta(x,2,3)
y2 = rep(1,100)
m = max(y1/y2)
df = data.frame(a=c(x,x),
                val=c(y1,y2*m),
                gr = c(rep("1",100),rep("2",100)))

pdf("rej_samp2.pdf",height=5,width=12)
ggplot(df) +geom_line(aes(a,val,group=gr,color=gr),size=2)+
 geom_polygon(data=df[1:100,],aes(x=x,y=val,fill="red")) + guides(fill="none") 
dev.off()
