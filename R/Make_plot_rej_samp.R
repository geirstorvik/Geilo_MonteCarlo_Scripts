x = seq(0,1,length=100)
y1 = dbeta(x,2,3)
y2 = 1-abs(x-0.5)
m = max(y1/y2)
df = data.frame(a=c(x,x),
                val=c(y1,y2*m),
                gr = c(rep("1",100),rep("2",100)))

pdf("rej_samp.pdf",height=5,width=10)
ggplot(df) +geom_line(aes(a,val,group=gr,color=gr))+
 geom_polygon(data=df[1:100,],aes(x=x,y=val,fill="red")) + guides(fill="none") 
dev.off()