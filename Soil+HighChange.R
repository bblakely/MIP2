#Soil moisture analyses
soilramp<-colorRampPalette(c("tan", "black"))
soilexp<-expression(Delta~"Soil Moisture")
#ED2

databin<-(-databin.ed)
gdat.sm<-data.frame(cbind(colMeans(databin[summer,,1], na.rm=TRUE),colMeans(databin[summer,,4]))); colnames(gdat.sm)<-c("walb",'wsno')

toy<-cbind(gdat.sm$wsno, colMeans(databin[,,1], na.rm=TRUE), seq(1:length(gdat.sm$wsno)))
toy2<-toy[order(toy[,1]),];colgrad<-soilramp(nrow(toy2))
toy3<-cbind.data.frame(toy2, colgrad);toy4<-toy3[order(toy3[,3]),]
colscheme.ed.sm<-as.character(toy4$colgrad)

p2sm.ed.s<-ggplot(gdat.sm, aes(x=wsno,y=walb))+geom_point(size=4, col=colscheme.ed.sm)+ylim(-0.07,0.075)+stat_smooth(method="lm", col='dark gray', fill='gray')+theme_bw()+ylab(albexp)+xlab(soilexp)+ggtitle("Summer")+geom_hline(yintercept=0, linetype="dashed", size=0.2)
p2sm.ed.s
summary(lm(gdat.sm$walb~gdat.sm$wsno))

gdat.sm<-data.frame(cbind(colMeans(databin[winter,,1], na.rm=TRUE),colMeans(databin[winter,,4]))); colnames(gdat.sm)<-c("walb",'wsno')
p2sm.ed.w<-ggplot(gdat.sm, aes(x=wsno,y=walb))+geom_point(size=4, col=colscheme.ed.sm)+ylim(-0.07,0.075)+theme_bw()+ylab(albexp)+xlab(soilexp)+ggtitle("Winter")+geom_hline(yintercept=0, linetype="dashed", size=0.2)
p2sm.ed.w
summary(lm(gdat.sm$walb~gdat.sm$wsno))
grid.arrange(p2sm.ed.w,p2sm.ed.s,nrow=2, ncol=1)
dev.copy(png, "Figures/ED_Soil.png", width=370, height=500);dev.off()

par(mfrow=c(1,1))
plot(rowMeans(ed.alb), type='l', ylim=c(-0.07, 0.07), main='ED2', lwd=2, 
     ylab=ylab.exp, xlab="Month", cex.axis=1.5, cex.lab=2.5, font.axis=2, font.lab=2, cex.main=2)

for(i in 1:ncol(ed.alb)){
  lines(ed.alb[,i], lwd=2, col=colscheme.ed.sm[i])
  lines(rowMeans(ed.alb), lwd=4, col='black')
}
abline(h=0)
box(lwd=4)

dev.copy(png, "Figures/ED_Soillines.png", width=450, height=600);dev.off()


par(mfrow=c(2,1))
beer.closed<-exp(1)^-closed*0.6
plot(beer.closed[closed>1]~closed[closed>1])
#points(beer.closed[closed<1]~closed[closed<1], col='red')

gdat.ls<-data.frame(cbind(colMeans(databin[summer,,2], na.rm=TRUE),colMeans(databin[summer,,4]))); colnames(gdat.ls)<-c("walb",'wsno')
ggplot(gdat.ls, aes(x=wsno,y=walb))+geom_point(size=4, col=colscheme.ed.sm)+stat_smooth(method="lm", col='dark gray', fill='gray')+theme_bw()+ylab(laiexp)+xlab(soilexp)+ggtitle("Summer")+geom_hline(yintercept=0, linetype="dashed", size=0.2)
dev.copy(png, "Figures/ED_SoilLeaf.png", width=450, height=300);dev.off()
summary(lm(colMeans(databin[summer,,2])~colMeans(databin[summer,,4])))
summary(lm(colMeans(databin[summer,,1])~colMeans(databin[summer,,2])+colMeans(databin[summer,,4])))

abline(h=0);abline(v=0)


##JULES soil moisture
#soil moisture
databin<-(-databin.jules[,c(1:5, 7:12),])
gdat.sm<-data.frame(cbind(colMeans(databin[summer,,1], na.rm=TRUE),colMeans(databin[summer,,4]))); colnames(gdat.sm)<-c("walb",'wsno')

toy<-cbind(gdat.sm$wsno, colMeans(databin[,,1], na.rm=TRUE), seq(1:length(gdat.sm$wsno)))
toy2<-toy[order(toy[,1]),];colgrad<-soilramp(nrow(toy2))
toy3<-cbind.data.frame(toy2, colgrad);toy4<-toy3[order(toy3[,3]),]
colscheme.jules.sm<-as.character(toy4$colgrad)

p2sm.jules.s<-ggplot(gdat.sm, aes(x=wsno,y=walb))+geom_point(size=4, col=colscheme.jules.sm)+ylim(-0.07,0.075)+theme_bw()+ylab(albexp)+xlab(soilexp)+ggtitle("Summer")+geom_hline(yintercept=0, linetype="dashed", size=0.2)
#p2sm.jules.s
summary(lm(gdat.sm$walb~gdat.sm$wsno))

gdat.sm<-data.frame(cbind(colMeans(databin[winter,,1], na.rm=TRUE),colMeans(databin[winter,,4]))); colnames(gdat.sm)<-c("walb",'wsno')
p2sm.jules.w<-ggplot(gdat.sm, aes(x=wsno,y=walb))+geom_point(size=4, col=colscheme.jules.sm)+ylim(-0.07,0.075)+stat_smooth(method="lm", col='dark gray', fill='gray')+theme_bw()+ylab(albexp)+xlab(soilexp)+ggtitle("Winter")+geom_hline(yintercept=0, linetype="dashed", size=0.2)
#p2sm.jules.w
summary(lm(gdat.sm$walb~gdat.sm$wsno))
grid.arrange(p2sm.jules.w,p2sm.jules.s,nrow=2, ncol=1)
dev.copy(png, "Figures/JULES_Soil.png", width=370, height=500);dev.off()

par(mfrow=c(1,1))
plot(rowMeans(jules.alb), type='l', ylim=c(-0.07, 0.07), main='JULES', lwd=2, 
     ylab=ylab.exp, xlab="Month", cex.axis=1.5, cex.lab=2.5, font.axis=2, font.lab=2, cex.main=2)

for(i in 1:ncol(jules.alb)){
  lines(jules.alb[,i], lwd=2, col=colscheme.jules.sm[i])
  lines(rowMeans(jules.alb), lwd=4, col='black')
}
abline(h=0)
box(lwd=4)

dev.copy(png, "Figures/JULES_Soillines.png", width=450, height=600);dev.off()

par(mfrow=c(2,1))
beer.closed<-exp(1)^-closed*0.6
plot(closed~beer.closed,xlim=c(0,0.4))
points(closed[closed<1]~beer.closed[closed<1], col='red')

plot(colMeans(databin[winter,,2], na.rm=TRUE)~colMeans(databin[winter,,4], na.rm=TRUE))
abline(h=0);abline(v=0)



plot(colMeans(databin[winter,,3])~colMeans(databin[winter,,4]))
AIC(lm(colMeans(databin[winter,,1])~colMeans(databin[winter,,3])+colMeans(databin[winter,,2])))        



#Ed high shift
databin<-(-databin.ed)

bigchg<-which(colMeans(databin[,,6])>0.2)

ed.alb<--databin.ed[,,1]

ylab.exp<-expression(bold(Delta~alpha))

par(mfrow=c(1,1))
plot(rowMeans(ed.alb), type='l', ylim=c(-0.07, 0.07), main='ED2', lwd=2, 
     ylab=ylab.exp, xlab="Month", cex.axis=1.5, cex.lab=2.5, font.axis=2, font.lab=2, cex.main=2)


for(i in 1:ncol(ed.alb)){
  lines(ed.alb[,i], lwd=1, col='light gray')
  lines(rowMeans(ed.alb[,bigchg]), lwd=4, col='black')
  if(colMeans(databin[,,6])[i]>0.20){lines(ed.alb[,i],col=colscheme.ed[i], lwd=4)}
}
abline(h=0)
box(lwd=4)

mean(rowMeans(ed.alb[,bigchg])-a.empir)

#Jules high shift

databin<-(-databin.jules[,c(1:5, 7:12),])
bigchg<-which(colMeans(databin[,,6])>0.2)

plot(rowMeans(jules.alb), type='l', ylim=c(-0.07, 0.07), main='JULES', lwd=2, 
     ylab="", xlab="Month", cex.axis=1.5, cex.lab=2.5, font.axis=2, font.lab=2, cex.main=2)


for(i in 1:ncol(jules.alb)){
  #if(i !=4){
  lines(jules.alb[,i], lwd=1, col='light gray')
  lines(rowMeans(jules.alb[,bigchg]), lwd=4, col='black')
  if(colMeans(databin[,,6])[i]>0.20){lines(jules.alb[,i],col=colscheme.jules[i], lwd=4)}
  #}
}
abline(h=0)
box(lwd=4)





