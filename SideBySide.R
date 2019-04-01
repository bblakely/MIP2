#Combined figure...

library(ggplot2)
library(gridExtra)

LAIramp<-colorRampPalette(c("orange4","antiquewhite", "forest green"))
Snowramp<-colorRampPalette(c("orchid", "cyan"))
soilramp<-colorRampPalette(c("tan", "black"))

winter=c(1:3,11:12);summer=c(5:9);
albexp<-expression(Delta~alpha);laiexp<-expression(Delta~"LAI")
###ED2###
databin<-(-databin.ed)
gdat<-data.frame(cbind(colMeans(databin[winter,,1], na.rm=TRUE),colMeans(databin[winter,,2]),colMeans(databin[summer,,1], na.rm=TRUE),colMeans(databin[summer,,2]))); colnames(gdat)<-c("walb",'wlai', "salb","slai")
toy<-cbind(gdat$slai, colMeans(databin[,,1], na.rm=TRUE), seq(1:length(gdat$slai)))
toy2<-toy[order(toy[,1]),];colgrad<-LAIramp(nrow(toy2))
toy3<-cbind.data.frame(toy2, colgrad);toy4<-toy3[order(toy3[,3]),]
colscheme.ed<-as.character(toy4$colgrad)

p1.ed<-ggplot(gdat, aes(x=wlai,y=walb))+geom_point(size=4, col=colscheme.ed)+stat_smooth(method="lm", col='dark gray', fill='gray')+xlim(-2.5,2.8)+ylim(-0.06,0.075)+theme_bw()+ylab(albexp)+xlab(laiexp)+ggtitle("Winter")+geom_hline(yintercept=0, linetype="dashed", size=0.2)+scale_color_gradientn(colours = rainbow(nrow(gdat)))
#p1.ed

p2.ed<-ggplot(gdat, aes(x=slai,y=salb, color=slai))+geom_point(size=4, col=colscheme.ed)+stat_smooth(method="lm", col='dark gray', fill='gray')+xlim(-2.5,2.8)+ylim(-0.06,0.075)+theme_bw()+ylab(albexp)+xlab(laiexp)+ggtitle("Summer")+geom_hline(yintercept=0, linetype="dashed", size=0.2)+scale_color_gradientn(colours = rainbow(nrow(gdat)))
#p2.ed

grid.arrange(p1.ed,p2.ed,nrow=2, ncol=1)
dev.copy(png, "Figures/ED_LAI.png", width=370, height=500);dev.off()


ed.alb<--databin.ed[,,1]
jules.alb<--databin.jules[,c(1:5, 7:12),1]

ylab.exp<-expression(bold(Delta~alpha))

par(mfrow=c(1,1))
plot(rowMeans(ed.alb), type='l', ylim=c(-0.07, 0.07), main='ED2', lwd=2, 
     ylab=ylab.exp, xlab="Month", cex.axis=1.5, cex.lab=2.5, font.axis=2, font.lab=2, cex.main=2)


for(i in 1:ncol(ed.alb)){
  lines(ed.alb[,i], lwd=2, col=colscheme.ed[i])
  lines(rowMeans(ed.alb), lwd=4, col='black')
  #if(colMeans(databin[,,6])[i]>0.23){lines(ed.alb[,i],col='red', lwd=4)}
}
abline(h=0)
box(lwd=4)

mean(colMeans(databin[,,1])[which(colMeans(databin[,,6])>0.2)])

dev.copy(png, "Figures/ED_LAIlines.png", width=450, height=600);dev.off()

##Snow
snoexp<-expression(Delta~"SWE")
gdat.ws<-data.frame(cbind(colMeans(databin[winter,,1], na.rm=TRUE),colMeans(databin[winter,,3]))); colnames(gdat.ws)<-c("walb",'wsno')

toy<-cbind(gdat.ws$wsno, colMeans(databin[,,1], na.rm=TRUE), seq(1:length(gdat.ws$wsno)))
toy2<-toy[order(toy[,1]),];colgrad<-Snowramp(nrow(toy2))
toy3<-cbind.data.frame(toy2, colgrad);toy4<-toy3[order(toy3[,3]),]
colscheme.ed.s<-as.character(toy4$colgrad)

p2s.ed<-ggplot(gdat.ws, aes(x=wsno,y=walb))+geom_point(size=4, col=colscheme.ed.s)+xlim(-30,15)+ylim(-0.07,0.075)+ theme_bw()+ylab(albexp)+xlab(snoexp)+ggtitle("Snow")+geom_hline(yintercept=0, linetype="dashed", size=0.2)
p2s.ed
dev.copy(png, "Figures/ED_Snow.png", width=370, height=500);dev.off()

plot(rowMeans(ed.alb), type='l', ylim=c(-0.07, 0.07), main='ED2', lwd=2, 
     ylab=ylab.exp, xlab="Month", cex.axis=1.5, cex.lab=2.5, font.axis=2, font.lab=2, cex.main=2)

for(i in 1:ncol(ed.alb)){
  lines(ed.alb[,i], lwd=2, col=colscheme.ed.s[i])
  lines(rowMeans(ed.alb), lwd=4, col='black')
}
abline(h=0)
box(lwd=4)
dev.copy(png, "Figures/ED_Snowlines.png", width=450, height=600);dev.off()




###JULES####
par(mfrow=c(1,1))
databin<-(-databin.jules[,c(1:5, 7:12),])
gdat<-data.frame(cbind(colMeans(databin[winter,,1], na.rm=TRUE),colMeans(databin[winter,,2]),colMeans(databin[summer,,1], na.rm=TRUE),colMeans(databin[summer,,2]))); colnames(gdat)<-c("walb",'wlai', "salb","slai")

toy<-cbind(gdat$slai, colMeans(databin[,,1], na.rm=TRUE), seq(1:length(gdat$slai)))
toy2<-toy[order(toy[,1]),];colgrad<-LAIramp(nrow(toy2))
toy3<-cbind.data.frame(toy2, colgrad);toy4<-toy3[order(toy3[,3]),]
colscheme.jules<-as.character(toy4$colgrad)

p1.jules<-ggplot(gdat, aes(x=wlai,y=walb))+geom_point(size=4, col=colscheme.jules)+xlim(-2.5,2.8)+ylim(-0.06,0.075)+theme_bw()+ylab(albexp)+xlab(laiexp)+ggtitle("Winter")+geom_hline(yintercept=0, linetype="dashed", size=0.2)+scale_color_gradientn(colours = rainbow(nrow(gdat)))
#p1.jules

p2.jules<-ggplot(gdat, aes(x=slai,y=salb))+geom_point(size=4, col=colscheme.jules)+xlim(-2.5,2.8)+ylim(-0.06,0.075)+theme_bw()+ylab(albexp)+xlab(laiexp)+ggtitle("Summer")+geom_hline(yintercept=0, linetype="dashed", size=0.2)+scale_color_gradientn(colours = rainbow(nrow(gdat)))
#p2.jules

grid.arrange(p1.jules,p2.jules,nrow=2, ncol=1)
dev.copy(png, "Figures/JULES_LAI.png", width=370, height=500);dev.off()

#colvec=c("red","orange","yellow","green","blue")
plot(rowMeans(jules.alb), type='l', ylim=c(-0.07, 0.07), main='JULES', lwd=2, 
     ylab="", xlab="Month", cex.axis=1.5, cex.lab=2.5, font.axis=2, font.lab=2, cex.main=2)


for(i in 1:ncol(jules.alb)){
  #if(i !=4){
    lines(jules.alb[,i], lwd=2, col=colscheme.jules[i])
    lines(rowMeans(jules.alb), lwd=4, col='black')
  #}
}
abline(h=0)
box(lwd=4)
dev.copy(png, "Figures/JULES_LAIlines.png", width=450, height=600);dev.off()

#Snow
gdat.ws<-data.frame(cbind(colMeans(databin[winter,,1], na.rm=TRUE),colMeans(databin[winter,,3]))); colnames(gdat.ws)<-c("walb",'wsno')

toy<-cbind(gdat.ws$wsno, colMeans(databin[,,1], na.rm=TRUE), seq(1:length(gdat.ws$wsno)))
toy2<-toy[order(toy[,1]),];colgrad<-Snowramp(nrow(toy2))
toy3<-cbind.data.frame(toy2, colgrad);toy4<-toy3[order(toy3[,3]),]
colscheme.jules.s<-as.character(toy4$colgrad)

p2s.jules<-ggplot(gdat.ws, aes(x=wsno,y=walb))+geom_point(size=4, col=colscheme.jules.s)+stat_smooth(method="lm", col='dark gray', fill='gray')+xlim(-30,15)+ylim(-0.07,0.075)+ theme_bw()+ylab(albexp)+xlab(snoexp)+ggtitle("Snow")+geom_hline(yintercept=0, linetype="dashed", size=0.2)
p2s.jules
dev.copy(png, "Figures/JULES_Snow.png", width=370, height=500);dev.off()


plot(rowMeans(jules.alb), type='l', ylim=c(-0.07, 0.07), main='JULES', lwd=2, 
     ylab="", xlab="Month", cex.axis=1.5, cex.lab=2.5, font.axis=2, font.lab=2, cex.main=2)
for(i in 1:ncol(jules.alb)){
  #if(i !=4){
  lines(jules.alb[,i], lwd=2, col=colscheme.jules.s[i])
  lines(rowMeans(jules.alb), lwd=4, col='black')
  #}
}
abline(h=0)
box(lwd=4)

dev.copy(png, "Figures/JULES_Snowlines.png", width=450, height=600);dev.off()

#soil moisture
gdat.sm<-data.frame(cbind(colMeans(databin[summer,,1], na.rm=TRUE),colMeans(databin[summer,,4]))); colnames(gdat.sm)<-c("walb",'wsno')

toy<-cbind(gdat.sm$wsno, colMeans(databin[,,1], na.rm=TRUE), seq(1:length(gdat.sm$wsno)))
toy2<-toy[order(toy[,1]),];colgrad<-rev(cm.colors(nrow(toy2)))
toy3<-cbind.data.frame(toy2, colgrad);toy4<-toy3[order(toy3[,3]),]
colscheme.jules.sm<-as.character(toy4$colgrad)

p2sm.jules<-ggplot(gdat.sm, aes(x=wsno,y=walb))+geom_point(size=4, col=colscheme.jules.sm)+ylim(-0.07,0.075)+theme_bw()+ylab(albexp)+xlab(snoexp)+ggtitle("JULES")+geom_hline(yintercept=0, linetype="dashed", size=0.2)
p2sm.jules

####magnitude of change

ed.chg<-(colMeans(databin.ed[,,5]))
jules.chg<-(colMeans(databin.jules[,c(1:5, 7:12),5]))

boxplot(ed.chg, jules.chg)
