
model<-'ed'
if(model=='jules'){databin<-(-databin.jules)}; if(model=='ed'){databin<-(-databin.ed)} #;databin<-databin[,c(1:3,5),]

allplots=FALSE
if(allplots==TRUE){
#Plot snow change in Jan
par(mfrow=c(1,1)) 
plot(databin[,1,3], col='white', ylim=c(min(databin[,,3]),max(databin[,,3])))
for(i in 1:ncol(databin)){
lines(databin[,i,3])
} 



#Linear models of differences
#outliers
remove.outliers<-FALSE
if(remove.outliers==TRUE){
databin[,which(colMeans(databin[,,3])==boxplot.stats(colMeans(databin[,,3]))$out),3]<-NA
databin[,which(colMeans(databin[,,1])==boxplot.stats(colMeans(databin[,,1]))$out),1]<-NA
}

par(mfrow=c(1,2))

#By season
xlim<-c(-1.5,2.5);ylim<-c(-0.06, 0.1)
par(mfrow=c(2,2))
winter=c(1:3,11:12);summer=c(5:9);

plot(colMeans(databin[winter,,1])~colMeans(databin[winter,,2]), main='lai', col=col, xlim=xlim,ylim=ylim); abline(h=0)
summary(lm(colMeans(databin[winter,,1])~colMeans(databin[winter,,2])))

plot(colMeans(databin[winter,,1])~colMeans(databin[winter,,3]), main='snow', col=col, ylim=ylim); abline(h=0)

summary(lm(colMeans(databin[winter,,1])~colMeans(databin[winter,,3]))); AIC(lm(colMeans(databin[winter,,1])~colMeans(databin[winter,,3])))
summary(lm(colMeans(databin[winter,,1])~colMeans(databin[winter,,3])+colMeans(databin[winter,,2])));AIC(lm(colMeans(databin[winter,,1])~colMeans(databin[winter,,3])+colMeans(databin[winter,,2])))
summary(lm(colMeans(databin[winter,,1])~colMeans(databin[winter,,3])+colMeans(databin[winter,,2])+(colMeans(databin[winter,,3]):colMeans(databin[winter,,2])))); AIC(lm(colMeans(databin[winter,,1])~colMeans(databin[winter,,3])+colMeans(databin[winter,,2])+(colMeans(databin[winter,,3]):colMeans(databin[winter,,2]))))
#coefs<-coefficients(lm(colMeans(databin[winter,,1])~colMeans(databin[winter,,3])+colMeans(databin[winter,,2])+(colMeans(databin[winter,,3]):colMeans(databin[winter,,2]))))

print(paste("mean winter alb:", mean(colMeans(databin[winter,,1]))))

plot(colMeans(databin[summer,,1])~colMeans(databin[summer,,2]), main='lai', col=col,xlim=xlim,ylim=ylim); abline(h=0)
summary(lm(colMeans(databin[summer,,1])~colMeans(databin[summer,,2]))); coefs<-unname(coefficients(lm(colMeans(databin[summer,,1])~colMeans(databin[summer,,2]))))
#abline(coefs); abline(v=0, lty=2);abline(h=0, col='red', lty=2)

#plot(colMeans(databin[summer,,1])~colMeans(databin[summer,,3]), main='snow', col=col, ylim=ylim); abline(h=0)
#summary(lm(colMeans(databin[summer,,1])~colMeans(databin[summer,,3])))


#summary(lm(colMeans(databin[summer,,1])~colMeans(databin[summer,,3])+colMeans(databin[summer,,2])+colMeans(databin[summer,,2])+(colMeans(databin[summer,,3]):colMeans(databin[summer,,2]))))

if(model=='jules'){
  #crazy lm with all months
  
  summary(lm(c(databin.jules[,,1])~c(databin.jules[,,2])))
  summary(lm(c(databin.jules[,,1])~c(databin.jules[,,3])))
  
  summary(lm(c(databin.jules[,,1])~c(databin.jules[,,3])+c(databin.jules[,,2])))
  
  summary(lm(c(databin.jules[,,1])~c(databin.jules[,,3])+c(databin.jules[,,2])+(c(databin.jules[,,3]):c(databin.jules[,,2]))))
}

}

#Final nice sensitivity plots for LAI
library(ggplot2)
library(gridExtra)

winter=c(1:3,11:12);summer=c(5:9);
albexp<-expression(Delta~alpha);laiexp<-expression(Delta~"LAI")

gdat.w<-data.frame(cbind(colMeans(databin[winter,,1], na.rm=TRUE),colMeans(databin[winter,,2]))); colnames(gdat.w)<-c("walb",'wlai')
if(model=='ed'){
p1.ed<-ggplot(gdat.w, aes(x=wlai,y=walb))+geom_point(size=0.4)+stat_smooth(method="lm", col='cyan', fill='cyan')+xlim(-2.5,2.8)+ylim(-0.06,0.075)+theme_bw()+ylab(albexp)+xlab(laiexp)+ggtitle("ED2")+geom_hline(yintercept=0, linetype="dashed", size=0.2)
}
if(model=='jules'){
p1.jules<-ggplot(gdat.w, aes(x=wlai,y=walb))+geom_point(size=0.4)+stat_smooth(method="lm", col='cyan', fill='cyan')+xlim(-2.5,2.8)+ylim(-0.06,0.075)+theme_bw()+ylab(albexp)+xlab(laiexp)+ggtitle("JULES")+geom_hline(yintercept=0, linetype="dashed", size=0.2)
}

gdat.s<-data.frame(cbind(colMeans(databin[summer,,1], na.rm=TRUE),colMeans(databin[summer,,2]))); colnames(gdat.s)<-c("salb",'slai')
if(model=='ed'){
p2.ed<-ggplot(gdat.s, aes(x=slai,y=salb))+geom_point(size=0.4)+stat_smooth(method="lm", col='seagreen', fill='seagreen')+xlim(-2.5,2.8)+ylim(-0.06,0.075)+ theme_bw()+ylab(albexp)+xlab(laiexp)+ggtitle("ED2")+geom_hline(yintercept=0, linetype="dashed", size=0.2)
}
if(model=='jules'){
p2.jules<-ggplot(gdat.s, aes(x=slai,y=salb))+geom_point(size=0.4)+stat_smooth(method="lm", col='seagreen', fill='seagreen')+xlim(-2.5,2.8)+ylim(-0.06,0.075)+ theme_bw()+ylab(albexp)+xlab(laiexp)+ggtitle("JULES")+geom_hline(yintercept=0, linetype="dashed", size=0.2)
}


conversion<-'lax'
if(conversion=='strict'){p1e<-readRDS("p1es.rds");p2e<-readRDS("p2es.rds")}else{p1e<-readRDS("p1e.rds");p2e<-readRDS("p2e.rds")}


if(exists("p2.jules") & exists("p2.ed")){
  grid.arrange(p1.ed,p1.jules,p1e,p2.ed,p2.jules,p2e,nrow=2, ncol=3)
}


#Final nice sensitivity plots for snow
snoexp<-expression(Delta~"SWE")

gdat.ws<-data.frame(cbind(colMeans(databin[winter,,1], na.rm=TRUE),colMeans(databin[winter,,3]))); colnames(gdat.ws)<-c("walb",'wsno')
if(model=='ed'){
  p2s.ed<-ggplot(gdat.ws, aes(x=wsno,y=walb))+geom_point(size=0.4)+stat_smooth(method="lm", col='cyan', fill='cyan')+xlim(-17,5)+ylim(-0.1,0.075)+ theme_bw()+ylab(albexp)+xlab(snoexp)+ggtitle("ED2")+geom_hline(yintercept=0, linetype="dashed", size=0.2)
}
if(model=='jules'){
  p2s.jules<-ggplot(gdat.ws, aes(x=wsno,y=walb))+geom_point(size=0.4)+stat_smooth(method="lm", col='cyan', fill='cyan')+xlim(-17,5)+ylim(-0.1,0.075)+ theme_bw()+ylab(albexp)+xlab(snoexp)+ggtitle("JULES")+geom_hline(yintercept=0, linetype="dashed", size=0.2)
}

if(exists("p2s.jules") & exists("p2s.ed")){
  grid.arrange(p2s.ed,p2s.jules, ncol=2)
}


#reporting numbers
#model means
mean(colMeans(databin[winter,,1])); mean(apply(databin[winter,,1], 1, "sd"))
mean(colMeans(databin[c(1:2, 12),,1])) #DJF
mean(colMeans(databin[summer,,1])); mean(apply(databin[summer,,1], 1, "sd"))
åmean(colMeans(databin[c(6:8),,1]))#JJA

#variances

apply(databin[,,1], 1, "sd")
