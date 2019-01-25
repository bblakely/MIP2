
model<-'ed'
if(model=='jules'){databin<-databin.jules;databin<-databin[,c(1:3,5),]}; if(model=='ed'){databin<-databin.ed}


#Plot snow change in Jan
par(mfrow=c(1,1))
plot(databin[,1,3], col='white', ylim=c(min(databin[,,3]),max(databin[,,3])))
for(i in 1:ncol(databin)){
lines(databin[,i,3])
}
databin<-(-databin)



par(mfrow=c(1,3))
sds<-apply(databin[,,3],1,FUN='sd')
plot(rowMeans(databin[,,3]), type='l', ylim=c(min(rowMeans(databin[,,3])-sds),max(rowMeans(databin[,,3])+sds)))

lines(rowMeans(databin[,,3])+sds, col='red')
lines(rowMeans(databin[,,3])-sds, col='red')
abline(h=0)


sds<-apply(databin[,,2],1,FUN='sd')
plot(rowMeans(databin[,,2]), type='l', ylim=c(min(rowMeans(databin[,,2])-sds),max(rowMeans(databin[,,2])+sds)))

lines(rowMeans(databin[,,2])+sds, col='red')
lines(rowMeans(databin[,,2])-sds, col='red')
abline(h=0)


sds<-apply(databin[,,1],1,FUN='sd')
plot(rowMeans(databin[,,1]), type='l', ylim=c(min(rowMeans(databin[,,1])-sds),max(rowMeans(databin[,,1])+sds)))

lines(rowMeans(databin[,,1])+sds, col='red')
lines(rowMeans(databin[,,1])-sds, col='red')
abline(h=0)

#Individual pixels
for(j in 1:ncol(databin)){
  par(mfrow=c(1,3))
  
  plot((databin[,j,3]), type='l', ylim=c(min(databin[,,3]),max(databin[,,3])), main=j)
  abline(h=0)
  
  plot((databin[,j,2]), type='l', ylim=c(min(databin[,,2]),max(databin[,,2])))
  abline(h=0)
  
  plot((databin[,j,1]), type='l', ylim=c(min(databin[,,1]),max(databin[,,1])))
  abline(h=0)
}


#####Separate by increases, decreases, no change in ####
inc<-which(colMeans(databin[,,1])>.002)
dec<-which(colMeans(databin[,,1])<(-.002))
nc<-which(abs(colMeans(databin[,,1]))<.002)

#increase in albedo
par(mfrow=c(1,3))
sds<-apply(databin[,inc,3],1,FUN='sd')
plot(rowMeans(databin[,inc,3]), type='l', ylim=c(-0.1,0.1))
lines(rowMeans(databin[,inc,3])+sds, col='red')
lines(rowMeans(databin[,inc,3])-sds, col='red')
abline(h=0)

sds<-apply(databin[,inc,2],1,FUN='sd')
plot(rowMeans(databin[,inc,2]), type='l', ylim=c(-2,2))
lines(rowMeans(databin[,inc,2])+sds, col='red')
lines(rowMeans(databin[,inc,2])-sds, col='red')
abline(h=0)

sds<-apply(databin[,inc,1],1,FUN='sd')
plot(rowMeans(databin[,inc,1]), type='l', ylim=c(-0.05,0.05))
lines(rowMeans(databin[,inc,1])+sds, col='red')
lines(rowMeans(databin[,inc,1])-sds, col='red')
abline(h=0)

#decrease in albedo
par(mfrow=c(1,3))
sds<-apply(databin[,dec,3],1,FUN='sd')
plot(rowMeans(databin[,dec,3]), type='l', ylim=c(-0.1,0.1))
lines(rowMeans(databin[,dec,3])+sds, col='red')
lines(rowMeans(databin[,dec,3])-sds, col='red')
abline(h=0)

sds<-apply(databin[,dec,2],1,FUN='sd')
plot(rowMeans(databin[,dec,2]), type='l', ylim=c(-2,2))
lines(rowMeans(databin[,dec,2])+sds, col='red')
lines(rowMeans(databin[,dec,2])-sds, col='red')
abline(h=0)

sds<-apply(databin[,dec,1],1,FUN='sd')
plot(rowMeans(databin[,dec,1]), type='l', ylim=c(-0.05,0.05))
lines(rowMeans(databin[,dec,1])+sds, col='red')
lines(rowMeans(databin[,dec,1])-sds, col='red')
abline(h=0)
#####

col='black'
  #c('red','orange','yellow','green','blue')

#Linear models of differences
#outliers
remove.outliers<-FALSE
if(remove.outliers==TRUE){
databin[,which(colMeans(databin[,,3])==boxplot.stats(colMeans(databin[,,3]))$out),3]<-NA
databin[,which(colMeans(databin[,,1])==boxplot.stats(colMeans(databin[,,1]))$out),1]<-NA
}

par(mfrow=c(1,2))
plot(colMeans(databin[,,1])~colMeans(databin[,,2]), main='lai', col=col)
summary(lm(colMeans(databin[,,1])~colMeans(databin[,,2])))

plot(colMeans(databin[,,1])~colMeans(databin[,,3]), main='snow', col=col)
summary(lm(colMeans(databin[,,1])~colMeans(databin[,,3])))


summary(lm(colMeans(databin[,,1])~colMeans(databin[,,3])+colMeans(databin[,,2])+(colMeans(databin[,,3]):colMeans(databin[,,2]))))
summary(lm(colMeans(databin[,,1])~colMeans(databin[,,3])+colMeans(databin[,,2])))


#By season
winter=c(1:3,11:12);summer=c(5:9);

par(mfrow=c(1,2))
plot(colMeans(databin[winter,,1])~colMeans(databin[winter,,2]), main='lai', col=col)
summary(lm(colMeans(databin[winter,,1])~colMeans(databin[winter,,2])))

plot(colMeans(databin[winter,,1])~colMeans(databin[winter,,3]), main='snow', col=col)
summary(lm(colMeans(databin[winter,,1])~colMeans(databin[winter,,3])))
summary(lm(colMeans(databin[winter,,1])~colMeans(databin[winter,,3])+colMeans(databin[winter,,2])+(colMeans(databin[winter,,3]):colMeans(databin[winter,,2]))))
#coefs<-coefficients(lm(colMeans(databin[winter,,1])~colMeans(databin[winter,,3])+colMeans(databin[winter,,2])+(colMeans(databin[winter,,3]):colMeans(databin[winter,,2]))))


par(mfrow=c(1,2))
plot(colMeans(databin[summer,,1])~colMeans(databin[summer,,2]), main='lai', col=col)
summary(lm(colMeans(databin[summer,,1])~colMeans(databin[summer,,2]))); coefs<-unname(coefficients(lm(colMeans(databin[summer,,1])~colMeans(databin[summer,,2]))))
abline(coefs); abline(v=0, lty=2);abline(h=0, col='red', lty=2)

plot(colMeans(databin[summer,,1])~colMeans(databin[summer,,3]), main='snow', col=col)
summary(lm(colMeans(databin[summer,,1])~colMeans(databin[summer,,3])))


summary(lm(colMeans(databin[summer,,1])~colMeans(databin[summer,,3])+colMeans(databin[summer,,2])+colMeans(databin[summer,,2])+(colMeans(databin[summer,,3]):colMeans(databin[summer,,2]))))

if(model=='jules'){
#crazy lm with all months

summary(lm(c(databin.jules[,,1])~c(databin.jules[,,2])))
summary(lm(c(databin.jules[,,1])~c(databin.jules[,,3])))

summary(lm(c(databin.jules[,,1])~c(databin.jules[,,3])+c(databin.jules[,,2])))

summary(lm(c(databin.jules[,,1])~c(databin.jules[,,3])+c(databin.jules[,,2])+(c(databin.jules[,,3]):c(databin.jules[,,2]))))
}


