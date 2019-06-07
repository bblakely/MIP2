all.beta<-extract.beta(startyear.set=1500, endyear.set=1600)

all.beta.precip<-all.beta[,,6]; all.beta.mo.p<-as.matrix(aggregate(all.beta.precip, by=list(all.beta[,1,9]), FUN=mean)[2:236])
all.beta.temp<-all.beta[,,5]; all.beta.mo.t<-as.matrix(aggregate(all.beta.temp, by=list(all.beta[,1,9]), FUN=mean)[2:236])
all.beta.lai<-all.beta[,,2]; all.beta.mo.l<-as.matrix(aggregate(all.beta.lai, by=list(all.beta[,1,9]), FUN=mean)[2:236])


par(mfcol=c(3,2))
hist(colMeans(all.beta.mo.p), xlim=c(0,0.00005), main='allarea.p')
hist(colMeans(eg.databin[,,5]),xlim=c(0,0.00005), main='egphase.p')
hist(colMeans(dc.databin[,,5]),xlim=c(0,0.00005), main='dcphase.p')


hist(colMeans(all.beta.mo.t), xlim=c(265,295), main='allarea.t')
hist(colMeans(eg.databin[,,6]), xlim=c(265,295), main='egphase.t')
hist(colMeans(dc.databin[,,6]), xlim=c(265,295), main='dcphase.t')


hist(colMeans(all.beta.mo.l), xlim=c(0,9), main='allarea.lai_year')
hist(colMeans(eg.databin[,,2]), xlim=c(0,9), main='egphase.lai_year')
hist(colMeans(dc.databin[,,2]), xlim=c(0,9), main='dcphase.lai_year')

hist(colMeans(all.beta.mo.l[6:8,]), xlim=c(0,10), main='allarea.lai_gs')
hist(colMeans(eg.databin[6:8,,2]), xlim=c(0,10), main='egphase.lai_gs')
hist(colMeans(dc.databin[6:8,,2]), xlim=c(0,10), main='dcphase.lai_gs')


#Change during shift
par(mfrow=c(1,2))
data.pr<-colMeans(databin[,,5]);data.t<-colMeans(databin[,,6]);data.s<-colMeans(databin[,,3])
hist(data.pr, main=paste(round(mean(data.pr),8),"(+/-)",round(sd(data.pr), 8)));abline(v=0, col='red')
hist(data.t,main=paste(round(mean(data.t),3),"(+/-)",round(sd(data.t), 3)));abline(v=0, col='red')
#yep, pretty much none.

#hist(data.s,main=paste(round(mean(data.s),3),"+/-",round(sd(data.s), 3)));abline(v=0, col='red')


