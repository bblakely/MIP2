DC_emp<-read.csv('BinnedDeciduous_Albedo_fix.csv')
EG_emp<-read.csv('BinnedEvergreen_Albedo_fix.csv')


plot(colMeans(DC_emp[4:49], na.rm=TRUE), ylim=c(0.05,0.25))
plot(colMeans(EG_emp[4:49], na.rm=TRUE), ylim=c(0.05,0.25))


                  
                  
weighted.mean(DC_emp$LAI,DC_emp$Counts )
weighted.mean(EG_emp$LAI,EG_emp$Counts )

EG.LAIcount<-aggregate(EG_emp$Counts, by=list(EG_emp$LAI), FUN=sum)
DC.LAIcount<-aggregate(DC_emp$Counts, by=list(DC_emp$LAI), FUN=sum)

EG.LAInorm<-EG.LAIcount$x/max(EG.LAIcount$x);DC.LAInorm<-DC.LAIcount$x/max(DC.LAIcount$x)

plot(EG.LAInorm~EG.LAIcount$Group.1)
plot(DC.LAInorm~DC.LAIcount$Group.1)

#EG.emp.lai<-aggregate(EG_emp, by=list(EG_emp$LAI), FUN=mean, na.rm=TRUE);EG.emp.lai$Counts<-EG.LAIcount$x


par(mfrow=c(1,2))
laihold_dc<-rep(0, 46);snohold_dc<-rep(0,46)
for (i in 1:46){
  laihold_dc[i]<-mean(DC_emp$LAI[!is.nan(DC_emp[,i+3])])
  snohold_dc[i]<-mean(DC_emp$Snow[!is.nan(DC_emp[,i+3])])
}


laihold_eg<-rep(0, 46);snohold_eg<-rep(0,46)
for (i in 1:46){
  laihold_eg[i]<-mean(EG_emp$LAI[!is.nan(EG_emp[,i+3])])
  snohold_eg[i]<-mean(EG_emp$Snow[!is.nan(EG_emp[,i+3])])
}

plot(laihold_dc, ylim=c(0,4))
plot(laihold_eg, ylim=c(0,4))

chglai<-laihold_dc-laihold_eg
chgsno<-snohold_dc-snohold_eg





gs<-c(4:49)

par(mfrow=c(2,2))
coeflist_dc<-rep(0,45)
for(i in (gs)){
  plot(DC_emp[,i]~DC_emp$LAI, main=paste('DC',i-3), ylim=c(0.05, 0.25))
  coeflist_dc[i]<-coefficients(lm(DC_emp[,i]~DC_emp$LAI))[2]
}

par(mfrow=c(2,2))
coeflist_eg<-rep(0,45)
for(i in (gs)){
  plot(EG_emp[,i]~EG_emp$LAI, main=paste('EG',i-3), ylim=c(0.05, 0.25))
  coeflist_eg[i]<-coefficients(lm(EG_emp[,i]~EG_emp$LAI))[2]
}


par(mfrow=c(1,2))
plot(coeflist_dc, main='dc', ylim=c(-0.05, 0.01))
abline(h=0)
plot(coeflist_eg, main='eg', ylim=c(-0.05, 0.01))
abline(h=0)



DC_nosnow<-DC_emp[DC_emp$Snow==0,]
EG_nosnow<-EG_emp[EG_emp$Snow==0,]


par(mfrow=c(2,2))
gs<-c(25:35)
plot(rowMeans(DC_nosnow[,gs+3],na.rm=TRUE )~DC_nosnow$LAI, xlim=c(0,7), ylim=c(0.05,0.15))
abline(v=1.3)
plot(rowMeans(EG_nosnow[,gs+3],na.rm=TRUE )~EG_nosnow$LAI, xlim=c(0,7), ylim=c(0.05,0.15))
abline(v=2)

#gs<-c(1:46)
#plot(as.numeric(rowMeans(DC_nosnow[,gs+3],na.rm=TRUE )-rowMeans(EG_nosnow[,gs+3],na.rm=TRUE )))

singl<-unique(DC_emp$Snow)[7]
DC_snow<-DC_emp[DC_emp$Snow==singl,]
EG_snow<-EG_emp[EG_emp$Snow==singl,]

DC_snow<-DC_emp[DC_emp$Snow>0.5,]
EG_snow<-EG_emp[EG_emp$Snow>0.5,]

gs<-c(1:19, 39:46)
plot(rowMeans(DC_snow[,gs+3],na.rm=TRUE )~DC_snow$LAI, xlim=c(0,7), ylim=c(0.10,0.45))

plot(rowMeans(EG_snow[,gs+3],na.rm=TRUE )~EG_snow$LAI, xlim=c(0,7), ylim=c(0.10,0.45))





#Differences
xlim<-c(-1.5,2.5);ylim<-c(-0.06, 0.1)
par(mfrow=c(2,2));

#winter, LAI
gs<-c(1:12, 39:46)
plot(colMeans(DC_snow[,gs+3],na.rm=TRUE)-colMeans(EG_snow[,gs+3], na.rm=TRUE)~chglai[gs],ylim=ylim, main='lai', ylab="alb difference"); abline(h=0)
#winter,snow
gs<-c(1:12, 39:46)
plot(colMeans(DC_snow[,gs+3],na.rm=TRUE)-colMeans(EG_snow[,gs+3], na.rm=TRUE)~chgsno[gs],ylim=ylim, main='snow', ylab="alb difference"); abline(h=0)


#Summer, LAI
gs<-c(16:35)
plot(colMeans(DC_nosnow[,gs+3],na.rm=TRUE)-colMeans(EG_nosnow[,gs+3], na.rm=TRUE)~chglai[gs], xlim=xlim,ylim=ylim, main='lai', ylab="alb difference"); abline(h=0)
#Summer, Snow
gs<-c(16:35)
plot(colMeans(DC_emp[,gs+3],na.rm=TRUE)-colMeans(EG_emp[,gs+3], na.rm=TRUE)~chgsno[gs], xlim=xlim,ylim=ylim, main='snow', ylab="alb difference"); abline(h=0)

# #Differences, locations as points
# par(mfrow=c(2,2));
# xlim<-c(-1.5,2.5);ylim<-c(-0.06, 0.15)
# gs<-c(1:46)
# plot((rowMeans(DC_snow[,gs+3],na.rm=TRUE)-rowMeans(EG_snow[,gs+3],na.rm=TRUE))~DC_snow$LAI, ylim=ylim, xlim=xlim); abline(h=0)
# plot((rowMeans(DC_snow[,gs+3],na.rm=TRUE)-rowMeans(EG_snow[,gs+3],na.rm=TRUE))~DC_snow$Snow, ylim=ylim); abline(h=0)
# 
# 
# plot((rowMeans(DC_nosnow[,gs+3],na.rm=TRUE)-rowMeans(EG_nosnow[,gs+3],na.rm=TRUE))~DC_nosnow$LAI, ylim=ylim); abline(h=0)
# plot((rowMeans(DC_nosnow[,gs+3],na.rm=TRUE)-rowMeans(EG_nosnow[,gs+3],na.rm=TRUE))~DC_nosnow$Snow, ylim=ylim); abline(h=0)

