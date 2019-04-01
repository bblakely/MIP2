

#run seasonal profile for jules then ed


#a.eg<-read.csv('EGtoDC_Data/EGtoDC_Albedo_Historic.csv')
#a.dc<-read.csv('EGtoDC_Data/EGtoDC_Albedo_Modern.csv')
#a.chg<-read.csv('EGtoDC_Data/EGtoDC_Albedo_Change.csv')
a.chg<-read.csv('EGtoDC_Data/AlbChange.comp.csv'); #a.chg<-a.chg[a.chg$convert.code==-1,]
a.chg.twr<-readRDS("TwrAlb.rds")

#st.eg<-read.csv('EGtoDC_Data/EGtoDC_ST_Historic.csv')
#st.dc<-read.csv('EGtoDC_Data/EGtoDC_ST_Modern.csv')
st.chg<-read.csv('EGtoDC_Data/EGtoDC_ST_Change.csv')
st.chg.twr<-readRDS("TwrST")[,2]
#transp.chg<-read.csv('pls.fia.transp.diff.csv') #Actually no time to subset this by taxa and make it phenological

##All the plots
other<-'FALSE'

par(mfrow=c(1,3), mar=c(4,5.5,4,0.1))

###Albedo####
ed.alb<--databin.ed[,,1]
jules.alb<--databin.jules[,c(1:5, 7:12),1]

bigchg.ed<-which(colMeans(-databin.ed[,,6])>0.2)

ylab.exp<-expression(bold(Delta~alpha))

par(mfrow=c(1,3))
plot(rowMeans(ed.alb), type='l', ylim=c(-0.07, 0.07), main='ED2', lwd=2, 
     ylab=ylab.exp, xlab="Month", cex.axis=1.5, cex.lab=2.5, font.axis=2, font.lab=2, cex.main=2, col='white')

colexp<-rep("dark gray", 22); #colexp[which(colMeans(-databin.ed[summer,,5])>0.25)]<-'black'
for(i in 1:ncol(ed.alb)){
  lines(ed.alb[,i], lwd=1, col='dark gray')
  lines(rowMeans(ed.alb), lwd=4, col='forest green')
  lines(rowMeans(ed.alb[,bigchg.ed]), lwd=3, col='forest green', lty=6)
  if(colMeans(-databin.ed[,,6])[i]>0.20){lines(ed.alb[,i], lwd=2, col='gray45')}
}
abline(h=0)
box(lwd=4)
#legend(0.5, -0.05, legend=c("Tower-measured","Individual shifts", "Mean"), lwd=c(3,1,4), lty=c(2,1,1), col=c('dark red','dark gray', 'blue'), text.font=2, bty='n', cex=1.2)



bigchg.jules<-which(colMeans(-databin.jules[,,6])>0.2)
plot(rowMeans(jules.alb), type='l', ylim=c(-0.07, 0.07), main='JULES', lwd=2, 
     ylab="", xlab="Month", cex.axis=1.5, cex.lab=2.5, font.axis=2, font.lab=2, cex.main=2, col='white')
for(i in 1:ncol(jules.alb)){
  if(i !=4){
    lines(jules.alb[,i], lwd=1, col='dark gray')
    lines(rowMeans(jules.alb), lwd=4, col='orange')
    lines(rowMeans(jules.alb[,bigchg.jules]), lwd=3, col='orange', lty=6)
    if(colMeans(-databin.jules[,c(1:5, 7:12),6])[i]>0.20){lines(jules.alb[,i], lwd=2, col='gray45')}
  }
}
abline(h=0)
box(lwd=4)

a.empir<-colMeans(a.chg[2:13], na.rm=TRUE)

plot(colMeans(a.chg[2:13], na.rm=TRUE), type='l',ylim=c(-0.07, 0.07), main='DATA', lwd=2, 
     ylab="", xlab="Month", cex.axis=1.5, cex.lab=2.5, font.axis=2, font.lab=2, cex.main=2)
for(i in 1:nrow(a.chg)){#ncol(a.chg)
  lines(as.numeric(a.chg[i,2:13]), lwd=1, col='dark gray')
  lines(colMeans(a.chg[2:13], na.rm=TRUE), lwd=4, col='black')
}
lines(x=c(3:11), y=a.chg.twr[3:11], lwd=3, lty=2, col='dark red')
abline(h=0)
box(lwd=4)

legend(2, -0.06, legend= "Tower", lty=2, col='dark red', lwd=2, bty='n', cex=1.2)

dev.copy(png, filename="Figures/AlbCompare.png", width=700, height=350);dev.off()

# #Albedo RF
# par(mfrow=c(1,2), mar=c(4,5,1,0.5))
# albdiff.ed<-colMeans(a.chg[2:13], na.rm=TRUE)-rowMeans(ed.alb) 
# albdiff.jules<-colMeans(a.chg[2:13], na.rm=TRUE)-rowMeans(jules.alb) 
# #plot(albdiff.ed, type='l', main='mismatch')
# 
# ylabel<-expression(bold("RF"~ (Wm^-2)))
# 
# if(!exists('albkern')){source("/Users/bethanyblakely/Desktop/Analysis/Albedo/RadKernel_extract.R")}
# alb.rf.ed<-(albdiff.ed-0.02)*100*albkern
# plot(alb.rf.ed, ylim=c(-5,1), xlab='Month', ylab=ylabel, font=2, cex.lab=1.5,font.axis=2, cex=1.2, font.lab=2);lines(alb.rf.ed, lwd=3)
# box(lwd=3);abline(h=0)
# mean(alb.rf.ed)
# 
# alb.rf.jules<-(albdiff.jules-0.02)*100*albkern
# plot(alb.rf.jules, ylim=c(-5,1), xlab='Month', ylab='', font=2, cex.lab=1.5, font.axis=2, cex=1.2,font.lab=2);lines(alb.rf.jules, lwd=3)
# box(lwd=3);abline(h=0)
# mean(alb.rf.jules)
# 
# dev.copy(png, filename="Figures/AlbRF.png", width=550, height=175);dev.off()

####alternative radiative forcing plot:


#Albedo RF
par(mfrow=c(1,2), mar=c(4,5,3,0.5))
ylabel<-expression(bold("RF"~ (Wm^-2)))
if(!exists('albkern')){source("/Users/bethanyblakely/Desktop/Analysis/Albedo/RadKernel_extract.R")}

albforce.ed<-rowMeans(ed.alb)*100*albkern; albforce.jules<-rowMeans(jules.alb)*100*albkern
albforce.dat<-colMeans(a.chg[2:13], na.rm=TRUE)*100*albkern
albforce.twr<-a.chg.twr[3:11]*100*albkern[3:11]

par(mfrow=c(1,1))
plot(albforce.ed, type='l', ylim=c(-10, 1), col='forest green', lwd=4, ylab=ylabel, xlab="Month", cex.lab=1.4, font.lab=2, cex.axis=1.2); points(albforce.ed)
box(lwd=3)
lines(albforce.jules, col='orange', lwd=4);points(albforce.jules)
lines(albforce.dat, col='black', lwd=4);points(albforce.dat)
lines(x=c(3:11), y=albforce.twr, col="dark red", lwd=4, lty=3);points(x=c(3:11),y=albforce.twr)

legend(1,-7, legend=c("ED2","JULES","Reconstruction", "Tower"), lwd=4, 
       col=c("forest green", "orange", "black", "dark red"), lty=c(1,1,1,3),
       bty='n', cex=1)

dev.copy(png, filename="Figures/RFall.png", width=450, height=360);dev.off()


#ggplot version...



#dev.copy(png, filename='Figures/Albcompare.png', width=550, height=300); dev.off()




if(other=="TRUE"){
####Surface temp####
#SB law calcualtions

sb<-5.67e-08
ed.dc.st<-((lw.dc.ed/sb)^(1/4))-273.15
ed.eg.st<-((lw.eg.ed/sb)^(1/4))-273.15
ed.st.chg<-(ed.dc.st-ed.eg.st)

jules.dc.st<-((lw.dc.jules/sb)^(1/4))-273.15
jules.eg.st<-((lw.eg.jules/sb)^(1/4))-273.15
jules.st.chg<-(jules.dc.st-jules.eg.st)


par(mfrow=c(1,3), mar=c(4,5.5,4,0.1))

ylab.exp<-expression(bold(Delta~Surface~Temperature~(degree*C)))

plot(rowMeans(ed.st.chg), type='l', ylim=c(-3, 3), main='ED2', lwd=2, 
     ylab=ylab.exp, xlab="Month", cex.axis=1.5, cex.lab=2.5, font.axis=2, font.lab=2, cex.main=2)
for(i in 1:ncol(ed.st.chg)){
  lines(ed.st.chg[,i], lwd=0.5, col='gray50')
  lines(rowMeans(ed.st.chg), lwd=4, col='dark red')
}
abline(h=0)
box(lwd=4)

#colvec=c("red","orange","yellow","green","blue")
plot(rowMeans(jules.st.chg), type='l', ylim=c(-3, 3), main='JULES', lwd=2, 
     ylab="", xlab="Month", cex.axis=1.5, cex.lab=2.5, font.axis=2, font.lab=2, cex.main=2)
for(i in 1:ncol(jules.st.chg)){
  if(i !=4){
    lines(jules.st.chg[,i], lwd=0.5, col='gray50')
    lines(rowMeans(jules.st.chg), lwd=4, col='dark red')
  }
}
abline(h=0)
box(lwd=4)

plot(colMeans(st.chg[2:13], na.rm=TRUE), type='l',ylim=c(-3, 3), main='DATA', lwd=2, 
     ylab="", xlab="Month", cex.axis=1.5, cex.lab=2.5, font.axis=2, font.lab=2, cex.main=2)
for(i in 1:nrow(st.chg)){#ncol(st.chg)
  lines(as.numeric(st.chg[i,2:13]), lwd=0.5, col='gray50')
  lines(colMeans(st.chg[2:13], na.rm=TRUE), lwd=4, col='dark red')
}
lines(st.chg.twr, lwd=2, lty=2)
abline(h=0)
box(lwd=4)

#dev.copy(png, filename='Figures/STcompare.png', width=550, height=300); dev.off()

#####

#Transpiration

ed.transp<--databin.ed[,,8]

y.exp<-expression(bold(Delta~Transpiration~(mm~h^-1)))
par(mfrow=c(1,3))
plot(rowMeans(ed.transp*3600, na.rm=TRUE), type='l',ylim=c(-0.05,0.05), main='ED2', lwd=2, 
     ylab=y.exp, xlab="Month", col='blue', cex.axis=1.5, cex.lab=2.5, font.axis=2, font.lab=2, cex.main=2)
for(i in 1:ncol(ed.transp)){
  lines(ed.transp[,i]*3600, lwd=1, col='dark gray')
  lines(rowMeans(ed.transp)*3600, lwd=3, col=' dark slate blue')
}
abline(h=0)
box(lwd=3)


transp.dat<-read.csv("TranspChg_Sap.csv")[2:13]
transp.ind<-read.csv("EGtoDC_ind.csv")[,2]

dat.transp<-transp.dat[transp.ind,]*3600
plot(colMeans(dat.transp, na.rm=TRUE), type='l',ylim=c(-0.05, 0.05), main='DATA', 
     lwd=2, ylab="", xlab="Month", cex.axis=1.5, cex.lab=2.5, font.axis=2, font.lab=2, cex.main=2)
for(i in (1:nrow(dat.transp))){
  lines(as.numeric(dat.transp[i,]), col='dark gray', lwd=0.5)
  lines(colMeans(dat.transp), lwd=3, col='dark slate blue')
}
abline(h=0)
box(lwd=3)
#dev.copy(png, filename='Figures/Transpcompare.png', width=550, height=300); dev.off()
}



