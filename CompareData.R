

#run seasonal profile for jules then ed


#a.eg<-read.csv('EGtoDC_Data/EGtoDC_Albedo_Historic.csv')
#a.dc<-read.csv('EGtoDC_Data/EGtoDC_Albedo_Modern.csv')
#a.chg<-read.csv('EGtoDC_Data/EGtoDC_Albedo_Change.csv')
a.chg<-read.csv('EGtoDC_Data/AlbChange.comp.csv')
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
jules.alb<--databin.jules[,,1]

ylab.exp<-expression(bold(Delta~alpha))

par(mfrow=c(1,3))
plot(rowMeans(ed.alb), type='l', ylim=c(-0.09, 0.09), main='ED2', lwd=2, 
     ylab=ylab.exp, xlab="Month", cex.axis=1.5, cex.lab=2.5, font.axis=2, font.lab=2, cex.main=2)
for(i in 1:ncol(ed.alb)){
  lines(ed.alb[,i], lwd=1, col='dark gray')
  lines(rowMeans(ed.alb), lwd=4, col='blue')
}
abline(h=0)
box(lwd=4)
legend(0.5, -0.05, legend=c("Individual shifts", "Mean"), lwd=c(1,3), col=c('dark gray', 'blue'), text.font=2, bty='n')

#colvec=c("red","orange","yellow","green","blue")
plot(rowMeans(jules.alb), type='l', ylim=c(-0.09, 0.09), main='JULES', lwd=2, 
     ylab="", xlab="Month", cex.axis=1.5, cex.lab=2.5, font.axis=2, font.lab=2, cex.main=2)
for(i in 1:ncol(jules.alb)){
  if(i !=4){
    lines(jules.alb[,i], lwd=1, col='dark gray')
    lines(rowMeans(jules.alb), lwd=4, col='blue')
  }
}
abline(h=0)
box(lwd=4)


plot(colMeans(a.chg[2:13], na.rm=TRUE), type='l',ylim=c(-0.09, 0.09), main='DATA', lwd=2, 
     ylab="", xlab="Month", cex.axis=1.5, cex.lab=2.5, font.axis=2, font.lab=2, cex.main=2)
for(i in 1:nrow(a.chg)){#ncol(a.chg)
  lines(as.numeric(a.chg[i,2:13]), lwd=1, col='dark gray')
  lines(colMeans(a.chg[2:13], na.rm=TRUE), lwd=4, col='blue')
}
lines(a.chg.twr, lwd=2, lty=2)
abline(h=0)
box(lwd=4)

#Albedo RF
par(mfrow=c(1,2), mar=c(4,5,1,0.5))
albdiff.ed<-colMeans(a.chg[2:13], na.rm=TRUE)-rowMeans(ed.alb) 
albdiff.jules<-colMeans(a.chg[2:13], na.rm=TRUE)-rowMeans(jules.alb) 
#plot(albdiff.ed, type='l', main='mismatch')

ylabel<-expression(bold("RF"~ (Wm^-2)))

if(!exists('albkern')){source("/Users/bethanyblakely/Desktop/Analysis/Albedo/RadKernel_extract.R")}
alb.rf.ed<-albdiff.ed*100*albkern
plot(alb.rf.ed, ylim=c(-15,0), xlab='Month', ylab=ylabel, font=2, cex.lab=1.5,font.axis=2, cex=1.2, font.lab=2);lines(alb.rf.ed, lwd=3)
box(lwd=3)
mean(alb.rf.ed)

alb.rf.jules<-albdiff.jules*100*albkern
plot(alb.rf.jules, ylim=c(-15,0), xlab='Month', ylab='', font=2, cex.lab=1.5, font.axis=2, cex=1.2,font.lab=2);lines(alb.rf.jules, lwd=3)
box(lwd=3)
mean(alb.rf.jules)

#ggplot version...



#dev.copy(png, filename='Figures/Albcompare.png', width=550, height=300); dev.off()


#rm(list=setdiff(ls(), c("databin.jules","databin.ed","datasubset.jules",
#"datasubset.ed","var.want","varset")))
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



