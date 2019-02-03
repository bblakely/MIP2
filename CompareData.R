if(!exists('databin.jules')){source('Pull_data_JULES.R')}
if(!exists('databin.ed')){source('Pull_data_ED.R')}

#run seasonal profile for jules then ed


#a.eg<-read.csv('EGtoDC_Data/EGtoDC_Albedo_Historic.csv')
#a.dc<-read.csv('EGtoDC_Data/EGtoDC_Albedo_Modern.csv')
a.chg<-read.csv('EGtoDC_Data/EGtoDC_Albedo_Change.csv')

#st.eg<-read.csv('EGtoDC_Data/EGtoDC_ST_Historic.csv')
#st.dc<-read.csv('EGtoDC_Data/EGtoDC_ST_Modern.csv')
st.chg<-read.csv('EGtoDC_Data/EGtoDC_ST_Change.csv')

#transp.chg<-read.csv('pls.fia.transp.diff.csv') #Actually no time to subset this by taxa and make it phenological

##All the plots

par(mfrow=c(1,3), mar=c(4,5.5,4,0.1))

###Albedo####
ed.alb<--databin.ed[,,1]
jules.alb<--databin.jules[,,1]

ylab.exp<-expression(bold(Delta~Albedo))

par(mfrow=c(1,3))
plot(rowMeans(ed.alb), type='l', ylim=c(-0.07, 0.07), main='ED2', lwd=2, 
     ylab=ylab.exp, xlab="Month", cex.axis=1.5, cex.lab=2.5, font.axis=2, font.lab=2, cex.main=2)
for(i in 1:ncol(ed.alb)){
  lines(ed.alb[,i], lwd=1, col='dark gray')
  lines(rowMeans(ed.alb), lwd=4, col='blue')
}
abline(h=0)
box(lwd=4)

#colvec=c("red","orange","yellow","green","blue")
plot(rowMeans(jules.alb[,c(1:3,5)]), type='l', ylim=c(-0.07, 0.07), main='JULES', lwd=2, 
     ylab="", xlab="Month", cex.axis=1.5, cex.lab=2.5, font.axis=2, font.lab=2, cex.main=2)
for(i in 1:ncol(jules.alb)){
  if(i !=4){
    lines(jules.alb[,i], lwd=1, col='dark gray')
    lines(rowMeans(jules.alb[,c(1:3,5)]), lwd=4, col='blue')
  }
}
abline(h=0)
box(lwd=4)

plot(colMeans(a.chg[2:13], na.rm=TRUE), type='l',ylim=c(-0.07, 0.07), main='DATA', lwd=2, 
     ylab="", xlab="Month", cex.axis=1.5, cex.lab=2.5, font.axis=2, font.lab=2, cex.main=2)
for(i in 1:nrow(a.chg)){#ncol(a.chg)
  lines(as.numeric(a.chg[i,2:13]), lwd=1, col='dark gray')
  lines(colMeans(a.chg[2:13], na.rm=TRUE), lwd=4, col='blue')
}
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

####AGU abstract metrics####

colMeans(a.chg[2:13], na.rm=TRUE)->datmeans
rowMeans(ed.alb)->edmeans
rowMeans(ed.alb[,lai.symb$X1>45])->edmeans.n
rowMeans(jules.alb)->jmeans

plot(datmeans, ylim=c(-0.07,0.07), type='l')
lines(edmeans, col='red');lines(edmeans.n, col='blue')
abline(h=0)

ed.alb.high<-ed.alb[,which(ed.alb[6,]>0.02)]
brightsumm<-which(colMeans(ed.alb[5:10,])>quantile(colMeans(ed.alb[5:10,]), 0.8))
lightsumm<-which(colMeans(ed.alb[5:10,])>0)
darksumm<-which(colMeans(ed.alb[5:10,])<quantile(colMeans(ed.alb[5:10,]), 0.1))

(datmeans-edmeans)/edmeans # 3 - 16.5x higher in data; 
1-((datmeans-edmeans)/datmeans) #alternatively, 5-60% captured in model; most in the 5-10 range

datmeans-jmeans

(datmeans-jmeans)/jmeans #generally less than 3x higher in data, except for jan/feb where data is WAY higher
1-((datmeans-jmeans)/datmeans) #generally 30-100% of data change captured by model; Jan/feb and november are exceptions
#####
####Surface temp####
#SB law calcualtions

sb<-5.67e-08
ed.dc.st<-((lw.dc.ed/sb)^(1/4))-273.15
ed.eg.st<-((lw.eg.ed/sb)^(1/4))-273.15
ed.st.chg<-(-(ed.eg.st-ed.dc.st))

jules.dc.st<-((lw.dc.jules/sb)^(1/4))-273.15
jules.eg.st<-((lw.eg.jules/sb)^(1/4))-273.15
jules.st.chg<-(-(jules.eg.st-jules.dc.st))


par(mfrow=c(1,3))

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
plot(rowMeans(jules.st.chg[,c(1:3,5)]), type='l', ylim=c(-3, 3), main='JULES', lwd=2, 
     ylab="", xlab="Month", cex.axis=1.5, cex.lab=2.5, font.axis=2, font.lab=2, cex.main=2)
for(i in 1:ncol(jules.st.chg)){
  if(i !=4){
    lines(jules.st.chg[,i], lwd=0.5, col='gray50')
    lines(rowMeans(jules.st.chg[,c(1:3,5)]), lwd=4, col='dark red')
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
abline(h=0)
box(lwd=4)

#dev.copy(png, filename='Figures/STcompare.png', width=550, height=300); dev.off()

#####
####AGU metrics####

datstmeans<-colMeans(st.chg[2:13], na.rm=TRUE)
edstmeans<- rowMeans(ed.st.chg)
jstmeans<-rowMeans(jules.st.chg)

datstmeans-edstmeans
edstmeans/datstmeans

datstmeans-jstmeans
jstmeans/datstmeans
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




