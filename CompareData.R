#a.eg<-read.csv('EGtoDC_Data/EGtoDC_Albedo_Historic.csv')
#a.dc<-read.csv('EGtoDC_Data/EGtoDC_Albedo_Modern.csv')
a.chg<-read.csv('EGtoDC_Data/EGtoDC_Albedo_Change.csv')

#st.eg<-read.csv('EGtoDC_Data/EGtoDC_ST_Historic.csv')
#st.dc<-read.csv('EGtoDC_Data/EGtoDC_ST_Modern.csv')
st.chg<-read.csv('EGtoDC_Data/EGtoDC_ST_Change.csv')

transp.chg<-read.csv('pls.fia.transp.diff.csv') #Actually no time to subset this by taxa and make it phenological

##All the plots

###Albedo
ed.alb<--databin.ed[,,1]
jules.alb<--databin.jules[,,1]


par(mfrow=c(1,3))
plot(rowMeans(ed.alb), type='l', ylim=c(-0.07, 0.07), main='ED', lwd=2, ylab="Albedo", xlab="Month")
for(i in 1:ncol(ed.alb)){
  lines(ed.alb[,i], lwd=0.5, col='gray')
  lines(rowMeans(ed.alb), lwd=2)
}
abline(h=0)

#colvec=c("red","orange","yellow","green","blue")
plot(rowMeans(jules.alb[,c(1:3,5)]), type='l', ylim=c(-0.07, 0.07), main='JULES', lwd=2, ylab="Albedo", xlab="Month")
for(i in 1:ncol(jules.alb)){
  if(i !=4){
    lines(jules.alb[,i], lwd=0.5, col='gray')
    lines(rowMeans(jules.alb[,c(1:3,5)]), lwd=2)
  }
}
abline(h=0)

plot(colMeans(a.chg[2:13], na.rm=TRUE), type='l',ylim=c(-0.07, 0.07), main='Data', lwd=2, ylab="Albedo", xlab="Month")
for(i in 1:nrow(a.chg)){#ncol(a.chg)
  lines(as.numeric(a.chg[i,2:13]), lwd=0.5, col='gray')
  lines(colMeans(a.chg[2:13], na.rm=TRUE), lwd=2)
}
abline(h=0)
#rm(list=setdiff(ls(), c("databin.jules","databin.ed","datasubset.jules",
#"datasubset.ed","var.want","varset")))

##AGU abstract metrics

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

##Surface temp
#SB law calcualtions

sb<-5.67e-08
ed.dc.st<-((lw.dc.ed/sb)^(1/4))-273.15
ed.eg.st<-((lw.eg.ed/sb)^(1/4))-273.15
ed.st.chg<-(-(ed.eg.st-ed.dc.st))

jules.dc.st<-((lw.dc.jules/sb)^(1/4))-273.15
jules.eg.st<-((lw.eg.jules/sb)^(1/4))-273.15
jules.st.chg<-(-(jules.eg.st-jules.dc.st))


par(mfrow=c(1,3))
plot(rowMeans(ed.st.chg), type='l', ylim=c(-3, 3), main='ED', lwd=2, ylab="Surface T", xlab="Month")
for(i in 1:ncol(ed.st.chg)){
  lines(ed.st.chg[,i], lwd=0.5, col='gray')
  lines(rowMeans(ed.st.chg), lwd=2)
}
abline(h=0)

#colvec=c("red","orange","yellow","green","blue")
plot(rowMeans(jules.st.chg[,c(1:3,5)]), type='l', ylim=c(-3, 3), main='JULES', lwd=2, ylab="Surface T", xlab="Month")
for(i in 1:ncol(jules.st.chg)){
  if(i !=4){
    lines(jules.st.chg[,i], lwd=0.5, col='gray')
    lines(rowMeans(jules.st.chg[,c(1:3,5)]), lwd=2)
  }
}
abline(h=0)

plot(colMeans(st.chg[2:13], na.rm=TRUE), type='l',ylim=c(-3, 3), main='Data', lwd=2, ylab="Surface T", xlab="Month")
for(i in 1:nrow(st.chg)){#ncol(st.chg)
  lines(as.numeric(st.chg[i,2:13]), lwd=0.5, col='gray')
  lines(colMeans(st.chg[2:13], na.rm=TRUE), lwd=2)
}
abline(h=0)

#AGU metrics

datstmeans<-colMeans(st.chg[2:13], na.rm=TRUE)
edstmeans<- rowMeans(ed.st.chg)
jstmeans<-rowMeans(jules.st.chg)

datstmeans-edstmeans
edstmeans/datstmeans

datstmeans-jstmeans
jstmeans/datstmeans
