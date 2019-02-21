model<-'ed'

#ST calculations
sb<-5.67e-08
ed.dc.st<-((lw.dc.ed/sb)^(1/4))-273.15
ed.eg.st<-((lw.eg.ed/sb)^(1/4))-273.15
ed.st.chg<-(ed.dc.st-ed.eg.st) 

jules.dc.st<-((lw.dc.jules/sb)^(1/4))-273.15
jules.eg.st<-((lw.eg.jules/sb)^(1/4))-273.15
jules.st.chg<-(jules.dc.st-jules.eg.st)


#BR
ed.br.chg<-(br.dc.ed-br.eg.ed);ed.br.chg[abs(ed.br.chg)>5]<-NA
jules.br.chg<-(br.dc.jules-br.eg.jules)

#net shortwave
ed.swn.chg<-swn.dc.ed-swn.eg.ed
jules.swn.chg<-swn.dc.jules-swn.eg.jules


if(model=='jules'){databin<-(databin.jules);dat.st.chg<-jules.st.chg;dat.swn.chg<-jules.swn.chg}
if(model=='ed'){databin<-(databin.ed);dat.st.chg<-ed.st.chg;dat.swn.chg<-ed.swn.chg}

####Exploratory plots#####
par(mfrow=c(2,2), mar=c(4,4,4,4))
summer<-c(5:9)
for(i in 1:8){
  plot(colMeans(-databin[summer,,i])~colMeans(dat.st.chg[summer,]), main=var.want[i])
  
}

par(mfrow=c(2,2))
winter<-c(1:3,11:12)
for(i in 1:8){
  plot(colMeans(-databin[winter,,i])~colMeans(dat.st.chg[winter,]), main=var.want[i])
  
}
#####

library('corrplot')

#SWnet<-databin[,,which(var.want=="swdown")]*(1-databin[,,which(var.want=="SW_albedo")])
Netrad<-dat.swn.chg
  #(-databin[,,which(var.want=="LWnet")])+dat.swn.chg

var.plot<-c("SW_albedo", "LAI","Qh","Qle")

par(mfrow=c(1,1))
var.mat<-cor(matrix(cbind(colMeans(-databin)[,which(var.want%in%var.plot)],colMeans(Netrad), colMeans(dat.st.chg)), nrow=ncol(dat.st.chg), ncol=length(which(var.want%in%var.plot))+2))
colnames(var.mat)<-c(var.want[var.want%in%var.plot], "SWnet","ST");rownames(var.mat)<-c(var.want[var.want%in%var.plot], "Rnet","ST")
corrplot(var.mat, main='all year', method='shade',addCoef.col='black', number.cex=0.5,tl.cex=0.6, type = 'upper')

par(mfrow=c(1,2))
var.mat<-cor(matrix(cbind(colMeans(-databin[summer,,which(var.want%in%var.plot)]),colMeans(Netrad[summer,]),colMeans(dat.st.chg[summer,])), nrow=ncol(dat.st.chg), ncol=length(which(var.want%in%var.plot))+2))
colnames(var.mat)<-c(var.want[var.want%in%var.plot], "SWnet","ST");rownames(var.mat)<-c(var.want[var.want%in%var.plot], "Rnet","ST")
corrplot(var.mat, main='summer', method='shade',addCoef.col='black', number.cex=0.5,tl.cex=0.6, type = 'upper')


var.mat<-cor(matrix(cbind(colMeans(-databin[winter,,which(var.want%in%var.plot)]),colMeans(Netrad[winter,]),colMeans(dat.st.chg[winter,])), nrow=ncol(dat.st.chg), ncol=length(which(var.want%in%var.plot))+2))
colnames(var.mat)<-c(var.want[var.want%in%var.plot], "SWnet","ST");rownames(var.mat)<-c(var.want[var.want%in%var.plot], "Rnet","ST")
corrplot(var.mat, main='winter', method='shade',addCoef.col='black', number.cex=0.5,tl.cex=0.6, type = 'upper')


####LMs####
#All
summary(lm(colMeans(dat.st.chg[winter,], na.rm=TRUE)~colMeans(Netrad[winter,])+colMeans(-databin[winter,,which(var.want=="Qh")])+colMeans(-databin[winter,,which(var.want=="Qle")])))
summary(lm(colMeans(dat.st.chg[summer,], na.rm=TRUE)~colMeans(Netrad[summer,])+colMeans(-databin[summer,,which(var.want=="Qh")])+colMeans(-databin[summer,,which(var.want=="Qle")])))
#with interaction..
summary(lm(colMeans(dat.st.chg[winter,], na.rm=TRUE)~
             colMeans(Netrad[winter,])+colMeans(-databin[winter,,which(var.want=="Qh")])+colMeans(-databin[winter,,which(var.want=="Qle")])+
             colMeans(-databin[winter,,which(var.want=="Qh")]):colMeans(-databin[winter,,which(var.want=="Qle")])))

summary(lm(colMeans(dat.st.chg[summer,], na.rm=TRUE)~
             colMeans(Netrad[summer,])+colMeans(-databin[summer,,which(var.want=="Qh")])+colMeans(-databin[summer,,which(var.want=="Qle")])+
             colMeans(-databin[summer,,which(var.want=="Qh")]):colMeans(-databin[summer,,which(var.want=="Qle")])))

#rad only
summary(lm(colMeans(dat.st.chg[winter,], na.rm=TRUE)~colMeans(Netrad[winter,])))
summary(lm(colMeans(dat.st.chg[summer,], na.rm=TRUE)~colMeans(Netrad[summer,])))

#Qh/Qle only
summary(lm(colMeans(dat.st.chg[winter,], na.rm=TRUE)~colMeans(-databin[winter,,which(var.want=="Qh")])+colMeans(-databin[winter,,which(var.want=="Qle")])))
summary(lm(colMeans(dat.st.chg[summer,], na.rm=TRUE)~colMeans(-databin[summer,,which(var.want=="Qh")])+colMeans(-databin[summer,,which(var.want=="Qle")])))

#with interaction...
summary(lm(colMeans(dat.st.chg[winter,], na.rm=TRUE)~colMeans(-databin[winter,,which(var.want=="Qh")])*colMeans(-databin[winter,,which(var.want=="Qle")])))
summary(lm(colMeans(dat.st.chg[summer,], na.rm=TRUE)~colMeans(-databin[summer,,which(var.want=="Qh")])*colMeans(-databin[summer,,which(var.want=="Qle")])))
#####


#Nice sensitivity plots
library(ggplot2)
library(gridExtra)

winter=c(1:3,11:12);summer=c(5:9);
stexp<-expression(Delta~"LST");radexp<-expression(Delta~"SWnet");lexp<-expression(Delta~"Qle");hexp<-expression(Delta~"Qh")


#Sensitivity plots for Rnet
gdat.w<-data.frame(cbind(colMeans(dat.st.chg[winter,], na.rm=TRUE),colMeans(Netrad[winter,]))); colnames(gdat.w)<-c("wst",'wrad')
if(model=='ed'){
  p1.ed<-ggplot(gdat.w, aes(x=wrad,y=wst))+geom_point(size=0.4)+stat_smooth(method="lm", col='orange', fill='orange')+xlim(-16,13)+ylim(-2,3)+theme_bw()+ylab(stexp)+xlab(radexp)+ggtitle("ED2")+geom_hline(yintercept=0, linetype="dashed", size=0.2)
}
if(model=='jules'){
  p1.jules<-ggplot(gdat.w, aes(x=wrad,y=wst))+geom_point(size=0.4)+stat_smooth(method="lm", col='orange', fill='orange')+xlim(-16,13)+ylim(-2,3)+theme_bw()+ylab(stexp)+xlab(radexp)+ggtitle("JULES")+geom_hline(yintercept=0, linetype="dashed", size=0.2)
}


gdat.s<-data.frame(cbind(colMeans(dat.st.chg[summer,], na.rm=TRUE),colMeans(Netrad[summer,]))); colnames(gdat.s)<-c("wst",'wrad')
if(model=='ed'){
  p2.ed<-ggplot(gdat.s, aes(x=wrad,y=wst))+geom_point(size=0.4)+stat_smooth(method="lm", col='orange', fill='orange')+xlim(-16,13)+ylim(-2,3)+theme_bw()+ylab(stexp)+xlab(radexp)+ggtitle("ED2")+geom_hline(yintercept=0, linetype="dashed", size=0.2)
}
if(model=='jules'){
  p2.jules<-ggplot(gdat.s, aes(x=wrad,y=wst))+geom_point(size=0.4)+stat_smooth(method="lm", col='orange', fill='orange')+xlim(-16,13)+ylim(-2,3)+theme_bw()+ylab(stexp)+xlab(radexp)+ggtitle("JULES")+geom_hline(yintercept=0, linetype="dashed", size=0.2)
}


if(exists("p2.jules") & exists("p2.ed")){
  grid.arrange(p1.ed,p1.jules,p2.ed,p2.jules,nrow=2, ncol=2)
}


# #Sensitivity plots for LE
# 
# gdat.w<-data.frame(cbind(colMeans(dat.st.chg[winter,], na.rm=TRUE),colMeans(-databin[winter,,which(var.want=="Qle")]))); colnames(gdat.w)<-c("wst",'wle')
# if(model=='ed'){
#   q1.ed<-ggplot(gdat.w, aes(x=wle,y=wst))+geom_point(size=0.4)+stat_smooth(method="lm", col='blue', fill='blue')+xlim(-22,26)+ylim(-2,3)+theme_bw()+ylab(stexp)+xlab(lexp)+ggtitle("ED2")+geom_hline(yintercept=0, linetype="dashed", size=0.2)
# }
# if(model=='jules'){
#   q1.jules<-ggplot(gdat.w, aes(x=wle,y=wst))+geom_point(size=0.4)+stat_smooth(method="lm", col='blue', fill='blue')+xlim(-22,26)+ylim(-2,3)+theme_bw()+ylab(stexp)+xlab(lexp)+ggtitle("JULES")+geom_hline(yintercept=0, linetype="dashed", size=0.2)
# }
# 
# 
# gdat.s<-data.frame(cbind(colMeans(dat.st.chg[summer,], na.rm=TRUE),colMeans(-databin[summer,,which(var.want=="Qle")]))); colnames(gdat.s)<-c("wst",'wle')
# if(model=='ed'){
#   q2.ed<-ggplot(gdat.s, aes(x=wle,y=wst))+geom_point(size=0.4)+stat_smooth(method="lm", col='blue', fill='blue')+xlim(-22,26)+ylim(-2,3)+theme_bw()+ylab(stexp)+xlab(lexp)+ggtitle("ED2")+geom_hline(yintercept=0, linetype="dashed", size=0.2)
# }
# if(model=='jules'){
#   q2.jules<-ggplot(gdat.s, aes(x=wle,y=wst))+geom_point(size=0.4)+stat_smooth(method="lm", col='blue', fill='blue')+xlim(-22,26)+ylim(-2,3)+theme_bw()+ylab(stexp)+xlab(lexp)+ggtitle("JULES")+geom_hline(yintercept=0, linetype="dashed", size=0.2)
# }
# 
# if(exists("q2.jules") & exists("q2.ed")){
#   grid.arrange(q1.ed,q1.jules,q2.ed,q2.jules,nrow=2, ncol=2)
# }
# 
# 
# #Sensitivity plots for H
# 
# gdat.w<-data.frame(cbind(colMeans(dat.st.chg[winter,], na.rm=TRUE),colMeans(-databin[winter,,which(var.want=="Qh")]))); colnames(gdat.w)<-c("wst",'wle')
# if(model=='ed'){
#   r1.ed<-ggplot(gdat.w, aes(x=wle,y=wst))+geom_point(size=0.4)+stat_smooth(method="lm", col='dark red', fill='dark red')+xlim(-22,26)+ylim(-2,3)+theme_bw()+ylab(stexp)+xlab(hexp)+ggtitle("ED2")+geom_hline(yintercept=0, linetype="dashed", size=0.2)
# }
# if(model=='jules'){
#   r1.jules<-ggplot(gdat.w, aes(x=wle,y=wst))+geom_point(size=0.4)+stat_smooth(method="lm", col='dark red', fill='dark red')+xlim(-22,26)+ylim(-2,3)+theme_bw()+ylab(stexp)+xlab(hexp)+ggtitle("JULES")+geom_hline(yintercept=0, linetype="dashed", size=0.2)
# }
# 
# 
# gdat.s<-data.frame(cbind(colMeans(dat.st.chg[summer,], na.rm=TRUE),colMeans(-databin[summer,,which(var.want=="Qh")]))); colnames(gdat.s)<-c("wst",'wle')
# if(model=='ed'){
#   r2.ed<-ggplot(gdat.s, aes(x=wle,y=wst))+geom_point(size=0.4)+stat_smooth(method="lm", col='dark red', fill='dark red')+xlim(-22,26)+ylim(-2,3)+theme_bw()+ylab(stexp)+xlab(hexp)+ggtitle("ED2")+geom_hline(yintercept=0, linetype="dashed", size=0.2)
# }
# if(model=='jules'){
#   r2.jules<-ggplot(gdat.s, aes(x=wle,y=wst))+geom_point(size=0.4)+stat_smooth(method="lm", col='dark red', fill='dark red')+xlim(-22,26)+ylim(-2,3)+theme_bw()+ylab(stexp)+xlab(hexp)+ggtitle("JULES")+geom_hline(yintercept=0, linetype="dashed", size=0.2)
# }
# 
# if(exists("r2.jules") & exists("r2.ed")){
#   grid.arrange(r1.ed,r1.jules,r2.ed,r2.jules,nrow=2, ncol=2)
# }
# 
# 
