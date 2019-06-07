
extract.beta<-function(startyear.set=1700, endyear.set=1800, pixnum=235){
  endyear<-(endyear.set-900) #adjusts to modelm timeseries; run starts at 900, so 900 = 1, 1800 = 900, etc.
  startyear<-(startyear.set-899)
  excs<-c(((startyear*12)-11):(endyear*12))
  
  master.beta<-master[excs,,]#heh
  eg.beta<-eg.comp.match[excs,]; dc.beta<-dc.comp.match[excs,]
  monthTS.beta<-monthsTS[excs]; months.ts.beta.df<-matrix(data=rep(monthTS.beta, pixnum),ncol=ncol(master))
  yearsTS.beta<-DummyTS[excs];years.ts.beta.df<-matrix(data=rep(yearsTS.beta,pixnum),ncol=ncol(master))
  all.beta<-abind(master.beta, dc.beta, eg.beta,months.ts.beta.df, years.ts.beta.df, along=3)
  
  return(all.beta)
}


dat.flat<-list()
for(c in 1:length(DAT)){  #Number of pixels
  for(l in 1:length(DAT[[c]])){ #Number of shifts in each pixel
    
    dat.ex<-data.frame(DAT[[c]][l])
    dat.flat<-c(dat.flat, list(dat.ex))
    
  }
}

plotlist<-list()
xax<-list(nticks = 10,range = c(0,9))
if(model=="jules"){yax<-list(nticks = 8,range = c(100,800))};if(model=='ed'){yax<-list(nticks = 8,range = c(0,40))}
zax<-list(nticks = 5,range = c(0,0.3))

for (i in (which(goodind==1))){
  dat<-dat.flat[[i]]
  
  
  plotr<-data.frame(aggregate(dat[dat$month%in%c(6:8),], by=list(dat$year[dat$month%in%c(6:8)]), FUN='mean')[2:ncol(dat)]);colnames(plotr)<-c("alb",'lai','swe','sm','prec','temp','eg','dc', 'yr')
  plotlist[[i]]<-plot_ly(plotr, x= ~lai, y= ~sm,z= ~alb,mode="markers", color = ~dc, colors=c("purple4","antiquewhite","forest green"))%>%add_markers()%>%layout(title=paste(min(plotr$yr),"-",max(plotr$yr),  i),scene=list(xaxis=xax, yaxis=yax, zaxis=zax))
  
}
plotlist


#winter
plotlist<-list()
xax<-list(nticks = 10,range = c(0,9))
yax<-list(nticks = 8,range = c(0,300))
zax<-list(nticks = 5,range = c(0,0.5))

for (i in (which(goodind==1))){
  dat<-dat.flat[[i]]
  
  
  plotr<-data.frame(aggregate(dat[dat$month%in%c(1:2,12),], by=list(dat$year[dat$month%in%c(1:2, 12)]), FUN='mean')[2:ncol(dat)]);colnames(plotr)<-c("alb",'lai','swe','sm','prec','temp','eg','dc', 'yr')
  plotlist[[i]]<-plot_ly(plotr, x= ~lai, y= ~swe,z= ~alb,mode="markers", color = ~dc, colors=c("purple4","antiquewhite","forest green"))%>%add_markers()%>%layout(title=paste(min(plotr$yr),"-",max(plotr$yr),  i),scene=list(xaxis=xax, yaxis=yax, zaxis=zax))
  
}
plotlist


#overwrite with full climspace

plotlist<-list()
xax<-list(nticks = 10,range = c(0,9))
if(model=="jules"){yax<-list(nticks = 8,range = c(100,800))};if(model=='ed'){yax<-list(nticks = 8,range = c(0,40))}
zax<-list(nticks = 5,range = c(0,0.3))

for(i in(which(goodind==1))){

dat<-dat.flat[[i]]
plotr.shift<-data.frame(dat[dat$month%in%c(6:8),]);colnames(plotr.shift)<-c("alb",'lai','swe','sm','prec','temp','eg','dc', 'yr')


all.beta<-extract.beta(startyear.set=min(dat$year), endyear.set=max(dat$year), pixnum=166)
all.beta.mega<-matrix(data=all.beta, nrow=dim(all.beta)[1]*dim(all.beta)[2], ncol=dim(all.beta)[3])

samprow<-sample(1:nrow(all.beta.mega),10000)
all.beta.sample<-all.beta.mega[samprow,]


plotr<-data.frame(all.beta.sample[(which(all.beta.sample[,9]%in%c(6:8))),]);colnames(plotr)<-c("alb",'lai','swe','sm','temp','prec','dc','eg', 'mo','yr')
plotlist[[i]]<-plot_ly(plotr)%>%
  add_trace(x= ~lai, y= ~sm,z= ~alb,mode="markers", type='scatter3d', color = ~dc, colors=c("purple4","antiquewhite","forest green"))%>%
  add_trace(data=plotr.shift, x= ~lai, y= ~sm,z= ~alb,type='scatter3d', mode='markers')%>%
  layout(title=paste(min(plotr$yr),"-",max(plotr$yr),  i),scene=list(xaxis=xax, yaxis=yax, zaxis=zax))

}

plotlist


#and again for winter
plotlist<-list()
xax<-list(nticks = 10,range = c(0,7))
yax<-list(nticks = 15,range = c(0,700))
zax<-list(nticks = 5,range = c(0,0.5))

for(i in(which(goodind==1))){
  
  dat<-dat.flat[[i]]
  plotr.shift<-data.frame(dat[dat$month%in%c(1:2,12),]);colnames(plotr.shift)<-c("alb",'lai','swe','sm','prec','temp','eg','dc', 'yr')
  
  
  all.beta<-extract.beta(startyear.set=min(dat$year), endyear.set=max(dat$year), pixnum=235)
  all.beta.mega<-matrix(data=all.beta, nrow=dim(all.beta)[1]*dim(all.beta)[2], ncol=dim(all.beta)[3])
  
  samprow<-sample(1:nrow(all.beta.mega),10000)
  all.beta.sample<-all.beta.mega[samprow,]
  
  
  plotr<-data.frame(all.beta.sample[(which(all.beta.sample[,9]%in%c(1:2, 12))),]);colnames(plotr)<-c("alb",'lai','swe','sm','temp','prec','dc','eg', 'mo','yr')
  plotlist[[i]]<-plot_ly(plotr)%>%
    add_trace(x= ~lai, y= ~swe,z= ~alb,mode="markers", type='scatter3d', color = ~dc, colors=c("purple4","antiquewhite","forest green"))%>%
    add_trace(data=plotr.shift, x= ~lai, y= ~swe,z= ~alb,type='scatter3d', mode='markers')%>%
    layout(title=paste(min(plotr$yr),"-",max(plotr$yr),  i),scene=list(xaxis=xax, yaxis=yax, zaxis=zax))
  
}

plotlist


#Deciduous axis

#summer
plotlist<-list()
xax<-list(nticks = 10,range = c(0,7))
yax<-list(nticks = 5,range = c(0,1))
zax<-list(nticks = 5,range = c(0,0.5))

for(i in(which(goodind==1))){
  
  dat<-dat.flat[[i]]
  plotr.shift<-data.frame(dat[dat$month%in%c(6:8),]);colnames(plotr.shift)<-c("alb",'lai','swe','sm','prec','temp','eg','dc', 'yr')
  
  
  all.beta<-extract.beta(startyear.set=min(dat$year), endyear.set=max(dat$year), pixnum=166)
  all.beta.mega<-matrix(data=all.beta, nrow=dim(all.beta)[1]*dim(all.beta)[2], ncol=dim(all.beta)[3])
  
  samprow<-sample(1:nrow(all.beta.mega),10000)
  all.beta.sample<-all.beta.mega[samprow,]
  
  
  plotr<-data.frame(all.beta.sample[(which(all.beta.sample[,9]%in%c(6:8))),]);colnames(plotr)<-c("alb",'lai','swe','sm','temp','prec','dc','eg', 'mo','yr')
  plotlist[[i]]<-plot_ly(plotr)%>%
    add_trace(x= ~lai, y= ~dc,z= ~alb,mode="markers", type='scatter3d', color = ~dc, colors=c("purple4","antiquewhite","forest green"))%>%
    add_trace(data=plotr.shift, x= ~lai, y= ~dc,z= ~alb,type='scatter3d', mode='markers')%>%
    layout(title=paste(min(plotr$yr),"-",max(plotr$yr),  i),scene=list(xaxis=xax, yaxis=yax, zaxis=zax))
  
}

plotlist


#and again for winter
plotlist<-list()
xax<-list(nticks = 10,range = c(0,7))
yax<-list(nticks = 5,range = c(0,1))
zax<-list(nticks = 5,range = c(0,0.5))

for(i in(which(goodind==1))){
  
  dat<-dat.flat[[i]]
  plotr.shift<-data.frame(dat[dat$month%in%c(1:2,12),]);colnames(plotr.shift)<-c("alb",'lai','swe','sm','prec','temp','eg','dc', 'yr')
  
  
  all.beta<-extract.beta(startyear.set=min(dat$year), endyear.set=max(dat$year), pixnum=235)
  all.beta.mega<-matrix(data=all.beta, nrow=dim(all.beta)[1]*dim(all.beta)[2], ncol=dim(all.beta)[3])
  
  samprow<-sample(1:nrow(all.beta.mega),10000)
  all.beta.sample<-all.beta.mega[samprow,]
  
  
  plotr<-data.frame(all.beta.sample[(which(all.beta.sample[,9]%in%c(1:2, 12))),]);colnames(plotr)<-c("alb",'lai','swe','sm','temp','prec','dc','eg', 'mo','yr')
  plotlist[[i]]<-plot_ly(plotr)%>%
    add_trace(x= ~lai, y= ~dc,z= ~alb,mode="markers", type='scatter3d', color = ~dc, colors=c("purple4","antiquewhite","forest green"))%>%
    add_trace(data=plotr.shift, x= ~lai, y= ~dc,z= ~alb,type='scatter3d', mode='markers')%>%
    layout(title=paste(min(plotr$yr),"-",max(plotr$yr),  i),scene=list(xaxis=xax, yaxis=yax, zaxis=zax))
  
}

plotlist



#Sys.setenv("plotly_username"="Bblakely6")
#Sys.setenv("plotly_api_key"="ID4BSGf5Px1spZKEQ47a")




