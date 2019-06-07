library(beepr)

#Pull compositon
fcomp<-readRDS('ED2/ED2.fcomp.rds')
fcomp.slice<-fcomp[,,9]
goodpix<-which(fcomp.slice[1,]!=-9999)
fcomp.good<-fcomp[,goodpix,]
rm('fcomp')

#Pull georef
georef.name<-load('PalEON_siteInfo_all.RData')
georef<-paleon
goodlon<-georef$lon[goodpix]
goodlat<-georef$lat[goodpix]

npix<-length(goodpix) #To use later for easier subsetting

#Years and months
DummyTS<-rep(900:1999, each=12)
monthsTS<-rep(1:12, 1100)

#DC and EG PFTs
dc.comp<-fcomp.good[,,9:11]
dc.comp<-apply(dc.comp,c(1:2),FUN=sum, na.rm=TRUE)

eg.comp<-fcomp.good[,,c(6,8)]
eg.comp<-apply(eg.comp,c(1:2),FUN=sum, na.rm=TRUE)

notree.comp<-fcomp.good[,,c(1:5,7,12:17)]
notree.comp<-apply(notree.comp,c(1:2),FUN=sum, na.rm=TRUE)
rm('fcomp.good')

dc.comp.trim<-dc.comp[dc.comp[,1]!=-9999,] #Remove fill values
eg.comp.trim<-eg.comp[eg.comp[,1]!=-9999,]
nt.comp.trim<-notree.comp[notree.comp[,1]!=-9999,]

#Trim timeseries to 900 - 2000 
dc.comp.match<-dc.comp.trim[601:13800,]  #12*50 years before 900 = 600 ; 2000 - 850 = year 1150 of timeseries *12 = 13800
eg.comp.match<-eg.comp.trim[601:13800,]
nt.comp.match<-nt.comp.trim[601:13800,]

#Remove excess large objects
rm('dc.comp.trim','eg.comp.trim','notree.comp', 'dc.comp','eg.comp')

#Remove notree objects not using now
rm('nt.comp.trim','nt.comp.match')


#Variable read/clean loop
var.want<-c('SW_albedo','LAI', 'SWE', 'SoilMoist','tair','precipf')
#c('SW_albedo','swdown','LWnet', 'lwdown', 'Qh', 'Qle','LAI','SWE','evap','tair','precipf','SoilMoist','qair','wind', 'transp','SnowDepth')
varchunk<-list()

for (v in 1:length(var.want)){
  name<-paste('ED2/ED2.',var.want[v],'.rds', sep='')
  datvar<-readRDS(name)
  
  if(length(dim(datvar))==3 & var.want[v]!="SoilMoist"){  #If PFT level variable, collapse to whole grid cell
    datvar<-apply(datvar,c(1:2),FUN=sum, na.rm=TRUE)
    #beep(7)
  }
  if(var.want[v]=="SoilMoist"){datvar<-datvar[,,12]} #For soil moisture, only want top layer since that's what would affect albedo

  datvar.good<-datvar[,goodpix]
  rm('datvar')
  
  datvar.trim<-datvar.good[datvar.good[,1]!=-9999,]
  rm('datvar.good')
  
  datvar.match<-datvar.trim[601:13800,]
  
  datvar.df<-data.frame(datvar.match)
  rm('datvar.match')
  
  varchunk[[v]]<-datvar.df
  print(v)
}

names(varchunk)<-var.want


master<-array(unlist(varchunk), c(nrow(varchunk[[1]]),npix,length(var.want)))

#exploratory plots
library(abind)
library(plotly)
library(scatterplot3d)
startyear.set<-1700; endyear.set<-1800 #actual years you want

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

all.beta<-extract.beta(startyear.set=1300, endyear.set=1400)
#227 is a good pixel

plotr<-data.frame(all.beta[(which(all.beta[,1,9]%in%c(6:8))) ,227,]);colnames(plotr)<-c("alb",'lai','swe','sm','temp','prec','dc','eg', 'mo','yr')
plot_ly(plotr, x= ~lai, y= ~sm,z= ~alb,mode="markers", color = ~dc, colors=c("purple4","antiquewhite","forest green"))%>%add_markers()

fcol<-colorRampPalette(c("purple4",'antiquewhite', 'forest green'))
scatterplot3d(plotr$lai,plotr$sm,plotr$alb, angle=230, color=fcol(length(plotr$alb))[order(order(plotr$dc))], pch=16)

#100 year chunks of this cell
# start<-seq(from=900, to=1800, by=100); end<-seq(from=1000, to=1900, by=100); pixnum<-12
# par(mfrow=c(2,2))
# for(i in 1:12){
#   chunk<-extract.beta(start[i],end[i])
#   geolab<-paste("(",goodlat[pixnum],",",goodlon[pixnum],")", sep='')
#   plotr<-data.frame(chunk[(which(chunk[,1,7]%in%c(6:8))),pixnum,]);colnames(plotr)<-c("alb",'lai','swe','sm','dc','eg', 'mo')
#   scatterplot3d(plotr$lai,plotr$sm,plotr$alb, angle=220, xlim=c(0,10),ylim=c(0,50),color=fcol(length(plotr$alb))[order(order(plotr$dc))], pch=16, main=paste(geolab, ";", start[i]))
#   
# }

#Finding dry/warm/wet/cool places

all.precip<-all.beta[,,which(colnames(plotr)=='prec')]; all.temp<-all.beta[,,which(colnames(plotr)=='temp')]

#ann.precip<-aggregate(all.precip, by=list(all.beta[,1,10]),FUN='mean')
typ.precip<-colMeans(all.precip); typ.temp<-colMeans(all.temp)

#top 10% dry, warm

drwm<-which(typ.precip<quantile(typ.precip, 0.25)&typ.temp>quantile(typ.temp, 0.75))
wtcl<-which(typ.precip>quantile(typ.precip, 0.75)&typ.temp<quantile(typ.temp, 0.25))

drcl<-which(typ.precip<quantile(typ.precip, 0.25)&typ.temp<quantile(typ.temp, 0.25))
wtwm<-which(typ.precip>quantile(typ.precip, 0.75)&typ.temp>quantile(typ.temp, 0.75))

ind<-drcl




par(mfrow=c(2,2))
for(i in 1:length(ind)){
plotr<-data.frame(all.beta[(which(all.beta[,1,9]%in%c(6:8))) ,ind[i],]);colnames(plotr)<-c("alb",'lai','swe','sm','temp','prec','dc','eg', 'mo','yr')
scatterplot3d(plotr$lai,plotr$sm,plotr$alb, ylim=c(0, 40), xlim=c(0,9), angle=220, color=fcol(length(plotr$alb))[order(order(plotr$dc))], pch=16, main=paste(i, ind[i]))
}


#3, 14 -> dry and cold

plotr<-data.frame(all.beta[(which(all.beta[,1,9]%in%c(6:8))) ,ind[4],]);colnames(plotr)<-c("alb",'lai','swe','sm','temp','prec','dc','eg', 'mo','yr')
plot_ly(plotr, x= ~lai, y= ~sm,z= ~alb,mode="markers", color = ~dc, colors=c("purple4","antiquewhite","forest green"))%>%add_markers()




