library(ncdf4)
library (beepr)

files<-list.files(pattern='.nc')

master.series.dc<-matrix(NA,0,40)  #Empty matrices for whole timeseries
master.series.eg<-matrix(NA,0,40)

for (f in 1:length(files)){

file<-files[f]
ex.open<-nc_open(file)  #open the file. Takes some time

lon<-ncvar_get(ex.open, "lon")
lat<-ncvar_get(ex.open, "lat")

time<-ncvar_get(ex.open, "time")
vars<-names(ex.open$var)

#alb<-ncvar_get(ex.open, "SW_albedo")
comp<-ncvar_get(ex.open, "Fcomp")  #also takes some time

coords.lon<-matrix(data=rep(lon,30),nrow=80,ncol=30)
coords.lat<-matrix(data=rep(lat,each=80),nrow=80,ncol=30)

goodpix<-which(!is.na(comp[,,6,1]), arr.ind=T) #where is comp @ all lons, all lats, PFT6, and timestep 1 real data
goodpix.dumb<-which(!is.na(comp[,,6,1]))  #Same thing, but leveraging R's way of indexing matrices as vectors; counts down column 1 then moves onto column 2 and counts down that... 

goodlat<-coords.lat[goodpix.dumb]  
goodlon<-coords.lon[goodpix.dumb]


comp.df<-array(data=9999,dim=c(1200,nrow(goodpix),17))  #Empty matrix with each column being a good grid cell, each row a timepoint and each slice a PFT
for(i in 1:nrow(goodpix)){
  #plot(comp[unname(goodpix[i,1]), unname(goodpix[i,2]),], type='l')
  comp.df[,i,]<-t(comp[unname(goodpix[i,1]), unname(goodpix[i,2]),,])  #Crazy indexing

}

rm(comp) #comp is huge and we've pulled what we want (pixels with good data) so rm it to free memory

comp.eg<-comp.df[,,c(6,8)]  #Slices 6 & 8 are EG PFTs
comp.dc<-comp.df[,,9:11]    #Slices 9 thru 11 are deciduous PFTs

comp.dc.sweep<-apply(comp.dc,(1:2),FUN=sum, na.rm=TRUE)  #Adds proportions of EG PFTs
comp.eg.sweep<-apply(comp.eg,c(1:2),FUN=sum, na.rm=TRUE) #Same for DC PFTs

master.series.dc<-rbind(master.series.dc, comp.dc.sweep)  #Stick this century onto previous centuries
master.series.eg<-rbind(master.series.eg, comp.eg.sweep)

print(f)
nc_close(ex.open)
}



##Plotting to find compshift pixels

DummyTS<-rep(900:1999, each=12)
#aggregate to annual
dc.agg.2<-aggregate(master.series.dc, by=list(DummyTS), FUN=mean)[,2:41] #Should I use summer values here? Biomass of DC drops each winter
eg.agg.2<-aggregate(master.series.eg, by=list(DummyTS), FUN=mean)[,2:41]


ann.ts<-c(900:1999)
regimeshift<-rep(0,40)

dom<-0.6  #Sets cutoff for EG or DC dominance; prevents consistently mixed forests from getting into the list
ndom<-1-dom

for(c in 1:40){
  rg<-range(dc.agg.2[,c])
  if(rg[1]< ndom & rg[2] > dom){  #rg[1]< 0.5 & rg[2]>0.5 &Must (1) cross the 0.5 point, i.e. change dominance and (2) cross it far enough to be ecologically dominant
    regimeshift[c]<-1
    plot(dc.agg.2[,c]~ann.ts, type='l', col='green', ylim=c(0,1), main=paste(goodlat[c],goodlon[c], sep=', '))
    lines(eg.agg.2[,c]~ann.ts,col='purple')
  }
  
}

length(which(regimeshift==1))

#pixels where a shift happens at least once
coords<-cbind(goodlat[regimeshift==1], goodlon[regimeshift==1])

dc.shift.df<-dc.agg.2[,regimeshift==1]
eg.shift.df<-eg.agg.2[,regimeshift==1]


#rm('master.series.dc','master.series.eg')
varset<-list()
for (g in (1:nrow(coords))){
  dom<-which(dc.shift.df[,g] > 0.6)  #Times of deciduous dominance
  ndom<-which(dc.shift.df[,g]< 0.4)  #Times of EG dominance
  mid<-which(dc.shift.df[,g]<0.6 & dc.shift.df[,g]>0.4) #inbetween
  
  endEG<-ndom[c(which(diff(ndom)>20),length(ndom))]
  begEG<-c(ndom[c(1, which(diff(ndom)>20)+1)], 9998)
  
  endDC<-c(dom[c(which(diff(dom)>20),length(dom))], 9999)
  begDC<-dom[c(1, which(diff(dom)>20)+1)]
  
  #Pulling good starts and ends
  goodstarts<-rep(0,length(begEG)-1)
  goodends<-rep(0, length(begEG)-1)
  for (e in (1:length(begEG)-1)){
    cand<-begEG[e]
    nextEG<-begEG[min(which(begEG>cand))]
    if(is.na(nextEG)){nextEG<-9998}
    
    nextDC<-endDC[min(which(endDC>cand))]
    if(is.na(nextDC)){nextDC<-9999}
    
    if(nextDC<nextEG){
      goodstarts[e]<-cand
      goodends[e]<-nextDC
    }
  }
  
  
  cyc.length<-goodends-goodstarts
  
  shifts<-data.frame(cbind(goodstarts, goodends))
  shifts$cyc.length<-cyc.length
  shifts$lat<-rep(coords[g,1], nrow(shifts))
  shifts$lon<-rep(coords[g,2], nrow(shifts))
  
  shifts[shifts$cyc.length>200 | shifts$cyc.length<50,]<-999
  shifts[shifts$cyc.length==0]<-0
  
  shifts1<-shifts[(shifts[,1]!=999 & shifts[,1]!= 0),]

  varset[[g]]<-shifts1
  
  colvec<-rep('black',nrow(dc.shift.df))
  colvec[dom]<-'forest green'
  colvec[ndom]<-'light green'
  
  plot(dc.shift.df[,g], col=colvec, pch='.', cex=5,ylim=c(0,1), main=g)
  abline(v=shifts1$goodstarts, col='purple4')
  abline(v=shifts1$goodends, col='dark gray')
  #abline(v=dom[c(which(diff(dom)>20),length(dom))], col='dark gray')  #end of dc dominance
  #abline(v=dom[c(1, which(diff(dom)>20)+1)], col='forest green')      #start of dc dominance
  #abline(v=ndom[c(which(diff(ndom)>20),length(ndom))], col='dark gray') #end of EG dominance
  #abline(v=ndom[c(1, which(diff(ndom)>20)+1)], col='purple4')     #start of EG dominance
  beep(2)
  if (f==length(files)){beep(8)}
  }
  



