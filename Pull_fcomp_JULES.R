library('beepr')

fcomp<-readRDS('TRIFFID/TRIFFID.fcomp.rds')

fcomp.slice<-fcomp[,,1]
goodpix<-which(fcomp.slice[1,]!=-9999)

georef.name<-load('PalEON_siteInfo_all.RData')
georef<-paleon

goodlon<-georef$lon[goodpix]
goodlat<-georef$lat[goodpix]

npix<-length(goodpix) #To use later for easier subsetting

fcomp.good<-fcomp[,goodpix,]

rm('fcomp')

dc.comp<-fcomp.good[,,1]   #This is the same as master.series.dc from ED extractions
eg.comp<-fcomp.good[,,2]      #This is the same as master.series.eg from ED

notree.comp<-fcomp.good[,,3:5]
notree.comp<-apply(notree.comp,c(1:2),FUN=sum, na.rm=TRUE)

rm('fcomp.good')

#Trim timeseries to same length as ED
dc.comp.trim<-dc.comp[dc.comp[,1]!=-9999,] #Remove fill values
eg.comp.trim<-eg.comp[eg.comp[,1]!=-9999,]
nt.comp.trim<-notree.comp[notree.comp[,1]!=-9999,]
#Trim to 900 - 2000

dc.comp.match<-dc.comp.trim[601:13800,]  #12*50 years before 900 = 600 ; 2000 - 850 = year 1150 of timeseries *12 = 13800
eg.comp.match<-eg.comp.trim[601:13800,]
nt.comp.match<-nt.comp.trim[601:13800,]

#Remove excess large objects
rm('dc.comp.trim','eg.comp.trim','nt.comp.trim', 'dc.comp','eg.comp', 'notree.comp')

###The following code is adapted from ED extractions####

DummyTS<-rep(900:1999, each=12)

monthsTS<-rep(1:12, 1100)


#Aggregate to annual
#Option 1 - full year
dc.agg<-aggregate(dc.comp.match, by=list(DummyTS), FUN=mean)[,2:(npix+1)] 
eg.agg<-aggregate(eg.comp.match, by=list(DummyTS), FUN=mean)[,2:(npix+1)]
nt.agg<-aggregate(nt.comp.match, by=list(DummyTS), FUN=mean)[,2:(npix+1)]

# #Option 2 - growing season
# YearTS.gs<-rep(900:1999, each=5)
# dc.gs<-dc.comp.match[monthsTS>4 & monthsTS<10,]
# eg.gs<-eg.comp.match[monthsTS>4 & monthsTS<10,]
# 
# dc.agg<-aggregate(dc.gs, by=list(YearTS.gs), FUN=mean)[,2:(npix+1)] 
# eg.agg<-aggregate(eg.gs, by=list(YearTS.gs), FUN=mean)[,2:(npix+1)]
# 

ann.ts<-c(900:1999)
regimeshift<-rep(0,npix)

dom<-0.6  #Sets cutoff for EG or DC dominance; prevents consistently mixed forests from getting into the list
ndom<-1-dom
domlength<-10 #number of years of dominance needed

for(c in 1:npix){
  rg<-range(dc.agg[,c])
  if(rg[1]< ndom & rg[2] > dom){  #rg[1]< 0.5 & rg[2]>0.5 &Must (1) cross the 0.5 point, i.e. change dominance and (2) cross it far enough to be ecologically dominant
    regimeshift[c]<-1
    plot(dc.agg[,c]~ann.ts, type='l', col='green', ylim=c(0,1), main=c, sep=', ')#paste(goodlat[c],goodlon[c], sep=', ')
    lines(eg.agg[,c]~ann.ts,col='purple')
    lines(nt.agg[,c]~ann.ts, col='orange')
  }
  
}


length(which(regimeshift==1))
coords<-cbind(goodlat[regimeshift==1], goodlon[regimeshift==1])

dc.shift.df<-dc.agg[,regimeshift==1]
eg.shift.df<-eg.agg[,regimeshift==1]


varset<-list()


#Big plotting loop

for (g in (1:nrow(coords))){
  dom<-which(dc.shift.df[,g] > 0.6)  #Times of deciduous dominance
  ndom<-which(eg.shift.df[,g]> 0.6)  #Times of EG dominance
  #mid<-which(dc.shift.df[,g]<0.6 & dc.shift.df[,g]>0.4) #inbetween
  
  endEG<-ndom[c(which(diff(ndom)>10),length(ndom))]
  begEG<-c(ndom[c(1, which(diff(ndom)>10)+1)], 9998)
  
  endDC<-c(dom[c(which(diff(dom)>10),length(dom))], 9999)
  begDC<-dom[c(1, which(diff(dom)>10)+1)]
  
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
  
  #shifts[shifts$cyc.length>200 | shifts$cyc.length<50,]<-999
  shifts[shifts$cyc.length==0,]<-0
  
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
  #if (f==length(files)){beep(8)}
}
  
varset->varset.jules
