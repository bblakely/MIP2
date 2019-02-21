library(beepr)

fcomp<-readRDS('ED2/ED2.fcomp.rds')

fcomp.slice<-fcomp[,,9]
goodpix<-which(fcomp.slice[1,]!=-9999)

georef.name<-load('PalEON_siteInfo_all.RData')
georef<-paleon

goodlon<-georef$lon[goodpix]
goodlat<-georef$lat[goodpix]

npix<-length(goodpix) #To use later for easier subsetting

fcomp.good<-fcomp[,goodpix,]

rm('fcomp')

dc.comp<-fcomp.good[,,9:11]
dc.comp<-apply(dc.comp,c(1:2),FUN=sum, na.rm=TRUE)

eg.comp<-fcomp.good[,,c(6,8)]   #This is the same as master.series.eg from ED
eg.comp<-apply(eg.comp,c(1:2),FUN=sum, na.rm=TRUE)

notree.comp<-fcomp.good[,,c(1:5,7,12:17)]
notree.comp<-apply(notree.comp,c(1:2),FUN=sum, na.rm=TRUE)
rm('fcomp.good')


#Trim timeseries to same length as ED
dc.comp.trim<-dc.comp[dc.comp[,1]!=-9999,] #Remove fill values
eg.comp.trim<-eg.comp[eg.comp[,1]!=-9999,]
nt.comp.trim<-notree.comp[notree.comp[,1]!=-9999,]
#Trim to 900 - 2000 
#CHECK THIS
dc.comp.match<-dc.comp.trim[601:13800,]  #12*50 years before 900 = 600 ; 2000 - 850 = year 1150 of timeseries *12 = 13800
eg.comp.match<-eg.comp.trim[601:13800,]
nt.comp.match<-nt.comp.trim[601:13800,]

#Remove excess large objects
rm('dc.comp.trim','eg.comp.trim','notree.comp', 'dc.comp','eg.comp')

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

dom<-0.60  #Sets cutoff for EG or DC dominance; prevents consistently mixed forests from getting into the list
ndom<-1-dom
domlength<-10 #number of years of dominance needed
allowgap<-5


for(c in 1:npix){
  rg<-range(dc.agg[,c])
  if(rg[1]< ndom & rg[2] > dom){  #rg[1]< 0.5 & rg[2]>0.5 &Must (1) cross the 0.5 point, i.e. change dominance and (2) cross it far enough to be ecologically dominant
    regimeshift[c]<-1
    plot(dc.agg[,c]~ann.ts, type='l', col='green', ylim=c(0,1), main=paste(goodlat[c],goodlon[c], sep=', '))
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
  dom<-which(dc.shift.df[,g] >= 0.6)  #Times of deciduous dominance
  ndom<-which(eg.shift.df[,g]>= 0.6)  #Times of EG dominance
  #mid<-which(dc.shift.df[,g]<0.6 & dc.shift.df[,g]>0.4) #inbetween
  
  #Bounding periods of dominance:
  #endEG pulls the last year of dominance for each period of dominance
  #diff(ndom)is the # of years since the last eg dominance. When diff(ndom) is > 10, there's been a 10+ year gap in dominance prior to that point
  #diff() clips the first value, so which(diff...) will actually pull the last year of dominance *before* the gap, aka the last year of dominance
  #begEG does the opposite. The +1 'corrects' for the diff() clipping, so this will catch the first year of dominance *after* a gap, aka the first year of the next period of dominance
  
  endEG<-ndom[c(which(diff(ndom)>allowgap),length(ndom))]   #the length(ndom) catches the last moment of dominance in the series
  begEG<-c(ndom[c(1, which(diff(ndom)>allowgap)+1)], 9998)  #the 1, like length(ndom), catches the first moment of dominance in the series

  endDC<-c(dom[c(which(diff(dom)>allowgap),length(dom))], 9999) #Same thing here, but for deciduous
  begDC<-dom[c(1, which(diff(dom)>allowgap)+1)]
  
  DClen<-endDC[1:(length(endDC)-1)]-begDC;EGlen<-endEG-begEG[1:(length(begEG)-1)]
  
  begDC<-begDC[DClen>=domlength]; endDC<-c(endDC[DClen>=domlength], 9999)
  begEG<-c(begEG[EGlen>=domlength], 9998); endEG<-endEG[EGlen>=domlength]
  #begEG[EGlen<domlength]<-NA; endEG[EGlen<domlength]<-NA
  
  #Pulling good starts and ends
  if((length(begEG)-1)>0){
  goodstarts<-rep(0,length(begEG)-1)
  goodends<-rep(0, length(begEG)-1)
  for (e in (1:(length(begEG)-1))){ #e indexes individual periods of evergreen dominance
    cand<-begEG[e] #candidate start of composition shift; a shift is an eg->dc transition, so it starts with the beginning of evergreen dominance
    nextEG<-begEG[min(which(begEG>cand))]
    if(is.na(nextEG)){nextEG<-9998}
    
    finDC<-endDC[min(which(endDC>cand))] #end of the shift, the end of a period of deciduous dominance that is *after* a period of evergreen dominance (>cand)
    if(is.na(finDC)){finDC<-9999}
    
    #uses only closest period of evergreen dominance to the period of deciduous dominance 
    #without this, centuries of flips between evergreen and mixed could all be included in the starting point of the shift
    
    if(finDC<nextEG){    #Test: is the end of the period of deciduous dominance *before* the next period of evergreen dominance?
      goodstarts[e]<-cand #If so, record. If not, skip this period of evergreen dominance and go to the next one
      goodends[e]<-finDC
    }
  }
  
  cyc.length<-goodends-goodstarts #How long does the shift need to occur?
  
  shifts<-data.frame(cbind(goodstarts, goodends))
  shifts$cyc.length<-cyc.length
  shifts$lat<-rep(coords[g,1], nrow(shifts))
  shifts$lon<-rep(coords[g,2], nrow(shifts))
  
  shifts[shifts$cyc.length>200 | shifts$cyc.length<50,]<-999 #If the shift takes too long (>200 years) or not long enough (<50 years), remove.
  shifts[shifts$cyc.length==0,]<-0
  
  shifts1<-shifts[(shifts[,1]!=999 & shifts[,1]!= 0),]
  
  varset[[g]]<-shifts1
  
  colvec<-rep('black',nrow(dc.shift.df))
  colvec[dom]<-'forest green'
  colvec[ndom]<-'light green'
  
  plot(dc.shift.df[,g], col=colvec, pch='.', cex=5,ylim=c(0,1), main=g); lines(dc.shift.df[,g], lwd=0.5)
  abline(v=shifts1$goodstarts, col='purple4')
  abline(v=shifts1$goodends, col='dark gray')
  #abline(v=dom[c(which(diff(dom)>20),length(dom))], col='dark gray')  #end of dc dominance
  #abline(v=dom[c(1, which(diff(dom)>20)+1)], col='forest green')      #start of dc dominance
  #abline(v=ndom[c(which(diff(ndom)>20),length(ndom))], col='dark gray') #end of EG dominance
  #abline(v=ndom[c(1, which(diff(ndom)>20)+1)], col='purple4')     #start of EG dominance
  #beep(2)
  #if (f==length(files)){beep(8)}
  }
}

varset->varset.ed
